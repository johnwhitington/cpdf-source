open Pdfutil

(* We allow \n in titles. Split for typesetter. *)
let rec split_toc_title_inner a = function
  | '\\'::'n'::r -> rev a :: split_toc_title_inner [] r
  | x::xs -> split_toc_title_inner (x::a) xs
  | [] -> [rev a]

let split_toc_title = split_toc_title_inner []

(* And for new bookmark for TOC, change \\n to \n *)
let rec real_newline = function
  | '\\'::'n'::r -> '\n'::real_newline r
  | x::r -> x::real_newline r
  | [] -> []

let width_table_cache = null_hash ()

let rec width_of_runs runs =
  match runs with
  | Cpdftype.Font (id, f, fontsize)::Cpdftype.Text t::more ->
      let width_table =
        match Hashtbl.find width_table_cache (id, fontsize) with
        | w -> w
        | exception Not_found ->
            let ws = Cpdftype.font_widths id f fontsize in Hashtbl.add width_table_cache (id, fontsize) ws; ws
      in
        Cpdftype.width_of_string width_table t +. width_of_runs more
  | [] -> 0.
  | _ -> failwith "width_of_runs"

(* Run of Font / Text elements from a fontpack and UTF8 text *)
let of_utf8 fontpack fontsize t =
  let codepoints = Pdftext.codepoints_of_utf8 t in
  let fonted = option_map (Cpdfembed.get_char fontpack) codepoints in
  let collated = Cpdfembed.collate_runs fonted in
    flatten
      (map
        (function
         | [] -> []
         | (_, n, font) as h::t ->
             let charcodes = map (fun (c, _, _) -> char_of_int c) (h::t) in
               [Cpdftype.Font (string_of_int n, font, fontsize); Cpdftype.Text charcodes])
        collated)

(* Cpdftype codepoints from a font and PDFDocEndoding string *)
let of_pdfdocencoding fontpack fontsize t =
  of_utf8 fontpack fontsize (Pdftext.utf8_of_pdfdocstring t)

(* Remove characters until it is below the length. Then remove three more and
   add dots for an ellipsis *)
let rec shorten_text_inner l t =
  match rev t with
  | Cpdftype.Text text::Cpdftype.Font (id, f, fs)::more ->
      let width_table =
        match Hashtbl.find width_table_cache (id, fs) with
        | w -> w
        | exception Not_found ->
            let ws = Cpdftype.font_widths id f fs in Hashtbl.add width_table_cache (id, fs) ws; ws
      in
      if Cpdftype.width_of_string width_table text > l then
        shorten_text_inner l (rev (Cpdftype.Text (all_but_last text)::Cpdftype.Font (id, f, fs)::more))
      else
        t
  | _ -> t

let shorten_text fontpack fontsize l t =
  let short = shorten_text_inner l t in
    if short = t then t else
      let charcode, dotfontnum, dotfont =
        unopt (Cpdfembed.get_char fontpack (int_of_char '.'))
      in
      let charcode = char_of_int charcode in
        short @ [Cpdftype.Font (string_of_int dotfontnum, dotfont, fontsize); Cpdftype.Text [charcode; charcode; charcode]]

(* Calculate the used codepoints *)
let used pdf fastrefnums labels title marks =
  let codepoints = null_hash () in
  Hashtbl.add codepoints (int_of_char '.') ();
  let addtext t =
    iter
      (fun c -> Hashtbl.replace codepoints c ())
      (Pdftext.codepoints_of_utf8 (Pdftext.utf8_of_pdfdocstring t))
  in
    iter (fun c -> Hashtbl.replace codepoints c ()) (Pdftext.codepoints_of_utf8 title);
    iter
      (fun m ->
        addtext m.Pdfmarks.text;
        let pnum = Pdfpage.pagenumber_of_target ~fastrefnums pdf m.Pdfmarks.target in
        let labeltext =
          try Pdfpagelabels.pagelabeltext_of_pagenumber pnum labels with Not_found -> string_of_int pnum
        in
          addtext labeltext)
      marks;
    codepoints

(* Make a dot leader *)
let make_dots space fontpack fontsize =
  let dotruns = of_utf8 fontpack fontsize "." in
  let dotwidth = width_of_runs dotruns in
  let runs = flatten (many dotruns (int_of_float (floor (space /. dotwidth)))) in
  let remainder = space -. width_of_runs runs in
    [Cpdftype.HGlue remainder] @ runs

(* Prepend structure tree items. FIXME: What to do if not present? Currently we do nothing. *)
let prepend_structitems pdf items =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/K"] with
  | Some (Pdf.Array a) ->
      Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"; "/K"] (Pdf.Array (items @ a))
  | Some (Pdf.Dictionary d) ->
      Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"; "/K"] (Pdf.Array (items @ [Pdf.Dictionary d]))
  | _ -> ()

(* FIXME Would be better with a Pdf.remove_chain *)
let remove_parent_tree_next_key pdf =
  match Pdf.lookup_obj pdf pdf.Pdf.root with
  | Pdf.Dictionary d ->
      begin match lookup "/StructTreeRoot" d with
      | Some (Pdf.Indirect i) ->
          Pdf.addobj_given_num pdf (i, Pdf.remove_dict_entry (Pdf.lookup_obj pdf i) "/ParentTreeNextKey")
      | Some (Pdf.Dictionary d2) ->
          let newstroot = Pdf.remove_dict_entry (Pdf.Dictionary d2) "/ParentTreeNextKey" in
          let newroot = Pdf.add_dict_entry (Pdf.Dictionary d) "/StructTreeRoot" newstroot in
            Pdf.addobj_given_num pdf (pdf.Pdf.root, newroot)
      | _ -> ()
      end
  | _ -> ()

(* FIXME Again relies upon there being an existing structure tree. Fix. *)
(* FIXME Again, replace_chain would be much better here if it could deal with a final indirect. *)
let add_to_parent_tree pdf p =
  match Pdf.lookup_chain pdf (Pdf.lookup_obj pdf pdf.Pdf.root) ["/StructTreeRoot"; "/ParentTree"] with
  | Some tree ->
      let t = Pdftree.read_number_tree pdf tree in
      let n = match t with [] -> 0 | l -> int_of_string (fst (last l)) + 1 in
      let newtree = Pdftree.build_name_tree true pdf ((string_of_int n, p)::t) in
        begin match Pdf.lookup_direct pdf "/StructTreeRoot" (Pdf.lookup_obj pdf pdf.Pdf.root) with
        | Some (Pdf.Dictionary d) ->
            begin match lookup "/ParentTree" d with
            | Some (Pdf.Indirect i) ->
                Pdf.addobj_given_num pdf (i, newtree)
            | _ -> ()
            end
        | _ -> ()
        end;
        n
  | None -> 0

(* Typeset a table of contents with given font, font size and title. Mediabox
   (and CropBox) copied from first page of existing PDF. Margin of 10% inside
   CropBox. Font size of title twice body font size. Null page labels added for
   TOC, others bumped up and so preserved. *)
let typeset_table_of_contents ~font ~fontsize ~title ~bookmark ~dotleader ~process_struct_tree pdf =
  let optional l = if process_struct_tree then l else [] in
  Hashtbl.clear width_table_cache;
  let marks = Pdfmarks.read_bookmarks pdf in
  if marks = [] then (Pdfe.log "No bookmarks, not making table of contents\n"; pdf) else
  let labels = Pdfpagelabels.read pdf in
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
  let codepoints = map fst (list_of_hashtbl (used pdf fastrefnums labels title marks)) in
  let fontpack =
    match font with
    | Cpdfembed.PreMadeFontPack t -> t
    | Cpdfembed.EmbedInfo {fontfile; fontname; encoding} ->
       Cpdfembed.embed_truetype pdf ~fontfile ~fontname ~codepoints ~encoding
    | Cpdfembed.ExistingNamedFont -> raise (Pdf.PDFError "Cannot use existing font with -table-of-contents")
  in
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let width, firstpage_papersize, pmaxx, pmaxy, margin =
    let width, height, xmax, ymax =
      match Pdf.parse_rectangle pdf firstpage.Pdfpage.mediabox with
        xmin, ymin, xmax, ymax -> xmax -. xmin, ymax -. ymin, xmax, ymax
    in
      width, Pdfpaper.make Pdfunits.PdfPoint width height, xmax, ymax, fmin width height *. 0.1
  in
  let firstpage_cropbox =
    match Pdf.lookup_direct pdf "/CropBox" firstpage.Pdfpage.rest with
    | Some r -> Some (Pdf.parse_rectangle pdf r)
    | None -> None
  in
  let width =
    match firstpage_cropbox with
    | Some (xmin, _, xmax, _) -> xmax -. xmin
    | None -> width 
  in
  let lines =
    map
      (fun mark ->
         let indent = float mark.Pdfmarks.level *. fontsize *. 2. in 
         let textruns = of_pdfdocencoding fontpack fontsize mark.Pdfmarks.text in
         let labelruns =
           if mark.Pdfmarks.target = NullDestination then of_pdfdocencoding fontpack fontsize "" else 
           let pnum = Pdfpage.pagenumber_of_target ~fastrefnums pdf mark.Pdfmarks.target in
           let pde = try Pdfpagelabels.pagelabeltext_of_pagenumber pnum labels with Not_found -> string_of_int pnum in
             of_pdfdocencoding fontpack fontsize pde
         in
         let textgap = width -. margin *. 2. -. indent -. width_of_runs labelruns in
         let textruns = shorten_text fontpack fontsize (textgap -. fontsize *. 3.) textruns in
         let space = textgap -. width_of_runs textruns in
         let leader =
           if dotleader && labelruns <> []
             then make_dots space fontpack fontsize
             else [Cpdftype.HGlue space]
         in
             [Cpdftype.BeginDest (mark.Pdfmarks.target, Some mark.Pdfmarks.text); Cpdftype.HGlue indent]
           @ optional [(Cpdftype.Tag ("Link", 0))] @ textruns @  optional [Cpdftype.EndTag]
           @ leader
           @ optional [Cpdftype.Tag ("Link", 0)] @ labelruns @ optional [Cpdftype.EndTag]
           @ [Cpdftype.EndDest; Cpdftype.NewLine])
      (Pdfmarks.read_bookmarks pdf)
  in
  let toc_pages, toc_tags =
    let title =
      let glue = Cpdftype.VGlue (fontsize *. 2.) in
        optional [Cpdftype.Tag ("P", 0)]
        @ flatten
           (map
             (fun l -> l @ [Cpdftype.NewLine])
             (map (of_utf8 fontpack (fontsize *. 2.)) (map implode (split_toc_title (explode title)))))
        @
        optional [Cpdftype.EndTag] @ [glue]
    in
    let lm, rm, tm, bm =
      match firstpage_cropbox with
      | None -> (margin, margin, margin, margin)
      | Some (cminx, cminy, cmaxx, cmaxy) ->
          (cminx +. margin, (pmaxx -. cmaxx) +. margin, cminy +. margin, (pmaxy -. cmaxy) +. margin)
    in
      let firstfont =
        hd (keep (function Cpdftype.Font _ -> true | _ -> false) (title @ flatten lines))
      in
        Cpdftype.typeset ~process_struct_tree lm rm tm bm firstpage_papersize pdf
          ([firstfont; Cpdftype.BeginDocument] @ title @ flatten lines)
  in
  let toc_pages =
    match firstpage_cropbox with
    | Some (a, b, c, d) ->
        let rect =
          Pdf.Array [Pdf.Real a; Pdf.Real b; Pdf.Real c; Pdf.Real d]
        in
          map
            (fun p -> {p with Pdfpage.rest = Pdf.add_dict_entry p.Pdfpage.rest "/CropBox" rect})
            toc_pages
    | None -> toc_pages
  in
  let original_pages = Pdfpage.pages_of_pagetree pdf in
  let toc_pages_len = length toc_pages in
  let changes = map (fun n -> (n, n + toc_pages_len)) (indx original_pages) in
  let pdf = Pdfpage.change_pages ~changes true pdf (toc_pages @ original_pages) in
  let toc_pageobjnums = take (Pdf.page_reference_numbers pdf) toc_pages_len in
  let label =
    {Pdfpagelabels.labelstyle = NoLabelPrefixOnly;
     Pdfpagelabels.labelprefix = None;
     Pdfpagelabels.startpage = 1;
     Pdfpagelabels.startvalue = 1}
  in
  if process_struct_tree then
    begin
      let struct_tree_root =
        match Pdf.lookup_immediate "/StructTreeRoot" (Pdf.lookup_obj pdf pdf.Pdf.root) with
        | Some (Pdf.Indirect i) -> i
        | _ -> 0 (* Will never be written, because we only write if there is an existing tree. To revisit. *)
      in
      let p_struct_elem_first_page =
        Pdf.addobj pdf
          (Pdf.Dictionary [("/S", Pdf.Name "/P");
                           ("/Pg", Pdf.Indirect (hd toc_pageobjnums));
                           ("/K", Pdf.Array [Pdf.Integer 0]);
                           ("/P", Pdf.Indirect struct_tree_root)])
      in
      let mcid = ref 1 in
      let link_struct_elems_for_each_page =
        map2
          (fun page pageobjnum ->
            let annot_objnums =
              match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
              | Some (Pdf.Array a) -> map (function Pdf.Indirect i -> i | _ -> 0) a
              | _ -> []
            in
            let r = map
                (fun annot_i ->
                   let r =
                     let objr = Pdf.addobj pdf (Pdf.Dictionary [("/Type", Pdf.Name "/OBJR"); ("/Obj", Pdf.Indirect annot_i)]) in
                       Pdf.addobj pdf
                         (Pdf.Dictionary [("/S", Pdf.Name "/Link");
                                          ("/K", Pdf.Array [Pdf.Integer !mcid; Pdf.Indirect objr]);
                                          ("/P", Pdf.Indirect struct_tree_root);
                                          ("/Pg", Pdf.Indirect pageobjnum)])
                  in
                    incr mcid; r)
                annot_objnums
            in
              mcid := 0; r)
          toc_pages
          toc_pageobjnums
      in
      let prepending_structitems =
        map (fun x -> Pdf.Indirect x) (p_struct_elem_first_page::flatten link_struct_elems_for_each_page)
      in
      (* Add the key and value structure item (any p, and that page's links) to the parent tree for each TOC page *)
      let toc_structure_items_per_page =
        match link_struct_elems_for_each_page with
        | h::t -> (p_struct_elem_first_page::h)::t
        | [] -> []
      in
        iter2
          (fun o ns ->
             let page = Pdf.lookup_obj pdf o in
             (* For each annotation, add a structparent entry too. *)
             let annot_objnums =
               match Pdf.lookup_direct pdf "/Annots" page with
               | Some (Pdf.Array a) -> map (function Pdf.Indirect i -> i | _ -> 0) a
               | _ -> []
             in
             (* Remove the Title P from first page list *)
             let ns2 = if length ns > length annot_objnums then tl ns else ns in
             iter2
               (fun annot_objnum n ->
                  let annot = Pdf.lookup_obj pdf annot_objnum in
                  let sp_num = add_to_parent_tree pdf (Pdf.Indirect n) in
                  let new_annot = Pdf.add_dict_entry annot "/StructParent" (Pdf.Integer sp_num) in
                  Pdf.addobj_given_num pdf (annot_objnum, new_annot))
               annot_objnums
               ns2;
             let ptn = add_to_parent_tree pdf (Pdf.Array (map (fun x -> Pdf.Indirect x) ns)) in
             Pdf.addobj_given_num pdf (o, Pdf.add_dict_entry page "/StructParents" (Pdf.Integer ptn)))
          toc_pageobjnums
          toc_structure_items_per_page;
      remove_parent_tree_next_key pdf;
      begin match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/K"] with
      | Some (Pdf.Array a) ->
          Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"; "/K"] (Pdf.Array (prepending_structitems @ a))
      | Some (Pdf.Dictionary d) ->
          Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"; "/K"] (Pdf.Array (prepending_structitems @ [Pdf.Dictionary d]))
      | _ ->
          () (* None found. In future, may fabricate. For now, no. *)
      end
    end;
  let labels' = label::map (fun l -> {l with Pdfpagelabels.startpage = l.Pdfpagelabels.startpage + toc_pages_len}) labels in
    Pdfpagelabels.write pdf labels';
    if bookmark then
      let marks = Pdfmarks.read_bookmarks pdf in
      let refnums = Pdf.page_reference_numbers pdf in
      let newmark =
        {Pdfmarks.level = 0;
         Pdfmarks.text = Pdftext.pdfdocstring_of_utf8 (implode (real_newline (explode title)));
         Pdfmarks.target = Pdfdest.XYZ (Pdfdest.PageObject (hd refnums), None, None, None);
         Pdfmarks.isopen = false;
         Pdfmarks.colour = (0., 0., 0.);
         Pdfmarks.flags = 0}
      in
        Pdfmarks.add_bookmarks (newmark::marks) pdf
    else
      pdf
