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

(* Typeset a table of contents with given font, font size and title. Mediabox
   (and CropBox) copied from first page of existing PDF. Margin of 10% inside
   CropBox. Font size of title twice body font size. Null page labels added for
   TOC, others bumped up and so preserved. *)
let typeset_table_of_contents ~font ~fontsize ~title ~bookmark ~dotleader ~process_struct_tree pdf =
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
             [Cpdftype.Tag ("Link", 0); Cpdftype.BeginDest (mark.Pdfmarks.target, Some mark.Pdfmarks.text); Cpdftype.HGlue indent] @ textruns @ leader @ labelruns
           @ [Cpdftype.EndDest; Cpdftype.EndTag; Cpdftype.NewLine])
      (Pdfmarks.read_bookmarks pdf)
  in
  let toc_pages, toc_tags =
    let title =
      let glue = Cpdftype.VGlue (fontsize *. 2.) in
        [Cpdftype.Tag ("P", 0)]
        @ flatten
           (map
             (fun l -> l @ [Cpdftype.NewLine])
             (map (of_utf8 fontpack (fontsize *. 2.)) (map implode (split_toc_title (explode title)))))
        @
        [Cpdftype.EndTag; glue]
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
  let label =
    {Pdfpagelabels.labelstyle = NoLabelPrefixOnly;
     Pdfpagelabels.labelprefix = None;
     Pdfpagelabels.startpage = 1;
     Pdfpagelabels.startvalue = 1}
  in
  (* Building the structure tree. We have MCIDS and page numbers in toc_tags,
     and annotations in the pages /Annots entries.  We build the new structure
     tree elements.
       1) One <p> for the title
       2) One for each link:
            link = <</Type /StructElem /S /Link /K [<mcid> <objr>]>>
            objr = <</Type /ObjR /Obj <linkannot>>>
     Then we have to graft them onto the file's existing structure tree
     Then we have to do something about /StructParents.

     It's all a simpler version of what Cpdftexttopdf does (simpler because no
     paras spanning multiple pages). *)
     iter (fun x -> Printf.printf "PAGE\n"; iter (fun (_, i) -> Printf.printf "Paragraph number %i\n" i) x) toc_tags;
  let p_struct_elem_first_page =
    Pdf.addobj pdf (Pdf.Dictionary [("/Type", Pdf.Name "/StructElem"); ("/S", Pdf.Name "/P")])
  in
  let mcid = ref 1 in
  let link_struct_elems_for_each_page =
    flatten
      (map
        (fun page ->
          let annot_objnums =
            match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
            | Some (Pdf.Array a) -> map (function Pdf.Indirect i -> i | _ -> 0) a
            | _ -> []
          in
          let r = map
              (fun annot_i ->
                 let r =
                   let objr = Pdf.addobj pdf (Pdf.Dictionary [("/Type", Pdf.Name "/ObjR"); ("/Obj", Pdf.Indirect annot_i)]) in
                     Pdf.addobj pdf
                       (Pdf.Dictionary [("/Type", Pdf.Name "/StructElem"); 
                                        ("/S", Pdf.Name "/Link");
                                        ("/K", Pdf.Array [Pdf.Integer !mcid; Pdf.Indirect objr])])
                in
                  incr mcid; r)
              annot_objnums
          in
            mcid := 0; r)
        toc_pages)
  in
  let prepending_structitems =
    map (fun x -> Pdf.Indirect x) (p_struct_elem_first_page::link_struct_elems_for_each_page)
  in
  prepend_structitems pdf prepending_structitems;
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
