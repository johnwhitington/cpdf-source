open Pdfutil
open Cpdferror

let of_utf8_with_newlines fontpack fontsize t =
  let items = ref [] in
  let currfont = ref ~-1 in
  let codepoints = Pdftext.codepoints_of_utf8 t in
  let currtext = ref [] in
  let process_codepoints cs =
    iter
      (fun u ->
         match Cpdfembed.get_char fontpack u with
         | Some (c, n, f) ->
             begin if n <> !currfont then
               begin
                 if !currtext <> [] then items := Cpdftype.Text (rev !currtext)::!items;
                 currtext := [];
                 currfont := n;
                 items := Cpdftype.Font (string_of_int n, f, fontsize)::!items;
                 currtext := char_of_int c::!currtext;
               end
             else
               currtext := char_of_int c::!currtext
             end
         | None -> Printf.printf "No glyph for unicode U+%04X in this font\n" u)
      cs;
      items := Cpdftype.Text (rev !currtext)::!items
  in
  let buf = ref [] in
    List.iter
      (function
       | 10 (*'\n'*) ->
           let c = rev !buf in
             if c <> [] then process_codepoints c;
             items := Cpdftype.NewLine::!items;
             currtext := [];
             buf := []
       | 13 (*'\r'*) -> ()
       | x ->
           buf := x::!buf)
      codepoints;
    (* Do last one *)
    let c = rev !buf in
      if c <> [] then process_codepoints c;
      rev !items

(* Post process, adding Tag / EndTag around paragraphs *)
let rec tag_paragraphs = function
  | Cpdftype.NewLine::Cpdftype.NewLine::t ->
      Cpdftype.EndTag::Cpdftype.NewLine::Cpdftype.NewLine::Cpdftype.Tag ("P", 0)::tag_paragraphs t
  | x::t -> x::tag_paragraphs t
  | [] -> [Cpdftype.EndTag]

let tag_paragraphs l =
  Cpdftype.Tag ("P", 0)::tag_paragraphs l

let typeset ~process_struct_tree ?subformat ?title ~papersize ~font ~fontsize text =
  let process_struct_tree =
    match process_struct_tree, subformat with
    | _, (Some Cpdfua.PDFUA1 | Some Cpdfua.PDFUA2) | true, _ -> true
    | _ -> false
  in
  let pdf, title =
    match subformat with
    | None -> Pdf.empty (), begin match title with Some x -> x | None -> "" end
    | Some Cpdfua.PDFUA1 ->
        begin match title with
        | None -> error "no -title given" 
        | Some title -> Cpdfua.create_pdfua1 title papersize 1, title
        end
    | Some Cpdfua.PDFUA2 ->
        begin match title with
        | None -> error "no -title given"
        | Some title -> Cpdfua.create_pdfua2 title papersize 1, title
        end
  in
  let codepoints = setify (Pdftext.codepoints_of_utf8 (Pdfio.string_of_bytes text)) in
  let fontpack =
    match font with
    | Cpdfembed.PreMadeFontPack t -> t
    | Cpdfembed.EmbedInfo {fontfile; fontname; encoding} ->
        Cpdfembed.embed_truetype pdf ~fontfile ~fontname ~codepoints ~encoding
    | Cpdfembed.ExistingNamedFont ->
        raise (Pdf.PDFError "Can't use existing named font for text-to-PDF")
  in
  let instrs = of_utf8_with_newlines fontpack fontsize (Pdfio.string_of_bytes text) in
  (*flprint (Cpdftype.to_string instrs);
  flprint "------------------------------";*)
  let tagged = if process_struct_tree then tag_paragraphs instrs else instrs in
  (*flprint (Cpdftype.to_string tagged);*)
  let margin = Pdfunits.points (Pdfpaper.width papersize) (Pdfpaper.unit papersize) /. 15.  in
  let instrs =
    if tagged = [] then [] else
      let firstfont = hd (keep (function Cpdftype.Font _ -> true | _ -> false) tagged) in 
        [firstfont; Cpdftype.BeginDocument] @ tagged
  in
  let pages, tags = Cpdftype.typeset ~process_struct_tree margin margin margin margin papersize pdf instrs in
  (*iter (fun x -> Printf.printf "PAGE\n"; iter (fun (_, i) -> Printf.printf "Paragraph number %i\n" i) x) tags;*)
  (* We make (tag number, page number, mcid) triples *)
  let tagtriples =
    flatten
      (map2
       (fun pn tags ->
          map2 (fun (_, tagnum) mcid -> (tagnum, pn, mcid)) tags (indx0 tags))
       (indx0 tags)
       tags)
  in
   (* Printf.printf "(paragraph number, page number, mcid) triples:\n";
    iter (fun (tagnum, pn, mcid) -> Printf.printf "%i, %i, %i\n" tagnum pn mcid) tagtriples;*)
  (* Now work out the nodes and which MCIDs in which pages they point to. Each paragraph may point to 1 or more nodes. *)
  let rec find_nodes (a : ((int * int * int) list) list) = function
  | (para, page, mcid)::nodes ->
      begin match a with
      | ((para', page', mcid')::t)::rest when para = para' ->
          find_nodes (((para, page, mcid)::(para', page', mcid')::t)::rest) nodes
      | (h::t)::rest ->
          find_nodes (([(para, page, mcid)])::(h::t)::rest) nodes
      | []::rest ->
          find_nodes (([(para, page, mcid)])::rest) nodes
      | [] -> assert false
      end
  | [] -> rev (map rev a)
  in
  let nodes = find_nodes [[]] tagtriples in
    (*Printf.printf "Paragraphs and their page and MCIDs\n";
    iter
      (fun parts_of_para ->
         Printf.printf "Paragraph:\n";
         iter (fun (para, page, mcid) -> Printf.printf "Para %i, Page %i, MCID %i\n" para page mcid) parts_of_para)
      nodes;*)
    let pages =
      map2
        (fun pn p -> if process_struct_tree then {p with Pdfpage.rest = Pdf.add_dict_entry p.Pdfpage.rest "/StructParents" (Pdf.Integer pn)} else p)
        (indx0 pages)
        pages
    in
      let pdf, pageroot = Pdfpage.add_pagetree pages pdf in
      let pdf = Pdfpage.add_root pageroot [] pdf in
      let refnums = let ns = Pdf.page_reference_numbers pdf in combine (indx0 ns) ns in
      if process_struct_tree || subformat = Some Cpdfua.PDFUA1 || subformat = Some Cpdfua.PDFUA2 then
        begin
          let namespace = if subformat = Some Cpdfua.PDFUA2 then Pdf.addobj pdf (Pdf.Dictionary [("/NS", Pdf.String "http://iso.org/pdf2/ssn")]) else 0 in
          let document = if subformat = Some Cpdfua.PDFUA2 then Pdf.addobj pdf Pdf.Null else 0 in

          let str = Pdf.addobj pdf Pdf.Null in
          let topks =
            map
              (fun parts_of_para ->
                 let ks =
                   map (fun (_, pagenumber, mcid) -> Pdf.Dictionary [("/Type", Pdf.Name "/MCR"); ("/Pg", Pdf.Indirect (unopt (lookup pagenumber refnums))); ("/MCID", Pdf.Integer mcid)]) parts_of_para
                 in
                   Pdf.Indirect (Pdf.addobj pdf (Pdf.Dictionary [("/K", Pdf.Array ks); ("/P", Pdf.Indirect (if subformat = Some Cpdfua.PDFUA2 then document else str)); ("/S", Pdf.Name "/P")])))
              nodes
          in
          if subformat = Some Cpdfua.PDFUA2 then
            Pdf.addobj_given_num pdf (document, Pdf.Dictionary [("/K", Pdf.Array topks); ("/P", Pdf.Indirect str); ("/S", Pdf.Name "/Document"); ("/NS", Pdf.Indirect namespace)]);
          let parent_tree =
            let pairs =
              map
                (fun pn ->
                  let this_page_triples = keep (fun (para, page, mcid) -> page = pn) tagtriples in
                    (string_of_int pn, Pdf.Array (map (function (para, _, _) -> (List.nth topks para)) this_page_triples)))
                (indx0 pages)
            in
            Pdf.addobj pdf (Pdftree.build_name_tree true pdf pairs)
          in
          let stns =
            if subformat = Some Cpdfua.PDFUA2 then [("/Namespaces", Pdf.Array [Pdf.Indirect namespace])] else []
          in
          let k =
            if subformat = Some Cpdfua.PDFUA2 then Pdf.Indirect document else Pdf.Array topks
          in
            Pdf.addobj_given_num pdf (str, Pdf.Dictionary (stns @ [("/Type", Pdf.Name "/StructTreeRoot"); ("/K", k); ("/ParentTree", Pdf.Indirect parent_tree)]));
            Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"] (Pdf.Indirect str)
        end;
        pdf
