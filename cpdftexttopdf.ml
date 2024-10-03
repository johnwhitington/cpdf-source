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
  let tagged = tag_paragraphs instrs in
  (*flprint (Cpdftype.to_string tagged);*)
  let margin = Pdfunits.points (Pdfpaper.width papersize) (Pdfpaper.unit papersize) /. 15.  in
  let instrs =
    if tagged = [] then [] else
      let firstfont = hd (keep (function Cpdftype.Font _ -> true | _ -> false) tagged) in 
        [firstfont; Cpdftype.BeginDocument] @ tagged
  in
    if subformat = Some Cpdfua.PDFUA2 then
      begin
        let str = Pdf.addobj pdf Pdf.Null in
        let p = Pdf.addobj pdf Pdf.Null in
        let parent_tree = Pdf.addobj pdf Pdf.Null in
        let namespace = Pdf.addobj pdf (Pdf.Dictionary [("/NS", Pdf.String "http://iso.org/pdf2/ssn")]) in
        let document = Pdf.addobj pdf Pdf.Null in
        Pdf.addobj_given_num pdf (document, Pdf.Dictionary [("/K", Pdf.Array [Pdf.Indirect p]); ("/P", Pdf.Indirect str); ("/S", Pdf.Name "/Document"); ("/NS", Pdf.Indirect namespace)]);
        Pdf.addobj_given_num pdf (parent_tree, Pdf.Dictionary [("/Nums", Pdf.Array [Pdf.Integer 1; Pdf.Array [Pdf.Indirect p]])]);
        Pdf.addobj_given_num pdf (p, Pdf.Dictionary [("/K", Pdf.Array [Pdf.Integer 0]); ("/P", Pdf.Indirect document); ("/S", Pdf.Name "/P")]);
        Pdf.addobj_given_num pdf (str, Pdf.Dictionary [("/Namespaces", Pdf.Array [Pdf.Indirect namespace]); ("/Type", Pdf.Name "/StructTreeRoot");
                                                       ("/K", Pdf.Array [Pdf.Indirect document]); ("/ParentTree", Pdf.Indirect parent_tree)]);
        Pdf.replace_chain pdf ["/Root"] ("/StructTreeRoot", (Pdf.Indirect str))
      end
    else if process_struct_tree || subformat = Some Cpdfua.PDFUA1 then
      begin
        let str = Pdf.addobj pdf Pdf.Null in
        let p = Pdf.addobj pdf Pdf.Null in
        let parent_tree = Pdf.addobj pdf Pdf.Null in
        Pdf.addobj_given_num pdf (parent_tree, Pdf.Dictionary [("/Nums", Pdf.Array [Pdf.Integer 1; Pdf.Array [Pdf.Indirect p]])]);
        Pdf.addobj_given_num pdf (p, Pdf.Dictionary [("/K", Pdf.Array [Pdf.Integer 0]); ("/P", Pdf.Indirect str); ("/S", Pdf.Name "/P")]);
        Pdf.addobj_given_num pdf (str, Pdf.Dictionary [("/Type", Pdf.Name "/StructTreeRoot"); ("/K", Pdf.Array [Pdf.Indirect p]); ("/ParentTree", Pdf.Indirect parent_tree)]);
        Pdf.replace_chain pdf ["/Root"] ("/StructTreeRoot", (Pdf.Indirect str))
      end;
  let pages, tags = Cpdftype.typeset ~process_struct_tree margin margin margin margin papersize pdf instrs in
  let pages =
    map
      (fun p -> if process_struct_tree then {p with Pdfpage.rest = Pdf.add_dict_entry p.Pdfpage.rest "/StructParents" (Pdf.Integer 1)} else p)
      pages
  in
    let pdf, pageroot = Pdfpage.add_pagetree pages pdf in
      Pdfpage.add_root pageroot [] pdf
