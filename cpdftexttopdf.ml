open Pdfutil

let rec of_utf8_with_newlines used charcode_extractor t =
  let items = ref [] in
  let buf = ref [] in
  let codepoints = Pdftext.codepoints_of_utf8 t in
  let charcodes_of_codepoints cs =
    option_map
      (fun u ->
         match charcode_extractor u with
         | Some c -> Hashtbl.replace used c (); Some (char_of_int c)
         | None -> Printf.printf "No glyph for unicode U+%04X in this font\n" u; None)
      cs
  in
    List.iter
      (function
       | 10 (*'\n'*) ->
           let c = rev !buf in
             if c <> [] then items := Cpdftype.Text (charcodes_of_codepoints c)::!items;
             items := Cpdftype.NewLine::!items;
             buf := []
       | 13 (*'\r'*) -> ()
       | x ->
           buf := x::!buf)
      codepoints;
    (* Do last one *)
    let c = rev !buf in
      if c <> [] then items := Text (charcodes_of_codepoints c)::!items;
      rev !items

let typeset ~papersize ~font ~fontsize text =
  let pdf = Pdf.empty () in
  let font =
    match font with
    | Cpdfembed.PreMadeFontPack t -> hd (fst t)
    | Cpdfembed.EmbedInfo {fontfile; fontname; encoding} ->
        hd (fst (Cpdfembed.embed_truetype pdf ~fontfile ~fontname ~codepoints ~encoding))
    | Cpdfembed.ExistingNamedFont _ -> raise (Pdf.PDFError "Can't use existing named font for text-to-PDF")
  in
  let charcode_extractor = Pdftext.charcode_extractor_of_font_real font in
  let margin =
    Pdfunits.convert
      72. (Pdfpaper.unit papersize) (Pdfunits.PdfPoint) (Pdfpaper.width papersize) /. 15.
  in
  let used = null_hash () in
  let instrs = of_utf8_with_newlines used charcode_extractor (Pdfio.string_of_bytes text) in
  let codepoints = map fst (list_of_hashtbl used) in
  let pages =
    Cpdftype.typeset
      margin margin margin margin papersize pdf
      ([Cpdftype.Font (font, fontsize); Cpdftype.BeginDocument] @ instrs)
  in
    let pdf, pageroot = Pdfpage.add_pagetree pages pdf in
      Pdfpage.add_root pageroot [] pdf
