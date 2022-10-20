open Pdfutil

(* Return set of unicode characters in this text *)
let used_characters t =
  let codepoints = Pdftext.codepoints_of_utf8 t in
    setify codepoints

let rec of_utf8_with_newlines fontpack fontsize t =
  let items = ref [] in
  let currfont = ref 0 in
  let codepoints = Pdftext.codepoints_of_utf8 t in
  let currtext = ref [] in
  let process_codepoints cs =
    iter
      (fun u ->
         match Cpdfembed.get_char fontpack u with
         | Some (c, n, f) ->
             Printf.printf "Charcode %i, font number %i\n" c n;
             begin if n <> !currfont then
               begin
                 if !currtext <> [] then items := Cpdftype.Text (rev !currtext)::!items;
                 currtext := [];
                 currfont := n;
                 items := Cpdftype.Font (f, fontsize)::!items;
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
             buf := []
       | 13 (*'\r'*) -> ()
       | x ->
           buf := x::!buf)
      codepoints;
    (* Do last one *)
    let c = rev !buf in
      if c <> [] then process_codepoints c;
      rev !items

let typeset ~papersize ~font ~fontsize text =
  let pdf = Pdf.empty () in
  let codepoints = used_characters (Pdfio.string_of_bytes text) in
  let font, fontpack =
    match font with
    | Cpdfembed.PreMadeFontPack t -> (hd (fst t), t)
    | Cpdfembed.EmbedInfo {fontfile; fontname; encoding} ->
        let embedded = Cpdfembed.embed_truetype pdf ~fontfile ~fontname ~codepoints ~encoding in
         (hd (fst embedded), embedded)
    | Cpdfembed.ExistingNamedFont -> raise (Pdf.PDFError "Can't use existing named font for text-to-PDF")
  in
  let instrs = of_utf8_with_newlines fontpack fontsize (Pdfio.string_of_bytes text) in
  let margin =
    Pdfunits.convert
      72. (Pdfpaper.unit papersize) (Pdfunits.PdfPoint) (Pdfpaper.width papersize) /. 15.
  in
  let instrs = [Cpdftype.Font (font, fontsize); Cpdftype.BeginDocument] @ instrs in
  Printf.printf "to_string: %s\n" (Cpdftype.to_string instrs);
  let pages = Cpdftype.typeset margin margin margin margin papersize pdf instrs in
    let pdf, pageroot = Pdfpage.add_pagetree pages pdf in
      Pdfpage.add_root pageroot [] pdf
