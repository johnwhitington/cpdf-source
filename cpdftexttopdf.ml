open Pdfutil

(* Return set of unicode characters in this text *)
let used_characters t =
  let codepoints = Pdftext.codepoints_of_utf8 t in
    setify codepoints

(* Just first font, expand later. Move into cpdfembed? *)
let get_char (fonts, table) u =
  match Hashtbl.find table u with
  | (n, charcode) -> Some charcode
  | exception Not_found -> None

let rec of_utf8_with_newlines fontpack t =
  let items = ref [] in
  let buf = ref [] in
  let codepoints = Pdftext.codepoints_of_utf8 t in
  let charcodes_of_codepoints cs =
    option_map
      (fun u ->
         match get_char fontpack u with
         | Some c -> Some (char_of_int c)
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
  let codepoints = used_characters (Pdfio.string_of_bytes text) in
  let font, fontpack =
    match font with
    | Cpdfembed.PreMadeFontPack t -> (hd (fst t), t)
    | Cpdfembed.EmbedInfo {fontfile; fontname; encoding} ->
        let embedded = Cpdfembed.embed_truetype pdf ~fontfile ~fontname ~codepoints ~encoding in
         (hd (fst embedded), embedded)
    | Cpdfembed.ExistingNamedFont _ -> raise (Pdf.PDFError "Can't use existing named font for text-to-PDF")
  in
  let instrs = of_utf8_with_newlines fontpack (Pdfio.string_of_bytes text) in
  let margin =
    Pdfunits.convert
      72. (Pdfpaper.unit papersize) (Pdfunits.PdfPoint) (Pdfpaper.width papersize) /. 15.
  in
  let pages =
    Cpdftype.typeset
      margin margin margin margin papersize pdf
      ([Cpdftype.Font (font, fontsize); Cpdftype.BeginDocument] @ instrs)
  in
    let pdf, pageroot = Pdfpage.add_pagetree pages pdf in
      Pdfpage.add_root pageroot [] pdf
