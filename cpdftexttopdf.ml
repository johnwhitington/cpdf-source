open Pdfutil

let of_utf8 (f, fontsize) t =
     Pdftext.codepoints_of_utf8 t
  |> option_map (Pdftext.charcode_extractor_of_font_real f)
  |> map char_of_int
  |> implode

let of_pdfdocencoding (f, fontsize) t =
  of_utf8 (f, fontsize) (Pdftext.utf8_of_pdfdocstring t)

let rec of_utf8_with_newlines t =
  let items = ref [] in
  let buf = Buffer.create 256 in
    String.iter
      (function
       | '\n' ->
           let c = Buffer.contents buf in
             if c <> "" then items := Cpdftype.Text (explode c)::!items;
             items := Cpdftype.NewLine::!items;
             Buffer.clear buf
       | '\r' -> ()
       | x ->
           Buffer.add_char buf x)
      t;
    (* Do last one *)
    let c = Buffer.contents buf in
      if c <> "" then items := Text (explode c)::!items;
    rev !items

(* FIXME margins, hyphenation of too-long words, efficiency *)
let typeset ~font ~fontsize text =
  let pdf = Pdf.empty () in
  let f = (Pdftext.StandardFont (font, Pdftext.WinAnsiEncoding), fontsize) in
  let pages =
    Cpdftype.typeset
      20. 20. 20. 20. Pdfpaper.a4 pdf ([Cpdftype.Font f] @ of_utf8_with_newlines (Pdfio.string_of_bytes text))
  in
    let pdf, pageroot = Pdfpage.add_pagetree pages pdf in
      Pdfpage.add_root pageroot [] pdf
