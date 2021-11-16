(* Simple typesetter *)
type box =
  Text of string
| Glue of float
| Newline
| Font of string
| BoldOn
| BoldOff
| ItalicOn
| ItalicOff

type t = box list

val typeset : Pdfpaper.t -> t -> Pdfpage.t list
