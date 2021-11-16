(* cpdftype.ml *)
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

let typeset papersize boxes =
  [Pdfpage.blankpage papersize]
