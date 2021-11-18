type text = int list

type glue =
  {glen : float;
   stretch : float}

type code =
  Text of text
| HGlue of glue
| VGlue of glue
| NewLine
| NewPage
| Font of Pdftext.font

type t = code list

val typeset : Pdfpaper.t -> t -> Pdfpage.t list
