type text = int list

type glue =
  {glen : float;
   stretch : float}

type element =
  Text of text
| HGlue of glue
| VGlue of glue
| NewLine
| NewPage
| Font of Pdftext.font * float

type t = element list

val typeset : float -> float -> float -> float -> Pdfpaper.t -> Pdf.t -> t -> Pdfpage.t list
