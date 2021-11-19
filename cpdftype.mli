type glue =
  {glen : float;
   gstretch : float}

type element =
  Text of string
| HGlue of glue
| VGlue of glue
| NewLine
| NewPage
| Font of (Pdftext.font * float)

type t = element list

val typeset : float -> float -> float -> float -> Pdfpaper.t -> Pdf.t -> t -> Pdfpage.t list
