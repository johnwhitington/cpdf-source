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
| BeginDest of Pdfdest.t
| EndDest

type t = element list

val to_string : t -> string

val typeset : float -> float -> float -> float -> Pdfpaper.t -> Pdf.t -> t -> Pdfpage.t list
