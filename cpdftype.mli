(** Experimental typesetter for cpdf *)
type element =
  Text of char list
| HGlue of float
| VGlue of float
| NewLine
| NewPage
| Font of (Pdftext.font * float)
| BeginDest of Pdfdest.t
| EndDest
| BeginDocument

type t = element list

val to_string : t -> string

val font_widths : Pdftext.font -> float -> float array

val width_of_string : float array -> char list -> float

val typeset : float -> float -> float -> float -> Pdfpaper.t -> Pdf.t -> t -> Pdfpage.t list
