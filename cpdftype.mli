(** Experimental typesetter for cpdf *)
type element =
  Text of char list
| HGlue of float
| VGlue of float
| NewLine
| NewPage
| Font of string * Pdftext.font * float
| BeginDest of Pdfdest.t
| EndDest
| BeginDocument
| Tag of string * int
| EndTag

type t = element list

(** Debug printing *)
val to_string : t -> string

(** Return the font width table for a given (id, font, fontsize) combination. *)
val font_widths : string -> Pdftext.font -> float -> float array

(** Calculate the widths of a string given a font width table, and list of char codes *)
val width_of_string : float array -> char list -> float

(** [typeset process_struct_tree lmargin rmargin tmargin bmargin papersize pdf contents] builds a
    list of pages of typset content. *)
val typeset : process_struct_tree:bool -> float -> float -> float -> float -> Pdfpaper.t -> Pdf.t -> t -> Pdfpage.t list * (string * int) list list

(** Add artifact markers to any un-marked part of the content stream. *)
val add_artifacts : Pdfops.t list -> Pdfops.t list
