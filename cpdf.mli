(** Coherent PDF Tools Core Routines *)
open Pdfutil

(** {2 Stamping} *)

(** [combine_pages fast under over scaletofit swap equalize] combines the page
content of two PDFs, page-by-page. If [equalize] is true the output will have
the same number of pages as the shorter file. If [scaletofit] is true, the
[over] file will be scaled to fit. If [swap] is true, [over] and [under] are
swapped.  If [fast] is true, the PDFs are assumed to be well-formed and no
fixes are done. *)
val combine_pages : bool -> Pdf.t -> Pdf.t -> bool -> bool -> bool -> Pdf.t

(** [stamp relative_to_cropbox position topline midline fast scale_to_fit isover range over pdf] stamps the first page of
[over] over each page of the PDF. The arguments have the same meaning as in
[combine_pages]. *)
val stamp : bool -> Cpdfposition.position -> bool -> bool -> bool -> bool -> bool -> int list -> Pdf.t -> Pdf.t -> Pdf.t

(**  {2 Page geometry} *)

(** True if a given page in a PDF has a given box *)
val hasbox : Pdf.t -> int -> string -> bool

(** [crop_pdf xywhlist pdf range] sets the cropbox on the given pages. *)
val crop_pdf : ?box:string -> (float * float * float * float) list -> Pdf.t -> int list -> Pdf.t

(** [setBox boxname x y w h pdf range] sets the given box on the given pages. *)
val setBox : string -> float -> float -> float -> float -> Pdf.t -> int list -> Pdf.t

(** Flip the given pages vertically *)
val vflip_pdf : ?fast:bool -> Pdf.t -> int list -> Pdf.t

(** Flip the given pages horizontally *)
val hflip_pdf : ?fast:bool -> Pdf.t -> int list -> Pdf.t

val trim_marks : ?fast:bool -> Pdf.t -> int list -> Pdf.t

val show_boxes : ?fast:bool -> Pdf.t -> int list -> Pdf.t

(** {2 Miscellany} *)

(** Make all lines in the PDF at least a certain thickness. *)
val thinlines : int list -> float -> Pdf.t -> Pdf.t

(** Make all text on certain pages black. *)
val blacktext : Cpdfaddtext.color -> int list -> Pdf.t -> Pdf.t

(** Make all lines on certain pages black. *)
val blacklines : Cpdfaddtext.color -> int list -> Pdf.t -> Pdf.t

(** Make all fills on certain pages black. *)
val blackfills : Cpdfaddtext.color -> int list -> Pdf.t -> Pdf.t

(**/**)

val append_page_content : string -> bool -> bool -> int list -> Pdf.t -> Pdf.t

val remove_dict_entry : Pdf.t -> string -> Pdf.pdfobject option -> unit

val replace_dict_entry : Pdf.t -> string -> Pdf.pdfobject -> Pdf.pdfobject option -> unit

val print_dict_entry : Pdf.t -> string -> unit

val remove_clipping : Pdf.t -> int list -> Pdf.t 

val copy_box : string -> string -> bool -> Pdf.t -> int list -> Pdf.t

val remove_unused_resources : Pdf.t -> Pdf.t
