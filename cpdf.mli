(** Coherent PDF Tools Core Routines *)
open Pdfutil

(** {2 Working with pages} *)

val copy_cropbox_to_mediabox : Pdf.t -> int list -> Pdf.t

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

val hard_box : Pdf.t -> int list -> string -> bool -> bool -> Pdf.t

(** [set_mediabox xywhlist pdf range] sets the media box on the given pages. *)
val set_mediabox : (float * float * float * float) list -> Pdf.t -> int list -> Pdf.t

(** [setBox boxname x y w h pdf range] sets the given box on the given pages. *)
val setBox : string -> float -> float -> float -> float -> Pdf.t -> int list -> Pdf.t

(** Remove any cropping from the given pages. *)
val remove_cropping_pdf : Pdf.t -> int list -> Pdf.t

(** Remove any trim box from the given pages. *)
val remove_trim_pdf : Pdf.t -> int list -> Pdf.t

(** Remove any bleed box from the given pages. *)
val remove_bleed_pdf : Pdf.t -> int list -> Pdf.t

(** Remove any art box from the given pages. *)
val remove_art_pdf : Pdf.t -> int list -> Pdf.t

(** Change rotation to a given value 0, 90, 180, 270 on given pages. *)
val rotate_pdf : int -> Pdf.t -> int list -> Pdf.t

(** Rotate clockwise by 0, 90, 180, 270 on given pages. *)
val rotate_pdf_by : int -> Pdf.t -> int list -> Pdf.t

(** Rotate the contents by the given angle on the given pages. If [fast] is true, assume PDF is well-formed. *)
val rotate_contents : ?fast:bool -> float -> Pdf.t -> int list -> Pdf.t

(** Modify the rotation of the page and its contents to leave the rotation at 0 with the page effectively unaltered. *)
val upright : ?fast:bool -> int list -> Pdf.t -> Pdf.t

(** Flip the given pages vertically *)
val vflip_pdf : ?fast:bool -> Pdf.t -> int list -> Pdf.t

(** Flip the given pages horizontally *)
val hflip_pdf : ?fast:bool -> Pdf.t -> int list -> Pdf.t

(** Shift a PDF in x and y (in pts) in the given pages. List of (x, y) pairs is
for all pages in pdf. *)
val shift_pdf : ?fast:bool -> (float * float) list -> Pdf.t -> int list -> Pdf.t

(** Scale a PDF in sx, sy in the given pages. List of (sx, sy) pairs is
for all pages in pdf. *)
val scale_pdf : ?fast:bool -> (float * float) list -> Pdf.t -> int list -> Pdf.t

(** [scale_to_fit_pdf fast position input_scale x y op pdf range] scales a page to fit the
page size given by (x, y) and by the [input_scale] (e.g 1.0 = scale to fit, 0.9
= scale to fit leaving a border etc.). [op] is unused. *) 
val scale_to_fit_pdf : ?fast:bool -> Cpdfposition.position -> float -> (float * float) list -> 'a -> Pdf.t -> int list -> Pdf.t

(** Scale the contents of a page by a given factor centred around a given point in a given range. *)
val scale_contents : ?fast:bool -> Cpdfposition.position -> float -> Pdf.t -> int list -> Pdf.t

val trim_marks : ?fast:bool -> Pdf.t -> int list -> Pdf.t

val show_boxes : ?fast:bool -> Pdf.t -> int list -> Pdf.t

(** {2 Imposition} *)

val impose : x:float -> y:float -> fit:bool -> columns:bool -> rtl:bool -> btt:bool -> center:bool -> margin:float -> spacing:float -> linewidth:float -> fast:bool -> Pdf.t -> Pdf.t

(** The twoup_stack operation puts two logical pages on each physical page,
rotating them 90 degrees to do so. The new mediabox is thus larger. Bool true
(fast) if assume well-formed ISO content streams. *)
val twoup_stack : bool -> Pdf.t -> Pdf.t

(** The twoup operation does the same, but scales the new sides down so that
the media box is unchanged. Bool true (fast) if assume well-formed ISO content streams. *)
val twoup : bool -> Pdf.t -> Pdf.t

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
