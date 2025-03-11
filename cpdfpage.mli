(** Working with pages *)

(** Print page info (Mediabox etc) to standard output. *)
val output_page_info : ?json:bool -> ?raisejson:bool -> Pdfunits.t -> Pdf.t -> int list -> unit

(** Return page info for selected pages in JSON format. *)
val json_page_info : Pdf.t -> int list -> Pdfunits.t -> Cpdfyojson.Safe.t

(** Given a function from page number and page to page, a document, and a list
of page numbers to apply it to, apply the function to all those pages. *)
val process_pages : (int -> Pdfpage.t -> Pdfpage.t * int * Pdftransform.transform_matrix) ->
                    Pdf.t -> int list -> Pdf.t

(** Same as [process_pages], but iterate rather than map. *)
val iter_pages : (int -> Pdfpage.t -> unit) -> Pdf.t -> int list -> unit

(** Same as [process_pages] but return the list of outputs of the map function. *)
val map_pages : (int -> Pdfpage.t -> 'a) -> Pdf.t -> int list -> 'a list

(** Clip a page to one of its boxes, or the media box if that box is not
 present. This is a hard clip, done by using a clipping rectangle, so that the
 page may then be used as a stamp without extraneous material reapearing. *)
val hard_box : Pdf.t -> int list -> string -> bool -> bool -> Pdf.t

(** Shift a PDF in x and y (in pts) in the given pages. List of (x, y) pairs is
for all pages in pdf. *)
val shift_pdf : ?fast:bool -> (float * float) list -> Pdf.t -> int list -> Pdf.t

(** Shift a PDF's boxes in x and y (in pts) in the given pages. List of (x, y)
pairs is for all pages in pdf. *)
val shift_boxes : (float * float) list -> Pdf.t -> int list -> Pdf.t

(** Change a page's media box so its minimum x and y are 0, making other
operations simpler to think about. Any shift that is done is reflected in
other boxes (clip etc.) *)
val rectify_boxes : ?fast:bool -> Pdf.t -> Pdfpage.t -> Pdfpage.t

(** Change the media box and other known boxes by a function which takes
xmin, xmax, ymin, ymax as input. *)
val change_boxes : (float * float * float * float -> float * float * float * float) ->
           Pdf.t -> Pdfpage.t -> Pdfpage.t 

(** Scale the contents of a page by a given factor centred around a given point in a given range. *)
val scale_contents : ?fast:bool -> Cpdfposition.position -> float -> Pdf.t -> int list -> Pdf.t

(** [set_mediabox xywhlist pdf range] sets the media box on the given pages. *)
val set_mediabox : (float * float * float * float) list -> Pdf.t -> int list -> Pdf.t

(** Remove any cropping from the given pages. *)
val remove_cropping_pdf : Pdf.t -> int list -> Pdf.t

(** Remove any trim box from the given pages. *)
val remove_trim_pdf : Pdf.t -> int list -> Pdf.t

(** Remove any bleed box from the given pages. *)
val remove_bleed_pdf : Pdf.t -> int list -> Pdf.t

(** Remove any art box from the given pages. *)
val remove_art_pdf : Pdf.t -> int list -> Pdf.t

(** Modify the rotation of the page and its contents to leave the rotation at 0 with the page effectively unaltered. *)
val upright : ?fast:bool -> int list -> Pdf.t -> Pdf.t

(** Change rotation to a given value 0, 90, 180, 270 on given pages. *)
val rotate_pdf : int -> Pdf.t -> int list -> Pdf.t

(** Rotate clockwise by 0, 90, 180, 270 on given pages. *)
val rotate_pdf_by : int -> Pdf.t -> int list -> Pdf.t

(** Rotate the contents by the given angle on the given pages. If [fast] is true, assume PDF is well-formed. *)
val rotate_contents : ?fast:bool -> float -> Pdf.t -> int list -> Pdf.t

(** Scale a PDF in sx, sy in the given pages. List of (sx, sy) pairs is
for all pages in pdf. *)
val scale_pdf : ?fast:bool -> (float * float) list -> Pdf.t -> int list -> Pdf.t

(** [scale_to_fit_pdf fast position input_scale x y op pdf range] scales a page to fit the
page size given by (x, y) and by the [input_scale] (e.g 1.0 = scale to fit, 0.9
= scale to fit leaving a border etc.). [op] is unused. *) 
val scale_to_fit_pdf : ?fast:bool -> Cpdfposition.position -> float -> (float * float) list -> 'a -> Pdf.t -> int list -> Pdf.t

val stretch : ?fast:bool -> (float * float) list -> Pdf.t -> int list -> Pdf.t

val center_to_fit : (float * float) list -> Pdf.t -> int list -> Pdf.t

(** {2 Stamping} *)

(** [combine_pages fast under over scaletofit swap equalize] combines the page
content of two PDFs, page-by-page. If [equalize] is true the output will have
the same number of pages as the shorter file. If [scaletofit] is true, the
[over] file will be scaled to fit. If [swap] is true, [over] and [under] are
swapped.  If [fast] is true, the PDFs are assumed to be well-formed and no
fixes are done. *)
val combine_pages : process_struct_tree:bool -> bool -> Pdf.t -> Pdf.t -> bool -> bool -> Pdf.t

(** [stamp relative_to_cropbox position topline midline fast scale_to_fit isover range over pdf] stamps the first page of
[over] over each page of the PDF. The arguments have the same meaning as in
[combine_pages]. *)
val stamp : process_struct_tree:bool -> bool -> Cpdfposition.position -> bool -> bool -> bool -> bool -> bool -> int list -> Pdf.t -> Pdf.t -> Pdf.t

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

(** Add trim marks. *)
val trim_marks : ?fast:bool -> Pdf.t -> int list -> Pdf.t

(** Show the page boxes for debug. *)
val show_boxes : ?fast:bool -> Pdf.t -> int list -> Pdf.t

(** Copy one box to another in the given pages. *)
val copy_box : string -> string -> bool -> Pdf.t -> int list -> Pdf.t

(** True if all pages are "upright" i.e no rotation and (0,0)-based. *)
val allupright : int list -> Pdf.t -> bool

(** True if all pages are "upright" i.e no rotation. *)
val alluprightonly : int list -> Pdf.t -> bool

(** When a page is transformed, its patterns must be too. *)
val change_pattern_matrices_page : Pdf.t -> Pdftransform.transform_matrix -> Pdfpage.t -> Pdfpage.t

val redact : process_struct_tree:bool -> Pdf.t -> int list -> Pdf.t

(** Remove a structure tree entirely from a file, including unmarking marked content. *)
val remove_struct_tree : Pdf.t -> Pdf.t

(** Mark a PDF as begin entirely artifacts (may be used after running [remove_struct_tree]. *)
val mark_all_as_artifact : Pdf.t -> Pdf.t

