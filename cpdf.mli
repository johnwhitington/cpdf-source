(** Coherent PDF Tools Core Routines *)
open Pdfutil

(** {2 Types and Exceptions} *)

type encoding = Raw | UTF8 | Stripped

exception SoftError of string
exception HardError of string

(** {2 Debug} *)

(** Debug: Print out a PDF in readable form to the terminal *)
val print_pdf_objs : Pdf.t -> unit

(** {2 Working with pages} *)

(** Like [Pdfpage.endpage], but from an input and possible password - does the
minimal work to find the number of pages. *)
val endpage_io : Pdfio.input -> string option -> string option -> int

(** Given a function from page number and page to page, a document, and a list
of page numbers to apply it to, apply the function to all those pages. *)
val process_pages : (int -> Pdfpage.t -> Pdfpage.t) -> Pdf.t -> int list -> Pdf.t

(** Same, but just iterate *)
val iter_pages : (int -> Pdfpage.t -> unit) -> Pdf.t -> int list -> unit

(** Same, but map *)
val map_pages : (int -> Pdfpage.t -> 'a) -> Pdf.t -> int list -> 'a list

(** {2 Page specifications and ranges } *)

val parse_pagespec : Pdf.t -> string -> int list

val string_of_pagespec : Pdf.t -> int list -> string

val validate_pagespec : string -> bool

val name_of_spec : bool -> Pdfmarks.t list -> Pdf.t -> int -> string -> int -> string -> int -> int -> string

(** {2 Compress and Decompress} *)

(** Compresses all streams in the PDF document which are uncompressed, using
/FlateDecode, leaving out metadata.  If the PDF is encrypted, does nothing. *)
val recompress_pdf : Pdf.t -> Pdf.t

val decompress_pdf : Pdf.t -> Pdf.t

(** {2 Metadata and settings} *)

(** [copy_id keepversion copyfrom copyto] *) 
val copy_id : bool -> Pdf.t -> Pdf.t -> Pdf.t

(** [set_pdf_info (key, value, version)] sets the entry [key] in the /Info directory, updating
the PDF minor version to [version].*)
val set_pdf_info : (string * Pdf.pdfobject * int) -> Pdf.t -> Pdf.t

(** [set_pdf_info (key, value, version)] sets the entry [key] in the
/ViewerPreferences directory, updating the PDF minor version to [version].*)
val set_viewer_preference : (string * Pdf.pdfobject * int) -> Pdf.t -> Pdf.t

(** Set the page layout to the given name (sans slash) e.g SinglePage *)
val set_page_layout : Pdf.t -> string -> Pdf.t

(** Set the page layout to the given name (sans slash) e.g SinglePage *)
val set_page_mode : Pdf.t -> string -> Pdf.t

val set_version : int -> Pdf.t -> unit

(** Output to standard output general information about a PDF. *)

val get_info_utf8 : Pdf.t -> string -> string

val output_info : encoding -> Pdf.t -> unit

(** {2 Presentations} *)

(** [presentation range t d h i dir effect_dur pdf] *)
val presentation : int list -> string option ->
    float option -> bool -> bool -> int -> float -> Pdf.t -> Pdf.t

(** {2 File Attachments} *)
(** [attach_file keep-version topage pdf filename] *)
val attach_file : bool -> int option -> Pdf.t -> string -> Pdf.t

(** Remove attached files. *)
val remove_attached_files : Pdf.t -> Pdf.t

(** List attached files. Attachment name and page number. Page 0 is document level. *)
val list_attached_files : Pdf.t -> (string * int) list

(** {2 Bookmarks} *)

(** [parse_bookmark_file verify pdf input] *)
val parse_bookmark_file : bool -> Pdf.t -> Pdfio.input -> Pdfmarks.t list

(** [add_bookmarks verify input pdf] *) 
val add_bookmarks : bool -> Pdfio.input -> Pdf.t -> Pdf.t

(** [list_bookmarks deunicode range page_offset pdf output] *)
val list_bookmarks : encoding -> int list -> Pdf.t -> Pdfio.output -> unit

(** {2 XML Metadata} *)

(** [set_metadata keepversion filename pdf] *) 
val set_metadata : bool -> string -> Pdf.t -> Pdf.t

val set_metadata_from_bytes : bool -> Pdfio.bytes -> Pdf.t -> Pdf.t

(** Remove the metadata from a file *)
val remove_metadata : Pdf.t -> Pdf.t

(** Print metadate to stdout *)
val get_metadata : Pdf.t -> Pdfio.bytes

val print_metadata : Pdf.t -> unit

(** {2 Stamping} *)

(** [combine_pages fast under over scaletofit swap equalize] *)
val combine_pages : bool -> Pdf.t -> Pdf.t -> bool -> bool -> bool -> Pdf.t

(** [stamp scale_to_fit fast isover range over pdf] stamps the first page of [over] over each page of the PDF. *)
val stamp : bool -> bool -> bool -> int list -> Pdf.t -> Pdf.t -> Pdf.t

(** {2 Splitting PDFs} *)

(** [split_at_bookmarks linearize nobble level spec pdf] *)
val split_at_bookmarks : string -> bool -> (Pdf.t -> Pdf.t) -> int -> string -> Pdf.t -> unit

(** The new one *)
val split_on_bookmarks : Pdf.t -> int -> Pdf.t list

(** [split_pdf printf chunksize linearize nobble spec pdf] *)
val split_pdf : Pdfwrite.encryption option -> bool -> string -> int -> bool -> (Pdf.t -> Pdf.t) -> string -> Pdf.t -> unit

(** {2 Listing fonts} *)

(** Print font list to stdout *)
val print_fonts : Pdf.t -> unit

val list_fonts : Pdf.t -> (int * string * string * string * string) list

(** {2 Adding text} *)

(** Expand the string "now" to a PDF date string, ignoring any other string *)
val expand_date : string -> string

(** Possible positions for adding text and other uses. *)
type position =
  | PosCentre of float * float
  | PosLeft of float * float
  | PosRight of float * float
  | Top of float
  | TopLeft of float
  | TopRight of float
  | Left of float
  | BottomLeft of float
  | Bottom of float
  | BottomRight of float
  | Right of float
  | Diagonal
  | ReverseDiagonal

val string_of_position : position -> string

type orientation =
  | Horizontal
  | Vertical
  | VerticalDown

type justification =
  | LeftJustify
  | CentreJustify
  | RightJustify

(** [calculate ignore_d w (xmin, ymin, xmax, ymax) shorterside pos] *)
val calculate_position :
    bool ->
    float ->
    float * float * float * float ->
    orientation -> position -> float * float * float

(** Returns what the added text string would be *)
(*val addtext_returntext : Pdf.t -> string -> int -> string -> int -> string*)

val metrics_howmany : unit -> int
val metrics_text : int -> string
val metrics_x : int -> float
val metrics_y : int -> float
val metrics_rot : int -> float
val metrics_baseline_adjustment : unit -> float

(** [add_texts fontname font bates colour positino linespacing fontsize shorterside text pages pdf] *)
val addtexts :
    bool -> (*metrics*)
    float -> (*linewidth*)
    bool -> (*outline*)
    bool -> (*fast*)
    string -> (*fontname*)
    Pdftext.standard_font option -> (*font*)
    int -> (*bates number *)
    float * float * float -> (*colour*)
    position -> (*position*)
    float -> (*linespacing*)
    float -> (*fontsize*)
    bool -> (*underneath*)
    string ->(*text*)
    int list ->(*page range*)
    orientation ->(*orientation*)
    bool ->(*relative to cropbox?*)
    float ->(*opacity*)
    justification ->(*justification*)
    bool ->(*midline adjust?*)
    string ->(*filename*)
    Pdf.t ->(*pdf*)
    Pdf.t

(** Remove text from the given pages. *)
val removetext : int list -> Pdf.t -> Pdf.t

(**  {2 Page geometry} *)

(** Print page info (Mediabox etc) *)
val output_page_info : Pdf.t -> unit

val hasbox : Pdf.t -> int -> string -> bool

(** [crop_pdf x y w h pdf range] *)
val crop_pdf : float -> float -> float -> float -> Pdf.t -> int list -> Pdf.t

(** [set_mediabox x y w h pdf range] *)
val set_mediabox : float -> float -> float -> float -> Pdf.t -> int list -> Pdf.t

val setBox : string -> float -> float -> float -> float -> Pdf.t -> int list -> Pdf.t

(** Remove any cropping from the given pages. *)
val remove_cropping_pdf : Pdf.t -> int list -> Pdf.t
val remove_trim_pdf : Pdf.t -> int list -> Pdf.t
val remove_bleed_pdf : Pdf.t -> int list -> Pdf.t
val remove_art_pdf : Pdf.t -> int list -> Pdf.t

(** Change rotation to a given value 0, 90, 180, 270 on given pages. *)
val rotate_pdf : int -> Pdf.t -> int list -> Pdf.t

(** Rotate clockwise by 0, 90, 180, 270 on given pages. *)
val rotate_pdf_by : int -> Pdf.t -> int list -> Pdf.t

(** Rotate the contents by the given angle on the given pages. *)
val rotate_contents : ?fast:bool -> float -> Pdf.t -> int list -> Pdf.t

(** Modify the rotation of the page and its contents to leave the rotation at 0 with the page effectively unaltered. *)
val upright : ?fast:bool -> int list -> Pdf.t -> Pdf.t

(** Flip the given pages vertically *)
val vflip_pdf : ?fast:bool -> Pdf.t -> int list -> Pdf.t

(** Flip the given pages horizontally *)
val hflip_pdf : ?fast:bool -> Pdf.t -> int list -> Pdf.t

(** Shift a PDF in x and y (in pts) in the given pages. *)
val shift_pdf : ?fast:bool -> float -> float -> Pdf.t -> int list -> Pdf.t

(** Scale a PDF in sx, sy in the given pages. *)
val scale_pdf : ?fast:bool -> float -> float -> Pdf.t -> int list -> Pdf.t

(** [scale_to_fit_pdf input_scale x y op pdf range] *) 
val scale_to_fit_pdf : ?fast:bool -> float -> float -> float -> 'a -> Pdf.t -> int list -> Pdf.t

(** Scale the contents of a page by a given factor centred around a given point in a given range. *)
val scale_contents : ?fast:bool -> position -> float -> Pdf.t -> int list -> Pdf.t

(** {2 Padding} *)

(** Put blank pages before the given page numbers *)
val padbefore : int list -> Pdf.t -> Pdf.t

(** Put blank pages after the given page numbers *)
val padafter : int list -> Pdf.t -> Pdf.t

(** Pad to a multiple of n pages *)
val padmultiple : int -> Pdf.t -> Pdf.t

(** {2 Annotations} *)

(** List the annotations to standard output *)
val list_annotations : encoding -> Pdf.t -> unit

val list_annotations_more : Pdf.t -> unit

val get_annotations : encoding -> Pdf.t -> (int * string) list

(** Copy the annotations on a given set of pages from a to b yielding c. *)
val copy_annotations : int list -> Pdf.t -> Pdf.t -> Pdf.t

(** Remove the annotations on given pages. *)
val remove_annotations : int list -> Pdf.t -> Pdf.t

(** {2 Imposition} *)

(** Two-up a PDF. *)
val twoup : Pdf.t -> Pdf.t

(** Stack Two-up a PDF. *)
val twoup_stack : Pdf.t -> Pdf.t

(** {2 Making new documents} *)

val blank_document : float -> float -> int -> Pdf.t

val blank_document_paper : Pdfpaper.t -> int -> Pdf.t

(** {2 Miscellany} *)

(** Make all lines in the PDF at least a certain thickness. *)
val thinlines : int list -> float -> Pdf.t -> Pdf.t

(** Make all text on certain pages black. *)
val blacktext : int list -> Pdf.t -> Pdf.t

(** Make all lines on certain pages black. *)
val blacklines : int list -> Pdf.t -> Pdf.t

(** Make all fills on certain pages black. *)
val blackfills : int list -> Pdf.t -> Pdf.t

(** Remove images from a PDF, optionally adding crossed boxes. *)
val draft : bool -> int list -> Pdf.t -> Pdf.t

(**/**)

(** Custom CSP1 *)
val custom_csp1 : Pdf.t -> Pdf.t

(** Custom CSP2 *)
val custom_csp2 : float -> Pdf.t -> Pdf.t

(** Nobble a page, given pdf, pagenumber and page *)
val nobble_page : Pdf.t -> 'a -> Pdfpage.t -> Pdfpage.t

