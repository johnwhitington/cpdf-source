(** Coherent PDF Tools Core Routines *)
open Pdfutil

(** {2 Types and Exceptions} *)

(** Possible output encodings for some function. [Raw] does no processing at
all - the PDF string is output as-is. [UTF8] converts loslessly to UTF8.
[Stripped] extracts the unicode codepoints and returns only those which
correspond to 7 bit ASCII. *)
type encoding = Raw | UTF8 | Stripped

exception SoftError of string
exception HardError of string
(** Two exceptions recommended for use with the library, though currently not
raised by any function in this module. Cpdfcommand uses them extensively. *)

(** Possible positions for adding text and other uses. See cpdfmanual.pdf *)
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
  | Centre

(** {2 Debug} *)

(** Debug: Print out a PDF in readable form to the terminal *)
val print_pdf_objs : Pdf.t -> unit

(** {2 Working with pages} *)

(** Like [Pdfpage.endpage], but from an input and possible password - does the
minimal work to find the number of pages. *)
val endpage_io : ?revision:int -> Pdfio.input -> string option -> string option -> int

(** Given a function from page number and page to page, a document, and a list
of page numbers to apply it to, apply the function to all those pages. *)
val process_pages : (int -> Pdfpage.t -> Pdfpage.t) -> Pdf.t -> int list -> Pdf.t

(** Same as [process_pages], but iterate rather than map. *)
val iter_pages : (int -> Pdfpage.t -> unit) -> Pdf.t -> int list -> unit

(** Same as [process_pages] but return the list of outputs of the map function. *)
val map_pages : (int -> Pdfpage.t -> 'a) -> Pdf.t -> int list -> 'a list

(** {2 Page specifications and ranges } *)

(** Here are the rules for building input ranges:

{ul
{- A comma (,) allows one to specify several ranges, e.g. 1-2,4-5.}
{- The word end represents the last page number. }
{- The words odd and even can be used in place of or at the end of a page range to restrict to just the odd or even pages. }
{- The word reverse is the same as end-1.}
{- The word all is the same as 1-end.}
{- A range must contain no spaces.}
{- A tilde (~) defines a page number counting from the end of the document rather than the beginning. Page ~1 is the last page, ~2 the penultimate page etc.}
}
*)

(** Parse a (valid) page specification to a page range *)
val parse_pagespec : Pdf.t -> string -> int list

(** Return a string for the given range. Knows how to identify all, odd, even,
x-y ranges etc. *)
val string_of_pagespec : Pdf.t -> int list -> string

(** Is a page specification, in theory, valid? This is the most we can find out
without supplying a PDF, and thus knowing how many pages there are in it. *)
val validate_pagespec : string -> bool

val parse_pagespec_without_pdf : string -> int list

(** Compresses all streams in the PDF document which are uncompressed, using
/FlateDecode, leaving out metadata.  If the PDF is encrypted, does nothing. *)
val recompress_pdf : Pdf.t -> Pdf.t

(** Decompresses all streams in a PDF document, assuming it isn't encrypted. *)
val decompress_pdf : Pdf.t -> Pdf.t

(** {2 Metadata and settings} *)

(** [copy_id keepversion copyfrom copyto] copies the ID, if any, from
[copyfrom] to [copyto]. If [keepversion] is true, the PDF version of [copyto]
won't be affected. *)
val copy_id : bool -> Pdf.t -> Pdf.t -> Pdf.t

(** [set_pdf_info (key, value, version)] sets the entry [key] in the /Info directory, updating
the PDF minor version to [version].*)
val set_pdf_info : ?xmp_also:bool -> ?xmp_just_set:bool -> (string * Pdf.pdfobject * int) -> Pdf.t -> Pdf.t

(** [set_pdf_info (key, value, version)] sets the entry [key] in the
/ViewerPreferences directory, updating the PDF minor version to [version].*)
val set_viewer_preference : (string * Pdf.pdfobject * int) -> Pdf.t -> Pdf.t

(** Set the page layout to the given name (sans slash) e.g SinglePage *)
val set_page_layout : Pdf.t -> string -> Pdf.t

(** Set the page layout to the given name (sans slash) e.g SinglePage *)
val set_page_mode : Pdf.t -> string -> Pdf.t

(** Set the open action. If the boolean is true, /Fit will be used, otherwise /XYZ *)
val set_open_action : Pdf.t -> bool -> int -> Pdf.t

(** Set the PDF version number *)
val set_version : int -> Pdf.t -> unit

(** Given a PDF, returns a function which can lookup a given dictionary entry
from the /Info dictionary, returning it as a UTF8 string *)
val get_info_utf8 : Pdf.t -> string -> string

(** Output to standard output general information about a PDF. *)
val output_info : encoding -> Pdf.t -> unit

(** Output to standard output information from any XMP metadata stream in a PDF. *)
val output_xmp_info : encoding -> Pdf.t -> unit

(** {2 Presentations} *)

(** [presentation range t d horizontal inward direction effect_duration pdf]
adds a presentation on the pages in [range]. See cpdfmanual.pdf for details.
*)
val presentation : int list -> string option ->
    float option -> bool -> bool -> int -> float -> Pdf.t -> Pdf.t

(** {2 File Attachments} *)
(** [attach_file keepversion topage pdf filename] attaches the file in [filename] to the pdf, optionally to a page (rather than document-level). If keepversion is true, the PDF version number won't be altered. *)
val attach_file : ?memory:Pdfio.bytes -> bool -> int option -> Pdf.t -> string -> Pdf.t

(** Remove attached files. *)
val remove_attached_files : Pdf.t -> Pdf.t

type attachment =
  {name : string;
   pagenumber : int;
   data : unit -> Pdfio.bytes}

(** List attached files. Attachment name and page number. Page 0 is document level. *)
val list_attached_files : Pdf.t -> attachment list

(** {2 Bookmarks} *)

(** [parse_bookmark_file verify pdf input] parses the bookmark file in [input].
Details of the bookmark file format can be found in cpdfmanual.pdf *)
val parse_bookmark_file : bool -> Pdf.t -> Pdfio.input -> Pdfmarks.t list

(** [add_bookmarks verify input pdf] adds bookmarks from the bookmark file
give. If [verify] is given, bookmarks will be verified to ensure, for example,
that they are not out of the page range. *) 
val add_bookmarks : bool -> Pdfio.input -> Pdf.t -> Pdf.t

(** [list_bookmarks encoding range pdf output] lists the bookmarks to the given
output in the format specified in cpdfmanual.pdf *)
val list_bookmarks : encoding -> int list -> Pdf.t -> Pdfio.output -> unit

(** {2 XML Metadata} *)

(** [set_metadata keepversion filename pdf] sets the XML metadata of a PDF to the contents of [filename]. If [keepversion] is true, the PDF version will not be altered. *) 
val set_metadata : bool -> string -> Pdf.t -> Pdf.t

(** The same, but the content comes from [bytes]. *)
val set_metadata_from_bytes : bool -> Pdfio.bytes -> Pdf.t -> Pdf.t

(** Remove the metadata from a file *)
val remove_metadata : Pdf.t -> Pdf.t

(** Extract metadata to a [Pdfio.bytes] *)
val get_metadata : Pdf.t -> Pdfio.bytes option

(** Print metadate to stdout *)
val print_metadata : Pdf.t -> unit

(** Set the metadata date *)
val set_metadata_date : Pdf.t -> string -> Pdf.t

(** Create XMP metadata from scratch *)
val create_metadata : Pdf.t -> Pdf.t

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
val stamp : bool -> position -> bool -> bool -> bool -> bool -> bool -> int list -> Pdf.t -> Pdf.t -> Pdf.t

(** {2 Splitting PDFs} *)

(** Split a PDF on bookmarks of a given level or below. Level 0 is top level. *)
val split_on_bookmarks : Pdf.t -> int -> Pdf.t list

(** {2 Listing fonts} *)

(** Print font list to stdout *)
val print_fonts : Pdf.t -> unit

(** Return font list. Page number, name, subtype, basefont, encoding.  *)
val list_fonts : Pdf.t -> (int * string * string * string * string) list

(** {2 Adding text} *)

(** Expand the string "now" to a PDF date string, ignoring any other string *)
val expand_date : string -> string


(** Produce a debug string of a [position] *)
val string_of_position : position -> string

(** Orientation of the string on the page *)
type orientation =
  | Horizontal
  | Vertical
  | VerticalDown

(** Justification of multiline text *)
type justification =
  | LeftJustify
  | CentreJustify
  | RightJustify

(** [calculate_position ignore_d w (xmin, ymin, xmax, ymax) orientation pos] calculates
the absolute position of text given its width, bounding box, orientation and
position. If [ignore_d] is true, the distance from the position (e.g 10 in
TopLeft 10) is ignored (considered zero). *)
val calculate_position :
    bool ->
    float ->
    float * float * float * float ->
    orientation -> position -> float * float * float

(** Call [add_texts metrics linewidth outline fast fontname font bates batespad colour
position linespacing fontsize underneath text pages orientation
relative_to_cropbox midline_adjust topline filename pdf]. For details see cpdfmanual.pdf *)
val addtexts :
    bool -> (*metrics*)
    float -> (*linewidth*)
    bool -> (*outline*)
    bool -> (*fast*)
    string -> (*fontname*)
    Pdftext.standard_font option -> (*font*)
    bool -> (* embed font *)
    int -> (* bates number *)
    int option -> (* bates padding width *)
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
    bool ->(*topline adjust?*)
    string ->(*filename*)
    float option -> (*extract_text_font_size*)
    Pdf.t ->(*pdf*)
    Pdf.t

val metrics_howmany : unit -> int
val metrics_text : int -> string
val metrics_x : int -> float
val metrics_y : int -> float
val metrics_rot : int -> float
val metrics_baseline_adjustment : unit -> float
(** These functions returns some details about the text if [addtexts] is called with [metrics] true. The integer arguments are  1 for the first one, 2 for the second etc. Call [metrics_howmany] first to find out how many. *)

(** Remove text from the given pages. *)
val removetext : int list -> Pdf.t -> Pdf.t

(**  {2 Page geometry} *)

(** Print page info (Mediabox etc) to standard output. *)
val output_page_info : Pdf.t -> int list -> unit

(** True if a given page in a PDF has a given box *)
val hasbox : Pdf.t -> int -> string -> bool

(** [crop_pdf xywhlist pdf range] sets the cropbox on the given pages. *)
val crop_pdf : ?box:string -> (float * float * float * float) list -> Pdf.t -> int list -> Pdf.t

val hard_box : Pdf.t -> int list -> string -> bool -> bool -> Pdf.t

(** [set_mediabox xywhlist pdf range] sets the media box on the given pages. *)
val set_mediabox : (float * float * float * float) list -> Pdf.t -> int list -> Pdf.t

(*(** [setBox boxname x y w h pdf range] sets the given box on the given pages. *)
val setBox : string -> float -> float -> float -> float -> Pdf.t -> int list -> Pdf.t*)

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
val scale_to_fit_pdf : ?fast:bool -> position -> float -> (float * float) list -> 'a -> Pdf.t -> int list -> Pdf.t

(** Scale the contents of a page by a given factor centred around a given point in a given range. *)
val scale_contents : ?fast:bool -> position -> float -> Pdf.t -> int list -> Pdf.t

(** {2 Padding} *)

(** Put blank pages before the given page numbers *)
val padbefore : ?padwith:Pdf.t -> int list -> Pdf.t -> Pdf.t

(** Put blank pages after the given page numbers *)
val padafter : ?padwith:Pdf.t -> int list -> Pdf.t -> Pdf.t

(** Pad to a multiple of n pages *)
val padmultiple : int -> Pdf.t -> Pdf.t

(** {2 Annotations} *)

(** List the annotations to standard output in a given encoding. See cpdfmanual.pdf for the format details. *)
val list_annotations : encoding -> Pdf.t -> unit

(** The same, but giving more information. Deprecated *)
val list_annotations_more : Pdf.t -> unit

(** Return the annotations as a (pagenumber, content) list *)
val get_annotations : encoding -> Pdf.t -> (int * string) list

(** Copy the annotations on a given set of pages from a to b. b is returned. *)
val copy_annotations : int list -> Pdf.t -> Pdf.t -> Pdf.t

(** Remove the annotations on given pages. *)
val remove_annotations : int list -> Pdf.t -> Pdf.t

(** {2 Imposition} *)

(** The twoup_stack operation puts two logical pages on each physical page,
rotating them 90 degrees to do so. The new mediabox is thus larger. *)
val twoup_stack : Pdf.t -> Pdf.t

(** The twoup operation does the same, but scales the new sides down so that
the media box is unchanged. *)
val twoup : Pdf.t -> Pdf.t

(** {2 Making new documents} *)

(** Make a blank document given x and y page dimensions in points and a number of pages *)
val blank_document : float -> float -> int -> Pdf.t

(** The same, but give a Pdfpaper.t paper size. *)
val blank_document_paper : Pdfpaper.t -> int -> Pdf.t

(** {2 Page labels} *)

(** Add page labels. *)
val add_page_labels :
  Pdf.t -> Pdfpagelabels.labelstyle -> string option -> int -> int list -> unit 

(** {2 Miscellany} *)

(** Make all lines in the PDF at least a certain thickness. *)
val thinlines : int list -> float -> Pdf.t -> Pdf.t

(** Make all text on certain pages black. *)
val blacktext : float * float * float -> int list -> Pdf.t -> Pdf.t

(** Make all lines on certain pages black. *)
val blacklines : float * float * float -> int list -> Pdf.t -> Pdf.t

(** Make all fills on certain pages black. *)
val blackfills : float * float * float -> int list -> Pdf.t -> Pdf.t

(** Remove images from a PDF, optionally adding crossed boxes. *)
val draft : bool -> int list -> Pdf.t -> Pdf.t

(** Squeeze a PDF *)
val squeeze : ?logto:string -> Pdf.t -> unit

(**/**)

val process_xobjects : Pdf.t -> Pdfpage.t -> (Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject list -> Pdf.pdfobject list) -> unit

(** Custom CSP1 *)
val custom_csp1 : Pdf.t -> Pdf.t

(** Custom CSP2 *)
val custom_csp2 : float -> Pdf.t -> Pdf.t

(** Nobble a page, given pdf, pagenumber and page *)
val nobble_page : Pdf.t -> 'a -> Pdfpage.t -> Pdfpage.t

val find_cpdflin : string option -> string

val call_cpdflin : string -> string -> string -> string -> int

val debug : bool ref

val extract_text : float option -> Pdf.t -> int list -> string 
