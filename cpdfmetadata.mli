(** Metadata *)

(** {2 Types and Exceptions} *)

(** Possible output encodings for some function. [Raw] does no processing at
all - the PDF string is output as-is. [UTF8] converts loslessly to UTF8.
[Stripped] extracts the unicode codepoints and returns only those which
correspond to 7 bit ASCII. *)
type encoding = Raw | UTF8 | Stripped

(** Encode a string using a given encoding. *) 
val encode_output : encoding -> string -> string

(** {2 Metadata and settings} *)

(** [copy_id keepversion copyfrom copyto] copies the ID, if any, from
[copyfrom] to [copyto]. If [keepversion] is true, the PDF version of [copyto]
won't be affected. *)
val copy_id : bool -> Pdf.t -> Pdf.t -> Pdf.t

(** [set_pdf_info (key, value, version)] sets the entry [key] in the /Info directory, updating
the PDF minor version to [version].*)
val set_pdf_info : ?xmp_also:bool -> ?xmp_just_set:bool -> (string * Pdf.pdfobject * int) -> Pdf.t -> Pdf.t

(** Get XMP information for a given key. *)
val get_xmp_info : Pdf.t -> string -> string

(** [set_pdf_info (key, value, version)] sets the entry [key] in the
/ViewerPreferences directory, updating the PDF minor version to [version].*)
val set_viewer_preference : (string * Pdf.pdfobject * int) -> Pdf.t -> Pdf.t

(** Set the page layout to the given name (sans slash) e.g SinglePage *)
val set_page_layout : Pdf.t -> string -> Pdf.t

(** Set the page mode to the given name (sans slash) e.g UseThumbs *)
val set_page_mode : Pdf.t -> string -> Pdf.t

(** Set the non full screen page mode to the given name (sans slash) e.g UseThumbs *)
val set_non_full_screen_page_mode : Pdf.t -> string -> Pdf.t

(** Set the open action. If the boolean is true, /Fit will be used, otherwise /XYZ *)
val set_open_action : Pdf.t -> ?dest:string -> bool -> int -> Pdf.t

(** Set the PDF version number *)
val set_version : int -> Pdf.t -> unit

(** Given a PDF, returns a function which can lookup a given dictionary entry
from the /Info dictionary, returning it as a UTF8 string *)
val get_info_utf8 : Pdf.t -> string -> string

(** Output to standard output general information about a PDF. *)
val output_info : ?json:(string * Cpdfyojson.Safe.t) list ref -> encoding -> Pdf.t -> unit

(** Output to standard output information from any XMP metadata stream in a PDF. *)
val output_xmp_info : ?json:(string * Cpdfyojson.Safe.t) list ref -> encoding -> Pdf.t -> unit

(** Create XMP metadata from scratch *)
val create_metadata : Pdf.t -> Pdf.t

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

(** Expands the date ["now"] to the date now. Leaves any other string alone. *) 
val expand_date : string -> string

(**/**)
val get_catalog_item : string -> Pdf.t -> string

val get_viewer_pref_item : string -> Pdf.t -> string

val determine_subformats : Pdf.t -> string list

val language : Pdf.t -> string option

val set_language : Pdf.t -> string -> unit

val adobe : string
val xmp : string
val dc : string
val rdf : string
val pdfaid : string
val pdfxid : string
val pdfe : string
val pdfuaid : string
val pdfvtid : string

type xmltree =
    E of Cpdfxmlm.tag * xmltree list
  | D of string

val xmltree_of_bytes : Pdfio.bytes -> Cpdfxmlm.dtd * xmltree

val get_data_for : string -> string -> xmltree -> string option

val string_of_xmltree : xmltree -> string

val bytes_of_xmltree : Cpdfxmlm.dtd * xmltree -> Pdfio.bytes
