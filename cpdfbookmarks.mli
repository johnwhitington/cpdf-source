(** {2 Bookmarks} *)

(** [parse_bookmark_file verify pdf input] parses the bookmark file in [input].
Details of the bookmark file format can be found in cpdfmanual.pdf *)
val parse_bookmark_file : bool -> Pdf.t -> Pdfio.input -> Pdfmarks.t list

(** [add_bookmarks verify input pdf] adds bookmarks from the bookmark file
give. If [verify] is given, bookmarks will be verified to ensure, for example,
that they are not out of the page range. *) 
val add_bookmarks : json:bool -> bool -> Pdfio.input -> Pdf.t -> Pdf.t

(** [list_bookmarks encoding range pdf output] lists the bookmarks to the given
output in the format specified in cpdfmanual.pdf *)
val list_bookmarks : json:bool -> Cpdfmetadata.encoding -> int list -> Pdf.t -> Pdfio.output -> unit

val name_of_spec : Cpdfmetadata.encoding ->
           Pdfmarks.t list ->
           Pdf.t -> int -> string -> int -> string -> int -> int -> string
