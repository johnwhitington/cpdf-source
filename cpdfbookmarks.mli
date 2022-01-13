(** Bookmarks *)

(** [parse_bookmark_file verify pdf input] parses the bookmark file in [input].
Details of the bookmark file format can be found in cpdfmanual.pdf. *)
val parse_bookmark_file : bool -> Pdf.t -> Pdfio.input -> Pdfmarks.t list

(** [add_bookmarks verify input pdf] adds bookmarks from the bookmark file
give. If [verify] is given, bookmarks will be verified to ensure, for example,
that they are not out of the page range. In the new JSON format if chosen. *) 
val add_bookmarks : json:bool -> bool -> Pdfio.input -> Pdf.t -> Pdf.t

(** [list_bookmarks encoding range pdf output] lists the bookmarks to the given
output in the format specified in cpdfmanual.pdf. In the new JSON format if
chosen. *)
val list_bookmarks : json:bool -> Cpdfmetadata.encoding -> int list -> Pdf.t -> Pdfio.output -> unit

(** [name_of_spec encoding marks pdf splitlevel spec n filename startpage
    endpage] expands a bookmark specifiation filename. *)
val name_of_spec : Cpdfmetadata.encoding ->
           Pdfmarks.t list ->
           Pdf.t -> int -> string -> int -> string -> int -> int -> string

(** Indent bookmarks in each file by one and add a title bookmark pointing to
   the first page. If the boolean is set, then use the PDF's metadata title
   instead of the filename. *)
val add_bookmark_title : string -> bool -> Pdf.t -> Pdf.t

(** Set bookmarks to be open to the given level. *) 
val bookmarks_open_to_level : int -> Pdf.t -> Pdf.t

(** Alter bookmark destinations given a hash table of (old page reference
number, new page reference number) pairings *)
val change_bookmark : (int, int) Hashtbl.t -> Pdfmarks.t -> Pdfmarks.t 
