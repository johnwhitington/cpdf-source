(** Annotations *)

(* {2 Modern functions} *)

(** Get annotations as JSON *)
val get_annotations_json : ?subtype:string -> Pdf.t -> int list -> Pdfio.bytes

(** Set annotations from JSON. *)
val set_annotations_json : Pdf.t -> Pdfio.input -> unit

(** Remove the annotations on given pages. *)
val remove_annotations : int list -> Pdf.t -> Pdf.t

(** Copy the annotations on a given set of pages *)
val copy_annotations : int list -> Pdf.t -> Pdf.t -> unit

(** Add an annotation (presently just redaction). *)
val add_annotation :
  (float * float * float * float) ->
    main_color:Cpdfaddtext.colour ->
    outline_color:Cpdfaddtext.colour ->
    overlay:string option ->
    overlay_text_colour:Cpdfaddtext.colour ->
    overlay_justification:Cpdfaddtext.justification ->
    overlay_repeat:bool ->
    overlay_auto_size:bool ->
    Pdf.t -> int list -> Pdf.t

(** Annotation type from a string. *)
val subtype_of_string : string -> Pdfannot.subtype

(* {2 Old-style functions *)

(** Return the annotations as a simple old-style (pagenumber, content) list. *)
val get_annotations : Cpdfmetadata.encoding -> Pdf.t -> (int * string) list

(** List the annotations to standard output in a given encoding. *)
val list_annotations : int list -> Cpdfmetadata.encoding -> Pdf.t -> unit
