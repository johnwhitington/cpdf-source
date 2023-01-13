(** Annotations *)

(** Old fashioned functions first, still in use for backward-compatibilty *)

(** Return the annotations as a (pagenumber, content) list. *)
val get_annotations : Cpdfmetadata.encoding -> Pdf.t -> (int * string) list

(** Copy the annotations on a given set of pages from a to b. b is returned. *)
val copy_annotations : int list -> Pdf.t -> Pdf.t -> Pdf.t

(** List the annotations to standard output in a given encoding. See cpdfmanual.pdf for the format details. *)
val list_annotations : json:bool -> int list -> Cpdfmetadata.encoding -> Pdf.t -> unit

(** Remove the annotations on given pages. *)
val remove_annotations : int list -> Pdf.t -> Pdf.t

(** Modern functions *)

(** Get annotations as JSON *)
val get_annotations_json : Pdf.t -> Pdfio.bytes

(** Set annotations from JSON. Existing annotations will be removed. *)
val set_annotations_json : Pdf.t -> Pdfio.input -> unit
