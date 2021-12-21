(** {2 Annotations} *)

(** List the annotations to standard output in a given encoding. See cpdfmanual.pdf for the format details. *)
val list_annotations : json:bool -> Cpdfmetadata.encoding -> Pdf.t -> unit

(** Return the annotations as a (pagenumber, content) list *)
val get_annotations : Cpdfmetadata.encoding -> Pdf.t -> (int * string) list

(** Copy the annotations on a given set of pages from a to b. b is returned. *)
val copy_annotations : int list -> Pdf.t -> Pdf.t -> Pdf.t

(** Remove the annotations on given pages. *)
val remove_annotations : int list -> Pdf.t -> Pdf.t
