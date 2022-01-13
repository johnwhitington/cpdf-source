(** Images *)

(** Extract images. *)
val extract_images : string ->
           string ->
           Cpdfmetadata.encoding -> bool -> bool -> Pdf.t -> int list -> string -> unit

(** Report image resolutions. *)        
val image_resolution : Pdf.t -> int list -> float -> (int * string * int * int * float * float) list
