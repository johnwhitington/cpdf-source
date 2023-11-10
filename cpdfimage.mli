(** Images *)

(** Extract images. *)
val extract_images : ?raw:bool -> ?path_to_p2p:string ->
           ?path_to_im:string ->
           Cpdfmetadata.encoding -> bool -> bool -> Pdf.t -> int list -> string -> unit

(** Report image resolutions. *)        
val image_resolution : Pdf.t -> int list -> float -> (int * string * int * int * float * float) list

(**/**)
val image_of_input : (Pdfio.bytes -> Pdf.pdfobject) -> Pdfio.input -> Pdf.t
val obj_of_jpeg_data : Pdfio.bytes -> Pdf.pdfobject
val obj_of_png_data : Pdfio.bytes -> Pdf.pdfobject
