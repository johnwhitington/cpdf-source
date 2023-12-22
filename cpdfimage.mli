(** Images *)

(** Extract images. *)
val extract_images : ?raw:bool -> ?path_to_p2p:string ->
           ?path_to_im:string ->
           Cpdfmetadata.encoding -> bool -> bool -> Pdf.t -> int list -> string -> unit

(** Report image resolutions. *)        
val image_resolution : Pdf.t -> int list -> float -> (int * string * int * int * float * float) list

(** List images in JSON format *)
val images : Pdf.t -> int list -> Cpdfyojson.Safe.t

val process : ?q:int -> ?qlossless:int -> ?jbig2:int -> Pdf.t -> path_to_convert:string -> unit

(**/**)
val image_of_input : (unit -> Pdfio.bytes -> Pdf.pdfobject * (int * Pdf.pdfobject) list) -> Pdfio.input -> Pdf.t
val obj_of_jpeg_data : Pdfio.bytes -> Pdf.pdfobject * (int * Pdf.pdfobject) list
val obj_of_png_data : Pdfio.bytes -> Pdf.pdfobject * (int * Pdf.pdfobject) list
val obj_of_jbig2_data : ?global:Pdfio.bytes -> Pdfio.bytes -> Pdf.pdfobject * (int * Pdf.pdfobject) list
