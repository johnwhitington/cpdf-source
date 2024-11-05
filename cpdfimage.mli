(** Images *)

(** Print info when processing image *)
val debug_image_processing : bool ref

(** Extract images. *)
val extract_images :
  ?raw:bool -> ?path_to_p2p:string -> ?path_to_im:string ->
  Cpdfmetadata.encoding -> bool -> bool -> Pdf.t -> int list -> string -> unit

(** Report image resolutions. *)        
val image_resolution : Pdf.t -> int list -> float -> (int * string * int * int * float * float * int) list

(** Report image resolution data in JSON format *)
val image_resolution_json : Pdf.t -> int list -> float -> Pdfio.bytes

(** List images in JSON format *)
val images : Pdf.t -> int list -> Cpdfyojson.Safe.t

(** Reprocess images. See manual for details. *)
val process :
  q:float -> qlossless:float -> onebppmethod:string -> jbig2_lossy_threshold:float ->
  length_threshold:int -> percentage_threshold:float -> pixel_threshold:int ->
  dpi_threshold:float -> factor:float -> interpolate:bool ->
  jpeg_to_jpeg_scale:float -> jpeg_to_jpeg_dpi:float ->
  path_to_jbig2enc:string -> path_to_convert:string -> int list -> Pdf.t -> unit

(**/**)
val image_of_input : ?subformat:Cpdfua.subformat -> ?title:string -> process_struct_tree:bool -> (unit -> Pdfio.bytes -> Pdf.pdfobject * (int * Pdf.pdfobject) list) -> Pdfio.input -> Pdf.t
val obj_of_jpeg_data : Pdfio.bytes -> Pdf.pdfobject * (int * Pdf.pdfobject) list
val obj_of_png_data : Pdfio.bytes -> Pdf.pdfobject * (int * Pdf.pdfobject) list
val obj_of_jbig2_data : ?global:Pdfio.bytes -> Pdfio.bytes -> Pdf.pdfobject * (int * Pdf.pdfobject) list
val obj_of_jpeg2000_data : Pdfio.bytes -> Pdf.pdfobject * (int * Pdf.pdfobject) list
