type t =
  {width : int;
   height : int;
   idat : Pdfio.bytes}

(* Read a non-interlaced, non-transparent 24 bit PNG for inclusion in a PDF
   file. Raises BadPNG on failure. *)
val read_png : Pdfio.input -> t

(* Write a non-interlaced, non-transparent 24 bit PNG from PDF image contents *)
val write_png : t -> Pdfio.output -> unit
