type t =
  {width : int;
   height : int;
   idat : Pdfio.bytes}

(* Read a non-interlaced, non-transparent 24 bit PNG for inclusion in a PDF
   file. Raises BadPNG on failure. *)
val read_png : Pdfio.input -> t
