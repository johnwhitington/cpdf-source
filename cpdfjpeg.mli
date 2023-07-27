(** JPEG Utilities *)

(** Return the width and height of a JPEG *)
val jpeg_dimensions : Pdfio.bytes -> int * int
