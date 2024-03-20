(** JPEG2000 Utilities *)

(** Return the width and height of a JPEG2000 stream *)
val jpeg2000_dimensions : Pdfio.bytes -> int * int
