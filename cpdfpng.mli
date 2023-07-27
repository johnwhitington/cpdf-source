(** Read PNG files *)

(** PNG files, represented only to the extent required to insert as PDF image objects *)
type t =
  {width : int;
   height : int;
   idat : Pdfio.bytes}

(** Read a non-interlaced, non-transparent 24 bit PNG for inclusion in a PDF file. *)
val read_png : Pdfio.input -> t
