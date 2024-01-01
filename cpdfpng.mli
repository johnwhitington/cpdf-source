(** Read PNG files *)

(** PNG files, represented only to the extent required to insert as PDF image objects *)
type t =
  {width : int;
   height : int;
   bitdepth : int;
   colortype : int;
   idat : Pdfio.bytes}

(** Read a non-interlaced, non-alpha, non-palette PNG for inclusion in a PDF file. *)
val read_png : Pdfio.input -> t
