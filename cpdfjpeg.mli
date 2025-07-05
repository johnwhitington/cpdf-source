(** JPEG Utilities *)

(** Return the width and height of a JPEG *)
val jpeg_dimensions : Pdfio.bytes -> int * int

(** Backup version, calls out to magick. This is because ours can't do progressive JPEG yet. *)
val backup_jpeg_dimensions : path_to_im:string -> string -> int * int
