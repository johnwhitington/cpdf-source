val embed_missing_fonts : string -> bool -> string -> string -> unit

val copy_font : Pdf.t -> string -> int -> int list -> Pdf.t -> Pdf.t

val missing_fonts : Pdf.t -> int list -> unit

val print_font_table : Pdf.t -> string -> int -> unit

val extract_fontfile : int -> string -> Pdf.t -> unit

val remove_fonts : Pdf.t -> Pdf.t

(** {2 Listing fonts} *)

(** Print font list to stdout *)
val print_fonts : Pdf.t -> int list -> unit

(** Return font list. Page number, name, subtype, basefont, encoding.  *)
val list_fonts : Pdf.t -> int list -> (int * string * string * string * string) list

