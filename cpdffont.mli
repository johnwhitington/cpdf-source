val embed_missing_fonts : string -> bool -> string -> string -> unit

val copy_font : Pdf.t -> string -> int -> int list -> Pdf.t -> Pdf.t

val missing_fonts : Pdf.t -> int list -> unit

val print_font_table : Pdf.t -> string -> int -> unit

val extract_fontfile : int -> string -> Pdf.t -> unit

val remove_fonts : Pdf.t -> Pdf.t
