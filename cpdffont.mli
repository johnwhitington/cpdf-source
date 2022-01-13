(** Fonts *)

(** {2 Listing fonts} *)

(** Print font list to stdout *)
val print_fonts : Pdf.t -> int list -> unit

(** Return font list. Page number, name, subtype, basefont, encoding.  *)
val list_fonts : Pdf.t -> int list -> (int * string * string * string * string) list

(** {2 Miscellany.} *)

(** Embed missing fonts *)
val embed_missing_fonts : string -> bool -> string -> string -> unit

(** Copy a font *)
val copy_font : Pdf.t -> string -> int -> int list -> Pdf.t -> Pdf.t

(** Report missing fonts *)
val missing_fonts : Pdf.t -> int list -> unit

(** Print a font table to Standard Output. *)
val print_font_table : Pdf.t -> string -> int -> unit

(** Extract a font file to disk. *)
val extract_fontfile : int -> string -> Pdf.t -> unit

(** Remove fonts from a document. *)
val remove_fonts : Pdf.t -> Pdf.t
