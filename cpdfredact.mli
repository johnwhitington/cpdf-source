(** Redaction. *)

val redact : Pdf.t -> path:((float * float) list) -> int list -> Pdf.t

val apply : Pdf.t -> int list -> unit

val apply_type : Pdf.t -> string -> int list -> unit
