(** JavaScript *)

(** True if a document contains JavaScript *)
val contains_javascript : Pdf.t -> bool

(** Remove JavaScript from a document *)
val remove_javascript : Pdf.t -> unit
