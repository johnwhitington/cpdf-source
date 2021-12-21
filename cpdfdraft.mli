(** Remove images from a PDF, optionally adding crossed boxes. *)
val draft : string option -> bool -> int list -> Pdf.t -> Pdf.t
