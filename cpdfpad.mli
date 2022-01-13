(** Padding *)

(** Put blank pages before the given page numbers *)
val padbefore : ?padwith:Pdf.t -> int list -> Pdf.t -> Pdf.t

(** Put blank pages after the given page numbers *)
val padafter : ?padwith:Pdf.t -> int list -> Pdf.t -> Pdf.t

(** Pad to a multiple of n pages *)
val padmultiple : int -> Pdf.t -> Pdf.t
