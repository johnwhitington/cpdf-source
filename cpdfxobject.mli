(** XObjects *)

(** Stamp a PDF as an xobject in another. *)
val stamp_as_xobject : Pdf.t -> int list -> Pdf.t -> Pdf.t * string
