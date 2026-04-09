(** Representing page content as objects without loss. *)

val filter_ops : f:(unit -> unit) -> mediabox:(float * float * float * float) -> resources:Pdf.pdfobject -> ops:Pdfops.t list -> Pdfops.t list
