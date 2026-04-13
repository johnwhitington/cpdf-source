(** Representing page content as objects without loss. *)

val filter_ops :
  pdf:Pdf.t ->
  f:(float * float * float * float -> bool) ->
  mediabox:(float * float * float * float) ->
  resources:Pdf.pdfobject -> ops:Pdfops.t list -> Pdfops.t list

val show_bounding_boxes : Pdf.t -> int list -> Pdf.t
