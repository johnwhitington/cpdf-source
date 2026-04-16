(** Representing page content as objects without loss. *)

(* Filter ops based on a bounding-box predicate. *)
val filter_ops :
  pdf:Pdf.t ->
  f:(float * float * float * float -> bool) ->
  mediabox:(float * float * float * float) ->
  resources:Pdf.pdfobject -> ops:Pdfops.t list -> Pdfops.t list
