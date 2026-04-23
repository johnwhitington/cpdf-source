(** Representing page content as objects without loss. *)

type content = Glyph | InlineImage | Image | Path

(* Filter ops based on a bounding-box predicate. *)
val filter_ops :
  pdf:Pdf.t ->
  f:(content * (float * float * float * float * float * float * float * float) -> bool) ->
  mediabox:(float * float * float * float) ->
  resources:Pdf.pdfobject -> ops:Pdfops.t list -> Pdfops.t list
