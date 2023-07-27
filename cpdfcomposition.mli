(** Show composition of a PDF *)

(** [show_composition filesize json pdf] prints the composition of a document to
   Standard Output. *)
val show_composition : int -> bool -> Pdf.t -> unit
