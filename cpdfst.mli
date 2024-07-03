(** Structure trees *)

(** Trim structure tree to remove parts marked as not in the page range given. *)
val trim_structure_tree : Pdf.t -> int list -> unit
