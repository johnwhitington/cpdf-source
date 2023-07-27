(** Remove text from documents *)

(** Remove text added by [Cpdfaddtext] from the given pages. *)
val removetext : int list -> Pdf.t -> Pdf.t

(** Remove all text from the given pages *)
val remove_all_text : int list -> Pdf.t -> Pdf.t 
