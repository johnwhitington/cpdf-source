(** Utilities *)

(** Remove a dictionary entry. *)
val remove_dict_entry : Pdf.t -> string -> Pdf.pdfobject option -> unit

(** Replace a dictionary entry. *)
val replace_dict_entry : Pdf.t -> string -> Pdf.pdfobject -> Pdf.pdfobject option -> unit

(** Check for injectible characters in a string, and error out if so. *)
val check_injectible : string -> unit
