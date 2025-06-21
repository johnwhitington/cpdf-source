(** Utilities *)

val progress : bool ref
val progress_line : string -> unit
val progress_page : int -> unit
val progress_endpage : unit -> unit
val progress_done : unit -> unit

(** Remove a dictionary entry. *)
val remove_dict_entry : Pdf.t -> string -> Pdf.pdfobject option -> unit

(** Replace a dictionary entry. *)
val replace_dict_entry : Pdf.t -> string -> Pdf.pdfobject -> Pdf.pdfobject option -> unit

(** Check for injectible characters in a string, and error out if so. *)
val check_injectible : string -> unit
