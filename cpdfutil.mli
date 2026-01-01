(** Utilities *)

(** Turns Cpdf's progress-printing system on or off. *)
val progress : bool ref

(** Print a progress line. E.g "Loading file foo.pdf" with a newline. *)
val progress_line : string -> unit

(** Print a progres line with no line ending. *)
val progress_line_no_end : string -> unit

(** Begin page progress - prints the page number. *)
val progress_page : int -> unit

(** End page progress - prints a full stop. *)
val progress_endpage : unit -> unit

(** Print just a newline, for example when ending a page sequence. *)
val progress_done : unit -> unit

(** Remove a dictionary entry. *)
val remove_dict_entry : Pdf.t -> string -> Pdf.pdfobject option -> unit

(** Replace a dictionary entry. *)
val replace_dict_entry : Pdf.t -> string -> Pdf.pdfobject -> Pdf.pdfobject option -> unit

(** Check for injectible characters in a string, and error out if so. *)
val check_injectible : string -> unit
