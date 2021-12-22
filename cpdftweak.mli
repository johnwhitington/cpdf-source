(** {2 Miscellany} *)

(** Make all lines in the PDF at least a certain thickness. *)
val thinlines : int list -> float -> Pdf.t -> Pdf.t

(** Make all text on certain pages black. *)
val blacktext : Cpdfaddtext.color -> int list -> Pdf.t -> Pdf.t

(** Make all lines on certain pages black. *)
val blacklines : Cpdfaddtext.color -> int list -> Pdf.t -> Pdf.t

(** Make all fills on certain pages black. *)
val blackfills : Cpdfaddtext.color -> int list -> Pdf.t -> Pdf.t

val append_page_content : string -> bool -> bool -> int list -> Pdf.t -> Pdf.t

val remove_dict_entry : Pdf.t -> string -> Pdf.pdfobject option -> unit

val replace_dict_entry : Pdf.t -> string -> Pdf.pdfobject -> Pdf.pdfobject option -> unit

val print_dict_entry : Pdf.t -> string -> unit

val remove_clipping : Pdf.t -> int list -> Pdf.t 

val remove_unused_resources : Pdf.t -> Pdf.t
