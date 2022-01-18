(** Miscellany *)

(** Make all lines in the PDF at least a certain thickness. *)
val thinlines : int list -> float -> Pdf.t -> Pdf.t

(** Make all text on certain pages black. *)
val blacktext : Cpdfaddtext.color -> int list -> Pdf.t -> Pdf.t

(** Make all lines on certain pages black. *)
val blacklines : Cpdfaddtext.color -> int list -> Pdf.t -> Pdf.t

(** Make all fills on certain pages black. *)
val blackfills : Cpdfaddtext.color -> int list -> Pdf.t -> Pdf.t

(** Append page content. *)
val append_page_content : string -> bool -> bool -> int list -> Pdf.t -> Pdf.t

(** Remove a dictionary entry. *)
val remove_dict_entry : Pdf.t -> string -> Pdf.pdfobject option -> unit

(** Replace a dictionary entry. *)
val replace_dict_entry : Pdf.t -> string -> Pdf.pdfobject -> Pdf.pdfobject option -> unit

(** Print a dictionary entry. *)
val print_dict_entry : Pdf.t -> string -> unit

(** Get dictionary entries in JSON format *)
val get_dict_entries : Pdf.t -> string -> Pdfio.bytes

(** Remove clipping. *)
val remove_clipping : Pdf.t -> int list -> Pdf.t 

(** Remove unused resources. *)
val remove_unused_resources : Pdf.t -> Pdf.t
