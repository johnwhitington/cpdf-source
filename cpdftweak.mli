(** Miscellany *)

(** Make all lines in the PDF at least a certain thickness. *)
val thinlines : int list -> float -> Pdf.t -> Pdf.t

(** Make all text on certain pages black. *)
val blacktext : Cpdfaddtext.colour -> int list -> Pdf.t -> Pdf.t

(** Make all lines on certain pages black. *)
val blacklines : Cpdfaddtext.colour -> int list -> Pdf.t -> Pdf.t

(** Make all fills on certain pages black. *)
val blackfills : Cpdfaddtext.colour -> int list -> Pdf.t -> Pdf.t

(** Append page content. *)
val append_page_content : string -> bool -> bool -> int list -> Pdf.t -> Pdf.t

(** Print a dictionary entry. *)
val print_dict_entry : utf8:bool -> Pdf.t -> string -> unit

(** Get dictionary entries in JSON format *)
val get_dict_entries : utf8:bool -> Pdf.t -> string -> Pdfio.bytes

(** Remove clipping. *)
val remove_clipping : Pdf.t -> int list -> Pdf.t 

(** Find an object from an objspec. Raises and error if the chain is not found. *)
val find_obj : Pdf.t -> string -> Pdf.pdfobject

(** [remove_stream pdf objspec filename] replaces the stream at [objspec] with the contents of [filename]. *)
val replace_stream : Pdf.t -> string -> string -> unit

(** [replace_obj pdf objspec obj] replace the object at [objspec] (which must exist) with [obj]. *)
val replace_obj : Pdf.t -> string -> Pdf.pdfobject -> unit
