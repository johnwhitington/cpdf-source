(** Show composition of a PDF *)

(** [show_composition filesize json pdf] prints the composition of a document to
Standard Output. *)
val show_composition : int -> bool -> Pdf.t -> unit

(** [show_composition_json_blob filesize json pdf] returns the composition of a
document in JSON format. *)
val show_composition_json_blob : int -> Pdf.t -> Pdfio.bytes
