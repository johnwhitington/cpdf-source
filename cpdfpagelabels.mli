(** Page labels *)

(** Add page labels. *)
val add_page_labels :
  Pdf.t -> bool -> Pdfpagelabels.labelstyle -> string option -> int -> int list -> unit

val add_page_labels_json :
  Pdf.t -> Cpdfyojson.Safe.t -> unit
