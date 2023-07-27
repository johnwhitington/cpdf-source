(** Experimental text extractor *)

(** Extract page text for a given page. *) 
val extract_page_text : float option -> Pdf.t -> 'a -> Pdfpage.t -> string

(** Extract page text for given page range. *)
val extract_text : float option -> Pdf.t -> int list -> string 
