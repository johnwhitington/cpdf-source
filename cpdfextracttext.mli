(** Extract text *)
val extract_page_text : float option -> Pdf.t -> 'a -> Pdfpage.t -> string

val extract_text : float option -> Pdf.t -> int list -> string 
