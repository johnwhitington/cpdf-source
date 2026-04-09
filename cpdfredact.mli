
val redact : Pdf.t -> path:((float * float) list) -> page:Pdfpage.t -> unit

val apply : Pdf.t -> int list -> unit

val apply_type : string -> Pdf.t -> int list -> unit
