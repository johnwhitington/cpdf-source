(** Text to PDF *)

(** Typeset a text file as a PDF. *)
val typeset : ?embedinfo:(Pdf.t * Pdfio.bytes * string * Pdftext.encoding) -> papersize:Pdfpaper.t -> font:Pdftext.font -> fontsize:float -> Pdfio.bytes -> Pdf.t
