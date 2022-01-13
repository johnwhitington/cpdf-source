(** Text to PDF *)

(** Typeset a text file as a PDF. *)
val typeset : papersize:Pdfpaper.t -> font:Pdftext.standard_font -> fontsize:float -> Pdfio.bytes -> Pdf.t
