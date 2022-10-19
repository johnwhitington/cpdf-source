(** Text to PDF *)

(** Typeset a text file as a PDF. *)
val typeset : papersize:Pdfpaper.t -> font:Cpdfembed.cpdffont -> fontsize:float -> Pdfio.bytes -> Pdf.t
