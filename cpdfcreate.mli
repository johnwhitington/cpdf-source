(** Making new documents *)

(** Make a blank document given x and y page dimensions in points and a number of pages *)
val blank_document : float -> float -> int -> Pdf.t

(** The same, but give a Pdfpaper.t paper size. *)
val blank_document_paper : Pdfpaper.t -> int -> Pdf.t
