(** Text to PDF *)

(** Typeset a text file as a PDF. *)
val typeset : process_struct_tree:bool -> ?subformat:Cpdfua.subformat -> ?title:string -> papersize:Pdfpaper.t -> font:Cpdfembed.cpdffont -> fontsize:float -> Pdfio.bytes -> Pdf.t
