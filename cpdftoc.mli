(** Table of contents *)

(** Typeset a table of contents and prepend to the document. *)
val typeset_table_of_contents : font:Pdftext.font -> fontsize:float -> title:string -> bookmark:bool -> Pdf.t -> Pdf.t
