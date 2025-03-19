(** Table of contents *)

(** Typeset a table of contents and prepend to the document. *)
val typeset_table_of_contents : font:Cpdfembed.cpdffont -> fontsize:float -> title:string -> bookmark:bool -> dotleader:bool -> process_struct_tree:bool -> ?subformat:Cpdfua.subformat -> Pdf.t -> Pdf.t
