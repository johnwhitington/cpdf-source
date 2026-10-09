(** Text to PDF *)

(** Typeset a text file as a PDF. *)
val typeset :
  process_struct_tree:bool ->
  ?subformat:Cpdfua.subformat ->
  ?title:string ->
  papersize:Pdfpaper.t ->
  font:Cpdfembed.cpdffont ->
  fontsize:float ->
  colour:Cpdfutil.colour ->
  opacity:float ->
  linespacing:float ->
  outline:bool ->
  Pdfio.bytes ->
  Pdf.t

(** Typeset just one page, with resources in the PDF but not added to its page
    tree. Used for generating annotation appearances only. *)
val typeset_fake_pages :
  Pdf.t ->
  papersize:Pdfpaper.t ->
  font:Cpdfembed.cpdffont ->
  fontsize:float ->
  colour:Cpdfutil.colour ->
  opacity:float ->
  linespacing:float ->
  outline:bool ->
  Pdfio.bytes ->
  Pdfpage.t list
