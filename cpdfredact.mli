(** Redaction. *)

(** Redact a PDF in the given shape in the given range. *)
val redact : 
  Pdf.t ->
  annots:bool ->
  path_to_jbig2dec:string ->
  path_to_convert:string ->
  path_to_jbig2enc:string ->
  path:(float * float * float * float) ->
  color:Cpdfaddtext.colour ->
  outline:bool ->
  opacity:float ->
  linewidth:float ->
  underneath:bool ->
  int list ->
  Pdf.t

(** Apply redaction annotations in a given range. *)
val apply :
  Pdf.t ->
  annots:bool ->
  path_to_jbig2dec:string ->
  path_to_convert:string ->
  path_to_jbig2enc:string ->
  ?typ:string ->
  color:Cpdfaddtext.colour ->
  outline:bool ->
  opacity:float ->
  linewidth:float ->
  underneath:bool ->
  int list ->
  Pdf.t

(** Show bounding boxes in the given shape or, if None, the whole page int the
    given rage. [light] will use lighter colours. *)
val show_bounding_boxes : fast:bool -> shape:(float * float * float * float) option -> light:bool -> Pdf.t -> int list -> Pdf.t
