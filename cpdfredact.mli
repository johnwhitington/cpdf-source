(** Redaction. *)

type operation = Remove | Leave | Chop

type detection = Touching | Enclosing

type spec = operation * detection option

(** Redact a PDF in the given shape in the given range. *)
val redact : 
  Pdf.t ->
  text:spec ->
  images:spec ->
  inline_images:spec ->
  vectors:spec ->
  annotations:spec ->
  path_to_jbig2dec:string ->
  path_to_convert:string ->
  path_to_jbig2enc:string ->
  paths:(float * float * float * float) list ->
  invert:bool ->
  show:bool ->
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
  text:spec ->
  images:spec ->
  inline_images:spec ->
  vectors:spec ->
  annotations:spec ->
  path_to_jbig2dec:string ->
  path_to_convert:string ->
  path_to_jbig2enc:string ->
  ?typ:string ->
  invert:bool ->
  show:bool ->
  color:Cpdfaddtext.colour ->
  outline:bool ->
  opacity:float ->
  linewidth:float ->
  underneath:bool ->
  int list ->
  Pdf.t

(** Show bounding boxes in the given shape or, if None, the whole page in the
    given rage. [light] will use lighter colours. *)
val show_bounding_boxes : fast:bool -> paths:(float * float * float * float) option list -> light:bool -> Pdf.t -> int list -> Pdf.t
