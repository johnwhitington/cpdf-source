(** Redaction. *)

(** Redact a PDF in the given shape in the given range. Not implemented yet. *)
val redact : Pdf.t -> path:(float * float * float * float) -> int list -> Pdf.t

(** Apply redaction annotations in a given range. Not implemented yet. *)
val apply : Pdf.t -> int list -> unit

(** Apply the given type of annotations as if they were redaction annotations in the given range. Not implemented yet. *)
val apply_type : Pdf.t -> string -> int list -> unit

(** Show bounding boxes in the given shape or, if None, the whole page int the
    given rage. [light] will use lighter colours. *)
val show_bounding_boxes : fast:bool -> shape:(float * float * float * float) option -> light:bool -> Pdf.t -> int list -> Pdf.t
