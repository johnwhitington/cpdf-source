(** Lossless compression *)

(** Compresses all streams in the PDF document which are uncompressed, using
/FlateDecode, leaving out metadata.  If the PDF is encrypted, does nothing. *)
val recompress_pdf : Pdf.t -> Pdf.t

(** Decompresses all streams in a PDF document, assuming it isn't encrypted. *)
val decompress_pdf : Pdf.t -> Pdf.t

(** Squeeze a PDF *)
val squeeze : ?logto:string -> ?pagedata:bool -> ?recompress:bool -> Pdf.t -> unit
