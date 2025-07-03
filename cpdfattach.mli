(** File Attachments *)
type t =
  {name : string;
   pagenumber : int;
   data : unit -> Pdfio.bytes;
   description : string option;
   relationship : string option}

(** Remove characters which might not make good filenames from a UTF8 string. *)
val remove_unsafe_characters : string -> string

(** [attach_file keepversion topage pdf filename] attaches the file in
    [filename] to the pdf, optionally to a page (rather than document-level).
    If keepversion is true, the PDF version number won't be altered. *)
val attach_file : ?memory:Pdfio.bytes -> bool -> int option -> Pdf.t -> string -> Pdf.t

(** Remove attached files. *)
val remove_attached_files : Pdf.t -> Pdf.t

(** List attached files. Attachment name and page number. Page 0 is document level. *)
val list_attached_files : Pdf.t -> t list

(** Dump attached files to a given directory. *)
val dump_attached_files : Pdf.t -> string -> unit

(** Total size in bytes of all attached files. *)
val size_attached_files : Pdf.t -> int
