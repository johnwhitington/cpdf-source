(** Read and write PDFs in CPDFJSON format *)

(** Write a PDF in CPDFJSON format. [parse_content] parses page content
    streams, [no_stream_data] will omit stream data, [decompress_streams]
    decompresses all streams, [clean_strings] will convert any UTF16BE strings
    to PDFDocEncoding if it can. *)
val to_output : Pdfio.output -> ?utf8:bool -> parse_content:bool -> no_stream_data:bool -> decompress_streams:bool -> ?clean_strings:bool -> Pdf.t -> unit

(** Read a CPDFJSON PDF from an input. /Length entries will be corrected automatically. *)
val of_input : Pdfio.input -> Pdf.t

(** Convert a single PDF object to CPDFJSON format. [clean_strings] is as above.
   Then the PDF file, then a function which is usually [function _ -> ()], then
   [no_stream_data] as above, then [parse_content_streams] as above, and
   finally the object itself. *)
val json_of_object : ?utf8:bool -> ?clean_strings:bool -> Pdf.t -> (int -> unit) -> no_stream_data:bool -> parse_content:bool -> Pdf.pdfobject -> Cpdfyojson.Safe.t

(** Convert a single CPDFJSON object to a PDF object *)
val object_of_json : ?utf8:bool -> Cpdfyojson.Safe.t -> Pdf.pdfobject
