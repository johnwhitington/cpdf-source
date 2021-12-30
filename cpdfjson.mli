val json_of_object : ?clean_strings:bool -> Pdf.t -> (int -> unit) -> bool -> bool -> Pdf.pdfobject -> Cpdfyojson.Safe.t
val object_of_json : Cpdfyojson.Safe.t -> Pdf.pdfobject
val to_output : Pdfio.output -> parse_content:bool -> no_stream_data:bool -> decompress_streams:bool -> ?clean_strings:bool -> Pdf.t -> unit
val of_input : Pdfio.input -> Pdf.t
