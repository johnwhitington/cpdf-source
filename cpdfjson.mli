val json_of_object : Pdf.t -> (int -> unit) -> bool -> bool -> Pdf.pdfobject -> Cpdfyojson.Safe.t
val to_output : Pdfio.output -> parse_content:bool -> no_stream_data:bool -> decompress_streams:bool -> Pdf.t -> unit
val of_input : Pdfio.input -> Pdf.t
