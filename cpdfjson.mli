val to_output : Pdfio.output -> parse_content:bool -> no_stream_data:bool -> decompress_streams:bool -> precombine_page_content:bool -> Pdf.t -> unit
val of_input : Pdfio.input -> Pdf.t
