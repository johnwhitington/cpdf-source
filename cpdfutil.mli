val process_xobjects : Pdf.t ->
           Pdfpage.t ->
           (Pdf.t ->
            Pdf.pdfobject -> Pdf.pdfobject list -> Pdf.pdfobject list) ->
           unit

(*val change_pattern_matrices_page : Pdf.t -> Pdftransform.transform_matrix -> Pdfpage.t -> Pdfpage.t*)

val combine_pdf_resources : Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject -> Pdf.pdfobject

val ppstub : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c * 'a * Pdftransform.transform_matrix
