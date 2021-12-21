val process_xobjects : Pdf.t ->
           Pdfpage.t ->
           (Pdf.t ->
            Pdf.pdfobject -> Pdf.pdfobject list -> Pdf.pdfobject list) ->
           unit
