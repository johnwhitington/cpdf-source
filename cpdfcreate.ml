let blank_document width height pages =
  let pdf_pages =
    map (fun () -> Pdfpage.blankpage (Pdfpaper.make Pdfunits.PdfPoint width height)) (many () pages)
  in
    let pdf, pageroot = Pdfpage.add_pagetree pdf_pages (Pdf.empty ()) in
      Pdfpage.add_root pageroot [] pdf

let blank_document_paper papersize pages =
  let pdf_pages =
    map (fun () -> Pdfpage.blankpage papersize) (many () pages)
  in
    let pdf, pageroot = Pdfpage.add_pagetree pdf_pages (Pdf.empty ()) in
      Pdfpage.add_root pageroot [] pdf
