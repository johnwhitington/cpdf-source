open Pdfutil 

let blank_document width height pages =
  let pdf_pages =
    map (fun () -> Pdfpage.blankpage (Pdfpaper.make Pdfunits.PdfPoint width height)) (many () pages)
  in
    let pdf, pageroot = Pdfpage.add_pagetree pdf_pages (Pdf.empty ()) in
    let pdf = Pdfpage.add_root pageroot [] pdf in
      Pdf.change_id pdf (string_of_float (Random.float 1.));
      pdf

let blank_document_paper papersize pages =
  let pdf_pages =
    map (fun () -> Pdfpage.blankpage papersize) (many () pages)
  in
    let pdf, pageroot = Pdfpage.add_pagetree pdf_pages (Pdf.empty ()) in
    let pdf = Pdfpage.add_root pageroot [] pdf in
      Pdf.change_id pdf (string_of_float (Random.float 1.));
      pdf
