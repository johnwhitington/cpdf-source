open Pdfutil

let print_pdf_objs pdf =
  Printf.printf "Trailerdict: %s\n" (Pdfwrite.string_of_pdf pdf.Pdf.trailerdict);
  Printf.printf "Root: %i\n" pdf.Pdf.root;
  begin match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | Some catalog -> 
      Printf.printf "Catalog: %s\n" (Pdfwrite.string_of_pdf catalog);
      begin match Pdf.lookup_direct pdf "/Pages" catalog with
      | Some pages ->
          Printf.printf "Pages: %s\n" (Pdfwrite.string_of_pdf pages)
      | None ->
          flprint "no catalog\n"
      end
  | None ->
       flprint "No catalog!\n"
  end;
  Pdf.objiter
    (fun n obj ->
       Printf.printf "%i 0 obj:\n\n" n;
       Printf.printf "%s\n" (Pdfwrite.string_of_pdf obj))
    pdf
