open Pdfutil

(* Embed missing fonts with Ghostscript. *)
let embed_missing_fonts path_to_ghostscript gs_quiet fi fo =
  if path_to_ghostscript = "" then begin
    Printf.eprintf "Please supply path to gs with -gs\n%!";
    exit 2
  end;
    let gscall =
      path_to_ghostscript ^
      " -dNOPAUSE " ^ (if gs_quiet then "-dQUIET" else "") ^ " -sDEVICE=pdfwrite -sOUTPUTFILE=" ^ Filename.quote fo ^
      " -dBATCH " ^ Filename.quote fi
    in
      match Sys.command gscall with
      | 0 -> exit 0
      | _ -> Printf.eprintf "Font embedding failed.\n%!"; exit 2

(* Copy a font from [frompdf] with name [fontname] on page [fontpage] to [pdf] on all pages in [range] *)
let copy_font frompdf fontname fontpage range pdf =
  match Pdf.renumber_pdfs [frompdf; pdf] with
  | [] | [_] | _::_::_::_ -> assert false
  | [frompdf; pdf] ->
      (* 1. Get fontpage *)
      let frompdf_pages = Pdfpage.pages_of_pagetree frompdf in
        let frompdf_page =
           try select fontpage frompdf_pages with
             Not_found -> failwith "copy_font: Page not found in input pdf"
        in
      (* 2. Extract font *)
      let fonts =
        match Pdf.lookup_direct frompdf "/Font" frompdf_page.Pdfpage.resources with
        | Some f -> f
        | None -> failwith "copy_font: font not found"
      in
        let fromfont =
          match Pdf.lookup_direct frompdf fontname fonts with
          | Some f -> f
          | None -> failwith "copy_font: font not found"
        in
          let basefontname =
            match Pdf.lookup_direct frompdf "/BaseFont" fromfont with
            | Some (Pdf.Name n) -> n
            | _ -> "/CopyFontAddedNoName"
          in
      (* 3. Get all objects forming font (except main /Font one) *)
      let objnumbers = Pdf.objects_referenced [] [] frompdf fromfont in
      (* 4. Copy them to from frompdf to pdf. *)
      iter (function objnum -> Pdf.addobj_given_num pdf (objnum, Pdf.lookup_obj frompdf objnum)) objnumbers;
      (* 5. Get pages from pdf *)
      let pdf_pages = Pdfpage.pages_of_pagetree pdf in
      (* 6. Add the font to pages in range *)
      let pages' =
        map
        (function (page, pagenum) ->
           if mem pagenum range then
             let font =
               match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
               | Some f -> f
               | None -> Pdf.Dictionary []
             in
               let font' =
                 match font with
                 | (Pdf.Dictionary _) as d ->
                     Pdf.add_dict_entry d basefontname fromfont
                 | _ -> failwith "copy_font: error"
               in
                 let resources' =
                   Pdf.add_dict_entry page.Pdfpage.resources "/Font" font'
                 in
                   {page with
                      Pdfpage.resources = resources'}
             else page)
        (combine pdf_pages (indx pdf_pages));
      in
      (* 7. Put the pages back into the pdf, and return *)
      let pdf, root = Pdfpage.add_pagetree pages' pdf in
        Pdfpage.add_root root [] pdf

(* Missing Fonts *)
let is_missing pdf dict =
  match Pdf.lookup_direct pdf "/FontDescriptor" dict with
  | None -> true
  | Some d ->
      match Pdf.lookup_direct pdf "/FontFile" d with
      | Some _ -> false
      | None ->
          match Pdf.lookup_direct pdf "/FontFile2" d with
          | Some _ -> false
          | None ->
              match Pdf.lookup_direct pdf "/FontFile3" d with
              | Some _ -> false
              | None -> true

let missing_font pdf page (name, dict) =
  if is_missing pdf dict then
    let subtype =
      match Pdf.lookup_direct pdf "/Subtype" dict with
      | Some (Pdf.Name n) -> n
      | _ -> ""
    and basefont =
      match Pdf.lookup_direct pdf "/BaseFont" dict with
      | Some (Pdf.Name n) -> n
      | _ -> ""
    and encoding =
     match Pdf.lookup_direct pdf "/Encoding" dict with
      | Some (Pdf.Name n) -> n
      | _ -> ""
    in 
      if Pdftext.standard_font_of_name basefont <> None then () else
      Printf.printf "%i, %s, %s, %s, %s\n" page name subtype basefont encoding

let missing_fonts pdf range =
  Cpdf.iter_pages
    (fun num page ->
       match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
       | Some (Pdf.Dictionary fontdict) ->
           (* Extract descendant fonts *)
           let name_dict_pairs =
             flatten
               (map
                  (fun (name, dict) ->
                     match Pdf.lookup_direct pdf "/DescendantFonts" dict with
                     | Some (Pdf.Array desc_fonts) -> map (fun d -> name, d) desc_fonts
                     | _ -> [(name, dict)])
                  fontdict)
           in
             iter (missing_font pdf num) name_dict_pairs
       | _ -> ())
    pdf
    range
