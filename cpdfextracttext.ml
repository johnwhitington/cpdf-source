open Pdfutil

let extract_page_text only_fontsize pdf _ page =
  let text_extractor = ref None in
  let right_font_size = ref false in
    fold_left ( ^ ) ""
      (map
        (function
         | Pdfops.Op_Tf (fontname, fontsize) ->
             right_font_size :=
               begin match only_fontsize with
                 Some x -> x = fontsize
               | _ -> false
               end;
             let fontdict =
               match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
               | None -> raise (Pdf.PDFError "Missing /Font in text extraction")
               | Some d ->
                   match Pdf.lookup_direct pdf fontname d with
                   | None -> raise (Pdf.PDFError "Missing font in text extraction")
                   | Some d -> d
             in
               text_extractor := Some (Pdftext.text_extractor_of_font pdf fontdict);
               ""
         | Pdfops.Op_Tj text when !text_extractor <> None ->
             if not !right_font_size then
               ""
             else
               Pdftext.utf8_of_codepoints
                 (Pdftext.codepoints_of_text (unopt !text_extractor) text)
         | Pdfops.Op_TJ (Pdf.Array objs) when !text_extractor <> None ->
             if not !right_font_size then
               ""
             else
               fold_left ( ^ ) ""
                 (option_map
                    (function
                     | Pdf.String text ->
                         Some
                           (Pdftext.utf8_of_codepoints
                             (Pdftext.codepoints_of_text (unopt !text_extractor) text))
                     | _ -> None)
                    objs)
         | _ -> "")
        (Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content))

(* For each page, extract all the ops with text in them, and concatenate it all together *)
let extract_text extract_text_font_size pdf range =
  fold_left (fun x y -> x ^ (if x <> "" && y <> "" then "\n" else "") ^ y) ""
    (Cpdfpage.map_pages (extract_page_text extract_text_font_size pdf) pdf range)

