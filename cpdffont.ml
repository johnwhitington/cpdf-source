open Pdfutil
open Cpdferror
open Pdfio

(* Embed missing fonts with Ghostscript. *)
let embed_missing_fonts path_to_ghostscript gs_quiet fi fo =
  if path_to_ghostscript = "" then begin
    Pdfe.log "Please supply path to gs with -gs\n";
    exit 2
  end;
    let gscall =
      Filename.quote_command path_to_ghostscript
       ((if gs_quiet then ["-dQUIET"] else []) @ 
       ["-dNOPAUSE";
        "-sDEVICE=pdfwrite";
        ("-sOUTPUTFILE=" ^ fo);
        "-dBATCH";
        fi])
    in
      match Sys.command gscall with
      | 0 -> exit 0
      | _ -> Pdfe.log "Font embedding failed.\n"; exit 2

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
  Cpdfpage.iter_pages
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

let print_font_table pdf fontname pagenumber =
  let page = try List.nth (Pdfpage.pages_of_pagetree pdf) (pagenumber - 1) with e -> error "page not found" in
    match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
    | Some fontdict ->
        let font =
          begin match Pdf.lookup_direct pdf fontname fontdict with
          | Some font -> font
          | None ->
             (* For each item in the fontdict, follow its value and find the basename. If it matches, return that font *)
             let font = ref None in
             iter
               (fun (k, v) ->
                  match Pdf.lookup_direct pdf "/BaseFont" v with
                  | Some (Pdf.Name n) when n = fontname -> font := Some v
                  | _ -> ())
               (match fontdict with Pdf.Dictionary d -> d | _ -> []);
             match !font with Some f -> f | None -> failwith (Printf.sprintf "print_font_encoding: font %s not found" fontname)
          end
        in
          let pdftextfont = Pdftext.read_font pdf font in
          let charset =
            match pdftextfont with
            | Pdftext.SimpleFont {Pdftext.fontdescriptor = Some {Pdftext.charset = Some cs}} -> Some cs
            | _ -> None
          in
          let extractor = Pdftext.text_extractor_of_font_real pdftextfont in
          let unicodedata = Cpdfunicodedata.unicodedata () in
          let unicodetable = Hashtbl.create 16000 in
           iter
            (fun x ->
               Hashtbl.add
                 unicodetable
                 (int_of_string ("0x" ^ x.Cpdfunicodedata.code_value))
                 (x.Cpdfunicodedata.code_value,
                  x.Cpdfunicodedata.general_category,
                  x.Cpdfunicodedata.character_name,
                  x.Cpdfunicodedata.iso_10646_comment_field))
            unicodedata;
            for x = 0 to 255 do
              let str =
                  (if Pdftext.is_identity_h pdftextfont then "\000" else "")
                ^ string_of_char (char_of_int x)
              in
              let codepoints = Pdftext.codepoints_of_text extractor str in
              let unicodenumber, unicodename, is_control =
                match codepoints with
                | [c] ->
                    begin try
                      let codeval, category, character_name, comment = Hashtbl.find unicodetable c in
                        codeval, character_name, category = "Cc"
                    with
                      Not_found -> "", "", false
                    end
                | _ -> "***multiple", "***multiple", false
              in
              let utf8 = if is_control then "<nonprintable>" else Pdftext.utf8_of_codepoints codepoints in
              let glyphnames = fold_left ( ^ ) "" (Pdftext.glyphnames_of_text extractor str) in
              let is_in_charset s = match charset with None -> true | Some cs -> mem s cs in
                if glyphnames <> ".notdef" && is_in_charset glyphnames then
                  Printf.printf
                    "%i = U+%s (%s - %s) = %s\n" x unicodenumber utf8 unicodename glyphnames
            done
    | _ -> failwith "addtext: font not found for width"

(* Extracts font to font.dat in CWD. *)
let extract_fontfile pagenumber fontname pdf =
  let resources = (select pagenumber (Pdfpage.pages_of_pagetree pdf)).Pdfpage.resources in
    match Pdf.lookup_direct pdf "/Font" resources with
    | None -> failwith "extract_fontfile: font not found"
    | Some fonts ->
        let fontobj = Pdf.lookup_fail ("no font " ^ fontname) pdf fontname fonts in
          let font = Pdftext.read_font pdf fontobj in
            match font with
            | Pdftext.CIDKeyedFont (_, {Pdftext.cid_fontdescriptor = {Pdftext.fontfile = Some fontfile}}, _)
            | Pdftext.SimpleFont {Pdftext.fontdescriptor = Some {Pdftext.fontfile = Some fontfile}} ->
                begin let objnum =
                  match fontfile with
                  | Pdftext.FontFile i -> i
                  | Pdftext.FontFile2 i -> i
                  | Pdftext.FontFile3 i -> i
                in
                  match Pdf.lookup_obj pdf objnum with
                  | Pdf.Stream s as obj ->
                      Pdfcodec.decode_pdfstream pdf obj;
                      begin match s with
                      | {contents = (_, Pdf.Got bytes)} ->
                           let fh = open_out_bin "font.dat" in
                             for x = 0 to bytes_size bytes - 1 do output_byte fh (bget bytes x) done;
                             close_out fh;
                             (* Now try to read using Pdfcff module *)
                             (*let font = Pdftruetype.to_type3 pdf font in*)
                               (*let extractor = Pdftext.text_extractor_of_font pdf fontobj in*)
                                 (*flprint "glyph names for incodes 0,1,2,3...";
                                 iter print_string (Pdftext.glyphnames_of_text extractor "\000\001\002\003\004\005\006\007");
                                 flprint "\n";*)
                                 ()
                      | _ -> failwith "extract_fontfile"
                      end
                  | _ -> failwith "extract_fontfile"
                end
            | _ -> failwith "unsupported or unfound font"


(* Remove Embedded fonts. This is done by removing the Font Descriptor. *)
let remove_fontdescriptor pdf = function
  | Pdf.Dictionary d as font ->
      begin match lookup "/Type" d with
      | Some (Pdf.Name "/Font") ->
         (match Pdf.lookup_direct pdf "/FontDescriptor" font with
         | Some fontdes ->
             let fontdescriptor' =
               Pdf.remove_dict_entry
                (Pdf.remove_dict_entry
                  (Pdf.remove_dict_entry fontdes "/FontFile")
                   "/FontFile2")
                 "/FontFile3"
             in
               Pdf.add_dict_entry font "/FontDescriptor" (Pdf.Indirect (Pdf.addobj pdf fontdescriptor'))
         | _ -> font)
      | _ -> font
      end
  | x -> x

let remove_fonts pdf =
  Pdf.objiter (fun k v -> ignore (Pdf.addobj_given_num pdf (k, remove_fontdescriptor pdf v))) pdf;
  pdf

(* List fonts *)
let list_font pdf page (name, dict) =
  let subtype =
    match Pdf.lookup_direct pdf "/Subtype" dict with
    | Some (Pdf.Name n) -> Pdfwrite.string_of_pdf (Pdf.Name n)
    | _ -> ""
  in let basefont =
    match Pdf.lookup_direct pdf "/BaseFont" dict with
    | Some (Pdf.Name n) -> Pdfwrite.string_of_pdf (Pdf.Name n)
    | _ -> ""
  in let encoding =
   match Pdf.lookup_direct pdf "/Encoding" dict with
    | Some (Pdf.Name n) -> Pdfwrite.string_of_pdf (Pdf.Name n)
    | _ -> ""
  in 
    (page, name, subtype, basefont, encoding)

let list_fonts pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
    flatten
      (map
        (fun (num, page) ->
           if mem num range then
             begin match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
             | Some (Pdf.Dictionary fontdict) ->
                 map (list_font pdf num) fontdict
             | _ -> []
             end
           else
             [])
        (combine (ilist 1 (length pages)) pages))

let string_of_font (p, n, s, b, e) =
  Printf.sprintf "%i %s %s %s %s\n" p n s b e

let json_of_font (pagenum, name, subtype, basefont, encoding) =
  `Assoc
    [("page", `Int pagenum);
     ("name", `String name);
     ("subtype", `String subtype);
     ("basefont", `String basefont);
     ("encoding", `String encoding)]

let print_fonts ?(json=false) pdf range =
  if json
    then flprint (Cpdfyojson.Safe.pretty_to_string (`List (map json_of_font (list_fonts pdf range))))
    else flprint (fold_left ( ^ ) "" (map string_of_font (list_fonts pdf range)))
