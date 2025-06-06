open Pdfutil
open Cpdferror
open Pdfio

(* Embed missing fonts with Ghostscript. *)
let embed_missing_fonts path_to_ghostscript gs_quiet fi fo =
  if path_to_ghostscript = "" then begin
    Pdfe.log "Please supply path to gs with -gs\n";
    exit 2
  end;
  Cpdfutil.check_injectible fo;
  Cpdfutil.check_injectible fi;
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

(* Return any font dictionary from a page or xobject given a chain and the page number. *)
let font_from_name pdf fontname pagenumber =
  try
    let resources = ref (select pagenumber (Pdfpage.pages_of_pagetree pdf)).Pdfpage.resources in
    let chain = ref (tl (String.split_on_char '/' fontname)) in
    let font = ref Pdf.Null in
      while !chain <> [] do
        match !chain with
        | [f] ->
            font := unopt (Pdf.lookup_chain pdf !resources ["/Font"; ("/" ^ f)]);
            chain := []
        | x::xs ->
            resources := unopt (Pdf.lookup_chain pdf !resources ["/XObject"; "/" ^ x; "/Resources"]);
            chain := xs
        | [] -> ()
      done;
      !font
  with
    _ -> Pdfe.log (Printf.sprintf "Not found: font %s on page %i\n" fontname pagenumber); Pdf.Null

(* Copy a font from [frompdf] with name [fontname] on page [fontpage] to [pdf] on all pages in [range] *)
let copy_font frompdf fontname fontpage range pdf =
  match Pdf.renumber_pdfs [frompdf; pdf] with
  | [] | [_] | _::_::_::_ -> assert false
  | [frompdf; pdf] ->
      (* Get font and name *)
      let fromfont = font_from_name frompdf fontname fontpage in
      let basefontname =
        match Pdf.lookup_direct frompdf "/BaseFont" fromfont with
        | Some (Pdf.Name n) -> n
        | _ -> "/CopyFontAddedNoName"
      in
      (* Get all objects forming font (except main /Font one) *)
      let objnumbers = Pdf.objects_referenced [] [] frompdf fromfont in
      (* Copy them to from frompdf to pdf. *)
      iter (function objnum -> Pdf.addobj_given_num pdf (objnum, Pdf.lookup_obj frompdf objnum)) objnumbers;
      (* Get pages from pdf *)
      let pdf_pages = Pdfpage.pages_of_pagetree pdf in
      (* Add the font to pages in range *)
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
      (* Put the pages back into the pdf, and return *)
      let pdf, root = Pdfpage.add_pagetree pages' pdf in
        Pdfpage.add_root root [] pdf

(* Missing Fonts *)
let is_missing pdf dict =
  match Pdf.lookup_direct pdf "/Subtype" dict with
  | Some (Pdf.Name "/Type3") -> false
  | _ -> 
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

let missing_font ?l pdf page (name, dict) =
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
      | _ -> "Built-in"
    in 
      match l with
      | None -> Printf.printf "%i, %s, %s, %s, %s\n" page name subtype basefont encoding
      | Some r -> r := (page, name, subtype, basefont, encoding)::!r

(* FIXME We must look at fonts in Form xobjects too. And process them only
   once. Plus, process fonts from pages only once too. So, introduce object
   numbers as the key (fonts can be direct too though! *)
let missing_fonts ?l pdf range =
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
             iter (missing_font ?l pdf num) name_dict_pairs
       | _ -> ())
    pdf
    range

let missing_fonts_return pdf range =
  let l = ref [] in
    missing_fonts ~l pdf range;
    !l

let print_font_table pdf fontname pagenumber =
  let fontdict = font_from_name pdf fontname pagenumber in
  (* For each item in the fontdict, follow its value and find the basename. If it matches, return that font *)
  let font = ref (Some fontdict) in
    iter
      (fun (k, v) ->
         match Pdf.lookup_direct pdf "/BaseFont" v with
         | Some (Pdf.Name n) when n = fontname -> font := Some v
         | _ -> ())
      (match fontdict with Pdf.Dictionary d -> d | _ -> []);
  let font = match !font with Some f -> f | None -> failwith (Printf.sprintf "print_font_encoding: font %s not found" fontname) in
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

let extract_fontfile pagenumber fontname filename pdf =
  match Pdftext.read_font pdf (font_from_name pdf fontname pagenumber) with
  | Pdftext.CIDKeyedFont (_, {Pdftext.cid_fontdescriptor = {Pdftext.fontfile = Some fontfile}}, _)
  | Pdftext.SimpleFont {Pdftext.fontdescriptor = Some {Pdftext.fontfile = Some fontfile}} ->
      begin let objnum =
        match fontfile with
        | Pdftext.FontFile i | Pdftext.FontFile2 i | Pdftext.FontFile3 i -> i
      in
        match Pdf.lookup_obj pdf objnum with
        | Pdf.Stream s as obj ->
            Pdfcodec.decode_pdfstream pdf obj;
            begin match s with
            | {contents = (_, Pdf.Got bytes)} ->
                 let fh = open_out_bin filename in
                   for x = 0 to bytes_size bytes - 1 do output_byte fh (bget bytes x) done;
                   close_out fh
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
    (page, name, subtype, basefont, encoding, Pdf.direct pdf dict)

(* List the fonts used in an xobject, and in any of the xobjects it has. Do not
   process an xobject twice. *)
let xobjs_processed = null_hash ()

let rec list_fonts_xobject pdf pagenum xobjname xobjnum =
  match Hashtbl.find_opt xobjs_processed xobjnum with
  | None ->
      let from_xobjs =
        match Pdf.lookup_direct pdf "/Resources" (Pdf.lookup_obj pdf xobjnum) with
        | Some r ->
            begin match Pdf.lookup_direct pdf "/XObject" r with
            | Some (Pdf.Dictionary xobjs) ->
                flatten (option_map (function (n, Pdf.Indirect i) -> Some (list_fonts_xobject pdf pagenum (xobjname ^ n) i) | _ -> None) xobjs)
            | _ -> []
            end
        | _ -> []
      in
        begin match Pdf.lookup_direct pdf "/Resources" (Pdf.lookup_obj pdf xobjnum) with
        | Some r ->
            begin match Pdf.lookup_direct pdf "/Font" r with
            | Some (Pdf.Dictionary fonts) -> map (list_font pdf pagenum) (map (function (n, f) -> (xobjname ^ n, f)) fonts) @ from_xobjs
            | _ -> from_xobjs
            end
        | None -> from_xobjs
        end
  | Some _ ->
      Hashtbl.add xobjs_processed xobjnum ();
      []
  
let list_fonts pdf range =
  Hashtbl.clear xobjs_processed;
  let pages = Pdfpage.pages_of_pagetree pdf in
    flatten
      (map
        (fun (num, page) ->
           if mem num range then
             let from_xobjs =
               match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
               | Some (Pdf.Dictionary xobjs) ->
                   flatten (option_map (function (n, Pdf.Indirect i) -> Some (list_fonts_xobject pdf num n i) | _ -> None) xobjs)
               | _ -> []
             in
               begin match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
               | Some (Pdf.Dictionary fontdict) ->
                   map (list_font pdf num) fontdict @ from_xobjs
               | _ -> from_xobjs
               end
           else
             [])
        (combine (ilist 1 (length pages)) pages))

let string_of_font (p, n, s, b, e, _) =
  Printf.sprintf "%i %s %s %s %s\n" p n s b e

let json_of_font (pagenum, name, subtype, basefont, encoding, _) =
  `Assoc
    [("page", `Int pagenum);
     ("name", `String name);
     ("subtype", if subtype = "" then `Null else `String subtype);
     ("basefont", if basefont = "" then `Null else `String basefont);
     ("encoding", if encoding = "" then `Null else `String encoding)]

let json_fonts pdf range =
  `List (map json_of_font (list_fonts pdf range))

let print_fonts ?(json=false) pdf range =
  if json
    then flprint (Cpdfyojson.Safe.pretty_to_string (`List (map json_of_font (list_fonts pdf range))))
    else flprint (fold_left ( ^ ) "" (map string_of_font (list_fonts pdf range)))
