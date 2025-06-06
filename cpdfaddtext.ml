(* Superimpose text, page numbers etc. *)
open Pdfutil
open Cpdferror

type colour =
  Grey of float
| RGB of float * float * float
| CYMK of float * float * float * float

(* Process UTF8 text to charcodes, given a font. *)
let charcodes_of_utf8 font s =
  let extractor = Pdftext.charcode_extractor_of_font_real ~debug:false font in
  let codepoints = Pdftext.codepoints_of_utf8 s in
    let charcodes =
      option_map
        (fun codepoint ->
           match extractor codepoint with
           | Some cc -> Some cc
           | None ->
               Pdfe.log (Printf.sprintf "Warning: character not found in font for unicode codepoint 0x%X\n" codepoint);
               None)
        codepoints
    in
    implode (map char_of_int charcodes)

(* Get the width of some text in the given font *)
let width_of_text font text =
  match font with
  | Pdftext.SimpleFont {Pdftext.fontmetrics = Some fontmetrics} ->
       begin try
         fsum (map (fun c -> fontmetrics.(int_of_char c)) (explode text))
       with
         _ -> 0.
       end
  | _ -> 0.

let colour_op = function
  | RGB (r, g, b) -> Pdfops.Op_rg (r, g, b)
  | Grey g -> Pdfops.Op_g g
  | CYMK (c, y, m, k) -> Pdfops.Op_k (c, y, m, k)

let colour_op_stroke = function
  | RGB (r, g, b) -> Pdfops.Op_RG (r, g, b)
  | Grey g -> Pdfops.Op_G g
  | CYMK (c, y, m, k) -> Pdfops.Op_K (c, y, m, k)

let ops font fontpack fontpackpdfobjs fontname longest_w x y rotate hoffset voffset outline linewidth unique_fontname unique_fontnames unique_extgstatename colour fontsize text =
  let textops =
    match fontpack with
    | Some fontpack ->
        let codepoints = Pdftext.codepoints_of_utf8 text in
        let triples = option_map (Cpdfembed.get_char fontpack) codepoints in
        let collated = Cpdfembed.collate_runs triples in
          flatten
            (map
              (fun l ->
                let (_, fontnum, _) = hd l in
                  [Pdfops.Op_Tf (List.nth unique_fontnames fontnum, fontsize);
                   Pdfops.Op_Tj (implode (map (fun (charcode, _, _) -> char_of_int charcode) l))])
              collated)
    | None ->
        match font with
        | Some font -> [Pdfops.Op_Tf (unique_fontname, fontsize); Pdfops.Op_Tj (charcodes_of_utf8 font text)]
        | None -> [Pdfops.Op_Tf (unique_fontname, fontsize); Pdfops.Op_Tj text]
  in
    [Pdfops.begin_artifact;
     Pdfops.Op_q;
     Pdfops.Op_BMC "/CPDFSTAMP";
      Pdfops.Op_cm
        (Pdftransform.matrix_of_transform
          [Pdftransform.Translate (x -. hoffset, y -. voffset);
           Pdftransform.Rotate ((0., 0.), rotate)]);
      Pdfops.Op_BT]
    @ (if outline then [Pdfops.Op_w linewidth; Pdfops.Op_Tr 1] else [Pdfops.Op_Tr 0])
    @ [colour_op colour; colour_op_stroke colour]
    @ (match unique_extgstatename with None -> [] | Some n -> [Pdfops.Op_gs n])
    @ textops
    @ [Pdfops.Op_ET; Pdfops.Op_EMC; Pdfops.Op_Q; Pdfops.end_artifact]

type justification = LeftJustify | CentreJustify | RightJustify

(* Find the h-offset for justification based on the longest width, the current
width, the justification and the position. *)
let find_justification_offsets longest_w w position j =
  let open Cpdfposition in
    match j with
    | LeftJustify ->
        begin match position with
        | TopLeft _ | Left _ | PosLeft _ | BottomLeft _ -> 0.
        | Top _ | PosCentre _ | Bottom _ | Centre -> (longest_w -. w) /. 2.
        | TopRight _ | BottomRight _ | PosRight _ | Right _ -> longest_w -. w
        | Diagonal -> 0.
        | ReverseDiagonal -> 0.
        end
    | RightJustify ->
        begin match position with
        | TopLeft _ | Left _ | PosLeft _ | BottomLeft _ -> ~-.(longest_w -. w)
        | Top _ | PosCentre _ | Bottom _ | Centre -> ~-.((longest_w -. w) /. 2.)
        | TopRight _ | BottomRight _ | PosRight _ | Right _ -> 0.
        | Diagonal -> 0.
        | ReverseDiagonal -> 0.
        end
    | CentreJustify ->
        begin match position with
        | TopLeft _ | Left _ | PosLeft _ | BottomLeft _ -> ~-.((longest_w -. w) /. 2.)
        | Top _ | PosCentre _ | Bottom _ | Centre -> 0.
        | TopRight _ | BottomRight _ | PosRight _ | Right _ -> (longest_w -. w) /. 2.
        | Diagonal -> 0.
        | ReverseDiagonal -> 0.
        end

(* Lex an integer from the table *)
let extract_num header s =
  match Pdfgenlex.lex_string (Hashtbl.find header s) with
    [Pdfgenlex.LexInt i] -> Pdf.Integer i
  | [Pdfgenlex.LexReal f] -> Pdf.Real f
  | _ -> raise (Failure ("extract_num: " ^ s))

(* For finding the height for URL links, we try to find the Cap Height for the
   font. We fall back to using the font size alone if we cannot get the cap
   height. *)
let cap_height font fontname =
  match font with
  | Some (Pdftext.SimpleFont {fontdescriptor = Some {capheight}}) ->
      Some capheight
  | _ ->
      try
        let font = unopt (Pdftext.standard_font_of_name ("/" ^ fontname)) in
        let header, _, _, _ = Pdfstandard14.afm_data font in
          let capheight = try extract_num header "CapHeight" with _ -> Pdf.Integer 0 in
            Some (match capheight with Pdf.Integer i -> float_of_int i | Pdf.Real r -> r | _ -> 0.)
      with
        _ -> None
        
let rec process_text time text m =
  match m with
  | [] -> Cpdfstrftime.strftime ~time text
  | (s, r)::t -> process_text time (string_replace_all_lazy s r text) t

(* Find any %URL, sub in the text and return the new text together with a list
   of ordered (line num, URL, startpos, endpos) data.
   This will be used after any other %Specials have been processed, so that the
   positions do not change. *)

(* text|url]abc -> text, url, abc *)
let extract_url line =
  let text, rest = cleavewhile (neq '|') line in
    if rest = [] then error "bad URL syntax in text" else
    let url, rest = cleavewhile (neq ']') (tl rest) in
      if rest = [] then error "bad URL syntax in text" else
      (text, url, tl rest)

(* multiple %URL[a|b] *)
let get_urls_line line =
  let line = explode line in
  let urls = ref [] in
  let pos = ref 0 in
  let outline = ref [] in
  let rec loop = function
    | '%'::'U'::'R'::'L'::'['::t ->
        let text, url, rest = extract_url t in
          outline := rev text @ !outline;
          urls := (implode url, !pos, !pos + length text)::!urls;
          pos += length text;
          loop rest
    | h::t ->
        outline := h::!outline;
        pos += 1;
        loop t
    | [] -> ()
  in
    loop line;
    (implode (rev !outline), rev !urls)

(* Return page label at pdf page num, or page number in arabic if no label *) 
let pagelabel pdf num =
  Pdfpagelabels.pagelabeltext_of_pagenumber
    num
    (Pdfpagelabels.complete (Pdfpagelabels.read pdf))

(*let debug pdf fastrefnums =
  iter
    (fun m -> Printf.printf "%i %s (%i)\n" m.Pdfmarks.level (Pdftext.utf8_of_pdfdocstring m.Pdfmarks.text) (Pdfpage.pagenumber_of_target ~fastrefnums pdf m.Pdfmarks.target))*)
    
(* Return UTF8 of current bookmark at given level at start of page. No bookmark
   available = empty string. 

   Method: Remove from the list anything from end up to the last mark which is
   at higher level. This prevents sections in an earlier chapter showing up as
   bookmarks in a later chapter if no section has yet been introduced in that
   chapter.  Do this by reversing, then keeping everything up to any higher
   level. Then re-reverse and filter to only the level required.  Then, We want
   the first which is on the target page or, if none, the last available. *)
let bookmark marks fastrefnums level pdf num =
  let before = takewhile (fun mark -> Pdfpage.pagenumber_of_target ~fastrefnums pdf mark.Pdfmarks.target <= num) marks in
  let pickfrom = keep (fun mark -> mark.Pdfmarks.level = level) (rev (fst (cleavewhile (fun mark -> mark.Pdfmarks.level >= level) (rev before)))) in
  let on_target_page, before_target_page = List.partition (fun mark -> Pdfpage.pagenumber_of_target ~fastrefnums pdf mark.Pdfmarks.target = num) pickfrom in
    match on_target_page with
    | h::_ -> Pdftext.utf8_of_pdfdocstring h.Pdfmarks.text
    | [] ->
        match before_target_page with
        | _::_ -> Pdftext.utf8_of_pdfdocstring (last before_target_page).Pdfmarks.text
        | [] -> ""

let replace_pairs marks fastrefnums pdf endpage extract_text_font_size filename bates batespad num page =
    [
     "%PageDiv2", (fun () -> string_of_int ((num + 1) / 2));
     "%Page", (fun () -> string_of_int num);
     "%Roman", (fun () -> roman_upper num);
     "%roman", (fun () -> roman_lower num);
     "%filename", (fun () -> filename);
     "%Label", (fun () -> pagelabel pdf num);
     "%EndPage", (fun () -> string_of_int endpage);
     "%EndLabel", (fun () -> pagelabel pdf endpage);
     "%Bookmark0", (fun () -> bookmark marks fastrefnums 0 pdf num);
     "%Bookmark1", (fun () -> bookmark marks fastrefnums 1 pdf num);
     "%Bookmark2", (fun () -> bookmark marks fastrefnums 2 pdf num);
     "%Bookmark3", (fun () -> bookmark marks fastrefnums 3 pdf num);
     "%Bookmark4", (fun () -> bookmark marks fastrefnums 4 pdf num);
     "%ExtractedText", (fun () -> Cpdfextracttext.extract_page_text extract_text_font_size pdf num page);
     "%Bates",
        (fun () ->
          (let numstring = string_of_int (bates + num - 1) in
           match batespad with
             None -> numstring
           | Some w ->
               if String.length numstring >= w
                 then numstring
                 else implode (many '0' (w - String.length numstring)) ^ numstring))]

let expand_lines text time pdf endpage extract_text_font_size filename bates batespad num page lines =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
  let marks = Pdfmarks.read_bookmarks ~preserve_actions:false pdf in
  let expanded_lines =
    map
      (function text ->
         process_text time text (replace_pairs marks fastrefnums pdf endpage extract_text_font_size filename bates batespad num page))
      lines
  in
    (* process URLs for justification too *)
    map (fun line -> fst (get_urls_line line)) expanded_lines

let addtext
  time lines linewidth outline fast colour fontname encoding bates batespad
  fontsize fontpack font fontpdfobj fontpackpdfobjs underneath position hoffset voffset text pages 
  cropbox opacity justification filename extract_text_font_size shift raw pdf
=
  let endpage = Pdfpage.endpage pdf in
  let shifts = Cpdfcoord.parse_coordinates pdf shift in
  let addtext_page num page =
    let shift_x, shift_y = List.nth shifts (num - 1) in
    let resources', unique_extgstatename =
      if opacity < 1.0 then
        let dict =
          match Pdf.lookup_direct pdf "/ExtGState" page.Pdfpage.resources with
          | Some d -> d
          | None -> Pdf.Dictionary []
        in
          let unique_extgstatename = Pdf.unique_key "gs" dict in
            let dict' =
              Pdf.add_dict_entry dict unique_extgstatename
                (Pdf.Dictionary [("/ca", Pdf.Real opacity); ("/CA", Pdf.Real opacity)])
            in
              Pdf.add_dict_entry page.Pdfpage.resources "/ExtGState" dict', Some unique_extgstatename
      else
        page.Pdfpage.resources, None
    in
      let fontdict =
        match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
        | None -> Pdf.Dictionary []
        | Some d -> d
      in
        let calc_textwidth text =
          match fontpack with
          | Some fontpack ->
              let widthss =
                map2 (fun n font -> Cpdftype.font_widths (fontname ^ string_of_int n) font fontsize) (indx (fst fontpack)) (fst fontpack)
              in
              let triples = option_map (Cpdfembed.get_char fontpack) (Pdftext.codepoints_of_utf8 text) in
              let widths = map (fun (charcode, fontnum, _) -> (List.nth widthss fontnum).(charcode)) triples in
                fsum widths
          | None ->
          match font with
          | Some (Pdftext.StandardFont (f, _)) ->
              let rawwidth =
                Pdfstandard14.textwidth false encoding f text
              in
                (float rawwidth *. fontsize) /. 1000.
          | Some font ->
              let rawwidth = width_of_text font text in
                (rawwidth *. fontsize) /. 1000.
          | None -> 
              let font =
                match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
                | Some fontdict ->
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
                       match !font with Some f -> f | None -> failwith (Printf.sprintf "addtext: font %s not found" fontname)
                    end
                | _ -> failwith "addtext: font not found for width"
              in
                let rawwidth = width_of_text (Pdftext.read_font pdf font) text in
                  (rawwidth *. fontsize) /. 1000.
        in
        let unique_fontname = Pdf.unique_key "F" fontdict in
        let fd = ref fontdict in
        let unique_fontnames =
          match fontpack with None -> [] | Some fontpack ->
            map
              (fun _ ->
                let key = Pdf.unique_key "F" !fd in
                  fd := Pdf.add_dict_entry !fd key Pdf.Null;
                  key)
              (indx0 (fst fontpack))
        in
          let ops, urls, x, y, hoffset, voffset, text, joffset =
          let refnums = Pdf.page_reference_numbers pdf in
          let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
          let marks = Pdfmarks.read_bookmarks ~preserve_actions:false pdf in
            let text = process_text time text (replace_pairs marks fastrefnums pdf endpage extract_text_font_size filename bates batespad num page) in
            let text, urls = get_urls_line text in
            let lines = map (fun text -> if raw || fontpack <> None then text else charcodes_of_utf8 (Pdftext.read_font pdf fontpdfobj) text) lines in
            let expanded_lines = expand_lines text time pdf endpage extract_text_font_size filename bates batespad num page lines in
            let textwidth = calc_textwidth text
            and allwidths = map calc_textwidth expanded_lines in
              let longest_w = last (sort compare allwidths) in
              let joffset = find_justification_offsets longest_w textwidth position justification in
              let mediabox =
                if cropbox then
                  match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
                  | Some pdfobject -> Pdf.parse_rectangle pdf (Pdf.direct pdf pdfobject)
                  | None -> Pdf.parse_rectangle pdf page.Pdfpage.mediabox
                else
                  Pdf.parse_rectangle pdf page.Pdfpage.mediabox
              in
                let x, y, rotate = Cpdfposition.calculate_position false textwidth mediabox position in
                  let hoffset, voffset =
                    if position = Diagonal || position = ReverseDiagonal
                      then -. (cos ((pi /. 2.) -. rotate) *. voffset), sin ((pi /. 2.) -. rotate) *. voffset
                      else hoffset, voffset 
                  in
                    match font with
                    | Some f ->
                        ops font fontpack fontpackpdfobjs fontname longest_w (x +. shift_x) (y +. shift_y) rotate (hoffset +. joffset) voffset outline linewidth
                        unique_fontname unique_fontnames unique_extgstatename colour fontsize text,
                        urls, x, y, hoffset, voffset, text, joffset
                    | None ->
                        ops font fontpack fontpackpdfobjs fontname longest_w (x +. shift_x) (y +. shift_y) rotate (hoffset +. joffset) voffset outline linewidth
                        fontname unique_fontnames None colour fontsize text,
                        urls, x, y, hoffset, voffset, text, joffset
          in
            let newresources =
              match fontpack with
              | Some fontpack ->
                  let newfontdict =
                    let fd = ref fontdict in
                      iter2
                        (fun i n ->
                          fd := Pdf.add_dict_entry !fd (List.nth unique_fontnames n) (Pdf.Indirect i))
                        fontpackpdfobjs
                        (indx0 fontpackpdfobjs);
                      !fd
                  in
                    Pdf.add_dict_entry resources' "/Font" newfontdict
              | None ->
                  match font with
                  | Some (Pdftext.StandardFont _ as font) ->
                      let newfontdict =
                        Pdf.add_dict_entry fontdict unique_fontname (Pdf.Indirect (Pdftext.write_font pdf font))
                      in
                        Pdf.add_dict_entry resources' "/Font" newfontdict
                  | Some f ->
                      let newfontdict =
                        Pdf.add_dict_entry fontdict unique_fontname fontpdfobj
                      in
                        Pdf.add_dict_entry resources' "/Font" newfontdict
                  | None -> page.Pdfpage.resources
          in
            (* Build annotations from URL data (get_urls and some sense of metrics) *)
            let annot (minx, miny, maxx, maxy) url =
              Pdf.Dictionary
                [("/Subtype", Pdf.Name "/Link");
                 ("/Rect", Pdf.Array [Pdf.Real minx; Pdf.Real miny; Pdf.Real maxx; Pdf.Real maxy]);
                 ("/BS", Pdf.Dictionary [("/W", Pdf.Integer 0)]);
                 ("/A", Pdf.Dictionary [("/URI", Pdf.String url);
                                        ("/Type", Pdf.Name "/Action");
                                        ("/S", Pdf.Name "/URI")])]
            in
            let annots =
              let annot_coord text pos =
                let before = take (explode text) pos in
                  calc_textwidth (implode before)
              in
                map (fun (url, s, e) ->
                       let sx = annot_coord text s in
                       let ex = annot_coord text e in
                       let x, y = x -. hoffset -. joffset, y -. voffset in
                       let height =
                         match cap_height font fontname with
                         | Some c -> (c *. fontsize) /. 1000.
                         | None -> fontsize
                       in
                         Pdf.Indirect (Pdf.addobj pdf (annot (x +. sx, y, x +. ex, y +. height) url))) urls
            in
            let newrest =
              if annots = [] then page.Pdfpage.rest else
                let existing =
                  match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
                  | Some (Pdf.Array a) -> a
                  | _ -> []
                in
                  Pdf.add_dict_entry page.Pdfpage.rest "/Annots" (Pdf.Array (annots @ existing))
            in
            let page =
              {page with
                 Pdfpage.resources = newresources;
                 Pdfpage.rest = newrest}
            in
              if underneath
                then Pdfpage.prepend_operators pdf ops ~fast:fast page
                else Pdfpage.postpend_operators pdf ops ~fast:fast page
  in
    Cpdfpage.process_pages (Pdfpage.ppstub addtext_page) pdf pages

(* Prev is a list of lists of characters *)
let split_at_newline t =
  let rec split_at_newline_inner prev = function
    | [] -> rev (map implode (map rev prev))
    | '\\'::'\\'::'n'::t -> split_at_newline_inner (('n'::'\\'::'\\'::hd prev)::tl prev) t
    | '\\'::'n'::t -> split_at_newline_inner ([]::prev) t
    | h::t -> split_at_newline_inner ((h::hd prev)::tl prev) t
  in
    split_at_newline_inner [[]] (explode t)

let rec unescape_chars prev = function
  | [] -> rev prev
  | '\\'::('0'..'7' as a)::('0'..'7' as b)::('0'..'7' as c)::t ->
       let chr = char_of_int (int_of_string ("0o" ^ implode [a;b;c])) in
         unescape_chars (chr::prev) t
  | '\\'::'\\'::t -> unescape_chars ('\\'::prev) t
  | '\\'::c::t when c <> 'n' -> unescape_chars (c::prev) t
  | h::t -> unescape_chars (h::prev) t

let unescape_string s =
  implode (unescape_chars [] (explode s))

let
  addtexts linewidth outline fast fontname (cpdffont : Cpdfembed.cpdffont) bates batespad
  colour position linespacing fontsize underneath text pages cropbox opacity
  justification midline topline filename extract_text_font_size shift
  ?(raw=false) pdf
=
  if pages = [] then error "addtexts: empty page range" else
  let time = Cpdfstrftime.current_time () in
  let endpage = Pdfpage.endpage pdf in
  let ps = Pdfpage.pages_of_pagetree pdf in
  let used = null_hash () in
  let lines = map unescape_string (split_at_newline text) in
  iter2
    (fun num page ->
       let expanded_lines = expand_lines text time pdf endpage extract_text_font_size filename bates batespad num page lines in
         let codepoints = map Pdftext.codepoints_of_utf8 expanded_lines in
         iter (iter (fun x -> Hashtbl.replace used x ())) codepoints)
    pages
    (map (fun x -> List.nth ps (x - 1)) pages);
  if Hashtbl.length used = 0 then pdf else (* Avoid trying to build truetype font with no used set. *)
  let realfontname = ref fontname in
  let font, fontpack =
    match cpdffont with
    | Cpdfembed.PreMadeFontPack f ->
        Some (hd (fst f)), None
    | Cpdfembed.EmbedInfo {fontfile; fontname; encoding} ->
        let embedded = Cpdfembed.embed_truetype pdf ~fontfile ~fontname ~codepoints:(map fst (list_of_hashtbl used)) ~encoding in
          Some (hd (fst embedded)), Some embedded 
    | Cpdfembed.ExistingNamedFont -> None, None
  in
  let fontpdfobj =
    match font with
    | Some (Pdftext.StandardFont _ as font) ->
        Pdf.Indirect (Pdftext.write_font pdf font)
    | Some f ->
        Pdf.Indirect (Pdftext.write_font pdf f)
    | None -> 
        let firstpage =
          List.nth (Pdfpage.pages_of_pagetree pdf) (hd pages - 1)
        in
        match Pdf.lookup_direct pdf "/Font" firstpage.Pdfpage.resources with
        | Some fontdict ->
            begin match Pdf.lookup_direct pdf fontname fontdict with
            | Some font -> font
            | _ ->
               (* For each item in the fontdict, follow its value and find the basename. If it matches, return that font *)
               let font = ref None in
               iter
                 (fun (k, v) ->
                    match Pdf.lookup_direct pdf "/BaseFont" v with
                    | Some (Pdf.Name n) when n = fontname ->
                        font := Some v; realfontname := k
                    | _ -> ())
                 (match fontdict with Pdf.Dictionary d -> d | _ -> []);
               match !font with Some f -> f | None -> failwith (Printf.sprintf "addtext: font %s not found" fontname)
            end
        | _ -> failwith "addtext: font dictionary not present"
  in
    let lines = map unescape_string (split_at_newline text) in
      let pdf = ref pdf in
        let voffset =
          let open Cpdfposition in
          match position with
          | Bottom _ | BottomLeft _ | BottomRight _ ->
              ref (0. -. (linespacing *. fontsize *. (float (length lines) -. 1.)))
          | Left _ | Right _ ->
              (* Vertically align *)
              ref (0. -. (linespacing *. ((fontsize *. (float (length lines) -. 1.)) /. 2.)))
          | Diagonal | ReverseDiagonal ->
              (* Change so that the whole paragraph sits on the centre... *)
              ref (0. -. ((linespacing *. fontsize *. (float (length lines) -. 1.)) /. 2.))
          | _ -> ref 0.
        in
          if midline then
            begin match font with
              | Some (Pdftext.StandardFont (font, _)) ->
                  let baseline_adjustment =
                    (fontsize *. float (Pdfstandard14.baseline_adjustment font)) /. 1000.
                  in
                    voffset := !voffset +. baseline_adjustment
              | Some (Pdftext.SimpleFont {fontdescriptor = Some {capheight}})  ->
                  voffset := !voffset +. capheight /. 2. /. 1000.
              | _ ->
                  Pdfe.log "Unable to find midline adjustment in this font\n"
            end
          else
          if topline then
            begin match font with
              | Some (Pdftext.StandardFont (font, _)) ->
                  let baseline_adjustment =
                    (fontsize *. float (Pdfstandard14.baseline_adjustment font) *. 2.0) /. 1000.
                  in
                    voffset := !voffset +. baseline_adjustment
              | Some (Pdftext.SimpleFont {fontdescriptor = Some {capheight}})  ->
                  voffset := !voffset +. capheight /. 1000.
              | _ ->
                  Pdfe.log "Unable to find topline adjustment in this font\n"
            end;
          let encoding =
            match font with
            | Some (Pdftext.StandardFont (_, e)) -> e
            | Some (Pdftext.SimpleFont {encoding}) -> encoding
            | _ -> Pdftext.WinAnsiEncoding
          in
            let fontpackpdfobjs =
              match cpdffont with
              | Cpdfembed.EmbedInfo {fontfile; fontname; encoding} ->
                  let codepoints = map fst (list_of_hashtbl used) in
                  let fonts = fst (Cpdfembed.embed_truetype !pdf ~fontfile ~fontname ~codepoints ~encoding) in
                    map (Pdftext.write_font !pdf) fonts
              | _ -> []
            in
              iter
                (fun line ->
                   let voff, hoff = !voffset, 0. in
                     pdf :=
                       addtext time lines linewidth outline fast colour !realfontname encoding
                       bates batespad fontsize fontpack font fontpdfobj fontpackpdfobjs underneath
                       position hoff voff line pages cropbox opacity justification filename
                       extract_text_font_size shift raw !pdf;
                     voffset := !voffset +. (linespacing *. fontsize))
                lines;
                !pdf

let addrectangle
  fast coord colour outline linewidth opacity position relative_to_cropbox
  underneath range pdf
=
  let addrectangle_page _ page =
    let resources', unique_extgstatename =
      if opacity < 1.0 then
        let dict =
          match Pdf.lookup_direct pdf "/ExtGState" page.Pdfpage.resources with
          | Some d -> d
          | None -> Pdf.Dictionary []
        in
          let unique_extgstatename = Pdf.unique_key "gs" dict in
            let dict' =
              Pdf.add_dict_entry dict unique_extgstatename
                (Pdf.Dictionary [("/ca", Pdf.Real opacity); ("/CA", Pdf.Real opacity)])
            in
              Pdf.add_dict_entry page.Pdfpage.resources "/ExtGState" dict', Some unique_extgstatename
      else
        page.Pdfpage.resources, None
    in
    let mediabox =
      if relative_to_cropbox then
        match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
        | Some pdfobject -> Pdf.parse_rectangle pdf (Pdf.direct pdf pdfobject)
        | None -> Pdf.parse_rectangle pdf page.Pdfpage.mediabox
      else
        Pdf.parse_rectangle pdf page.Pdfpage.mediabox
    in
    let w, h =
      match Cpdfcoord.parse_units_string pdf page coord with
      | [w; h] -> w, h
      | _ -> error "bad coordinate specification"
    in
    let x, y, _ =
      Cpdfposition.calculate_position false w mediabox position
    in
    let x, y =
      match position with
        Cpdfposition.Top _ | Cpdfposition.TopLeft _ | Cpdfposition.TopRight _ -> (x, y -. h)
      | Cpdfposition.Centre | Cpdfposition.PosCentre _ -> (x, y -. (h /. 2.))
      | _ -> (x, y)
    in
    let ops =
      Pdfops.begin_artifact::
      [
       Pdfops.Op_q;
       Pdfops.Op_BMC "/CPDFSTAMP";
       colour_op colour;
       colour_op_stroke colour;
      ]
      @
     (if outline then [Pdfops.Op_w linewidth] else [])
     @
     (match unique_extgstatename with None -> [] | Some n -> [Pdfops.Op_gs n])
     @
     [
       Pdfops.Op_re (x, y, w, h);
       (if outline then Pdfops.Op_s else Pdfops.Op_f);
       Pdfops.Op_EMC;
       Pdfops.Op_Q
      ]
     @ [Pdfops.end_artifact]
    in
      let page = {page with Pdfpage.resources = resources'} in
        if underneath
          then Pdfpage.prepend_operators pdf ops ~fast:fast page
          else Pdfpage.postpend_operators pdf ops ~fast:fast page
  in
    Cpdfpage.process_pages (Pdfpage.ppstub addrectangle_page) pdf range
