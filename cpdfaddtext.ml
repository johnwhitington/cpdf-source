open Pdfutil
open Cpdferror

type color =
  Grey of float
| RGB of float * float * float
| CYMK of float * float * float * float

(* Superimpose text, page numbers etc. *)

(* Process UTF8 text to /WinAnsiEncoding string (for standard 14) or whatever
   is in the font (for existing fonts). *)
let charcodes_of_utf8 font s =
  let extractor = Pdftext.charcode_extractor_of_font_real ~debug:false font in
  let codepoints = Pdftext.codepoints_of_utf8 s in
    let charcodes =
      option_map
        (fun codepoint ->
           match extractor codepoint with
           | Some cc -> Some cc
           | None -> Printf.eprintf "Warning: character not found in font for unicode codepoint 0x%X\n" codepoint; None)
        codepoints
    in
    implode (map char_of_int charcodes)

(* Process codepoints back to UTF8, assuming it came from UTF8 to start with *)
let utf8_of_winansi s =
  let text_extractor =
    Pdftext.text_extractor_of_font_real
      (Pdftext.StandardFont (Pdftext.TimesRoman, Pdftext.WinAnsiEncoding))
  in
    let codepoints = Pdftext.codepoints_of_text text_extractor s in
      Pdftext.utf8_of_codepoints codepoints

(* Get the width of some text in the given font *)
let width_of_text font text =
  match font with
  | Pdftext.SimpleFont {Pdftext.fontmetrics = Some fontmetrics} ->
       begin try
         fold_left ( +. ) 0. (map (fun c -> fontmetrics.(int_of_char c)) (explode text))
       with
         _ -> 0.
       end
  | _ -> 0.

type ops_metrics =
  {metrics_text : string;
   metrics_x : float;
   metrics_y : float;
   metrics_rot : float}
   
let ops_metrics : ops_metrics list ref = ref []

let ops_baseline_adjustment = ref 0.

let metrics_howmany () = length !ops_metrics

let metrics_text n =
  utf8_of_winansi (select n !ops_metrics).metrics_text

let metrics_x n =
  (select n !ops_metrics).metrics_x

let metrics_y n =
  (select n !ops_metrics).metrics_y

let metrics_rot n =
  (select n !ops_metrics).metrics_rot

let metrics_baseline_adjustment () = !ops_baseline_adjustment

let colour_op = function
  | RGB (r, g, b) -> Pdfops.Op_rg (r, g, b)
  | Grey g -> Pdfops.Op_g g
  | CYMK (c, y, m, k) -> Pdfops.Op_k (c, y, m, k)

let colour_op_stroke = function
  | RGB (r, g, b) -> Pdfops.Op_RG (r, g, b)
  | Grey g -> Pdfops.Op_G g
  | CYMK (c, y, m, k) -> Pdfops.Op_K (c, y, m, k)

let ops longest_w metrics x y rotate hoffset voffset outline linewidth unique_fontname unique_extgstatename colour fontsize text =
  if metrics then
    ops_metrics :=
      {metrics_text = text; metrics_x = x -. hoffset; metrics_y = y -. voffset; metrics_rot = rotate}
      ::!ops_metrics;
  [Pdfops.Op_q;
  Pdfops.Op_BMC "/CPDFSTAMP";
   Pdfops.Op_cm
     (Pdftransform.matrix_of_transform
       [Pdftransform.Translate (x -. hoffset, y -. voffset);
        Pdftransform.Rotate ((0., 0.), rotate)]);
   Pdfops.Op_BT;
   ] @
   (if outline then [Pdfops.Op_w linewidth; Pdfops.Op_Tr 1] else [Pdfops.Op_Tr 0]) @
   [colour_op colour; colour_op_stroke colour]
   @
   (match unique_extgstatename with None -> [] | Some n -> [Pdfops.Op_gs n])
   @
   [Pdfops.Op_Tf (unique_fontname, fontsize);
   Pdfops.Op_Tj text;
   Pdfops.Op_ET;
   Pdfops.Op_EMC;
   Pdfops.Op_Q]

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

let extract_fontbbox header s =
  let num = function
      Pdfgenlex.LexInt i -> Pdf.Integer i
    | Pdfgenlex.LexReal f -> Pdf.Real f
    | _ -> raise (Failure "extract_fontbbox")
  in
    match Pdfgenlex.lex_string (Hashtbl.find header s) with
      [a; b; c; d] -> [num a; num b; num c; num d] 
    | _ -> raise (Failure "extract_fontbbox")

let remove_slash s =
  match explode s with
   '/'::x -> implode x
  | _ -> raise (Failure "remove_slash")

let extract_widths chars_and_widths =
  let win_to_name = map (fun (x, y) -> (y, x)) Pdfglyphlist.name_to_win in
    map
      (fun x ->
          try
            let name = List.assoc x win_to_name in
              let width = List.assoc (remove_slash name) chars_and_widths in
                width
          with
            _ -> 0)
      (ilist 0 255)

(* For finding the height for URL links, we try to find the Cap Height for the
   font. For now, this will only work for built-in fonts. We fall back to using
   the font size alone if we cannot get the cap height. *)
let cap_height fontname =
  try
    let font = unopt (Pdftext.standard_font_of_name ("/" ^ fontname)) in
    let header, _, _, _ = Pdfstandard14.afm_data font in
      let capheight = try extract_num header "CapHeight" with _ -> Pdf.Integer 0 in
        Some (match capheight with Pdf.Integer i -> float_of_int i | Pdf.Real r -> r | _ -> 0.)
  with
    _ -> None

let make_font embed fontname =
  let font = unopt (Pdftext.standard_font_of_name ("/" ^ fontname)) in
  let header, width_data, _, chars_and_widths = Pdfstandard14.afm_data font in
    let widths = extract_widths (list_of_hashtbl chars_and_widths) in
    let flags = Pdfstandard14.flags_of_standard_font font in
    let fontbbox = extract_fontbbox header "FontBBox" in
    let italicangle = extract_num header "ItalicAngle" in
    let ascent = try extract_num header "Ascender" with _ -> Pdf.Integer 0 in
    let descent = try extract_num header "Descender" with _ -> Pdf.Integer 0 in
    let capheight = try extract_num header "CapHeight" with _ -> Pdf.Integer 0 in
    let stemv = Pdfstandard14.stemv_of_standard_font font in
      let fontdescriptor =
        Pdf.Dictionary
          [("/Type", Pdf.Name "/FontDescriptor");
           ("/FontName", Pdf.Name ("/" ^ fontname));
           ("/Flags", Pdf.Integer flags);
           ("/FontBBox", Pdf.Array fontbbox);
           ("/ItalicAngle", italicangle);
           ("/Ascent", ascent);
           ("/Descent", descent);
           ("/CapHeight", capheight);
           ("/StemV", Pdf.Integer stemv)]
      in
        (* With -no-embed-font, we use the standard encoding, and just the
         * minimal stuff. Without -no-embed-font, we switch to WinAnsiEncoding,
         * and fill out everything except the font file instead *)
        if embed then
          Pdf.Dictionary
            [("/Type", Pdf.Name "/Font");
             ("/Subtype", Pdf.Name "/Type1");
             ("/BaseFont", Pdf.Name ("/" ^ fontname));          
             ("/Encoding", Pdf.Name "/WinAnsiEncoding");
             ("/FirstChar", Pdf.Integer 0);
             ("/LastChar", Pdf.Integer 255);
             ("/Widths", Pdf.Array (map (fun x -> Pdf.Integer x) widths));
             ("/FontDescriptor", fontdescriptor)]
        else
          Pdf.Dictionary
            [("/Type", Pdf.Name "/Font");
             ("/Subtype", Pdf.Name "/Type1");
             ("/Encoding", Pdf.Name "/WinAnsiEncoding");
             ("/BaseFont", Pdf.Name ("/" ^ fontname))]

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

let addtext
  metrics lines linewidth outline fast colour fontname embed bates batespad fontsize font
  underneath position hoffset voffset text pages orientation cropbox opacity
  justification filename extract_text_font_size shift pdf
=
  let time = Cpdfstrftime.current_time () in
  let endpage = Pdfpage.endpage pdf in
  let replace_pairs pdf filename bates batespad num page =
      [
       "%PageDiv2", (fun () -> string_of_int ((num + 1) / 2));
       "%Page", (fun () -> string_of_int num);
       "%Roman", (fun () -> roman_upper num);
       "%roman", (fun () -> roman_lower num);
       "%filename", (fun () -> filename);
       "%Label", (fun () -> pagelabel pdf num);
       "%EndPage", (fun () -> string_of_int endpage);
       "%EndLabel", (fun () -> pagelabel pdf endpage);
       "%ExtractedText", (fun () -> extract_page_text extract_text_font_size pdf num page);
       "%Bates",
          (fun () ->
            (let numstring = string_of_int (bates + num - 1) in
             match batespad with
               None -> numstring
             | Some w ->
                 if String.length numstring >= w
                   then numstring
                   else implode (many '0' (w - String.length numstring)) ^ numstring))]
  in
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
                match font with
                | Some f ->
                    let rawwidth =
                      Pdfstandard14.textwidth
                        false
                        (if embed then Pdftext.WinAnsiEncoding else Pdftext.StandardEncoding)
                        f
                        text
                    in
                      (float rawwidth *. fontsize) /. 1000.
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
          let ops, urls, x, y, hoffset, voffset, text, joffset =
            let text = process_text time text (replace_pairs pdf filename bates batespad num page) in
            let text, urls = get_urls_line text in

                let expanded_lines =
                  map
                    (function text ->
                       process_text time text (replace_pairs pdf filename bates batespad num page))
                    lines
                in
                let expanded_lines = (* process URLs for justification too *)
                  map (fun line -> fst (get_urls_line line)) expanded_lines
                in
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
                    let x, y, rotate = Cpdfposition.calculate_position false textwidth mediabox orientation position in
                      let hoffset, voffset =
                        if position = Diagonal || position = ReverseDiagonal
                          then -. (cos ((pi /. 2.) -. rotate) *. voffset), sin ((pi /. 2.) -. rotate) *. voffset
                          else hoffset, voffset 
                      in
                        match font with
                        | Some f ->
                            ops longest_w metrics (x +. shift_x) (y +. shift_y) rotate (hoffset +. joffset) voffset outline linewidth
                            unique_fontname unique_extgstatename colour fontsize text,
                            urls, x, y, hoffset, voffset, text, joffset
                        | None ->
                            ops longest_w metrics (x +. shift_x) (y +. shift_y) rotate (hoffset +. joffset) voffset outline linewidth
                            fontname None colour fontsize text,
                            urls, x, y, hoffset, voffset, text, joffset
          in
            let newresources =
              match font with
              | Some _ ->
                  let newfontdict =
                    Pdf.add_dict_entry fontdict unique_fontname (make_font embed fontname)
                  in
                    Pdf.add_dict_entry resources' "/Font" newfontdict
              | None -> page.Pdfpage.resources
          in
            (* Build annotations from URL data (get_urls and some sense of metrics) *)
            let annot (minx, miny, maxx, maxy) url =
              Pdf.Dictionary
                [("/Subtype", Pdf.Name "/Link");
                 ("/Rect", Pdf.Array [Pdf.Real minx; Pdf.Real miny; Pdf.Real maxx; Pdf.Real maxy]);
                 ("/BS", Pdf.Dictionary [("/W", Pdf.Integer 1)]);
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
                         match cap_height fontname with
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
    if metrics then
      (ignore (Cpdfpage.iter_pages (fun a b -> ignore (addtext_page a b)) pdf pages); pdf)
    else
      Cpdfpage.process_pages (Cpdfutil.ppstub addtext_page) pdf pages

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
  addtexts metrics linewidth outline fast fontname (font : Pdftext.standard_font option) embed bates batespad colour position linespacing
  fontsize underneath text pages orientation cropbox opacity justification
  midline topline filename extract_text_font_size shift ?(raw=false) pdf
=
  if pages = [] then error "addtexts: empty page range" else
  (*flprint "addtexts:\n";
  iter (Printf.printf "%C ") (explode text);
  flprint "\n";
  Printf.printf "\nCpdf.addtexts: metrics = %b" metrics;
  flprint "\n";*)
  (*Printf.printf "linewidth = %f\n" linewidth;
  Printf.printf "outline = %b\n" outline;
  Printf.printf "fast = %b\n" fast;
  Printf.printf "fontname = %s\n" fontname;
  Printf.printf "winansi text = %s\n" text;
  Printf.printf "position = %s\n" (string_of_position position);
  Printf.printf "bates = %i\n" bates;
  Printf.printf "linespacing = %f\n" linespacing;
  Printf.printf "fontsize = %f\n" fontsize;
  Printf.printf "underneath = %b\n" underneath;
  Printf.printf "font = %s\n" begin match font with None -> "None" | Some x -> Pdftext.string_of_standard_font x end;
  Printf.printf "justification = %s\n"
  begin match justification with LeftJustify -> "left" | RightJustify -> "right" | CentreJustify -> "centre" end;
  Printf.printf "midline = %b\n" midline;
  begin match colour with r, g, b -> Printf.printf "%f, %f, %f\n" r g b end;
  Printf.printf "opacity = %f\n" opacity;
  flprint "\n";
  Printf.printf "relative-to-cropbox = %b" cropbox;
  flprint "\n";*)
  ops_metrics := [];
  let realfontname = ref fontname in
  let fontpdfobj =
    match font with
    | Some f ->
        make_font embed (Pdftext.string_of_standard_font f)
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
    (* 19th May 2022. Reversed the phase order (split first, then get charcodes. This allows \n in custom fonts. *)
    let lines = map unescape_string (split_at_newline text) in
    let lines = map (fun text -> if raw then text else charcodes_of_utf8 (Pdftext.read_font pdf fontpdfobj) text) lines in
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
              | Some font ->
                  let baseline_adjustment =
                    (fontsize *. float (Pdfstandard14.baseline_adjustment font)) /. 1000.
                  in
                    ops_baseline_adjustment := baseline_adjustment;
                    voffset := !voffset +. baseline_adjustment
              | _ ->
                 ops_baseline_adjustment := 0.
            end
          else
          if topline then
            begin match font with
              | Some font ->
                  let baseline_adjustment =
                    (fontsize *. float (Pdfstandard14.baseline_adjustment font) *. 2.0) /. 1000.
                  in
                    ops_baseline_adjustment := baseline_adjustment;
                    voffset := !voffset +. baseline_adjustment
              | _ ->
                 ops_baseline_adjustment := 0.
            end
          else
            ops_baseline_adjustment := 0.;
          iter
            (fun line ->
               let voff, hoff =
                 if orientation = Cpdfposition.Vertical then 0., -.(!voffset) else !voffset, 0.
               in
                 pdf :=
                   addtext metrics lines linewidth outline fast colour !realfontname
                   embed bates batespad fontsize font underneath position hoff voff line
                   pages orientation cropbox opacity justification filename
                   extract_text_font_size shift
                   !pdf;
                 voffset := !voffset +. (linespacing *. fontsize))
            lines;
            ops_metrics := rev !ops_metrics;
            !pdf

let removetext range pdf =
  (* Could fail on nesting, or other marked content inside our marked content.*)
  let rec remove_until_last_EMC level = function
    | [] -> []
    | Pdfops.Op_BMC "/CPDFSTAMP"::more ->
        remove_until_last_EMC (level + 1) more
    | Pdfops.Op_EMC::more ->
        if level = 1
          then more
          else remove_until_last_EMC (level - 1) more
    | _::more ->
        remove_until_last_EMC level more
  in
    let rec remove_stamps prev = function
      | [] -> rev prev
      | Pdfops.Op_BMC "/CPDFSTAMP"::more ->
          let rest = remove_until_last_EMC 1 more in
            remove_stamps prev rest
      | h::t -> remove_stamps (h::prev) t
    in
      let removetext_page _ page =
        {page with
           Pdfpage.content =
             let ops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content in
               [Pdfops.stream_of_ops (remove_stamps [] ops)]}
      in
        Cpdfpage.process_pages (Cpdfutil.ppstub removetext_page) pdf range

let addrectangle
  fast (w, h) colour outline linewidth opacity position relative_to_cropbox
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
    let x, y, _ =
      Cpdfposition.calculate_position false w mediabox Cpdfposition.Horizontal position
    in
    let x, y =
      match position with
        Cpdfposition.Top _ | Cpdfposition.TopLeft _ | Cpdfposition.TopRight _ -> (x, y -. h)
      | Cpdfposition.Centre | Cpdfposition.PosCentre _ -> (x, y -. (h /. 2.))
      | _ -> (x, y)
    in
    let ops =
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
    in
      let page = {page with Pdfpage.resources = resources'} in
        if underneath
          then Pdfpage.prepend_operators pdf ops ~fast:fast page
          else Pdfpage.postpend_operators pdf ops ~fast:fast page
  in
    Cpdfpage.process_pages (Cpdfutil.ppstub addrectangle_page) pdf range
open Pdfutil

let rec remove_all_text_ops pdf resources content =
  let is_textop = function
    Pdfops.Op_Tj _ | Pdfops.Op_' _ | Pdfops.Op_'' _ | Pdfops.Op_TJ _ -> true
  | _ -> false
  in
    let content' =
      let ops = Pdfops.parse_operators pdf resources content in
        Pdfops.stream_of_ops
          (option_map (function x -> if is_textop x then None else Some x) ops) 
    in
      [content']

let remove_all_text_page pdf p =
  let resources = p.Pdfpage.resources in
  let content = p.Pdfpage.content in
    Cpdfutil.process_xobjects pdf p remove_all_text_ops;
    {p with Pdfpage.content = remove_all_text_ops pdf resources content}, pdf

let remove_all_text range pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pagenums = indx pages in
    let pdf = ref pdf in
    let pages' = ref [] in
      iter2 
        (fun p pagenum ->
          let p', pdf' =
            if mem pagenum range
              then remove_all_text_page !pdf p
              else p, !pdf
          in
            pdf := pdf';
            pages' =| p')
        pages
        pagenums;
      Pdfpage.change_pages true !pdf (rev !pages')
