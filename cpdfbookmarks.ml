open Pdfutil
open Cpdferror

(* Verify a list of bookmarks. Positive jumps of > 1 not allowed, no numbers
smaller than 0. *)
let rec verify_bookmarks pdf lastlevel fastrefnums endpage = function
  | [] -> true
  | {Pdfmarks.level = level; Pdfmarks.target = target}::more ->
      let page = Pdfpage.pagenumber_of_target pdf ~fastrefnums target in
        level < lastlevel + 2 &&
        level >= 0 &&
        page <= endpage &&
        page >= 0 &&
        verify_bookmarks pdf level fastrefnums endpage more

let verify_bookmarks pdf lastlevel endpage marks =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
    match marks with
    | [] -> true
    | m::more -> m.Pdfmarks.level = 0 && verify_bookmarks pdf lastlevel fastrefnums endpage more

(* Parse a line of the bookmarks file. *)

(* Un-escape things which are escaped. Quotes, newlines and backslashes *)
let rec fixup_characters prev = function
  | [] -> rev prev
  | '\\'::'\\'::t -> fixup_characters ('\\'::prev) t
  | '\\'::'"'::t -> fixup_characters ('"'::prev) t
  | '\\'::'n'::t -> fixup_characters ('\n'::prev) t
  | h::t -> fixup_characters (h::prev) t

(* If optionaldest = [Pdfgenlex.LexString s], we parse the string, convert the
 * integer to an indirect of the real page target, and then put it in. *)
let target_of_markfile_obj pdf i' pdfobj =
  (*Printf.printf "Parsed %s\n" (Pdfwrite.string_of_pdf pdfobj);*)
  match pdfobj with
    Pdf.Array (Pdf.Integer x::more) ->
      let pageobjnum = Pdfpage.page_object_number pdf i' in
        begin match pageobjnum with
          None ->
            raise (Pdf.PDFError "bookmark_of_data: page obj num not found")
        | Some p ->
            Pdfdest.read_destination pdf (Pdf.Array (Pdf.Indirect p::more))
        end
  (* Need to deal with "null", "(string)", and "<<other thing like action" *) 
  | Pdf.Null -> Pdfdest.NullDestination
  | Pdf.String s -> Pdfdest.NamedDestination s
  | x -> Pdfdest.Action x

let target_of_markfile_target pdf i' = function
  | [Pdfgenlex.LexString s] ->
      let pdfobj = Pdfread.parse_single_object s in
        target_of_markfile_obj pdf i' pdfobj
  | _ -> Pdfpage.target_of_pagenumber pdf i'

let bookmark_of_data pdf i s i' isopen optionaldest =
  {Pdfmarks.level = i;
   Pdfmarks.text = Pdftext.pdfdocstring_of_utf8 (implode (fixup_characters [] (explode s)));
   Pdfmarks.target = target_of_markfile_target pdf i' optionaldest;
   Pdfmarks.isopen = isopen;
   Pdfmarks.colour = (0., 0., 0.);
   Pdfmarks.flags = 0} (* Old bookmark file format does not support colour or flags *)

let target_of_json_target pdf pagenumber target = 
  target_of_markfile_obj pdf pagenumber (Cpdfjson.object_of_json target)

let rec json_make_all_float = function
  | `Assoc l -> `Assoc (map (fun (k, v) -> (k, json_make_all_float v)) l)
  | `List l -> `List (map json_make_all_float l)
  | `Int i -> `Float (float_of_int i)
  | x -> x

let mark_of_json pdf = function
  | `Assoc [("bold", `Bool bold);
            ("colour", `List [`Float r; `Float g; `Float b]);
            ("italic", `Bool italic);
            ("level", `Float level);
            ("open", `Bool openstatus);
            ("page", `Float pagenumber);
            ("target", target);
            ("text", `String text)] ->
       {Pdfmarks.level = int_of_float level;
        Pdfmarks.text = Pdftext.pdfdocstring_of_utf8 text;
        Pdfmarks.target = target_of_json_target pdf (int_of_float pagenumber) target;
        Pdfmarks.isopen = openstatus;
        Pdfmarks.colour = (r, g, b);
        Pdfmarks.flags = (if italic then 1 else 0) lor ((if bold then 1 else 0) lsl 1)}
  | _ -> error "malformed mark in mark_of_json"

let mark_of_json_sort pdf = function
  | `Assoc l -> mark_of_json pdf (`Assoc (sort compare l))
  | _ -> error "malformed mark in mark_of_json_sort"

let marks_of_json pdf = function
  | `List ms -> map (mark_of_json_sort pdf) (map json_make_all_float ms)
  | _ -> error "top level of JSON boomark file not a list"

let parse_bookmark_file_json verify pdf i =
  let module J = Cpdfyojson.Safe in
    try
      let json =
        match i.Pdfio.caml_channel with
        | Some ch -> J.from_channel ch
        | None ->
          let content = Pdfio.string_of_input i in
            J.from_string content
      in
      let marks = marks_of_json pdf json in
        if verify then
          if verify_bookmarks pdf 0 (Pdfpage.endpage pdf) marks then marks else
            error "Bad bookmark file (References non-existant pages or is malformed)"
        else
          marks
    with
      e ->
        error (Printf.sprintf "Malformed JSON bookmark file (%s)" (Printexc.to_string e)) 

let parse_bookmark_file verify pdf input =
  let currline = ref 0 in
  try
    let lines = Pdfio.read_lines input in
      let currline = ref 0 in
      let bookmarks = ref [] in
        iter
          (function line ->
             match
               incr currline;
               Pdfgenlex.lex_string line
             with
             | Pdfgenlex.LexInt i::Pdfgenlex.LexString s::Pdfgenlex.LexInt i'::Pdfgenlex.LexName "open"::optionaldest ->
                 bookmarks =| bookmark_of_data pdf i s i' true optionaldest
             | Pdfgenlex.LexInt i::Pdfgenlex.LexString s::Pdfgenlex.LexInt i'::optionaldest ->
                 bookmarks =| bookmark_of_data pdf i s i' false optionaldest
             | [] -> () (* ignore blank lines *)
             | _ ->
                 error ("Bad bookmark file, line " ^ (string_of_int !currline)))
          lines;
        let bookmarks = rev !bookmarks in
          if verify then
            if verify_bookmarks pdf 0 (Pdfpage.endpage pdf) bookmarks
                then bookmarks
                else
                  error
                    "Bad bookmark file (References non-existant pages or is malformed)"
            else
              bookmarks
  with
    e ->
      error
        (Printf.sprintf 
           "Bad bookmark file (syntax) at line %i (error was %s)"
           !currline
           (Printexc.to_string e))

let add_bookmarks ~json verify input pdf =
  let parsed =
    (if json then parse_bookmark_file_json else parse_bookmark_file) verify pdf input in
    (*iter (fun b -> flprint (Pdfmarks.string_of_bookmark b); flprint "\n") parsed;*)
    Pdfmarks.add_bookmarks parsed pdf 

(* List bookmarks *)
let output_string_of_target pdf fastrefnums x =
  match Pdfdest.pdfobject_of_destination x with
  | Pdf.Array (_::more) ->
      let a =
        Pdf.Array (Pdf.Integer (Pdfpage.pagenumber_of_target ~fastrefnums pdf x)::more)
      in
        "\"" ^ Pdfwrite.string_of_pdf a ^ "\"" 
  | x -> "\"" ^ Pdfwrite.string_of_pdf x ^ "\""

let json_of_target pdf fastrefnums x =
  match Pdfdest.pdfobject_of_destination x with
  | Pdf.Array (_::more) ->
      let a =
        Pdf.Array (Pdf.Integer (Pdfpage.pagenumber_of_target ~fastrefnums pdf x)::more)
      in
        Cpdfjson.json_of_object pdf (fun _ -> ()) ~utf8:false ~no_stream_data:false ~parse_content:false a
  | x -> Cpdfjson.json_of_object pdf (fun _ -> ()) ~utf8:false ~no_stream_data:false ~parse_content:false x

let output_json_marks output calculate_page_number pdf fastrefnums marks =
  let module J = Cpdfyojson.Safe in
  let json_of_mark m =
    let r, g, b = m.Pdfmarks.colour in
      `Assoc
         [("level", `Int m.Pdfmarks.level);
          ("text", `String (Pdftext.utf8_of_pdfdocstring (Pdftext.simplify_utf16be m.Pdfmarks.text)));
          ("page", `Int (calculate_page_number m));
          ("open", `Bool m.Pdfmarks.isopen);
          ("target", json_of_target pdf fastrefnums m.Pdfmarks.target);
          ("colour", `List [`Float r; `Float g; `Float b]);
          ("italic", `Bool (m.Pdfmarks.flags land 1 > 0));
          ("bold", `Bool (m.Pdfmarks.flags land 2 > 0))]
  in
  let json = `List (map json_of_mark marks) in
    output.Pdfio.output_string (J.pretty_to_string json)

let process_string encoding s =
  let rec replace c x y = function
  | [] -> []
  | h::t when h = c -> x::y::replace c x y t
  | h::t -> h::replace c x y t
  in
    (* Convert to UTF8, raw, or stripped, and escape backslashed and quotation marks *)
    let codepoints = Pdftext.codepoints_of_pdfdocstring s in
      let escaped =
        let bs = int_of_char '\\'
        and nl = int_of_char '\n'
        and n = int_of_char 'n'
        and q = int_of_char '\"' in
          replace q bs q (replace nl bs n (replace bs bs bs codepoints))
      in
        let process_stripped escaped =
          let b = Buffer.create 200 in
            iter
              (fun x ->
                 if x <= 127 then Buffer.add_char b (char_of_int x))
              escaped;
            Buffer.contents b
        in
        match encoding with
        | Cpdfmetadata.UTF8 -> Pdftext.utf8_of_codepoints escaped
        | Cpdfmetadata.Stripped -> process_stripped escaped
        | Cpdfmetadata.Raw -> s

(* List the bookmarks in the given range to the given output *)
let list_bookmarks ~json ~json_preserve_actions encoding range pdf output =
  let bookmarks = Pdfmarks.read_bookmarks ~preserve_actions:(json && json_preserve_actions) pdf in
  let refnums = Pdf.page_reference_numbers pdf in
  let rangetable = hashset_of_list range in
  let range_is_all = range = ilist 1 (Pdfpage.endpage pdf) in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
    (* Find the pagenumber of each bookmark target. If it is in the range,
     * keep that bookmark. Also keep the bookmark if its target is the null
     * destination. *)
    let inrange =
      keep
        (function x ->
           range_is_all || 
           x.Pdfmarks.target = Pdfdest.NullDestination ||
           (match x.Pdfmarks.target with Pdfdest.NamedDestination _ -> true | _ -> false) ||
           Hashtbl.mem rangetable (Pdfpage.pagenumber_of_target ~fastrefnums pdf x.Pdfmarks.target)) bookmarks
    in
      let calculate_page_number mark =
        (* Some buggy PDFs use integers for page numbers instead of page
         * object references. Adobe Reader and Preview seem to support
         * this, for presumably historical reasons. So if we see a
         * OtherDocPageNumber (which is what Pdfdest parses these as,
         * because that's what they are legitimately, we use this as the
         * page number. It is zero based, though, and we are one-based, so
         * we add one. Pdfpage.pagenumber_of_target has been modified to support this.*)
        Pdfpage.pagenumber_of_target ~fastrefnums pdf mark.Pdfmarks.target
      in
        if json then
          output_json_marks output calculate_page_number pdf fastrefnums inrange
        else
          iter
            (function mark ->
               output.Pdfio.output_string
                 (Printf.sprintf "%i \"%s\" %i%s %s\n"
                   mark.Pdfmarks.level
                   (process_string encoding mark.Pdfmarks.text)
                   (calculate_page_number mark)
                   (if mark.Pdfmarks.isopen then " open" else "")
                   (output_string_of_target pdf fastrefnums mark.Pdfmarks.target)))
            inrange

let get_bookmarks_json ~json_preserve_actions pdf =
  let o, br = Pdfio.input_output_of_bytes (20 * 1024) in
    list_bookmarks ~json:true ~json_preserve_actions Cpdfmetadata.UTF8 (ilist 1 (Pdfpage.endpage pdf)) pdf o;
    Pdfio.extract_bytes_from_input_output o br

let get_bookmark_name encoding pdf marks splitlevel n _ =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
  match keep (function m -> n = Pdfpage.pagenumber_of_target ~fastrefnums pdf m.Pdfmarks.target && m.Pdfmarks.level <= splitlevel) marks with
  | {Pdfmarks.text = title}::_ -> Cpdfattach.remove_unsafe_characters (Pdftext.utf8_of_pdfdocstring title)
  | _ -> ""

(* @F means filename without extension *)
(* @N means sequence number with no padding *)
(* @S means start page of this section *)
(* @E means end page of this section *)
(* @B means bookmark name at start page *)
(* @b52@ means bookmark name at start page truncated to 52 characters using crude UTF8 truncation. *)
let process_others encoding marks pdf splitlevel filename sequence startpage endpage s =
  let trim_utf8 len l =
    let l = try take l len with _ -> l in
    (* This truncator is far from perfect, but it does yield a valid UTF8 string, when given one. *)
    match rev l with
    | b2::b1::b0::t ->
        if int_of_char b2 land 0x80 > 0 then
          begin
            if int_of_char b2 land 0x40 > 0 then rev (b1::b0::t)
            else if int_of_char b1 land 0xe0 = 0xe0 then rev (b0::t)
            else if int_of_char b0 land 0xf0 = 0xf0 then rev t
            else l
          end
        else
          l
    | _ -> l
  in
  let rec find_ats p = function
    '@'::r -> find_ats (p + 1) r
  | r -> (p, r)
  in
  let string_of_int_width w i =
    if w < 0 then raise (Pdf.PDFError "width of field too narrow")
    else if w > 8 then raise (Pdf.PDFError "width of field too broad") else
      let formats =
        [|format_of_string "%i";
          format_of_string "%i";
          format_of_string "%02i";
          format_of_string "%03i";
          format_of_string "%04i";
          format_of_string "%05i";
          format_of_string "%06i";
          format_of_string "%07i";
          format_of_string "%08i"|]
      in
        Printf.sprintf formats.(w) i
  in
    let rec procss prev = function
      | [] -> rev prev
      | '@'::'F'::t -> procss (rev (explode filename) @ prev) t
      | '@'::'N'::t ->
          let width, rest = find_ats 0 t in
            procss (rev (explode (string_of_int_width width sequence)) @ prev) rest
      | '@'::'S'::t ->
          let width, rest = find_ats 0 t in
            procss (rev (explode (string_of_int_width width startpage)) @ prev) rest
      | '@'::'E'::t ->
          let width, rest = find_ats 0 t in
            procss (rev (explode (string_of_int_width width endpage)) @ prev) rest
      | '@'::'B'::t ->
          procss (rev (explode (get_bookmark_name encoding pdf marks splitlevel startpage pdf)) @ prev) t
      | '@'::'b'::t ->
          let number, rest = cleavewhile (function '0'..'9' -> true | _ -> false) t in
          begin try ignore (int_of_string (implode number)) with _ -> error "Bad @b spec" end;
          let text = trim_utf8 (int_of_string (implode number)) (explode (get_bookmark_name encoding pdf marks splitlevel startpage pdf)) in
            procss (rev text @ prev) (if rest = [] then [] else tl rest)
      | h::t -> procss (h::prev) t
    in
       implode (procss [] (explode s))

let name_of_spec encoding marks (pdf : Pdf.t) splitlevel spec n filename startpage endpage =
  let fill l n =
    let chars = explode (string_of_int n) in
      if length chars > l
        then implode (drop chars (length chars - l))
        else implode ((many '0' (l - length chars)) @ chars)
  in
    let chars = explode spec in
      let before, including = cleavewhile (neq '%') chars in
        let percents, after = cleavewhile (eq '%') including in
          if percents = []
            then
              process_others encoding marks pdf splitlevel filename n startpage endpage spec
            else
              process_others encoding marks pdf splitlevel filename n startpage endpage
              (implode before ^ fill (length percents) n ^ implode after)

(* Indent bookmarks in each file by one and add a title bookmark pointing to the first page. *)
let add_bookmark_title filename use_title pdf =
  let title =
    if use_title then
      match Cpdfmetadata.get_info_utf8 pdf "/Title", Cpdfmetadata.get_xmp_info pdf "/Title" with
        "", x | x, "" | _, x -> x
    else
      Filename.basename filename
  in
  let marks = Pdfmarks.read_bookmarks ~preserve_actions:true pdf in
  let page1objnum =
    match Pdfpage.page_object_number pdf 1 with
      None -> error "add_bookmark_title: page not found"
    | Some x -> x
  in
  let newmarks =
      {Pdfmarks.level = 0;
       Pdfmarks.text = Pdftext.pdfdocstring_of_utf8 title;
       Pdfmarks.target = Pdfdest.XYZ (Pdfdest.PageObject page1objnum, None, None, None);
       Pdfmarks.isopen = false;
       Pdfmarks.colour = (0., 0., 0.);
       Pdfmarks.flags = 0}
    ::map (function m -> {m with Pdfmarks.level = m.Pdfmarks.level + 1}) marks
  in
    Pdfmarks.add_bookmarks newmarks pdf

let bookmarks_open_to_level n pdf =
  let marks = Pdfmarks.read_bookmarks ~preserve_actions:true pdf in
  let newmarks =
    map
      (fun m -> {m with Pdfmarks.isopen = m.Pdfmarks.level < n})
      marks
  in
    Pdfmarks.add_bookmarks newmarks pdf

(* Alter bookmark destinations given a hash table of (old page reference
 * number, new page reference number) pairings *)
let change_destination t = function
   Pdfdest.XYZ (Pdfdest.PageObject p, a, b, c) ->
     Pdfdest.XYZ (Pdfdest.PageObject (Hashtbl.find t p), a, b, c)
 | Pdfdest.Fit (Pdfdest.PageObject p) ->
     Pdfdest.Fit (Pdfdest.PageObject (Hashtbl.find t p))
 | Pdfdest.FitH (Pdfdest.PageObject p, x) ->
     Pdfdest.FitH (Pdfdest.PageObject (Hashtbl.find t p), x)
 | Pdfdest.FitV (Pdfdest.PageObject p, x) ->
     Pdfdest.FitV (Pdfdest.PageObject (Hashtbl.find t p), x)
 | Pdfdest.FitR (Pdfdest.PageObject p, a, b, c, d) ->
     Pdfdest.FitR (Pdfdest.PageObject (Hashtbl.find t p), a, b, c, d)
 | Pdfdest.FitB (Pdfdest.PageObject p) ->
     Pdfdest.Fit (Pdfdest.PageObject (Hashtbl.find t p))
 | Pdfdest.FitBH (Pdfdest.PageObject p, x) ->
     Pdfdest.FitBH (Pdfdest.PageObject (Hashtbl.find t p), x)
 | Pdfdest.FitBV (Pdfdest.PageObject p, x) ->
     Pdfdest.FitBV (Pdfdest.PageObject (Hashtbl.find t p), x)
 | x -> x

let change_bookmark t m =
  {m with Pdfmarks.target =
    try change_destination t m.Pdfmarks.target with Not_found -> m.Pdfmarks.target}
