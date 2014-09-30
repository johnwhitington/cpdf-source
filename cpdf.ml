(* CPDF Core routines *)
open Pdfutil
open Pdfio

(* Printf implementation *)
exception PrintfFailure of string

type convop = Ca | CA | Cc | Cd | Ce | CE | Cf | Cg | CG | Ci | Cn | Co | Cp | Cs | Cu | Cx | CX | CPercent

type sizespec = Sll | Sl | SL | Sh | Shh | Sj | Sz | St

type pformat =
  {leftjustify : bool;
   padzero : bool;
   signalways : bool;
   space : bool;
   variant : bool;
   minwidth : int option;
   precision : int option;
   sizespec : sizespec option;
   convop : convop}

let string_of_options f =
  (if f.leftjustify then "-" else "") ^
  (if f.padzero then "0" else "") ^
  (if f.signalways then "+" else "") ^
  (if f.space then " " else "") ^
  (if f.variant then "#" else "")

let string_of_minwidth = function
  | None -> ""
  | Some x -> string_of_int x

let string_of_precision = function
  | None -> ""
  | Some x -> "." ^ string_of_int x

let string_of_sizespec = function
  | None -> ""
  | Some s->
      match s with
      | Sll -> "ll" | Sl -> "l" | SL -> "L" | Sh -> "h"
      | Shh -> "hh" | Sj -> "j" | Sz -> "z" | St -> "t"

let string_of_convop = function
  | Ca -> "a" | CA -> "A" | Cc -> "c" | Cd -> "d"
  | Ce -> "e" | CE -> "E" | Cf -> "f" | Cg -> "g"
  | CG -> "G" | Ci -> "i" | Cn -> "n" | Co -> "o"
  | Cp -> "p" | Cs -> "s" | Cu -> "u" | Cx -> "x"
  | CX -> "X" | CPercent -> "%"

let string_of_format f =
  "%" ^
  string_of_options f ^
  string_of_minwidth f.minwidth ^
  string_of_precision f.precision ^
  string_of_sizespec f.sizespec ^
  string_of_convop f.convop

type section =
  | Format of pformat
  | String of string 

let sec_of_format cs =
  (* 1. Read zero or more flags -, +, 0, #, <space> *)
  let cs = ref cs in
    let lj, pz, sa, sp, va, fin =
      ref false, ref false, ref false, ref false, ref false, ref false
    in
  while not !fin do
    match !cs with
    | '-'::_ -> set lj; cs := tl !cs
    | '+'::_ -> set sa; cs := tl !cs
    | '0'::_ -> set pz; cs := tl !cs
    | ' '::_ -> set sp; cs := tl !cs
    | '#'::_ -> set va; cs := tl !cs
    | _ -> set fin
  done;
  (* 2. Read a possible minimum field width *)
  let minwidth =
    let fwchars, rest = cleavewhile isdigit !cs in
      cs := rest;
      if fwchars = [] then None else Some (int_of_string (implode fwchars))
  in
  (* 3. Read an optional precision specification *)
  let precision =
    match !cs with
    | '.'::more ->
        cs := more;
        let pchars, rest = cleavewhile isdigit !cs in
          cs := rest;
          if pchars = [] then None else Some (int_of_string (implode pchars))
    | _ -> None
  in
  (* 4. Read an optional size specification *)
  let sizespec =
    match !cs with
    | 'l'::'l'::r -> cs := r; Some Sll
    | 'l'::r -> cs := r; Some Sl
    | 'L'::r -> cs := r; Some SL
    | 'h'::'h'::r -> cs := r; Some Shh
    | 'h'::r -> cs := r; Some Sh
    | 'j'::r -> cs := r; Some Sj
    | 'z'::r -> cs := r; Some Sz
    | 't'::r -> cs := r; Some St
    | _ -> None
  in
  (* 5. Read the conversion operation *)
  let convop =
    match !cs with
    | 'a'::r -> cs := r; Ca
    | 'A'::r -> cs := r; CA
    | 'c'::r -> cs := r; Cc
    | 'd'::r -> cs := r; Cd
    | 'e'::r -> cs := r; Ce
    | 'E'::r -> cs := r; CE
    | 'f'::r -> cs := r; Cf
    | 'g'::r -> cs := r; Cg
    | 'G'::r -> cs := r; CG
    | 'i'::r -> cs := r; Ci
    | 'n'::r -> cs := r; Cn
    | 'o'::r -> cs := r; Co
    | 'p'::r -> cs := r; Cp
    | 's'::r -> cs := r; Cs
    | 'u'::r -> cs := r; Cu
    | 'x'::r -> cs := r; Cx
    | 'X'::r -> cs := r; CX
    | '%'::r -> cs := r; CPercent
    | _ -> raise (Failure "sec_of_format")
  in
    {leftjustify = !lj;
     padzero = !pz;
     signalways = !sa;
     space = !sp;
     variant = !va;
     minwidth = minwidth;
     precision = precision;
     sizespec = sizespec;
     convop = convop},
   !cs

let rec sections_of_string_inner secs currstr = function
  | '%'::m ->
      let sec, rest = sec_of_format m in
        if currstr = []
          then sections_of_string_inner (Format sec::secs) currstr rest
          else sections_of_string_inner (Format sec::String (implode (rev currstr))::secs) [] rest
  | x::xs ->
     sections_of_string_inner secs (x::currstr) xs
  | [] ->
     if currstr = [] then rev secs else rev (String (implode (rev currstr))::secs)

(* Take a format string, and split it into sections *)
let sections_of_string s =
  try
    sections_of_string_inner [] [] (explode s)
  with
    _ -> raise (PrintfFailure "Couldn't parse Printf format")

(* Substitute an integer into a format, returning the empty string if the format is not suitable. *)

(* For now, just 'd', 'u', 'i' *)
let sub_int i f =
  (*i Printf.printf "Substituting format |%s|\n" (string_of_format f); i*)
  let str = string_of_int i
  in let padding = if f.padzero then '0' else ' ' in
    if f.minwidth <> None && String.length str < unopt f.minwidth then
      let padding = many padding (unopt f.minwidth - String.length str) in
        if f.leftjustify then str ^ implode padding else implode padding ^ str
    else
      str

(* Given a list of integers, substitute into integer formats *)
let rec substitute_inner donesections sections = function
  | [] -> rev donesections @ sections
  | i::is ->
      match sections with
      | [] -> rev donesections @ sections
      | String s::more -> substitute_inner (String s::donesections) more (i::is)
      | Format f::more -> substitute_inner (String (sub_int i f)::donesections) more is

let substitute x =
  try substitute_inner [] x with
    _ -> raise (PrintfFailure "Failed to substitute integer")

(* Flatten a set of sections to a string *)
let string_of_section = function
  | String s -> s
  | Format f -> string_of_format f

let string_of_sections sections =
  try fold_left ( ^ ) "" (map string_of_section sections) with
    _ -> raise (PrintfFailure "Failed to build string from Printf sections")

type encoding =
  | Raw
  | UTF8
  | Stripped

(* Just strip everything which isn't 7 bit ASCII *)
let crude_de_unicode s =
  implode (map char_of_int (lose (fun x -> x > 127) (Pdftext.codepoints_of_pdfdocstring s)))

let encode_output enc s =
  match enc with
  | Raw -> s
  | UTF8 -> Pdftext.utf8_of_pdfdocstring s
  | Stripped -> crude_de_unicode s

(* Get the number of pages in file. Doesn't need decryption. *)
let endpage_io i user_pw owner_pw =
  let pdf = Pdfread.pdf_of_input_lazy user_pw owner_pw i in
    Pdfpage.endpage pdf

(* Raised when syntax is ok, but endpage is too low. Caught by validator.
Caught and reraised as normal failure by parse_pagespec. *)
exception PageSpecUnknownPage of int

(* There would be no pages *)
exception PageSpecWouldBeNoPages

(* Raised when syntax is wrong. Caught and reraised by parse_pagespec and
validator. *)
exception PageSpecBadSyntax

(* Parsing range specifications *)
let rec splitat_commas toks =
  match cleavewhile (neq (Pdfgenlex.LexName ",")) toks with
  | [], [] -> []
  | [], _ -> raise PageSpecBadSyntax
  | some, [] -> [some]
  | _::_ as before, _::rest -> before::splitat_commas rest 

let is_dimension comparison {Pdfpage.mediabox = box} =
  let minx, miny, maxx, maxy = Pdf.parse_rectangle box in
    comparison (maxx -. minx) (maxy -. miny)

let select_dimensions comparison pdf candidates =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pagenums, kept_pages =
      split
        (option_map
           (fun (index, page) ->
             if mem index candidates then Some (index, page) else None)
           (combine (indx pages) pages))
    in
      option_map2
        (fun pagenum page ->
          if is_dimension comparison page then Some pagenum else None)
        pagenums
        kept_pages

let select_portrait = select_dimensions ( < )

let select_landscape = select_dimensions ( > )

let rec mk_numbers pdf endpage lexemes =
  match lexemes with
  | [Pdfgenlex.LexInt n] -> [n]
  | [Pdfgenlex.LexName "end"] -> [endpage]
  | [Pdfgenlex.LexInt n; Pdfgenlex.LexName "-"; Pdfgenlex.LexInt n'] ->
      if n > n' then rev (ilist n' n) else ilist n n'
  | [Pdfgenlex.LexName "end"; Pdfgenlex.LexName "-"; Pdfgenlex.LexInt n] ->
      if n <= endpage
        then rev (ilist n endpage)
        else raise (PageSpecUnknownPage n)
  | [Pdfgenlex.LexInt n; Pdfgenlex.LexName "-"; Pdfgenlex.LexName "end"] ->
      if n <= endpage
        then ilist n endpage
        else raise (PageSpecUnknownPage n)
  | [Pdfgenlex.LexName "end"; Pdfgenlex.LexName "-"; Pdfgenlex.LexName "end"] ->
       [endpage]
  | [Pdfgenlex.LexName "even"] ->
       drop_odds (ilist 1 endpage)
  | [Pdfgenlex.LexName "portrait"] ->
       select_portrait pdf (ilist 1 endpage)
  | [Pdfgenlex.LexName "landscape"] ->
       select_landscape pdf (ilist 1 endpage)
  | [Pdfgenlex.LexName "odd"] ->
       really_drop_evens (ilist 1 endpage)
  | [Pdfgenlex.LexName "all"] ->
       ilist 1 endpage
  | [Pdfgenlex.LexName "reverse"] ->
       rev (ilist 1 endpage)
  | toks ->
      flprint "Unknown tokens\n";
      let ranges = splitat_commas toks in
        if ranges = [toks] then raise PageSpecBadSyntax else
          flatten (map (mk_numbers pdf endpage) ranges)

(* Space dashes and commas *)
let rec add_spaces = function
  | [] -> []
  | ('-' | ',') as h::t -> ' '::h::' '::add_spaces t
  | h::t -> h::add_spaces t

let space_string s =
  implode (add_spaces (explode s))

let fixup_negatives endpage = function
  | Pdfgenlex.LexName s when String.length s > 1 && s.[0] = '~' ->
      Pdfgenlex.LexInt (endpage + 1 + ~-(int_of_string (implode (tl (explode s)))))
  | x -> x

let parse_pagespec_inner endpage pdf spec =
  let spec = space_string spec in
    if endpage < 1 then raise (Pdf.PDFError "This PDF file has no pages and is therefore malformed") else
      let numbers =
        try
          match rev (explode spec) with
          | ['n'; 'e'; 'v'; 'e'] ->
              keep even (ilist 1 endpage)
          | ['d'; 'd'; 'o'] ->
              keep odd (ilist 1 endpage)
          | ['t'; 'i'; 'a'; 'r'; 't'; 'r'; 'o'; 'p'] ->
              select_portrait pdf (ilist 1 endpage)
          | ['e'; 'p'; 'a'; 'c'; 's'; 'd'; 'n'; 'a'; 'l'] ->
              select_landscape pdf (ilist 1 endpage)
          | 't'::'i'::'a'::'r'::'t'::'r'::'o'::'p'::more ->
              select_portrait
                pdf
                (mk_numbers pdf endpage (map (fixup_negatives endpage) (Pdfgenlex.lex_string (implode (rev more)))))
          | 'e'::'p'::'a'::'c'::'s'::'d'::'n'::'a'::'l'::more ->
              select_landscape
                pdf
                (mk_numbers pdf endpage (map (fixup_negatives endpage) (Pdfgenlex.lex_string (implode (rev more)))))
          | 'd'::'d'::'o'::more ->
              keep
                odd
                (mk_numbers pdf endpage (map (fixup_negatives endpage) (Pdfgenlex.lex_string (implode (rev more)))))
          | 'n'::'e'::'v'::'e'::more ->
              keep
                even
                (mk_numbers pdf endpage (map (fixup_negatives endpage) (Pdfgenlex.lex_string (implode (rev more)))))
          | _ ->
              mk_numbers pdf endpage (map (fixup_negatives endpage) (Pdfgenlex.lex_string spec))
        with
          e -> raise PageSpecBadSyntax
      in
       if numbers = [] then raise PageSpecWouldBeNoPages else
         iter
           (fun n ->
              if n <= 0 || n > endpage then raise (PageSpecUnknownPage n))
           numbers;
         numbers

let parse_pagespec pdf spec =
  try parse_pagespec_inner (Pdfpage.endpage pdf) pdf spec with
  | PageSpecUnknownPage n ->
     raise (Pdf.PDFError ("Page " ^ string_of_int n ^ " does not exist."))
  | PageSpecWouldBeNoPages ->
     raise (Pdf.PDFError ("Page range specifies no pages"))
  | e ->
     raise
       (Pdf.PDFError
         ("Bad page specification " ^ spec ^
          ". Raw error was " ^ Printexc.to_string e ^
          ". Last page was " ^ string_of_int (Pdfpage.endpage pdf)))

(* To validate a pagespec as being syntactically correct without the PDF in
question. This is nasty, since the parser above includes checking based on the
endpage of the PDF (which we don't have). Pass 100 as the endpage, doubling on
page range exception, bailng out above 500000. *)
let rec validate_pagespec_inner n spec =
  try
    ignore (parse_pagespec_inner n (Pdf.empty ()) spec); true
  with
  | PageSpecUnknownPage _ -> if n < 500000 then validate_pagespec_inner (n * 2) spec else false
  | PageSpecBadSyntax | _ -> false

let validate_pagespec spec =
  validate_pagespec_inner 100 spec

(* Convert an integer list representing a set to a page specification, in order. *)
let string_of_pagespec pdf = function
  | [] -> ""
  | is ->
      let iseven len is =
        drop_odds (ilist 1 len) = is
      in let isodd len is =
        really_drop_evens (ilist 1 len) = is
      in let isall len is =
        ilist 1 len = is
      in let is = sort compare is
      in let len = Pdfpage.endpage pdf in
        let rec mkranges prev = function
        | [] -> map extremes (rev (map rev prev))
        | h::t ->
            match prev with
            | (ph::pht)::pt when h = ph + 1 -> mkranges ((h::ph::pht)::pt) t
            | (_::_)::_ -> mkranges ([h]::prev) t
            | []::_ -> assert false
            | [] -> mkranges [[h]] t
        in
          if iseven len is && len > 3 then "even" else
            if isodd len is && len > 2 then "odd" else
              if isall len is then "all" else
                let ranges = mkranges [] is in
                  let rangestrings =
                    map
                      (function (s, e) ->
                         if s = e
                           then string_of_int s
                           else string_of_int s ^ "-" ^ string_of_int e)
                      ranges
                  in
                    fold_left ( ^ ) "" (interleave "," rangestrings)

let string_of_range r =
  fold_left (fun a b -> a ^ " " ^ b) "" (map string_of_int r)

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

(* Recompress anything which isn't compressed, unless it's metadata. *)
let recompress_stream pdf = function
  (* If there is no compression, compress with /FlateDecode *)
  | Pdf.Stream {contents = (dict, _)} as stream ->
      begin match
        Pdf.lookup_direct pdf "/Filter" dict, 
        Pdf.lookup_direct pdf "/Type" dict
      with
      | _, Some (Pdf.Name "/Metadata") -> ()
      | (None | Some (Pdf.Array [])), _ ->
          Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate stream
      | _ -> ()
      end
  | _ -> assert false

let recompress_pdf pdf =
  if not (Pdfcrypt.is_encrypted pdf) then
    Pdf.iter_stream (recompress_stream pdf) pdf;
    pdf

let decompress_pdf pdf =
  if not (Pdfcrypt.is_encrypted pdf) then
    (Pdf.iter_stream (Pdfcodec.decode_pdfstream_until_unknown pdf) pdf);
    pdf

(* Return page label at pdf page num, or page number in arabic if no label *) 
let pagelabel pdf num =
  Pdfpagelabels.pagelabeltext_of_pagenumber
    num
    (Pdfpagelabels.complete (Pdfpagelabels.read pdf))


let rec process_text text m =
  match m with
  | ([] : (string * string) list) -> Cpdfstrftime.strftime text
  | (s, r)::t -> process_text (string_replace_all s r text) t

let expand_date = function
  | "now" -> Cpdfstrftime.strftime "D:%Y%m%d%H%M%S"
  | x -> x

let process_pages f pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pages' =
      map2
        (fun n p -> if mem n range then f n p else p)
        (ilist 1 (length pages))
        pages
    in
      Pdfpage.change_pages true pdf pages'

let iter_pages f pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
    iter2
      (fun n p -> if mem n range then f n p)
      (ilist 1 (length pages))
      pages

let map_pages f pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
    option_map2
      (fun n p -> if mem n range then Some (f n p) else None)
      (ilist 1 (length pages))
      pages

(* Add stack operators to a content stream to ensure it is composeable. FIXME:
This will go away once we're using postpend_content or similar for twoup and do_stamp... *)
let protect_removeme pdf resources content =
  let ops = Pdfops.parse_operators pdf resources content in
    let qs = length (keep (eq Pdfops.Op_q) ops)
    and bigqs = length (keep (eq Pdfops.Op_Q) ops) in
    let deficit = if qs > bigqs then qs - bigqs else 0 in
      if deficit <> 0 then Printf.eprintf "Q Deficit was nonzero. Fixing. %i\n" deficit;
      Pdfops.stream_of_ops ([Pdfops.Op_q] @ ops @ many Pdfops.Op_Q deficit @ [Pdfops.Op_Q])

exception SoftError of string

let error s = raise (SoftError s)

exception HardError of string

(* Union two resource dictionaries from the same PDF. *)
let combine_pdf_resources pdf a b =
  let a_entries =
    match a with
    | Pdf.Dictionary entries -> entries
    | _ -> []
  in let b_entries =
    match b with
    | Pdf.Dictionary entries -> entries
    | _ -> []
  in
    let resource_keys =
      ["/Font"; "/ExtGState"; "/ColorSpace"; "/Pattern";
       "/Shading"; "/XObject"; "/Properties"]
    in
      let combine_entries key =
        let a_entries =
          match Pdf.lookup_direct pdf key a with
          | Some (Pdf.Dictionary d) -> d
          | _ -> []
        in let b_entries =
          match Pdf.lookup_direct pdf key b with
          | Some (Pdf.Dictionary d) -> d
          | _ -> []
        in
          key, Pdf.Dictionary (a_entries @ b_entries)
      in
        let unknown_keys_a =
          lose (fun (k, _) -> mem k resource_keys) a_entries
        in let unknown_keys_b =
          lose (fun (k, _) -> mem k resource_keys) b_entries
        in let combined_known_entries =
          map combine_entries resource_keys
        in
          Pdf.Dictionary (unknown_keys_a @ unknown_keys_b @ combined_known_entries)

(* \section{Build PDF Presentations} *)
let change_page_effect t d horizontal inward direction effect_duration page =
  let checkname = function
    | "Split" | "Blinds" | "Box" | "Wipe" | "Dissolve" | "Glitter" -> ()
    | _ -> error "Unknown presentation type"
  in
    let rest = page.Pdfpage.rest in
      let transdict =
        match t with
        | None ->
            Pdf.Dictionary []
        | Some name ->
            checkname name;
            Pdf.Dictionary [("/S", Pdf.Name ("/" ^ name))]
      in
        let transdict = Pdf.add_dict_entry transdict "/D" (Pdf.Real effect_duration) in
          let transdict =
            match t with
            | Some ("Split" | "Blinds") ->
                Pdf.add_dict_entry
                  transdict "/Dm" (Pdf.Name (if horizontal then "/H" else "/V"))
            | _ -> transdict
          in
           let transdict =
             match t with
             | Some ("Split" | "Box") ->
                 Pdf.add_dict_entry
                   transdict "/M" (Pdf.Name (if inward then "/I" else "/O"))
             | _ -> transdict
           in 
             let transdict =
               match t with
               | Some ("Wipe" | "Glitter") ->
                   Pdf.add_dict_entry transdict "/Di" (Pdf.Integer direction)
               | _ -> transdict
             in
              let rest = Pdf.add_dict_entry rest "/Trans" transdict in
                let rest =
                  match d with
                  | None -> Pdf.remove_dict_entry rest "/Dur"
                  | Some delay -> Pdf.add_dict_entry rest "/Dur" (Pdf.Real delay)
                in
                  {page with Pdfpage.rest = rest}

let presentation range t d h i dir effect_dur pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pages' =
      map2
        (fun page num ->
          if mem num range
            then change_page_effect t d h i dir effect_dur page
            else page)
        pages
        (indx pages)
    in
      Pdfpage.change_pages true pdf pages'

(* \section{Attaching files} *)
let make_filestream file =
  let data =
    let ch = open_in_bin file in
      let len = in_channel_length ch in
        let stream = mkbytes len in
          for x = 0 to bytes_size stream - 1 do
            bset stream x (input_byte ch)
          done;
          close_in ch;
          stream
  in
    Pdf.Stream
      (ref (Pdf.Dictionary
             [("/Length", Pdf.Integer (bytes_size data));
              ("/Type", Pdf.Name "/EmbeddedFile")],
            Pdf.Got data))

let attach_file keepversion topage pdf file =
  let filestream = make_filestream file in
    let filestream_num = Pdf.addobj pdf filestream in
      let filespec =
        Pdf.Dictionary
          [("/EF", Pdf.Dictionary ["/F", Pdf.Indirect filestream_num]);
           ("/F", Pdf.String (Filename.basename file));
           ("/Type", Pdf.Name "/F")]
      in
        match topage with
        | None ->
            (* Look up /Names and /EmbeddedFiles and /Names. *)
            let rootdict = Pdf.lookup_obj pdf pdf.Pdf.root in
              let namedict =
                match Pdf.lookup_direct pdf "/Names" rootdict with
                | None -> Pdf.Dictionary []
                | Some namedict -> namedict
              in
                let embeddednamedict =
                  match Pdf.lookup_direct pdf "/EmbeddedFiles" namedict with
                  | None -> Pdf.Dictionary []
                  | Some embeddednamedict -> embeddednamedict
                in
                  let elts =
                    match Pdf.lookup_direct pdf "/Names" embeddednamedict with
                    | Some (Pdf.Array elts) -> elts
                    | _ -> []
                  in
                    let names' = Pdf.Array (elts @ [Pdf.String (Filename.basename file); filespec]) in
                      let embeddednamedict' = Pdf.add_dict_entry embeddednamedict "/Names" names' in
                        let namedict' = Pdf.add_dict_entry namedict "/EmbeddedFiles" embeddednamedict' in
                          let rootdict' = Pdf.add_dict_entry rootdict "/Names" namedict' in
                            let rootnum = Pdf.addobj pdf rootdict' in
                              {pdf with
                                 Pdf.minor = if keepversion then pdf.Pdf.minor else max pdf.Pdf.minor 4;
                                 Pdf.root = rootnum;
                                 Pdf.trailerdict =
                                   Pdf.add_dict_entry
                                     pdf.Pdf.trailerdict "/Root" (Pdf.Indirect rootnum)}
        | Some pagenumber ->
            let pages = Pdfpage.pages_of_pagetree pdf in
              if pagenumber < 0 || pagenumber > length pages then error "attach_file: Page not found" else
                let page = select pagenumber pages in
                  let annots =
                    match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
                    | Some (Pdf.Array annots) -> annots
                    | _ -> []
                  in
                    let rect =
                      let minx, miny, maxx, maxy = Pdf.parse_rectangle page.Pdfpage.mediabox in
                        Pdf.Array [Pdf.Real 18.; Pdf.Real (maxy -. 45.); Pdf.Real 45.; Pdf.Real (maxy -. 18.)]
                    in
                      let annot =
                        Pdf.Dictionary
                          [("/FS", filespec);
                           ("/Subtype", Pdf.Name "/FileAttachment");
                           ("/Contents", Pdf.String (Filename.basename file));
                           ("/Rect", rect)]
                      in
                        let annots' = Pdf.Array (annot::annots) in
                          let page' =
                            {page with Pdfpage.rest = Pdf.add_dict_entry page.Pdfpage.rest "/Annots" annots'}
                          in
                            let pages' = replace_number pagenumber page' pages in
                              let pdf = Pdfpage.change_pages false pdf pages' in
                                {pdf with
                                   Pdf.minor = if keepversion then pdf.Pdf.minor else max pdf.Pdf.minor 4}

let list_attached_files pdf =
  let toplevel =
    match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
    | None -> []
    | Some rootdict ->
        match Pdf.lookup_direct pdf "/Names" rootdict with
        | None -> []
        | Some namedict ->
            match Pdf.lookup_direct pdf "/EmbeddedFiles" namedict with
            | Some nametree ->
                 map
                   (function x -> x, 0)
                   (option_map
                     (function (Pdf.String s, _) -> Some s | _ -> None)
                     (Pdf.contents_of_nametree pdf nametree))
            | _ -> []
  in let pagelevel =
    let pages = Pdfpage.pages_of_pagetree pdf in
      flatten
        (map2
          (fun page pagenumber ->
             option_map
               (function annot ->
                  match Pdf.lookup_direct pdf "/Subtype" annot with
                  | Some (Pdf.Name "/FileAttachment") ->
                      (match Pdf.lookup_direct pdf "/Contents" annot with
                      | Some (Pdf.String s) -> Some (s, pagenumber)
                      | _ -> None)
                  | _ -> None)
               (match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
               | Some (Pdf.Array annots) -> annots
               | _ -> []))
          pages
          (indx pages))
  in
    toplevel @ pagelevel

(* \section{Remove Attached files} *)
let remove_attached_files_on_pages pdf =
  let remove_from_page page =
    {page with Pdfpage.rest =
       Pdf.add_dict_entry page.Pdfpage.rest "/Annots"
         (Pdf.Array
           (option_map
             (function annot ->
                match Pdf.lookup_direct pdf "/Subtype" annot with
                | Some (Pdf.Name "/FileAttachment") -> None
                | _ -> Some annot)
             (match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
             | Some (Pdf.Array annots) -> annots
             | _ -> [])))}
  in
    Pdfpage.change_pages true pdf (map remove_from_page (Pdfpage.pages_of_pagetree pdf))

let remove_attached_files pdf =
  let pdf = remove_attached_files_on_pages pdf in
    match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
    | None -> pdf
    | Some rootdict ->
        match Pdf.lookup_direct pdf "/Names" rootdict with
        | None -> pdf
        | Some namedict ->
            let namedict' = Pdf.remove_dict_entry namedict "/EmbeddedFiles" in
              let rootdict' = Pdf.add_dict_entry rootdict "/Names" namedict' in
                let rootdict'num = Pdf.addobj pdf rootdict' in
                  {pdf with
                     Pdf.root =
                       rootdict'num;
                     Pdf.trailerdict =
                       Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect rootdict'num)}

(* \section{Copy an /ID from one file to another} *)
let copy_id keepversion copyfrom copyto =
  match Pdf.lookup_direct copyfrom "/ID" copyfrom.Pdf.trailerdict with
  | None -> copyto (* error "Source PDF file has no /ID entry to copy from" *)
  | Some id ->
      copyto.Pdf.trailerdict <-
        Pdf.add_dict_entry copyto.Pdf.trailerdict "/ID" id;
      copyto.Pdf.minor <-
        if keepversion then copyto.Pdf.minor else max copyto.Pdf.minor 1;
      copyto

(* \section{Remove bookmarks} *)

(* \section{Add bookmarks} *)
let read_lines input =
  let lines = ref [] in
   try
     while true do
       let c = read_line input in
         lines =| c
    done; []
   with
     _ -> rev !lines

(* Verify a list of bookmarks. Positive jumps of > 1 not allowed, no numbers
smaller than 0. *)
let rec verify_bookmarks pdf lastlevel endpage = function
  | [] -> true
  | {Pdfmarks.level = level; Pdfmarks.target = target}::more ->
      let page = Pdfpage.pagenumber_of_target pdf target in
        level < lastlevel + 2 &&
        level >= 0 &&
        page <= endpage &&
        page >= 0 &&
        verify_bookmarks pdf level endpage more

(* Parse a line of the bookmarks file. *)

(* Un-escape things which are escaped. Quotes, newlines and backslashes *)
let rec fixup_characters prev = function
  | [] -> rev prev
  | '\\'::'\\'::t -> fixup_characters ('\\'::prev) t
  | '\\'::'\"'::t -> fixup_characters ('\"'::prev) t
  | '\\'::'\n'::t -> fixup_characters ('\n'::prev) t
  | h::t -> fixup_characters (h::prev) t

let parse_bookmark_file verify pdf input =
  try
    let lines = Pdfio.read_lines input in
      (*i Printf.printf "Read %i lines\n" (length lines);
      iter (function x -> Printf.printf "%s\n" x) lines; i*)
      let currline = ref 0 in
      let bookmarks = ref [] in
        iter
          (function line ->
             match
               incr currline;
               Pdfgenlex.lex_string line
             with
             | [Pdfgenlex.LexInt i; Pdfgenlex.LexString s; Pdfgenlex.LexInt i'; Pdfgenlex.LexName "open"] ->
                 bookmarks =|
                   {Pdfmarks.level = i;
                    Pdfmarks.text = Pdftext.pdfdocstring_of_utf8 (implode (fixup_characters [] (explode s)));
                    Pdfmarks.target = Pdfpage.target_of_pagenumber pdf i';
                    Pdfmarks.isopen = true}
             | [Pdfgenlex.LexInt i; Pdfgenlex.LexString s; Pdfgenlex.LexInt i'; Pdfgenlex.LexName "closed"]
             | [Pdfgenlex.LexInt i; Pdfgenlex.LexString s; Pdfgenlex.LexInt i'] ->
                 bookmarks =|
                   {Pdfmarks.level = i;
                    Pdfmarks.text = Pdftext.pdfdocstring_of_utf8 (implode (fixup_characters [] (explode s)));
                    Pdfmarks.target = Pdfpage.target_of_pagenumber pdf i';
                    Pdfmarks.isopen = false}
             | [] -> () (* ignore blank lines *)
             | _ ->
                 (*i flprint (Pdfgenlex.string_of_tokens n); i*)
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
    _ -> (*i Printf.printf "%s\n" (Printexc.to_string e); i*) error "Bad bookmark file (syntax)"


let add_bookmarks verify input pdf =
  let parsed = parse_bookmark_file verify pdf input in
    (*iter (fun b -> flprint (Pdfmarks.string_of_bookmark b); flprint "\n") parsed;*)
    Pdfmarks.add_bookmarks parsed pdf 

(* \section{Set page mode} *)
let set_page_mode pdf s =
  match s with
  | "UseNone" | "UseOutlines" | "UseThumbs"
  | "FullScreen" | "UseOC" | "UseAttachments" ->
      begin match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
      | Some catalog ->
          let catalog' =
            Pdf.add_dict_entry catalog "/PageMode" (Pdf.Name ("/" ^ s))
          in
            let catalognum = Pdf.addobj pdf catalog' in
              let trailerdict' =
                Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect catalognum)
              in
                {pdf with
                  Pdf.root = catalognum;
                  Pdf.trailerdict = trailerdict'}
      | None -> error "bad root"
      end
  | _ -> error "Unknown page mode"

(* Set open action *)
let set_open_action pdf fit pagenumber =
  if pagenumber > Pdfpage.endpage pdf || pagenumber < 0 then
    raise (error "set_open_action: invalid page number")
  else
    let pageobjectnumber = select pagenumber (Pdf.page_reference_numbers pdf) in
      let destination =
        if fit then
          Pdf.Array [Pdf.Indirect pageobjectnumber; Pdf.Name "/Fit"]
        else
          Pdf.Array [Pdf.Indirect pageobjectnumber; Pdf.Name "/XYZ"; Pdf.Null; Pdf.Null; Pdf.Null]
      in
        let open_action =
          Pdf.Dictionary [("/D", destination); ("/S", Pdf.Name "/GoTo")]
        in
          match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
          | Some catalog ->
              let catalog' =
                Pdf.add_dict_entry catalog "/OpenAction" open_action
              in
                let catalognum = Pdf.addobj pdf catalog' in
                  let trailerdict' =
                    Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect catalognum)
                  in
                    {pdf with Pdf.root = catalognum; Pdf.trailerdict = trailerdict'}
          | None -> error "bad root"

(* \section{Set viewer preferences} *)
let set_viewer_preference (key, value, version) pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | Some catalog ->
      let viewer_preferences =
        match Pdf.lookup_direct pdf "/ViewerPreferences" catalog with
        | Some d -> d
        | None -> Pdf.Dictionary []
      in
        let viewer_preferences' =
          Pdf.add_dict_entry viewer_preferences key value
        in
          let catalog' =
            Pdf.add_dict_entry catalog "/ViewerPreferences" viewer_preferences'
          in
            let catalognum = Pdf.addobj pdf catalog' in
              let trailerdict' =
                Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect catalognum)
              in
                {pdf with
                  Pdf.minor = max pdf.Pdf.minor version;
                  Pdf.root = catalognum;
                  Pdf.trailerdict = trailerdict'}
  | None -> error "bad root"

(* \section{Set an entry in the /Info dictionary} *)
let set_pdf_info (key, value, version) pdf =
  let infodict =
    match Pdf.lookup_direct pdf "/Info" pdf.Pdf.trailerdict with
    | Some d -> d
    | None -> Pdf.Dictionary []
  in
    let infodict' = Pdf.add_dict_entry infodict key value in
      let objnum = Pdf.addobj pdf infodict' in
        pdf.Pdf.trailerdict <-
          Pdf.add_dict_entry pdf.Pdf.trailerdict "/Info" (Pdf.Indirect objnum);
        pdf.Pdf.minor <-
          max pdf.Pdf.minor version;
        pdf

(* \section{Set page layout} *)
let set_page_layout pdf s =
  match s with
  | "SinglePage" | "OneColumn" | "TwoColumnLeft"
  | "TwoColumnRight" | "TwoPageLeft" | "TwoPageRight" ->
      begin match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
      | Some catalog ->
          let catalog' =
            Pdf.add_dict_entry catalog "/PageLayout" (Pdf.Name ("/" ^ s))
          in
            let catalognum = Pdf.addobj pdf catalog' in
              let trailerdict' =
                Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect catalognum)
              in
                {pdf with
                  Pdf.root = catalognum;
                  Pdf.trailerdict = trailerdict'}
      | None -> error "bad root"
      end
  | _ -> error "Unknown page layout"

(* \section{Set or replace metadata} *)
let set_metadata_from_bytes keepversion data pdf =
  let metadata_stream =
    Pdf.Stream
      {contents =
        (Pdf.Dictionary
         ["/Length", Pdf.Integer (bytes_size data);
          "/Type", Pdf.Name "/Metadata";
          "/Subtype", Pdf.Name "/XML"],
         Pdf.Got data)}
  in
    let objnum = Pdf.addobj pdf metadata_stream in 
      let document_catalog =
        match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
        | Some s -> s
        | None -> error "Malformed PDF: No root."
      in
        let document_catalog' =
          Pdf.add_dict_entry document_catalog "/Metadata" (Pdf.Indirect objnum)
        in
          let rootnum = Pdf.addobj pdf document_catalog' in
            let trailerdict =
              Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect rootnum)
            in
            {pdf with
               Pdf.trailerdict = trailerdict;
               Pdf.root = rootnum;
               Pdf.minor =
                 if keepversion then pdf.Pdf.minor else max 4 pdf.Pdf.minor}

let set_metadata keepversion filename pdf =
  let ch = open_in_bin filename in
    let data = mkbytes (in_channel_length ch) in
      for x = 0 to bytes_size data - 1 do
        bset data x (input_byte ch)
      done;
      set_metadata_from_bytes keepversion data pdf

(* \section{Remove metadata} *)
let remove_metadata pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | None -> error "malformed file" 
  | Some root ->
      let root' = Pdf.remove_dict_entry root "/Metadata" in
        let rootnum = Pdf.addobj pdf root' in
          {pdf with
             Pdf.trailerdict =
               Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect rootnum);
             Pdf.root =
               rootnum}

(* List bookmarks *)

(* List the bookmarks, optionally deunicoding the text, in the given range to the given output *)
let list_bookmarks encoding range pdf output =
  let process_string s =
    let rec replace c cs = function
    | [] -> []
    | h::t when h = c -> cs @ replace c cs t
    | h::t -> h::replace c cs t
    in
      (* Convert to UTF8, raw, or stripped, and escape backslashed and quotation marks *)
      let codepoints = Pdftext.codepoints_of_pdfdocstring s in
        let escaped =
          let bs = int_of_char '\\'
          and nl = int_of_char '\n'
          and n = int_of_char 'n'
          and q = int_of_char '\"' in
            replace bs [bs; bs] (replace nl [bs; n] (replace q [bs; q] codepoints))
        in
          match encoding with
          | UTF8 -> Pdftext.utf8_of_codepoints escaped
          | Stripped -> implode (map char_of_int (lose (fun x -> x > 127) escaped))
          | Raw -> s
    in
      let bookmarks = Pdfmarks.read_bookmarks pdf in
        let inrange =
          keep
            (function x ->
               x.Pdfmarks.target = Pdfdest.NullDestination || 
               mem (Pdfpage.pagenumber_of_target pdf x.Pdfmarks.target) range) bookmarks
        in
          iter
            (function mark ->
               output.Pdfio.output_string
                 (Printf.sprintf "%i \"%s\" %i %s\n"
                   mark.Pdfmarks.level
                   (process_string mark.Pdfmarks.text)
                   (Pdfpage.pagenumber_of_target pdf mark.Pdfmarks.target)
                   (if mark.Pdfmarks.isopen then "open" else "")))
            inrange

(* \section{Stamping files} *)
(* o is the stamp, u is the main pdf page *)
let do_stamp fast scale_to_fit isover pdf o u opdf =
  (* Scale page stamp o to fit page u *)
  let o =
    if scale_to_fit then
      let sxmin, symin, sxmax, symax =
        Pdf.parse_rectangle
          (match Pdf.lookup_direct pdf "/CropBox" o.Pdfpage.rest with | Some r -> r | None -> o.Pdfpage.mediabox)
      in let txmin, tymin, txmax, tymax =
        Pdf.parse_rectangle
          (match Pdf.lookup_direct pdf "/CropBox" u.Pdfpage.rest with | Some r -> r | None -> u.Pdfpage.mediabox)
      in
        let xmag = (txmax -. txmin) /. (sxmax -. sxmin)
        in let ymag = (tymax -. tymin) /. (symax -. symin) in
          let scale =
            if xmag < 0.999 && ymag < 0.999 then
              if xmag > ymag then xmag else ymag
            else if xmag >= 1.001 && ymag >= 1.001 then
              if xmag > ymag then ymag else xmag
            else if xmag >= 1.001 then ymag
            else xmag
          in
            let dx = txmin +. ((txmax -. txmin) -. (sxmax -. sxmin) *. scale) /. 2.
            in let dy = tymin +. ((tymax -. tymin) -. (symax -. symin) *. scale) /. 2. in
              let scale_op =
                Pdfops.Op_cm
                  (Pdftransform.matrix_of_transform
                     [Pdftransform.Translate (dx, dy);
                      Pdftransform.Scale ((sxmin, symin), scale, scale)])
              in
                Pdfpage.prepend_operators opdf [scale_op] ~fast o
    else
      o
  in
    {u with
       Pdfpage.content =
         (if isover then ( @ ) else ( @@ ))
         [protect_removeme pdf u.Pdfpage.resources u.Pdfpage.content]
           [protect_removeme pdf o.Pdfpage.resources o.Pdfpage.content];
       Pdfpage.resources =
         combine_pdf_resources pdf u.Pdfpage.resources o.Pdfpage.resources}

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

let stamp (fast : bool) scale_to_fit isover range over pdf =
  let marks = Pdfmarks.read_bookmarks pdf in
  let marks_refnumbers = Pdf.page_reference_numbers pdf in
  let pdf = Pdfmarks.remove_bookmarks pdf in
  let over = Pdfmarks.remove_bookmarks over in
  let pageseqs = ilist 1 (Pdfpage.endpage pdf) in
    let over_firstpage_pdf =
      match Pdfpage.pages_of_pagetree over with
      | [] -> error "empty PDF"
      | h::_ -> Pdfpage.change_pages true over [h]
    in
      let merged =
        Pdfmerge.merge_pdfs
          ~rotations:[Pdfmerge.DNR; Pdfmerge.DNR]
          false false ["a"; "b"] [pdf; over_firstpage_pdf] [pageseqs; [1]]
      in
        let renamed_pdf =
          Pdfpage.change_pages true
            merged (Pdfpage.renumber_pages merged (Pdfpage.pages_of_pagetree merged))
        in
          let renamed_pages = Pdfpage.pages_of_pagetree renamed_pdf in
            let under_pages, over_page =
              all_but_last renamed_pages, last renamed_pages
            in
              let new_pages =
                map2
                  (fun pageseq under_page ->
                    do_stamp fast scale_to_fit isover renamed_pdf
                    (if mem pageseq range then over_page else
                      Pdfpage.blankpage Pdfpaper.a4)
                    under_page over)
                  pageseqs
                  under_pages 
              in
                let changed = Pdfpage.change_pages true renamed_pdf new_pages in
                let new_refnumbers = Pdf.page_reference_numbers changed in
                let changetable = hashtable_of_dictionary (List.combine marks_refnumbers new_refnumbers) in
                let new_marks = map (change_bookmark changetable) marks in
                Pdfmarks.add_bookmarks new_marks changed

(* Combine pages from two PDFs. For now, assume equal length. *)

(* If [over] has more pages than [under], chop the excess. If the converse, pad
[over] to the same length *)
let equalize_pages under over =
  let length_under = Pdfpage.endpage under
  in let length_over = Pdfpage.endpage over
  in
    if length_over > length_under then
      under,
      (Pdfpage.change_pages true over (take (Pdfpage.pages_of_pagetree over) length_under))
    else if length_under > length_over then
      under,
      Pdfpage.change_pages true
        over
        (Pdfpage.pages_of_pagetree over @
           (many (Pdfpage.blankpage Pdfpaper.a4) (length_under - length_over)))
    else
      under, over

let combine_pages (fast : bool) under over scaletofit swap equalize =
  let marks_under = Pdfmarks.read_bookmarks under in
  let marks_over = Pdfmarks.read_bookmarks over in
  let under, over = if equalize then equalize_pages under over else under, over in
    let under_length = Pdfpage.endpage under
    in let over_length = Pdfpage.endpage over in
      if under_length <> over_length then raise (Pdf.PDFError "combine_pages: not of equal length") else
      let pageseqs_under = ilist 1 (Pdfpage.endpage under)
      in let pageseqs_over = ilist 1 (Pdfpage.endpage over) in
        let merged =
          Pdfmerge.merge_pdfs ~rotations: [Pdfmerge.DNR; Pdfmerge.DNR] false false ["a"; "b"] [under; over] [pageseqs_under; pageseqs_over] in
          let renamed_pdf =
            Pdfpage.change_pages true
              merged (Pdfpage.renumber_pages merged (Pdfpage.pages_of_pagetree merged))
          in
            let under_pages, over_pages =
              cleave (Pdfpage.pages_of_pagetree renamed_pdf) under_length
            in
              let new_pages =
                map2 (fun o u -> do_stamp fast scaletofit (not swap) renamed_pdf o u over) over_pages under_pages
              in
                Pdfmarks.add_bookmarks (marks_under @ marks_over) (Pdfpage.change_pages true renamed_pdf new_pages)

(* \section{Split at bookmarks} *)

(* Returns empty string on failure. Should only be used in conjunction with
split at bookmarks code, so should never fail, by definiton. *)
let remove_unsafe_characters s =
  let chars =
    lose
      (function x ->
         match x with
         '/' | '?' | '<' | '>' | '\\' | ':' | '*' | '|' | '\"' | '^' | '+' | '=' -> true
         | x when int_of_char x < 32 || int_of_char x > 126 -> true
         | _ -> false)
      (explode s)
  in
    match chars with
    | '.'::more -> implode more
    | chars -> implode chars

let get_bookmark_name pdf marks splitlevel n _ =
  match keep (function m -> n = Pdfpage.pagenumber_of_target pdf m.Pdfmarks.target && m.Pdfmarks.level <= splitlevel) marks with
  | {Pdfmarks.text = title}::_ -> remove_unsafe_characters title
  | _ -> ""

(* @F means filename without extension *)
(* @N means sequence number with no padding *)
(* @S means start page of this section *)
(* @E means end page of this section *)
(* @B means bookmark name at start page *)
let process_others marks pdf splitlevel filename sequence startpage endpage s =
  let rec procss prev = function
    | [] -> rev prev
    | '@'::'F'::t -> procss (rev (explode filename) @ prev) t
    | '@'::'N'::t -> procss (rev (explode (string_of_int sequence)) @ prev) t
    | '@'::'S'::t -> procss (rev (explode (string_of_int startpage)) @ prev) t
    | '@'::'E'::t -> procss (rev (explode (string_of_int endpage)) @ prev) t
    | '@'::'B'::t -> procss (rev (explode (get_bookmark_name pdf marks splitlevel startpage pdf)) @ prev) t
    | h::t -> procss (h::prev) t
  in
     implode (procss [] (explode s))

let name_of_spec printf marks (pdf : Pdf.t) splitlevel spec n filename startpage endpage =
  if printf then
    let spec =
      string_of_sections (substitute (sections_of_string spec) [n])
    in
      process_others marks pdf splitlevel filename n startpage endpage spec
  else
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
                process_others marks pdf splitlevel filename n startpage endpage spec
              else
                process_others marks pdf splitlevel filename n startpage endpage
                (implode before ^ fill (length percents) n ^ implode after)

(* Find the stem of a filename *)
let stem s =
  implode (rev (tail_no_fail (dropwhile (neq '.') (rev (explode (Filename.basename s))))))

let fast_write_split_pdfs enc printf splitlevel original_filename linearize preserve_objstm create_objstm nobble spec main_pdf pagenums pdf_pages =
  let marks = Pdfmarks.read_bookmarks main_pdf in
    iter2
      (fun number pagenums ->
         let pdf = nobble (Pdfpage.pdf_of_pages main_pdf pagenums) in
           let startpage, endpage = extremes pagenums in
             let name = name_of_spec printf marks main_pdf splitlevel spec number (stem original_filename) startpage endpage in
               Pdf.remove_unreferenced pdf;
               Pdfwrite.pdf_to_file_options ~preserve_objstm ~generate_objstm:create_objstm linearize enc (not (enc = None)) pdf name)
      (indx pagenums)
      pagenums

let split_pdf enc printf original_filename chunksize linearize ~preserve_objstm ~create_objstm nobble spec pdf =
  let pdf_pages = Pdfpage.pages_of_pagetree pdf in
    fast_write_split_pdfs enc printf 0 original_filename linearize preserve_objstm
      create_objstm nobble spec pdf (splitinto chunksize (indx pdf_pages)) pdf_pages

(* Return list, in order, a *set* of page numbers of bookmarks at a given level *)
let bookmark_pages level pdf =
  setify_preserving_order
    (option_map
      (function l when l.Pdfmarks.level = level -> Some (Pdfpage.pagenumber_of_target pdf l.Pdfmarks.target) | _ -> None)
      (Pdfmarks.read_bookmarks pdf))

let split_at_bookmarks original_filename linearize ~preserve_objstm ~create_objstm nobble level spec pdf =
  let pdf_pages = Pdfpage.pages_of_pagetree pdf in
    let points = bookmark_pages level pdf in
      let points =
        lose (fun x -> x <= 0 || x > Pdfpage.endpage pdf) (map pred points)
      in
      let pts = splitat points (indx pdf_pages) in
      fast_write_split_pdfs None false level
        original_filename linearize preserve_objstm create_objstm nobble spec pdf pts pdf_pages

(* Called from cpdflib.ml - different from above *)
let split_on_bookmarks pdf level =
  let points = lose (eq 0) (map pred (bookmark_pages level pdf))
  in let pdf_pages = Pdfpage.pages_of_pagetree pdf in
    let ranges = splitat points (indx pdf_pages) in
      map (fun rs -> Pdfpage.pdf_of_pages pdf rs) ranges

(* Output information for each page *)
let output_page_info pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf
  and labels = Pdfpagelabels.read pdf in
    let getbox page box =
      if box = "/MediaBox" then
        match page.Pdfpage.mediabox with
        | Pdf.Array [a; b; c; d] ->
           Printf.sprintf "%f %f %f %f"
             (Pdf.getnum a) (Pdf.getnum b) (Pdf.getnum c) (Pdf.getnum d)
        | _ -> ""
      else
        match Pdf.lookup_direct pdf box page.Pdfpage.rest with
        | Some (Pdf.Array [a; b; c; d]) ->
           Printf.sprintf "%f %f %f %f"
             (Pdf.getnum a) (Pdf.getnum b) (Pdf.getnum c) (Pdf.getnum d)
        | _ -> ""
    and rotation page =
      Pdfpage.int_of_rotation page.Pdfpage.rotate
    in
      List.iter
        (fun pnum ->
           let page = select pnum pages in
             Printf.printf "Page %i:\n" pnum;
             Printf.printf "Label: %s\n"
               (try Pdfpagelabels.pagelabeltext_of_pagenumber pnum labels with Not_found -> "");
             Printf.printf "MediaBox: %s\n" (getbox page "/MediaBox");
             Printf.printf "CropBox: %s\n" (getbox page "/CropBox");
             Printf.printf "BleedBox: %s\n" (getbox page "/BleedBox");
             Printf.printf "TrimBox: %s\n" (getbox page "/TrimBox");
             Printf.printf "ArtBox: %s\n" (getbox page "/ArtBox");
             Printf.printf "Rotation: %i\n" (rotation page))
        range

(* Does the page have a defined box e.g "/CropBox" *)
let hasbox pdf page boxname =
  let pages = Pdfpage.pages_of_pagetree pdf in
    if page > length pages || page < 1 then raise (Failure "hasbox: bad page") else
      let p = select page pages in
        match Pdf.lookup_direct pdf boxname p.Pdfpage.rest with
        | Some _ -> true
        | _ -> false

(* \section{Print metadata} *)
let get_metadata pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | None -> error "malformed file"
  | Some root ->
      match Pdf.lookup_direct pdf "/Metadata" root with
      | Some ((Pdf.Stream _) as s) ->
          Pdf.getstream s;
          begin match s with
          | Pdf.Stream {contents = (_, Pdf.Got data)} -> Some data 
          | _ -> assert false
          end
      | _ -> None

let print_metadata pdf =
  match get_metadata pdf with
    None -> ()
  | Some data ->
      for x = 0 to bytes_size data - 1 do
        Printf.printf "%c" (char_of_int (bget data x))
      done

(* \section{Print font data} *)
let list_font pdf page (name, dict) =
  let subtype =
    match Pdf.lookup_direct pdf "/Subtype" dict with
    | Some (Pdf.Name n) -> n
    | _ -> ""
  in let basefont =
    match Pdf.lookup_direct pdf "/BaseFont" dict with
    | Some (Pdf.Name n) -> n
    | _ -> ""
  in let encoding =
   match Pdf.lookup_direct pdf "/Encoding" dict with
    | Some (Pdf.Name n) -> n
    | _ -> ""
  in 
    (*i Printf.printf
      "%i %s %s %s %s\n" i*)
      page, name, subtype, basefont, encoding

let list_fonts pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    flatten
      (map
        (fun (num, page) ->
           match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
           | Some (Pdf.Dictionary fontdict) ->
               map (list_font pdf num) fontdict
           | _ -> [])
        (combine (ilist 1 (length pages)) pages))

let string_of_font (p, n, s, b, e) =
  Printf.sprintf "%i %s %s %s %s\n" p n s b e

let print_fonts pdf =
  flprint
    (fold_left ( ^ ) "" (map string_of_font (list_fonts pdf)))

(* \section{Nobbling for Demo Version} *)
let nobble_page pdf _ page =
  let minx, miny, maxx, maxy =
    (* Use cropbox if available *)
    Pdf.parse_rectangle
      (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
       | Some r -> r
       | None -> page.Pdfpage.mediabox)
  in
    let fontdict =
      match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
      | None -> Pdf.Dictionary []
      | Some d -> d
    in
      let fontname = Pdf.unique_key "F" fontdict in
    let width = maxx -. minx in let height = maxy -. miny in
      let scalex =
        (width *. 1000.) /. float (Pdfstandard14.textwidth false Pdftext.Helvetica "DEMO")
      in
        let page' =
          let font =
            Pdf.Dictionary
              [("/Type", Pdf.Name "/Font");
               ("/Subtype", Pdf.Name "/Type1");
               ("/BaseFont", Pdf.Name "/Helvetica")]
          in let ops =
            [Pdfops.Op_BMC "/CPDFSTAMP";
             Pdfops.Op_cm
               (Pdftransform.matrix_of_transform
                  [Pdftransform.Translate (minx, miny +. height /. 2.)]);
             Pdfops.Op_gs "/gs0";
             Pdfops.Op_BT;
             Pdfops.Op_Tf (fontname, scalex);
             Pdfops.Op_Tj "DEMO";
             Pdfops.Op_ET;
             Pdfops.Op_EMC]
          in
            {(Pdfpage.blankpage Pdfpaper.a4) with
                Pdfpage.mediabox = page.Pdfpage.mediabox;
                Pdfpage.content = [Pdfops.stream_of_ops ops];
                Pdfpage.resources =
                  Pdf.Dictionary
                    [("/Font", Pdf.Dictionary [(fontname, font)]);
                     ("/ExtGState", Pdf.Dictionary
                        ["/gs0",
                        Pdf.Dictionary["/Type", Pdf.Name "/ExtGState"; "/ca", Pdf.Real 0.2]]);
                    ]
            }
        in
          do_stamp false false true pdf page' page (Pdf.empty ())

(* \section{Superimpose text, page numbers etc.} *)

type position =
  | PosCentre of float * float
  | PosLeft of float * float
  | PosRight of float * float
  | Top of float
  | TopLeft of float
  | TopRight of float
  | Left of float
  | BottomLeft of float
  | Bottom of float
  | BottomRight of float
  | Right of float
  | Diagonal
  | ReverseDiagonal

let string_of_position = function
  | PosCentre (a, b) -> Printf.sprintf "PosCentre %f %f" a b
  | PosLeft (a, b) -> Printf.sprintf "PosLeft %f %f" a b
  | PosRight (a, b) -> Printf.sprintf "PosRight %f %f" a b
  | Top a -> Printf.sprintf "Top %f" a
  | TopLeft a -> Printf.sprintf "TopLeft %f" a
  | TopRight a -> Printf.sprintf "TopRight %f" a
  | Left a -> Printf.sprintf "Left %f" a
  | BottomLeft a -> Printf.sprintf "BottomLeft %f" a
  | Bottom a -> Printf.sprintf "Bottom %f" a
  | BottomRight a -> Printf.sprintf "BottomRight %f" a
  | Right a -> Printf.sprintf "Right %f" a
  | Diagonal -> "Diagonal"
  | ReverseDiagonal -> "Reverse Diagonal"

type orientation =
  | Horizontal
  | Vertical
  | VerticalDown

type justification = LeftJustify | CentreJustify | RightJustify

(* Given the mediabox, calculate an absolute position for the text. *)
let calculate_position ignore_d w (xmin, ymin, xmax, ymax) orientation pos =
  (*i Printf.printf "calculate_position %b %f %f %f %f %f %b\n" ignore_d w xmin ymin xmax ymax shorterside; i*)
    let rot = if orientation = VerticalDown then rad_of_deg 270. else 0. in
      match pos with
      | Diagonal ->
          let angle = atan ((ymax -. ymin) /. (xmax -. xmin))
          in let cx, cy = (xmax +. xmin) /. 2., (ymax +. ymin) /. 2. in
            let dl = w /. 2. in
              let dx = dl *. cos angle
              in let dy = dl *. sin angle in
                (*i Printf.printf "Diagonal: angle = %f, cx = %f, cy = %f, dx = %f, dy = %f\n" angle cx cy dx dy; i*)
                cx -. dx, cy -. dy, angle
      | ReverseDiagonal ->
          (*flprint "REVERSE DIAGONAL IN CALCULATE POSITION\n";*)
          let angle = atan ((ymax -. ymin) /. (xmax -. xmin))
          in let cx, cy = (xmax +. xmin) /. 2., (ymax +. ymin) /. 2. in
            let dl = w /. 2. in
              let dx = dl *. cos angle
              in let dy = dl *. sin angle in
                (*Printf.printf "Diagonal: angle = %f\n" (deg_of_rad angle);*)
                cx -. dx, (ymax +. ymin) -. (cy -. dy), angle -. ((2. *. pi) -. ((pi -. (2. *. angle)) *. 2.) /. 2.) +. pi
      | PosLeft (x, y) -> xmin +. x, ymin +. y, rot
      | PosCentre (x, y) -> xmin +. x -. (w /. 2.), ymin +. y, rot
      | PosRight (x, y) -> xmin +. x -. w, ymin +. y, rot
      | Top d ->
          let d = if ignore_d then 0. else d in
            (xmin +. xmax) /. 2. -. w /. 2., ymax -. d, rot
      | TopLeft d ->
          let d = if ignore_d then 0. else d in
            xmin +. d, ymax -. d, rot
      | TopRight d ->
          let d = if ignore_d then 0. else d in
          xmax -. d -. w, ymax -. d, rot
      | Left d ->
          let d = if ignore_d then 0. else d in
            xmin +. d, (ymax +. ymin) /. 2., rot
      | BottomLeft d ->
          let d = if ignore_d then 0. else d in
            xmin +. d, ymin +. d, rot
      | Bottom d ->
          let d = if ignore_d then 0. else d in
            (xmin +. xmax) /. 2. -. w /. 2., ymin +. d, rot
      | BottomRight d ->
          let d = if ignore_d then 0. else d in
            xmax -. d -. w, ymin +. d, rot
      | Right d ->
          let d = if ignore_d then 0. else d in
            xmax -. d -. w, (ymax +. ymin) /. 2., rot

(* Process UTF8 text to /WinAnsiEncoding string. *)
let winansi_of_utf8 s =
  (*flprint "winansi_of_utf8:";
  iter (Printf.printf "%C ") (explode s);
  flprint "\n";*)
  let extractor = Pdftext.charcode_extractor_of_encoding Pdftext.WinAnsiEncoding
  and codepoints = Pdftext.codepoints_of_utf8 s in
    (*flprint "codepoints after Pdftext.codepoints_of_utf8\n";
    iter (Printf.printf "%i ") codepoints;
    flprint "\ndone\n";*)
    implode (map char_of_int (option_map extractor codepoints))

(* Process codepoints back to UTF8, assuming it came from UTF8 to start with *)
let utf8_of_winansi s =
  let text_extractor =
    Pdftext.text_extractor_of_font
      (Pdf.empty ())
      (Pdf.Dictionary
        [("/BaseFont", Pdf.Name "/TimesRoman");
         ("/Subtype", Pdf.Name "/Type1");
         ("/Encoding", Pdf.Name "/WinAnsiEncoding")]) 
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
   [
   (match colour with (r, g, b) -> Pdfops.Op_rg (r, g, b));
   (match colour with (r, g, b) -> Pdfops.Op_RG (r, g, b))]
   @
   (match unique_extgstatename with None -> [] | Some n -> [Pdfops.Op_gs n])
   @
   [Pdfops.Op_Tf (unique_fontname, fontsize);
   Pdfops.Op_Tj text;
   Pdfops.Op_ET;
   Pdfops.Op_EMC;
   Pdfops.Op_Q]

(* Find the h-offset for justification based on the longest width, the current
width, the justification and the position. *)
let find_justification_offsets longest_w w position = function
  | LeftJustify ->
      begin match position with
      | TopLeft _ | Left _ | PosLeft _ | BottomLeft _ -> 0.
      | Top _ | PosCentre _ | Bottom _ -> (longest_w -. w) /. 2.
      | TopRight _ | BottomRight _ | PosRight _ | Right _ -> longest_w -. w
      | Diagonal -> 0.
      | ReverseDiagonal -> 0.
      end
  | RightJustify ->
      begin match position with
      | TopLeft _ | Left _ | PosLeft _ | BottomLeft _ -> ~-.(longest_w -. w)
      | Top _ | PosCentre _ | Bottom _ -> ~-.((longest_w -. w) /. 2.)
      | TopRight _ | BottomRight _ | PosRight _ | Right _ -> 0.
      | Diagonal -> 0.
      | ReverseDiagonal -> 0.
      end
  | CentreJustify ->
      begin match position with
      | TopLeft _ | Left _ | PosLeft _ | BottomLeft _ -> ~-.((longest_w -. w) /. 2.)
      | Top _ | PosCentre _ | Bottom _ -> 0.
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

(* Get the widths for each character from the hash table, in order, noting the
first and last *)
let extract_widths_firstlast width_data =
  let sorted = List.sort compare (list_of_hashtbl width_data) in
    match sorted, rev sorted with
      (first, _)::_, (last, _)::_ ->
        for x = first to last do
          if not (Hashtbl.mem width_data x) then Hashtbl.add width_data x 0
        done;
        (first, last,
         map snd (List.sort compare (list_of_hashtbl width_data))) 
    | _ -> raise (Failure "extract_widths_firstlast") 

let make_font fontname =
  let font = unopt (Pdftext.standard_font_of_name ("/" ^ fontname)) in
  let header, width_data, _ = Pdfstandard14.afm_data font in
    let firstchar, lastchar, widths = extract_widths_firstlast width_data in
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
        Pdf.Dictionary
          [("/Type", Pdf.Name "/Font");
           ("/Encoding", Pdf.Name "/WinAnsiEncoding");
           ("/Subtype", Pdf.Name "/Type1");
           ("/BaseFont", Pdf.Name ("/" ^ fontname));
           ("/FirstChar", Pdf.Integer firstchar);
           ("/LastChar", Pdf.Integer lastchar);
           ("/Widths", Pdf.Array (map (fun x -> Pdf.Integer x) widths));
           ("/FontDescriptor", fontdescriptor)]

let addtext
  metrics lines linewidth outline fast colour fontname bates fontsize font
  underneath position hoffset voffset text pages orientation cropbox opacity
  justification filename pdf
=
  let endpage = Pdfpage.endpage pdf in
  let replace_pairs pdf filename bates num =
      ["%Page", string_of_int num;
       "%Roman", roman_upper num;
       "%roman", roman_lower num;
       "%filename", filename;
       "%Label", pagelabel pdf num;
       "%EndPage", string_of_int endpage;
       "%EndLabel", pagelabel pdf endpage;
       "%Bates", string_of_int (bates + num - 1)] in
  let addtext_page num page =
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
        let unique_fontname = Pdf.unique_key "F" fontdict in
          let ops =
            let text = process_text text (replace_pairs pdf filename bates num) in
              let calc_textwidth text =
                match font with
                | Some f ->
                    (* FIXME This is a bit wrong in the prescence of special
                    characters due to standard encoding not win encoding being
                    used in textwidth. When we have new AFM parsing up and
                    running, can improve. *)
                    let rawwidth = Pdfstandard14.textwidth false f text in
                      (float rawwidth *. fontsize) /. 1000.
                | None -> 
                    let font =
                      match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
                      | Some fontdict ->
                          begin match Pdf.lookup_direct pdf fontname fontdict with
                          | Some font -> font
                          | _ -> failwith "addtext: bad font"
                          end
                      | _ -> failwith "addtext: bad font"
                    in
                      let rawwidth = width_of_text (Pdftext.read_font pdf font) text in
                        (rawwidth *. fontsize) /. 1000.
              in
                let expanded_lines =
                  map (function text -> process_text text (replace_pairs pdf filename bates num)) lines
                in
                let textwidth = calc_textwidth text
                and allwidths = map calc_textwidth expanded_lines in
                  let longest_w = last (sort compare allwidths) in
                  let joffset = find_justification_offsets longest_w textwidth position justification in
                  let mediabox =
                    if cropbox then
                      match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
                      | Some pdfobject -> Pdf.parse_rectangle (Pdf.direct pdf pdfobject)
                      | None -> Pdf.parse_rectangle page.Pdfpage.mediabox
                    else
                      Pdf.parse_rectangle page.Pdfpage.mediabox
                  in
                    let x, y, rotate = calculate_position false textwidth mediabox orientation position in
                      let hoffset, voffset =
                        if position = Diagonal || position = ReverseDiagonal
                          then -. (cos ((pi /. 2.) -. rotate) *. voffset), sin ((pi /. 2.) -. rotate) *. voffset
                          else hoffset, voffset 
                      in
                        match font with
                        | Some f ->
                            ops longest_w metrics x y rotate (hoffset +. joffset) voffset outline linewidth
                            unique_fontname unique_extgstatename colour fontsize text
                        | None ->
                            ops longest_w metrics x y rotate (hoffset +. joffset) voffset outline linewidth
                            fontname None colour fontsize text 
          in
            let newresources =
              match font with
              | Some _ ->
                  let newfontdict =
                    Pdf.add_dict_entry fontdict unique_fontname (make_font fontname)
                  in
                    Pdf.add_dict_entry resources' "/Font" newfontdict
              | None -> page.Pdfpage.resources
          in
            let page = {page with Pdfpage.resources = newresources} in
              if underneath
                then Pdfpage.prepend_operators pdf ops ~fast:fast page
                else Pdfpage.postpend_operators pdf ops ~fast:fast page
  in
    if metrics then
      (ignore (iter_pages (fun a b -> ignore (addtext_page a b)) pdf pages); pdf)
    else
      process_pages addtext_page pdf pages

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
  | '\\'::('0'..'9' as a)::('0'..'9' as b)::('0'..'9' as c)::t ->
       let chr = char_of_int (int_of_string ("0o" ^ implode [a;b;c])) in
         unescape_chars (chr::prev) t
  | '\\'::'\\'::t -> unescape_chars ('\\'::prev) t
  | '\\'::c::t when c <> 'n' -> unescape_chars (c::prev) t
  | h::t -> unescape_chars (h::prev) t

let unescape_string s =
  implode (unescape_chars [] (explode s))

let
  addtexts metrics linewidth outline fast fontname font bates colour position linespacing
  fontsize underneath text pages orientation cropbox opacity justification midline filename pdf
=
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
  let text = winansi_of_utf8 text in
    let lines = map unescape_string (split_at_newline text) in
      let pdf = ref pdf in
        let voffset =
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
            ops_baseline_adjustment := 0.;
          iter
            (fun line ->
               let voff, hoff =
                 if orientation = Vertical then 0., -.(!voffset) else !voffset, 0.
               in
                 pdf :=
                   addtext metrics lines linewidth outline fast colour fontname bates fontsize font
                   underneath position hoff voff line pages orientation cropbox opacity justification filename !pdf;
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
        process_pages removetext_page pdf range

(* \section{Padding with blank pages.} *)
let insert_after pos page pages =
  let before, after = cleave pages pos in
    before @ [page] @ after

(* Insert many. *)
let rec insert_after_many pages = function
  | [] -> pages
  | (pos, page)::more ->
      let pages' = insert_after pos page pages in
        insert_after_many pages' (map (fun (p, pa) -> p + 1, pa) more)

let pad range pdf i =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let blankpages =
      map
        (fun n ->
           {Pdfpage.content = [];
            Pdfpage.mediabox = (select (n + i) pages).Pdfpage.mediabox;
            Pdfpage.resources = Pdf.Dictionary [];
            Pdfpage.rotate = (select (n + i) pages).Pdfpage.rotate;
            Pdfpage.rest = (select (n + i) pages).Pdfpage.rest})
        range
    in
      let pages' = insert_after_many pages (combine range blankpages) in
        Pdfpage.change_pages true pdf pages'

let padafter range pdf =
  let isinpdf n = mem n (ilist 1 (Pdfpage.endpage pdf)) in
    if not (fold_left ( && ) true (map isinpdf range)) then
      raise (Failure "padafter: range contains pages not present in pdf");
    pad range pdf 0

let padbefore range pdf =
  let isinpdf n = mem n (ilist 1 (Pdfpage.endpage pdf)) in
    if not (fold_left ( && ) true (map isinpdf range)) then
      raise (Failure "padbefore: range contains pages not present in pdf");
    pad (map pred range) pdf 1

let padmultiple n pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let len = length pages in
      let pages_to_add = if len / n * n = len then 0 else n - (len mod n) in
        if pages_to_add > 0 then
          let blankpages =
            many
              {Pdfpage.content = [];
               Pdfpage.mediabox = (select len pages).Pdfpage.mediabox;
               Pdfpage.resources = Pdf.Dictionary [];
               Pdfpage.rotate = (select len pages).Pdfpage.rotate;
               Pdfpage.rest = (select len pages).Pdfpage.rest}
              pages_to_add
          in
            Pdfpage.change_pages true pdf (pages @ blankpages)
        else
          pdf

(* \section{Shift page data} *)
let make_mediabox (xmin, ymin, xmax, ymax) =
  Pdf.Array
    [Pdf.Real xmin; Pdf.Real ymin; Pdf.Real xmax; Pdf.Real ymax]

(* Change the media box and other known boxes by the function [f] which takes
xmin, xmax, ymin, ymax as input. *)
let change_boxes f pdf page =
  let names = ["/TrimBox"; "/ArtBox"; "/CropBox"; "/BleedBox"]
  in let getbox n =
    Pdf.lookup_direct pdf n page.Pdfpage.rest
  in
    let boxes = combine names (map getbox names) in
      let toreplace = lose (function (_, None) -> true | _ -> false) boxes in
        let toreplace =
          map
            (function (name, Some value) -> (name, value) | _ -> assert false)
            toreplace
        in
          let rest' =
            fold_left
              (fun e (k, v) ->
                 let v =
                   make_mediabox (f (Pdf.parse_rectangle v))
                 in
                   Pdf.replace_dict_entry e k v)
              page.Pdfpage.rest
              toreplace
          in
            {page with
               Pdfpage.mediabox =
                 make_mediabox (f (Pdf.parse_rectangle page.Pdfpage.mediabox));
               Pdfpage.rest = rest'}

(* The content is flipped by altering any use of [Op_cm]. But we must also
alter any /Matrix entries in pattern dictionaries for tiled and shading
patterns. In addition, shadings used by Op_sh in the main page content and in
xobjects must be altered. *)
(*let rec change_shadings pdf tr resources =
  let transform_shading s =
    s
  in
    try
      let resources =
        begin match Pdf.lookup_direct pdf "/Shading" resources with
        | Some (Pdf.Dictionary shadings) ->
            let names, nums =
              split
                (map
                   (fun (name, shading) ->
                      Printf.printf "Fixing up shading %s\n" name;
                      name, Pdf.addobj pdf (transform_shading shading))
                   shadings)
            in
              let entries =
                map2 (fun name num -> name, Pdf.Indirect num) names nums
              in
                Pdf.add_dict_entry resources "/Shading" (Pdf.Dictionary entries)
        | _ -> resources
        end
      in
        let process_xobject xobject =
          change_shadings pdf tr xobject
        in
          begin match Pdf.lookup_direct pdf "/XObject" resources with
          | Some (Pdf.Dictionary xobjects) ->
              let names, nums =
                split
                  (map
                     (fun (name, xobject) ->
                        Printf.printf "Looking for shadings in xobject %s\n" name;
                        name, Pdf.addobj pdf (process_xobject xobject))
                     xobjects)
              in
                let entries =
                  map2 (fun name num -> name, Pdf.Indirect num) names nums
                in
                  Pdf.add_dict_entry resources "/XObject" (Pdf.Dictionary entries)
          | _ -> resources
          end
    with
      _ -> resources*)

let change_pattern_matrices pdf tr resources =
  try
    (*let resources = change_shadings pdf tr resources in*)
      begin match Pdf.lookup_direct pdf "/Pattern" resources with
      | Some (Pdf.Dictionary patterns) ->
          let names, nums =
            split
              (map
                (fun (name, p) ->
                  let old_pattern = Pdf.direct pdf p in
                    let new_pattern =
                      let existing_tr = Pdf.parse_matrix pdf "/Matrix" old_pattern in
                        let new_tr = Pdftransform.matrix_compose (Pdftransform.matrix_invert tr) existing_tr in
                          Pdf.add_dict_entry old_pattern "/Matrix" (Pdf.make_matrix new_tr)
                    in
                      name, Pdf.addobj pdf new_pattern)
                patterns)
          in
            let entries =
              map2 (fun name num -> name, Pdf.Indirect num) names nums
            in
              Pdf.add_dict_entry resources "/Pattern" (Pdf.Dictionary entries)
      | _ -> resources
    end
  with
    Pdftransform.NonInvertable -> resources

let shift_page ?(fast=false) dx dy pdf _ page =
  let transform_op =
    Pdfops.Op_cm (Pdftransform.matrix_of_op (Pdftransform.Translate (dx, dy)))
  in
    let resources' =
      change_pattern_matrices pdf (Pdftransform.mktranslate ~-.dx ~-.dy) page.Pdfpage.resources
    in
      Pdfpage.prepend_operators pdf [transform_op] ~fast {page with Pdfpage.resources = resources'}

let shift_pdf ?(fast=false) dx dy pdf range =
  process_pages (shift_page ~fast dx dy pdf) pdf range

(* Change a page's media box so its minimum x and y are 0, making other
operations simpler to think about. Any shift that is done is reflected in
other boxes (clip etc.) *)
let rectify_boxes ?(fast=false) pdf page =
  let minx, miny, _, _ =
    Pdf.parse_rectangle page.Pdfpage.mediabox
  in
    let f (iminx, iminy, imaxx, imaxy) =
      iminx -. minx, iminy -. miny, imaxx -. minx, imaxy -. miny
    in
      let page = change_boxes f pdf page in
        if minx <> 0. || miny <> 0.
          then shift_page ~fast (-.minx) (-.miny) pdf 0 page
          else page

(* \section{Flip pages} *)
let flip_page ?(fast=false) transform_op pdf _ page =
  let minx, miny, maxx, maxy =
    Pdf.parse_rectangle page.Pdfpage.mediabox
  in
    let tr = transform_op minx miny maxx maxy in
      let resources =
        change_pattern_matrices pdf tr page.Pdfpage.resources
      in
        Pdfpage.prepend_operators pdf [Pdfops.Op_cm tr] ~fast {page with Pdfpage.resources = resources}

let vflip_pdf ?(fast=false) pdf range =
  let transform_op _ miny _ maxy =
    Pdftransform.matrix_of_op
      (Pdftransform.Scale ((0., ((miny +. maxy) /. 2.)), 1., -.1.))
  in
    process_pages (flip_page ~fast transform_op pdf) pdf range

let hflip_pdf ?(fast=false) pdf range =
  let transform_op minx _ maxx _ =
    Pdftransform.matrix_of_op
      (Pdftransform.Scale (((minx +. maxx) /. 2., 0.), -.1., 1.))
  in
    process_pages (flip_page ~fast transform_op pdf) pdf range

(* \section{Set media box} *)
let set_mediabox x y w h pdf range =
  let crop_page _ page =
    {page with
       Pdfpage.mediabox =
        (Pdf.Array
           [Pdf.Real x; Pdf.Real y;
            Pdf.Real (x +.  w); Pdf.Real (y +. h)])}
  in
    process_pages crop_page pdf range

let setBox box minx maxx miny maxy pdf range =
  let set_box_page _ page =
    {page with
       Pdfpage.rest =
         Pdf.add_dict_entry
           page.Pdfpage.rest box
           (Pdf.Array [Pdf.Real minx; Pdf.Real miny; Pdf.Real maxx; Pdf.Real maxy])}
  in
    process_pages set_box_page pdf range

(* \section{Cropping} *)
let crop_pdf x y w h pdf range =
  let crop_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.add_dict_entry
            page.Pdfpage.rest
            "/CropBox"
            (Pdf.Array
               [Pdf.Real x; Pdf.Real y;
                Pdf.Real (x +.  w); Pdf.Real (y +. h)]))}
  in
    process_pages crop_page pdf range

let remove_cropping_pdf pdf range =
  let remove_cropping_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/CropBox")}
  in
    process_pages remove_cropping_page pdf range

let remove_trim_pdf pdf range =
  let remove_trim_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/TrimBox")}
  in
    process_pages remove_trim_page pdf range

let remove_art_pdf pdf range =
  let remove_art_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/ArtBox")}
  in
    process_pages remove_art_page pdf range

let remove_bleed_pdf pdf range =
  let remove_bleed_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/BleedBox")}
  in
    process_pages remove_bleed_page pdf range

(* \section{Rotating pages} *)
let rotate_pdf r pdf range =
  let rotate_page _ page =
    {page with Pdfpage.rotate =
       Pdfpage.rotation_of_int r}
  in
    process_pages rotate_page pdf range

let rotate_pdf_by r pdf range =
  let rotate_page_by _ page =
    {page with Pdfpage.rotate =
       Pdfpage.rotation_of_int ((Pdfpage.int_of_rotation page.Pdfpage.rotate + r) mod 360)}
  in
    process_pages rotate_page_by pdf range

let rotate_page_contents ~fast rotpoint r pdf _ page =
  let rotation_point =
    match rotpoint with
    | None ->
        let minx, miny, maxx, maxy = Pdf.parse_rectangle page.Pdfpage.mediabox in
          (minx +. maxx) /. 2.,  (miny +. maxy) /. 2.
    | Some point -> point
  in
    let tr =
      Pdftransform.matrix_of_op
        (Pdftransform.Rotate (rotation_point, -.(rad_of_deg r)))
    in let tr2 =
      Pdftransform.matrix_of_op
        (Pdftransform.Rotate (rotation_point, rad_of_deg r))
    in    
      let transform_op = Pdfops.Op_cm tr in
        let resources' = change_pattern_matrices pdf tr2 page.Pdfpage.resources in
          Pdfpage.prepend_operators pdf [transform_op] ~fast {page with Pdfpage.resources = resources'}

let rotate_contents ?(fast=false) r pdf range =
  process_pages (rotate_page_contents ~fast None r pdf) pdf range

(* Return the pages from the pdf in the range, unordered. *)
let select_pages range pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    option_map (function n -> try Some (select n pages) with _ -> None) range


(* Upright functionality *)

(* If all pages are already upright, do nothing to save time. *)
let allupright range pdf =
  let page_is_upright page =
    page.Pdfpage.rotate = Pdfpage.Rotate0
  in
    not (mem false (map page_is_upright (select_pages range pdf)))

let upright_transform page =
  let rotate =
    Pdfpage.int_of_rotation page.Pdfpage.rotate
  and cx, cy =
    let minx, miny, maxx, maxy = Pdf.parse_rectangle page.Pdfpage.mediabox in
      (minx +. maxx) /. 2., (miny +. maxy) /. 2.
  in
    Pdftransform.mkrotate (cx, cy) (rad_of_deg (~-.(float rotate)))

let transform_boxes tr pdf page =
  let f (minx, miny, maxx, maxy) =
    let minx, miny = Pdftransform.transform_matrix tr (minx, miny)
    and maxx, maxy = Pdftransform.transform_matrix tr (maxx, maxy) in
      (minx, miny, maxx, maxy)
  in
    change_boxes f pdf page

let transform_contents ?(fast=false) tr pdf page =
  let transform_op = Pdfops.Op_cm tr in
    let resources' = change_pattern_matrices pdf (Pdftransform.matrix_invert tr) page.Pdfpage.resources in
      Pdfpage.prepend_operators pdf [transform_op] ~fast {page with Pdfpage.resources = resources'}

let upright ?(fast=false) range pdf =
  if allupright range pdf then pdf else
    let upright_page _ _ page =
      let tr = upright_transform page in
        let page = transform_boxes tr pdf page in
          let page = transform_contents ~fast tr pdf page in
            rectify_boxes ~fast pdf {page with Pdfpage.rotate = Pdfpage.Rotate0}
    in
      process_pages (upright_page pdf) pdf range

(* \section{Scale page data} *)
let scale_pdf ?(fast=false) sx sy pdf range =
  let scale_page _ page =
    let f (xmin, ymin, xmax, ymax) =
      xmin *. sx, ymin *. sy, xmax *. sx, ymax *. sy
    in
      let page = change_boxes f pdf page
      and matrix = Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), sx, sy)) in
        let transform_op =
          Pdfops.Op_cm matrix
        and resources' =
          change_pattern_matrices pdf (Pdftransform.matrix_invert matrix) page.Pdfpage.resources
        in
         Pdfpage.prepend_operators pdf ~fast [transform_op] {page with Pdfpage.resources = resources'} 
    in
      process_pages scale_page pdf range

(* Scale to fit page of size x * y *)
(* FIXME: Can we do this in terms of scale_contents - and then just fix up the boxes? For 1.8 *)
let scale_to_fit_pdf ?(fast=false) input_scale x y op pdf range =
  let scale_page_to_fit _ page =
    let matrix =
      let (minx, miny, maxx, maxy) =
        (* Use cropbox if available *)
        Pdf.parse_rectangle
          (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
          | Some r -> r
          | None -> page.Pdfpage.mediabox)
      in
        if maxx <= 0. || maxy <= 0. then failwith "Zero-sized pages are invalid" else
          let fx = x /. maxx in let fy = y /. maxy in
            let scale = fmin fx fy *. input_scale in
              let trans_x = (x -. (maxx *. scale)) /. 2.
              in let trans_y = (y -. (maxy *. scale)) /. 2. in
                (Pdftransform.matrix_of_transform
                   [Pdftransform.Translate (trans_x, trans_y);
                    Pdftransform.Scale ((0., 0.), scale, scale)])
    in
      let page =
        change_boxes
          (function (minx, miny, maxx, maxy) -> 0., 0., x, y (* FIXME: scale boxes properly *))
          pdf page
      in
        Pdfpage.prepend_operators pdf [Pdfops.Op_cm matrix] ~fast
        {page with Pdfpage.resources = change_pattern_matrices pdf (Pdftransform.matrix_invert matrix) page.Pdfpage.resources}
  in
    process_pages scale_page_to_fit pdf range

(* Scale contents *)
let scale_page_contents ?(fast=false) scale position pdf _ page =
  let (minx, miny, maxx, maxy) as box =
    (* Use cropbox if available *)
    Pdf.parse_rectangle
      (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
       | Some r -> r
       | None -> page.Pdfpage.mediabox)
  in
    let sx, sy, _ = calculate_position true 0. box Horizontal position in
      let tx, ty =
        match position with
        | Top t -> 0., -.t
        | TopLeft t -> t, -.t
        | TopRight t -> 0., -.t
        | Left t -> t, 0.
        | BottomLeft t -> t, t
        | Bottom t -> 0., t
        | BottomRight t -> -.t, -.t
        | Right t -> -.t, 0.
        | _ -> 0., 0. (* centre it... FIXME: We will add a center position, eventually, for text and this... *)
    in
      let transform =
        Pdftransform.matrix_of_transform
          [Pdftransform.Translate (tx, ty);
           Pdftransform.Scale ((sx, sy), scale, scale)]
      in
        let transform_op = Pdfops.Op_cm transform in
          let resources' = change_pattern_matrices pdf transform page.Pdfpage.resources in
            Pdfpage.prepend_operators pdf [transform_op] ~fast {page with Pdfpage.resources = resources'}

let scale_contents ?(fast=false) position scale pdf range =
  process_pages (scale_page_contents ~fast scale position pdf) pdf range

(* \section{List annotations} *)
let get_annotation_string encoding pdf annot =
  match Pdf.lookup_direct pdf "/Contents" annot with
  | Some (Pdf.String s) -> encode_output encoding s
  | _ -> ""

let print_annotation encoding pdf num s =
  let s = get_annotation_string encoding pdf s in
  match s with
  | "" -> ()
  | s ->
    flprint (Printf.sprintf "Page %d: " num);
    flprint s;
    flprint "\n"

let list_page_annotations encoding pdf num page =
  match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
  | Some (Pdf.Array annots) ->
      iter (print_annotation encoding pdf num) (map (Pdf.direct pdf) annots)
  | _ -> ()

let list_annotations encoding pdf =
  let range = parse_pagespec pdf "all" in
  iter_pages (list_page_annotations encoding pdf) pdf range

let get_annotations encoding pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    flatten
      (map2
       (fun page pagenumber ->
         match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
         | Some (Pdf.Array annots) ->
             let strings =
               map (get_annotation_string encoding pdf) (map (Pdf.direct pdf) annots)
             in
               combine (many pagenumber (length strings)) strings
         | _ -> [])
        pages
        (ilist 1 (length pages))) 

let list_annotations_more pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    iter2
      (fun page pagenumber ->
         iter
           (function annot ->
              let print_annot annot =
                let annot_type_string =
                  match annot.Pdfannot.subtype with
                  | Pdfannot.Stamp -> "Stamp"
                  | Pdfannot.Text -> "Text"
                  | Pdfannot.Link -> "Link"
                  | Pdfannot.FreeText -> "FreeText"
                  | Pdfannot.Line -> "Line"
                  | Pdfannot.Square -> "Square"
                  | Pdfannot.Circle -> "Circle"
                  | Pdfannot.Polygon -> "Polygon"
                  | Pdfannot.PolyLine -> "PolyLine"
                  | Pdfannot.Highlight -> "Highlight"
                  | Pdfannot.Underline -> "Underline"
                  | Pdfannot.Squiggly -> "Squiggly"
                  | Pdfannot.StrikeOut -> "StrikeOut"
                  | Pdfannot.Caret -> "Caret"
                  | Pdfannot.Ink -> "Ink"
                  | Pdfannot.Popup _ -> "Popup"
                  | Pdfannot.FileAttachment -> "FileAttachment"
                  | Pdfannot.Sound -> "Sound"
                  | Pdfannot.Movie -> "Movie"
                  | Pdfannot.Widget -> "Widget"
                  | Pdfannot.Screen -> "Screen"
                  | Pdfannot.PrinterMark -> "PrinterMark"
                  | Pdfannot.TrapNet -> "TrapNet"
                  | Pdfannot.Watermark -> "Watermark"
                  | Pdfannot.ThreeDee -> "ThreeDee"
                  | Pdfannot.Unknown -> "Unknown"
                in let subject =
                  match annot.Pdfannot.subject with
                  | Some s -> s
                  | None -> ""
                in let contents =
                  match annot.Pdfannot.annot_contents with
                  | Some s -> s
                  | None -> ""
                in
                  Printf.printf "Page: %i\n" pagenumber;
                  Printf.printf "Subtype: %s\n" annot_type_string;
                  Printf.printf "Subject: %s\n" subject;
                  Printf.printf "Contents: %s\n" contents;
              in
                match annot.Pdfannot.subtype with
                | Pdfannot.Popup annot -> print_annot annot
                | _ -> print_annot annot
           )
           (Pdfannot.annotations_of_page pdf page))
      pages
      (ilist 1 (length pages));
      flprint "" (* flush *)

(* Equalise the page lengths of two PDFs by chopping or extending the first one.
*)
let equalise_lengths a b =
  let a' =
    if Pdfpage.endpage a < Pdfpage.endpage b then
      Pdfpage.change_pages true a
        (Pdfpage.pages_of_pagetree a @
           many (Pdfpage.blankpage Pdfpaper.a4) (Pdfpage.endpage b - Pdfpage.endpage a))
    else if Pdfpage.endpage a > Pdfpage.endpage b then
      Pdfpage.change_pages true a
        (take (Pdfpage.pages_of_pagetree a) (Pdfpage.endpage b))
    else a 
  in
     a', b

(* \section{Copy annotations} *)
let copy_annotations range frompdf topdf =
  let frompdf, topdf = equalise_lengths frompdf topdf in
    let copy_annotations_page topdf frompdf frompage topage =
      match Pdf.lookup_direct frompdf "/Annots" frompage.Pdfpage.rest with
      | Some ((Pdf.Array frompage_annots) as annots) ->
          let objects_to_copy = Pdf.objects_referenced [] [] frompdf annots in
            iter
              (fun n ->
                 ignore (Pdf.addobj_given_num topdf (n, Pdf.lookup_obj frompdf n)))
              objects_to_copy;
            let topage_annots =
              match Pdf.lookup_direct frompdf "/Annots" topage.Pdfpage.rest with
              | Some (Pdf.Array annots) -> annots
              | _ -> []
            in
              let merged_dict = Pdf.Array (frompage_annots @ topage_annots) in
                let topage' =
                  {topage with Pdfpage.rest =
                     Pdf.add_dict_entry topage.Pdfpage.rest "/Annots" merged_dict}
                in
                  topdf, topage'
      | Some x -> topdf, topage
      | None -> topdf, topage
    in
      match Pdf.renumber_pdfs [frompdf; topdf] with 
      | [frompdf; topdf] ->
          let frompdf_pages = Pdfpage.pages_of_pagetree frompdf
          in let topdf_pages = Pdfpage.pages_of_pagetree topdf in
            let pdf = ref topdf
            and pages = ref []
            and pnum = ref 1
            and frompdf_pages = ref frompdf_pages
            and topdf_pages = ref topdf_pages in
              (* Go through, updating pdf and collecting new pages. *)
              while not (isnull !frompdf_pages) do
                let frompdf_page = hd !frompdf_pages
                and topdf_page = hd !topdf_pages in
                  let pdf', page =
                    if mem !pnum range
                      then copy_annotations_page !pdf frompdf frompdf_page topdf_page
                      else !pdf, topdf_page
                  in
                    pdf := pdf';
                    pages =| page;
                    incr pnum;
                    frompdf_pages := tl !frompdf_pages;
                    topdf_pages := tl !topdf_pages
              done;
              Pdfpage.change_pages true !pdf (rev !pages)
      | _ -> assert false

(* \section{N-up} *)

(* Given a number to fit and a mediabox, return a list of transforms for the
2 pages. FIXME: Assumes mediabox (0, 0)-based. Check this for all operations for 1.8. *)
let twoup_transforms mediabox =
  let width, height =
    match Pdf.parse_rectangle mediabox with
      xmin, ymin, xmax, ymax -> xmax -. xmin, ymax -. ymin
  in
    let width_exceeds_height = width > height in
      let rotate = Pdftransform.Rotate ((0., 0.), rad_of_deg 90.) 
      in let sc =
        if width_exceeds_height
          then fmin (height /. width) ((width /. 2.) /. height)
          else fmin (width /. height) ((height /. 2.) /. width)
      in
        let scale = Pdftransform.Scale ((0., 0.), sc, sc) in
          let tr0, tr1 =
            if width_exceeds_height then
              Pdftransform.Translate (height *. sc, 0.),
              Pdftransform.Translate (height *. sc *. 2., 0.)
            else
              Pdftransform.Translate (height *. sc, 0.),
              Pdftransform.Translate (height *. sc, width *. sc)
          in
            let t0 = Pdftransform.matrix_of_transform [tr0; rotate; scale]
            in let t1 = Pdftransform.matrix_of_transform [tr1; rotate; scale] in
              [t0; t1]

(* Combine two pages into one throughout the document. The pages have already
had their objects renumbered so as not to clash.*)
let twoup_pages pdf = function
  | [] -> assert false
  | (h::_) as pages ->
     let resources' =
       pair_reduce
         (combine_pdf_resources pdf)
         (map (fun p -> p.Pdfpage.resources) pages)
     in
       let content' =
          let transform_stream contents transform =
            let ops = Pdfops.parse_operators pdf resources' contents in
              (* Need protect_removeme here? especially new, Q-adding protect? *)
              Pdfops.stream_of_ops
                ([Pdfops.Op_q] @ [Pdfops.Op_cm transform] @ ops @ [Pdfops.Op_Q])
          in
            map2
              (fun p -> transform_stream p.Pdfpage.content)
              pages
              (take (twoup_transforms h.Pdfpage.mediabox) (length pages))
       in
         {Pdfpage.mediabox = h.Pdfpage.mediabox;
          Pdfpage.rotate = h.Pdfpage.rotate;
          Pdfpage.content = content';
          Pdfpage.resources = resources';
          Pdfpage.rest = h.Pdfpage.rest}

(* Main function *)
let twoup pdf =
  let pdf = upright (ilist 1 (Pdfpage.endpage pdf)) pdf in
    let pages = Pdfpage.pages_of_pagetree pdf in
      let pagesets = splitinto 2 pages in
        let renumbered = map (Pdfpage.renumber_pages pdf) pagesets in
          let pages' = map (twoup_pages pdf) renumbered in
            Pdfpage.change_pages true pdf pages'

let twoup_stack_transforms mediabox =
  let width, height =
    match Pdf.parse_rectangle mediabox with
      xmin, ymin, xmax, ymax -> xmax -. xmin, ymax -. ymin
  in
    let rotate = Pdftransform.Rotate ((0., 0.), rad_of_deg 90.) 
    in let tr0 = Pdftransform.Translate (height, 0.)
    in let tr1 = Pdftransform.Translate (height, width) in
      let t0 = Pdftransform.matrix_of_transform [tr0; rotate]
      in let t1 = Pdftransform.matrix_of_transform [tr1; rotate] in
        [t0; t1]

let twoup_pages_stack pdf = function
  | [] -> assert false
  | (h::_) as pages ->
     let resources =
       pair_reduce
         (combine_pdf_resources pdf)
         (map (fun p -> p.Pdfpage.resources) pages)
     in
       (* Remove any CropBox *)
       let rest =
         Pdf.remove_dict_entry h.Pdfpage.rest "/CropBox"
       in
       let content' =
          let transform_stream contents transform =
            let ops = Pdfops.parse_operators pdf resources contents in
              Pdfops.stream_of_ops
                ([Pdfops.Op_q] @ [Pdfops.Op_cm transform] @ ops @ [Pdfops.Op_Q])
          in
            map2
              (fun p -> transform_stream p.Pdfpage.content)
              pages
              (take (twoup_stack_transforms h.Pdfpage.mediabox) (length pages))
       in
         {Pdfpage.mediabox =
            (let width, height =
              match Pdf.parse_rectangle h.Pdfpage.mediabox with
                xmin, ymin, xmax, ymax -> xmax -. xmin, ymax -. ymin
            in
              Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real height; Pdf.Real (width *. 2.)]);
          Pdfpage.rotate = h.Pdfpage.rotate;
          Pdfpage.content = content';
          Pdfpage.resources = resources;
          Pdfpage.rest = rest}

let twoup_stack pdf =
  let pdf = upright (ilist 1 (Pdfpage.endpage pdf)) pdf in
    let pages = Pdfpage.pages_of_pagetree pdf in
      let pagesets = splitinto 2 pages in
        let renumbered = map (Pdfpage.renumber_pages pdf) pagesets in
          let pages' = map (twoup_pages_stack pdf) renumbered in
            Pdfpage.change_pages true pdf pages'

(* \section{Output info} *)
let get_info raw pdf =
  let infodict =
    match Pdf.lookup_direct pdf "/Info" pdf.Pdf.trailerdict with
    | Some infodict -> infodict
    | _ -> Pdf.Dictionary []
  in
    let getstring name =
      match Pdf.lookup_direct pdf name infodict with
      | Some (Pdf.String s) ->
          if raw then s else crude_de_unicode s
      | _ -> ""
    in
      getstring
       
let get_info_utf8 pdf =
  let infodict =
    match Pdf.lookup_direct pdf "/Info" pdf.Pdf.trailerdict with
    | Some infodict -> infodict
    | _ -> Pdf.Dictionary []
  in
    (function name ->
      match Pdf.lookup_direct pdf name infodict with
      | Some (Pdf.String s) -> Pdftext.utf8_of_pdfdocstring s
      | _ -> "")

let output_xml_info pdf =
  match get_metadata pdf with
    None -> ()
  | Some metadata ->
      print_string (string_of_bytes metadata)
      (*let parsed = Xml.parse_string (string_of_bytes metadata) in
        print_string (Xml.to_string parsed)*)

let output_info encoding pdf =
  let getstring =
    match encoding with
    | Raw -> get_info true pdf
    | Stripped -> get_info false pdf
    | UTF8 -> get_info_utf8 pdf
  in
    Printf.printf "Version: %i.%i\n" pdf.Pdf.major pdf.Pdf.minor;
    Printf.printf "Pages: %i\n" (Pdfpage.endpage pdf);
    Printf.printf "Title: %s\n" (getstring "/Title");
    Printf.printf "Author: %s\n" (getstring "/Author");
    Printf.printf "Subject: %s\n" (getstring "/Subject");
    Printf.printf "Keywords: %s\n" (getstring "/Keywords");
    Printf.printf "Creator: %s\n" (getstring "/Creator");
    Printf.printf "Producer: %s\n" (getstring "/Producer");
    Printf.printf "Created: %s\n" (getstring "/CreationDate");
    Printf.printf "Modified: %s\n" (getstring "/ModDate");
    output_xml_info pdf

(* \section{Blacken text} *)

(*
   \begin{verbatim}
    Algorithm: Change
     BT
     <ops>
    ET
 
    ...to...
 
    BT
    Op_g 0.
    <ops minus any color, shading or gs operators>
    ET
    <ops minus any text positioning or text rendering ones>
    \end{verbatim}
*)
let blacktext_ops pdf resources content =
  let not_text = function
    | Pdfops.Op_Tj _ | Pdfops.Op_TJ _
    | Pdfops.Op_' _ | Pdfops.Op_'' (_, _, _)
    | Pdfops.Op_Td (_, _) | Pdfops.Op_TD (_, _)
    | Pdfops.Op_Tm _ | Pdfops.Op_T'
    | Pdfops.Op_Tc _
    | Pdfops.Op_Tw _
    | Pdfops.Op_Tz _
    | Pdfops.Op_TL _
    | Pdfops.Op_Tf (_, _)
    | Pdfops.Op_Tr _
    | Pdfops.Op_Ts _ -> false
    | _ -> true
  in let textlevel = ref 0
  in let removed = ref []
  in let operators =
    Pdfops.parse_operators pdf resources content
  in
    let rec remove_colourops prev = function
      | [] -> rev prev
      | Pdfops.Op_BT::more ->
          incr textlevel;
          remove_colourops
            (Pdfops.Op_g 0.::Pdfops.Op_BT::prev)
            more
      | Pdfops.Op_ET::more ->
          decr textlevel;
          let prev' = !removed @ Pdfops.Op_ET::prev in
            removed := [];
            remove_colourops prev' more
      | (Pdfops.Op_G _
         | Pdfops.Op_g _ 
         | Pdfops.Op_RG (_, _, _)
         | Pdfops.Op_rg (_, _, _)
         | Pdfops.Op_k (_, _, _, _)
         | Pdfops.Op_K (_, _, _, _)
         | Pdfops.Op_SCN _
         | Pdfops.Op_SC _
         | Pdfops.Op_scn _
         | Pdfops.Op_sc _
         | Pdfops.Op_SCNName (_, _)
         | Pdfops.Op_scnName (_, _)
         | Pdfops.Op_CS _
         | Pdfops.Op_cs _
         | Pdfops.Op_sh _
         | Pdfops.Op_gs _)
        as op::more ->
          if !textlevel > 0
            then
              begin
                removed =| op;
                remove_colourops prev more
              end
            else remove_colourops (op::prev) more
      | op::more ->
          if !textlevel > 0 && not_text op then removed =| op;
          remove_colourops (op::prev) more
    in
      let operators' = remove_colourops [] operators in
        [Pdfops.stream_of_ops operators']

(* Blacken a form xobject, writing it to the same object. *)
let process_xobject f pdf resources i =
  let xobj = Pdf.lookup_obj pdf i in
    match Pdf.lookup_direct pdf "/Subtype" xobj with
    | None -> raise (Pdf.PDFError "No /Subtype in Xobject") 
    | Some (Pdf.Name "/Form") ->
        Pdf.getstream xobj;
        begin match xobj with
        | Pdf.Stream ({contents = Pdf.Dictionary dict, Pdf.Got bytes} as rf) ->
            begin match f pdf resources [Pdf.Stream rf] with
            | [Pdf.Stream {contents = (Pdf.Dictionary dict', data)}] ->
                let dict' =
                  Pdf.remove_dict_entry
                    (Pdf.Dictionary (mergedict dict dict'))
                    "/Filter"
                in
                  rf := (dict', data)
            | _ -> assert false
            end
        | _ -> assert false (* getstream would have complained already *)
        end
    | Some _ -> ()


let process_xobjects pdf page f =
  match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
  | Some (Pdf.Dictionary elts) ->
      iter
        (fun (k, v) ->
          match v with
          | Pdf.Indirect i -> process_xobject f pdf page.Pdfpage.resources i
          | _ -> raise (Pdf.PDFError "blacktext"))
        elts
  | _ -> ()

let blacktext range pdf =
  let blacktext_page _ page =
    let content' =
      blacktext_ops pdf page.Pdfpage.resources page.Pdfpage.content
    in
      process_xobjects pdf page blacktext_ops;
      {page with Pdfpage.content = content'}
  in
    process_pages blacktext_page pdf range

(* \section{Blacken lines} *)
let blacklines_ops pdf resources content =
  let rec blacken_strokeops prev = function
    | [] -> rev prev
    | Pdfops.Op_CS _::t ->
        blacken_strokeops (Pdfops.Op_CS "/DeviceGray"::prev) t
    | (Pdfops.Op_SC _ | Pdfops.Op_SCN _ | Pdfops.Op_SCNName _ | Pdfops.Op_G _
       | Pdfops.Op_RG _ | Pdfops.Op_K _)::t ->
           blacken_strokeops (Pdfops.Op_G 0.::prev) t
    | h::t -> blacken_strokeops (h::prev) t
  and operators =
    Pdfops.parse_operators pdf resources content
  in
    let operators' = blacken_strokeops [] operators in
      [Pdfops.stream_of_ops operators']

let blacklines range pdf =
  let blacklines_page _ page =
    let content' =
      blacklines_ops pdf page.Pdfpage.resources page.Pdfpage.content
    in
      process_xobjects pdf page blacklines_ops;
      {page with Pdfpage.content = content'}
  in
    process_pages blacklines_page pdf range

(* \section{Blacken Fills} *)
let blackfills_ops pdf resources content =
  let rec blacken_fillops prev = function
    | [] -> rev prev
    | Pdfops.Op_cs _::t ->
        blacken_fillops (Pdfops.Op_cs "/DeviceGray"::prev) t
    | (Pdfops.Op_sc _ | Pdfops.Op_scn _ | Pdfops.Op_scnName _ | Pdfops.Op_g _
       | Pdfops.Op_rg _ | Pdfops.Op_k _)::t ->
           blacken_fillops (Pdfops.Op_g 0.::prev) t
    | h::t -> blacken_fillops (h::prev) t
  and operators =
    Pdfops.parse_operators pdf resources content
  in
    let operators' = blacken_fillops [] operators in
      [Pdfops.stream_of_ops operators']

let blackfills range pdf =
  let blackfills_page _ page =
    let content' =
      blackfills_ops pdf page.Pdfpage.resources page.Pdfpage.content
    in
      process_xobjects pdf page blackfills_ops;
      {page with Pdfpage.content = content'}
  in
    process_pages blackfills_page pdf range

(* \section{Set a minimum line width to avoid dropout} *)
let thinlines range width pdf =
  let thinpage _ page =
    let operators =
      Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content
    in
      let ctmstack = ref [ref Pdftransform.i_matrix] in
        let scaleof_ctm () =
          try
            match Pdftransform.decompose (!(hd !ctmstack)) with
              (scale, _, _, _, _, _) ->
                 scale
          with
            Failure "hd" -> 1.
        in
          let rec replace_operators prev = function
            | [] -> rev prev
            | (Pdfops.Op_w w)::more ->
              (* Alter width. *)
              let width' = width /. scaleof_ctm () in
                let w' =
                  if w >= width' then Pdfops.Op_w w else Pdfops.Op_w width'
                in
                  replace_operators (w'::prev) more
            | (Pdfops.Op_cm m)::more ->
                (* Update CTM *)
                begin try 
                  let top = hd !ctmstack in
                    top := Pdftransform.matrix_compose !top m
                with
                  Failure "hd" -> error "Malformed file."
                end;
                replace_operators ((Pdfops.Op_cm m)::prev) more
            | Pdfops.Op_q::more ->
                (* Push stack *)
                begin try
                  ctmstack =| ref (!(hd !ctmstack))
                with
                  Failure "hd" -> error "Malformed file"
                end;
                replace_operators (Pdfops.Op_q::prev) more
            | Pdfops.Op_Q::more ->
                (* Pop stack *)
                begin try
                  ctmstack := tl !ctmstack
                with
                  Failure "tl" -> error "Malformed file"
                end;
                replace_operators (Pdfops.Op_Q::prev) more
            | (Pdfops.Op_gs gsname)::more ->
                (* Perhaps insert [Op_w]. *)
                let opw =
                  match Pdf.lookup_direct pdf "/ExtGState" page.Pdfpage.resources with
                  | None -> []
                  | Some ext_state_dict ->
                      match Pdf.lookup_direct pdf gsname ext_state_dict with
                      | None -> []
                      | Some gdict ->
                          match Pdf.lookup_direct pdf "/LW" gdict with
                          | Some s -> (try [Pdfops.Op_w (Pdf.getnum s)] with _ -> [])
                          | None -> []
                in
                  replace_operators (opw @ ((Pdfops.Op_gs gsname)::prev)) more
            | x::more -> replace_operators (x::prev) more
          in
            let operators = replace_operators [] operators in
              (* 2. Add an initial 'w' if width more than default width *)
              let operators =
                if width > 1. then (Pdfops.Op_w width)::operators else operators
              in
                let content' = [Pdfops.stream_of_ops operators] in
                  {page with Pdfpage.content = content'} 
  in
    process_pages thinpage pdf range

(* \section{Remove annotations} *)
let remove_annotations range pdf =
  let remove_annotations_page pagenum page =
    if mem pagenum range then
      let rest' =
        Pdf.remove_dict_entry page.Pdfpage.rest "/Annots"
      in
        {page with Pdfpage.rest = rest'}
    else
      page
  in
    process_pages remove_annotations_page pdf range

(* \section{Making draft documents} *)

(* Predicate on an xobject: true if an image xobject. *)
let isimage pdf (_, xobj) =
  match Pdf.lookup_direct pdf "/Subtype" xobj with 
  | Some (Pdf.Name "/Image") -> true
  | _ -> false

(* Given a set of resources for a page, and the name of a resource, determine if
that name refers to an image xobject. *)
let xobject_isimage pdf resources name =
  match resources with
  | Pdf.Dictionary _ ->
      begin match Pdf.lookup_direct pdf "/XObject" resources with
      | Some xobjects ->
          isimage pdf ("", Pdf.lookup_fail "xobject not there" pdf name xobjects)
      | _ -> false
      end
  | _ -> failwith "bad resources"

(* The subsitute for an image. *)
let substitute boxes =
  if boxes then
    rev
      [Pdfops.Op_q;
       Pdfops.Op_w 0.;
       Pdfops.Op_G 0.;
       Pdfops.Op_re (0., 0., 1., 1.);
       Pdfops.Op_m (0., 0.);
       Pdfops.Op_l (1., 1.);
       Pdfops.Op_m (0., 1.);
       Pdfops.Op_l (1., 0.);
       Pdfops.Op_S;
       Pdfops.Op_Q]
  else
    []

(* Remove references to images from a graphics stream. *)
let rec remove_images_stream boxes pdf resources prev = function
  | [] -> rev prev
  | (Pdfops.Op_Do name) as h::t ->
      if xobject_isimage pdf resources name
        then remove_images_stream boxes pdf resources (substitute boxes @ prev) t
        else remove_images_stream boxes pdf resources (h::prev) t
  | Pdfops.InlineImage _::t ->
      remove_images_stream boxes pdf resources (substitute boxes @ prev) t
  | h::t ->
      remove_images_stream boxes pdf resources (h::prev) t

let rec process_form_xobject boxes pdf form =
  let form = Pdf.direct pdf form in
    let page =
      {Pdfpage.content = [form];
       Pdfpage.mediabox = Pdf.Null;
       Pdfpage.resources =
         begin match Pdf.lookup_direct pdf "/Resources" form with
         | Some r -> r
         | None -> Pdf.Dictionary []
         end;
       Pdfpage.rotate = Pdfpage.Rotate0;
       Pdfpage.rest = Pdf.Dictionary []}
    in
      let page', pdf =
        remove_images_page boxes pdf page
      in
        let form' =
          match form with
          | Pdf.Stream {contents = (dict, _)} ->
              begin match 
                Pdfops.stream_of_ops
                  (Pdfops.parse_operators pdf (Pdf.Dictionary []) page'.Pdfpage.content)
              with
              | Pdf.Stream {contents = (_, Pdf.Got data)} ->
                  let dict' =
                    Pdf.add_dict_entry dict "/Length" (Pdf.Integer (bytes_size data))
                  in
                    Pdf.Stream {contents = (dict', Pdf.Got data)}
              | _ -> assert false
              end
          | _ -> raise (Pdf.PDFError "not a stream")
        in
          form', pdf

(* Remove images from a page. *)
and remove_images_page boxes pdf page =
  let isform pdf xobj =
    match Pdf.lookup_direct pdf "/Subtype" xobj with Some (Pdf.Name "/Form") -> true | _ -> false
  in
  (* Remove image xobjects and look into form ones *)
  let form_xobjects =
    match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
    | Some (Pdf.Dictionary elts) ->
        keep (function (_, p) -> isform pdf p) elts
    | _ -> []
  in
  let resources', pdf =
    let names, pointers = split form_xobjects in
      let form_xobjects', pdf =
        let pdf = ref pdf
        in let outputs = ref [] in
          iter
            (fun p ->
              let p', pdf' = process_form_xobject boxes !pdf p in
                pdf := pdf';
                outputs =| p')
            pointers;
          rev !outputs, !pdf
      in
        let nums = ref [] in
          iter
            (fun xobj ->
               let objnum = Pdf.addobj pdf xobj in
                 nums =| objnum)
            form_xobjects';
            let newdict =
              Pdf.Dictionary (combine names (map (fun x -> Pdf.Indirect x) (rev !nums)))
            in
              Pdf.add_dict_entry page.Pdfpage.resources "/XObject" newdict, pdf
    in
      let content' =
        remove_images_stream boxes pdf page.Pdfpage.resources []
           (Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content)
      in
        {page with
          Pdfpage.content =
            (let stream = Pdfops.stream_of_ops content' in
              Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate stream;
              [stream]);
          Pdfpage.resources = resources'}, pdf

(* Remove images from all pages in a document. *)
let draft boxes range pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pagenums = indx pages in
    let pdf = ref pdf
    in let pages' = ref [] in
      iter2 
       (fun p pagenum ->
         let p', pdf' =
           if mem pagenum range
             then remove_images_page boxes !pdf p
             else p, !pdf
         in
           pdf := pdf';
           pages' =| p')
       pages
       pagenums;
      Pdfpage.change_pages true !pdf (rev !pages')

let set_version v pdf =
  pdf.Pdf.minor <- v
        
(* Custom Code: CSP1 - four up duplication. Alter media box and crop-box. 4-up the data. *)
let custom_csp1_page pdf _ page =
  let minx, miny, maxx, maxy =
    match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
    | Some r -> Pdf.parse_rectangle r
    | None -> Pdf.parse_rectangle page.Pdfpage.mediabox
  in
    let mx0 = -.minx
    in let my0 = -.miny
    in let dx = maxx -. minx
    in let dy = maxy -. miny in
      let content =
        let ops = 
          Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content
        in
          [Pdfops.Op_q;
           Pdfops.Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (mx0, my0)])] @ ops @ [Pdfops.Op_Q] @
          [Pdfops.Op_q;
           Pdfops.Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (mx0 +. dx, my0 +. dy)])] @ ops @ [Pdfops.Op_Q] @
          [Pdfops.Op_q;
           Pdfops.Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (mx0, my0 +. dy)])] @ ops @ [Pdfops.Op_Q] @
          [Pdfops.Op_q;
           Pdfops.Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (mx0 +. dx, my0)])] @ ops @ [Pdfops.Op_Q]
      in
        let new_mediabox =
          Pdf.Array
            [Pdf.Real 0.;
             Pdf.Real 0.;
             Pdf.Real ((maxx -. minx) *. 2.);
             Pdf.Real ((maxy -. miny) *. 2.)]
        in
          {page with
             Pdfpage.content = [Pdfops.stream_of_ops content];
             Pdfpage.mediabox = new_mediabox;
             Pdfpage.rest = Pdf.add_dict_entry page.Pdfpage.rest "/CropBox" new_mediabox}

let custom_csp1 pdf =
  process_pages (custom_csp1_page pdf) pdf (ilist 1 (Pdfpage.endpage pdf))

let custom_csp2 f pdf =
  let page = hd (Pdfpage.pages_of_pagetree pdf) in
    let m_minx, m_miny, m_maxx, m_maxy =
      match page.Pdfpage.mediabox with
      | Pdf.Array [a; b; c; d] ->
          Pdf.getnum a, Pdf.getnum b, Pdf.getnum c, Pdf.getnum d
      | _ -> 0., 0., 0., 0.
    in
      let c_minx, c_miny, c_maxx, c_maxy =
        match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
        | Some (Pdf.Array [a; b; c; d]) ->
            Pdf.getnum a, Pdf.getnum b, Pdf.getnum c, Pdf.getnum d
        | _ -> m_minx, m_miny, m_maxx, m_maxy
      in
        let x = (c_minx +. c_maxx) /. 2.
        in let y = (c_miny +. c_maxy) /. 2. in
          scale_contents (PosCentre (x, y)) (f /. 100.) pdf (ilist 1 (Pdfpage.endpage pdf))

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

(* Split the given range (which is in order) into multiple contiguous ones. *)
let rec ranges_of_range curr prev = function
  | [] -> begin match curr with [] -> rev prev | _ -> rev (rev curr::prev) end
  | x::xs ->
      match curr with
      | [] -> ranges_of_range [x] prev xs
      | c::cs when x = c + 1 -> ranges_of_range (x::curr) prev xs
      | cs -> ranges_of_range [x] (rev cs::prev) xs

(* Predicate which is true if at least one page range starts at page 1 *)
let page1 labels =
  mem true (map (function l -> l.Pdfpagelabels.startpage = 1) labels)

let add_page_labels pdf style prefix startval range =
  let ranges = map extremes (ranges_of_range [] [] range)
  and labels = Pdfpagelabels.read pdf in
    (*Printf.printf "We have %i existing labels\n" (List.length labels);
    Printf.printf "We have %i ranges to apply these labels to\n" (List.length ranges);*)
    let labels =
      if not (page1 labels) then
        ref
          ({Pdfpagelabels.labelstyle = Pdfpagelabels.DecimalArabic;
            Pdfpagelabels.labelprefix = None;
            Pdfpagelabels.startpage = 1;
            Pdfpagelabels.startvalue = 1}::labels)
      else
        ref labels
    in
      (*Printf.printf "Before adding, we have these labels:\n";
      iter (fun x -> flprint (Pdfpagelabels.string_of_pagelabel x)) !labels;*)
      iter
        (function (s, e) ->
           let label =
             {Pdfpagelabels.labelstyle = style;
              Pdfpagelabels.labelprefix = prefix;
              Pdfpagelabels.startpage = s;
              Pdfpagelabels.startvalue = startval}
           in
             (*Printf.printf "We are adding this label:\n";
             flprint (Pdfpagelabels.string_of_pagelabel label);*)
             labels := Pdfpagelabels.add_label (Pdfpage.endpage pdf) !labels label e)
        ranges;
        (*Printf.printf "After adding, we have these labels:\n";
        iter (fun x -> flprint (Pdfpagelabels.string_of_pagelabel x)) !labels;*)
        Pdfpagelabels.write pdf !labels

