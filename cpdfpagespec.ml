open Pdfutil

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

let is_dimension pdf comparison {Pdfpage.mediabox = box} =
  let minx, miny, maxx, maxy = Pdf.parse_rectangle pdf box in
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
          if is_dimension pdf comparison page then Some pagenum else None)
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

let invert_range endpage r =
  option_map (fun p -> if mem p r then None else Some p) (ilist 1 endpage)

let duplicate_range n r =
  flatten (map (fun x -> many x n) r)

(* e.g [1] -> 1, [iii] -> x, [/]] -> ] etc. *)
let resolve_pagelabels pdf spec =
  let labels =
    let labs = Pdfpagelabels.read pdf in
      map
        (fun pnum -> (begin try Pdfpagelabels.pagelabeltext_of_pagenumber pnum labs with Not_found -> "" end, pnum))
        (ilist 1 (Pdfpage.endpage pdf))
  in
  (*iter (fun (s, l) -> Printf.printf "%s = %i\n" s l) labels;*)
  let rec readuntilclose a t =
    match t with
    | ']'::t -> rev a, t
    | '\\'::('[' | ']' as c)::t -> readuntilclose (c::a) t
    | '['::t -> raise PageSpecBadSyntax
    | x::t -> readuntilclose (x::a) t
    | [] -> rev a, []
  in
  let rec resolve_pagelabels_inner = function
    | '['::t ->
        let pagelabel, rest = readuntilclose [] t in
        let resolved = explode (string_of_int (begin match lookup (implode pagelabel) labels with Some x -> x | None -> 0 end)) in
          resolved @ resolve_pagelabels_inner rest
    | '\\'::('[' | ']' as c)::t -> c::resolve_pagelabels_inner t
    | ']'::t -> raise PageSpecBadSyntax
    | h::t -> h::resolve_pagelabels_inner t
    | [] -> []
  in
    resolve_pagelabels_inner spec

let rec parse_pagespec_inner endpage pdf spec =
  let spec = if spec = "" then "all" else spec in
  let spec = implode (resolve_pagelabels pdf (explode spec)) in
  let spec = space_string spec in
    if endpage < 1 then raise (Pdf.PDFError "This PDF file has no pages and is therefore malformed") else
      let numbers =
        try
          match explode spec with
          | 'N'::'O'::'T'::r ->
              invert_range endpage (parse_pagespec_inner endpage pdf (implode r))
          | x::'D'::'U'::'P'::r ->
              duplicate_range (int_of_string (implode [x])) (parse_pagespec_inner endpage pdf (implode r))
          | x::y::'D'::'U'::'P'::r ->
              duplicate_range (int_of_string (implode [x; y])) (parse_pagespec_inner endpage pdf (implode r))
          | x::y::z::'D'::'U'::'P'::r ->
              duplicate_range (int_of_string (implode [x; y; z])) (parse_pagespec_inner endpage pdf (implode r))
          | x::y::z::a::'D'::'U'::'P'::r ->
              duplicate_range (int_of_string (implode [x; y; z; a])) (parse_pagespec_inner endpage pdf (implode r))
          | x::y::z::a::b::'D'::'U'::'P'::r ->
              duplicate_range (int_of_string (implode [x; y; z; a; b])) (parse_pagespec_inner endpage pdf (implode r))
          | x::y::z::a::b::c::'D'::'U'::'P'::r ->
              duplicate_range (int_of_string (implode [x; y; z; a; b; c])) (parse_pagespec_inner endpage pdf (implode r))
          | _ ->
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

let rec parse_pagespec_without_pdf_inner n spec =
  try
    parse_pagespec_inner n (Pdf.empty ()) spec
  with
    PageSpecUnknownPage _ ->
      if n < 500000
        then parse_pagespec_without_pdf_inner (n * 2) spec
        else raise (Pdf.PDFError "PageSpecUnknownPage")

let parse_pagespec_without_pdf spec =
  parse_pagespec_without_pdf_inner 100 spec

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
