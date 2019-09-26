open Pdfutil

let emptypage = Pdfpage.blankpage Pdfpaper.a4

let error s =
  prerr_string (s ^ "\nUse -help for help.\n");
  flush stderr;
  exit 2

(* Unit conversions to points. *)
let mm x = ((x /. 10.) /. 2.54) *. 72.

let cm x = (x /. 2.54) *. 72.

let inch x = x *. 72.

let points_of_papersize p =
  let unit = Pdfpaper.unit p
  and w = Pdfpaper.width p
  and h = Pdfpaper.height p in
    let c  = Pdfunits.convert 0. unit Pdfunits.PdfPoint in
      c w, c h

let cropbox pdf page =
  match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
  | Some pdfobject -> Pdf.direct pdf pdfobject
  | None -> page.Pdfpage.mediabox

let width box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in maxx -. minx
let height box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in maxy -. miny
let minx box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in minx
let miny box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in miny
let maxx box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in maxx
let maxy box = let minx, miny, maxx, maxy = Pdf.parse_rectangle box in maxy

let find_page_width pdf page = width page.Pdfpage.mediabox
let find_page_height pdf page = height page.Pdfpage.mediabox
let find_page_crop_width pdf page = width (cropbox pdf page)
let find_page_crop_height pdf page = height (cropbox pdf page)
let find_page_minx pdf page = minx page.Pdfpage.mediabox
let find_page_miny pdf page = miny page.Pdfpage.mediabox
let find_page_maxx pdf page = maxx page.Pdfpage.mediabox
let find_page_maxy pdf page = maxy page.Pdfpage.mediabox
let find_page_crop_minx pdf page = minx (cropbox pdf page)
let find_page_crop_miny pdf page = miny (cropbox pdf page)
let find_page_crop_maxx pdf page = maxx (cropbox pdf page)
let find_page_crop_maxy pdf page = maxy (cropbox pdf page)

let find_page_characteristic pdf page = function
  | "PW" -> find_page_width pdf page
  | "PH" -> find_page_height pdf page
  | "CW" -> find_page_crop_width pdf page
  | "CH" -> find_page_crop_height pdf page
  | "PMINX" -> find_page_minx pdf page
  | "PMINY" -> find_page_miny pdf page
  | "PMAXX" -> find_page_maxx pdf page
  | "PMAXY" -> find_page_maxy pdf page
  | "CMINX" -> find_page_crop_minx pdf page
  | "CMINY" -> find_page_crop_miny pdf page
  | "CMAXX" -> find_page_crop_maxx pdf page
  | "CMAXY" -> find_page_crop_maxy pdf page
  | _ -> failwith "find_page_characteristic"

let make_num pdf page unt num =
  let f =
    match num with
    | Pdfgenlex.LexInt i -> float_of_int i
    | Pdfgenlex.LexReal r -> r
    | Pdfgenlex.LexName
      (( "PW" | "PH" | "CW" | "CH" | "PMINX" | "PMINY" | "PMAXX" | "PMAXY"
      | "CMINX" | "CMINY" | "CMAXX" | "CMAXY") as page_characteristic) ->
        (*let r =*)
        find_page_characteristic pdf page page_characteristic
        (*in
          Printf.printf "characteristic %s is %f\n" page_characteristic r;
          r*)
    | _ -> failwith "make_num"
  in
    match unt with
    | Pdfgenlex.LexName "pt" -> f
    | Pdfgenlex.LexName "cm" -> cm f
    | Pdfgenlex.LexName "mm" -> mm f
    | Pdfgenlex.LexName "in" -> inch f
    | _ -> failwith "make_num"

let update_last_number pdf page unt op num = function
  [] -> []
| h::t ->
    let final_num = make_num pdf page unt num in
      let h' =
        match op with
          Pdfgenlex.LexName "add" -> h +. final_num
        | Pdfgenlex.LexName "sub" -> h -. final_num
        | Pdfgenlex.LexName "mul" -> h *. final_num
        | Pdfgenlex.LexName "div" -> h /. final_num
        | _ -> failwith "update_last_number"
      in
        h'::t

let rec parse_units_again pdf page numbers papersize more =
  let w, h = points_of_papersize papersize in
    parse_units pdf page (h::w::numbers) more

and parse_units pdf page numbers = function
  | Pdfgenlex.LexName "a10portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a10 more
  | Pdfgenlex.LexName "a9portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a9 more
  | Pdfgenlex.LexName "a8portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a8 more
  | Pdfgenlex.LexName "a7portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a7 more
  | Pdfgenlex.LexName "a6portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a6 more
  | Pdfgenlex.LexName "a5portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a5 more
  | Pdfgenlex.LexName "a4portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a4 more
  | Pdfgenlex.LexName "a3portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a3 more
  | Pdfgenlex.LexName "a2portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a2 more
  | Pdfgenlex.LexName "a1portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a1 more
  | Pdfgenlex.LexName "a0portrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.a0 more
  | Pdfgenlex.LexName "a10landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a10) more
  | Pdfgenlex.LexName "a9landscape"::more -> 
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a9) more
  | Pdfgenlex.LexName "a8landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a8) more
  | Pdfgenlex.LexName "a7landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a7) more
  | Pdfgenlex.LexName "a6landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a6) more
  | Pdfgenlex.LexName "a5landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a5) more
  | Pdfgenlex.LexName "a4landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a4) more
  | Pdfgenlex.LexName "a3landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a3) more
  | Pdfgenlex.LexName "a2landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a2) more
  | Pdfgenlex.LexName "a1landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a1) more
  | Pdfgenlex.LexName "a0landscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.a0) more
  | Pdfgenlex.LexName "uslegalportrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.uslegal more
  | Pdfgenlex.LexName "usletterportrait"::more ->
      parse_units_again pdf page numbers Pdfpaper.usletter more
  | Pdfgenlex.LexName "uslegallandscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.uslegal) more
  | Pdfgenlex.LexName "usletterlandscape"::more ->
      parse_units_again pdf page numbers (Pdfpaper.landscape Pdfpaper.usletter) more
  | Pdfgenlex.LexInt x::Pdfgenlex.LexName "mm"::more ->
      parse_units pdf page ((mm <| float_of_int x)::numbers) more
  | Pdfgenlex.LexReal x::Pdfgenlex.LexName "mm"::more ->
      parse_units pdf page (mm x::numbers) more
  | Pdfgenlex.LexInt x::Pdfgenlex.LexName "cm"::more ->
      parse_units pdf page ((cm <| float_of_int x)::numbers) more
  | Pdfgenlex.LexReal x::Pdfgenlex.LexName "cm"::more ->
      parse_units pdf page (cm x::numbers) more
  | Pdfgenlex.LexInt x::Pdfgenlex.LexName "in"::more ->
      parse_units pdf page ((inch <| float_of_int x)::numbers) more
  | Pdfgenlex.LexReal x::Pdfgenlex.LexName "in"::more ->
      parse_units pdf page (inch x::numbers) more
  | Pdfgenlex.LexInt x::more ->
      parse_units pdf page (float_of_int x::numbers) more
  | Pdfgenlex.LexReal x::more ->
      parse_units pdf page (x::numbers) more
  | Pdfgenlex.LexName "pt"::more ->
      parse_units pdf page numbers more
  | Pdfgenlex.LexName
      (( "PW" | "PH" | "CW" | "CH" | "PMINX" | "PMINY" | "PMAXX" | "PMAXY"
       | "CMINX" | "CMINY" | "CMAXX" | "CMAXY") as page_characteristic)::more ->
         let r =
           find_page_characteristic pdf page page_characteristic
         in
         (*  Printf.printf "characteristic %s is %f\n" page_characteristic r;*)
           parse_units pdf page (r::numbers) more
  | Pdfgenlex.LexName ("add" | "sub" | "mul" | "div") as op::
    ((Pdfgenlex.LexInt _ | Pdfgenlex.LexReal _ |  Pdfgenlex.LexName
      ( "PW" | "PH" | "CW" | "CH" | "PMINX" | "PMINY" | "PMAXX" | "PMAXY"
       | "CMINX" | "CMINY" | "CMAXX" | "CMAXY")) as num)::
    (Pdfgenlex.LexName ("pt" | "mm" | "cm" | "in") as unt)::more ->
      parse_units pdf page (update_last_number pdf page unt op num numbers) more
  | Pdfgenlex.LexName ("add" | "sub" | "mul" | "div") as op::
    ((Pdfgenlex.LexInt _ | Pdfgenlex.LexReal _ |  Pdfgenlex.LexName
      ( "PW" | "PH" | "CW" | "CH" | "PMINX" | "PMINY" | "PMAXX" | "PMAXY"
       | "CMINX" | "CMINY" | "CMAXX" | "CMAXY")) as num)::more ->
      parse_units pdf page (update_last_number pdf page (Pdfgenlex.LexName "pt") op num numbers) more
  | _ -> rev numbers

let rec space_units_inner = function
  | [] -> []
  | 'm'::'m'::t -> ' '::'m'::'m'::' '::space_units_inner t
  | 'c'::'m'::t -> ' '::'c'::'m'::' '::space_units_inner t
  | 'i'::'n'::t -> ' '::'i'::'n'::' '::space_units_inner t
  | 'p'::'t'::t -> ' '::'p'::'t'::' '::space_units_inner t
  | h::t -> h::space_units_inner t

let space_units s =
  implode (space_units_inner (explode s))

let parse_units_string pdf page s =
  let fs = parse_units pdf page [] (Pdfgenlex.lex_string <| space_units s) in
    (*(List.fold_left (fun x y -> x ^ " " ^ y) "" (List.map string_of_float * fs));*)
    fs

let parse_rectangle pdf s =
  try
    match parse_units_string pdf emptypage s with
    | [x; y; w; h] -> x, y, w, h
    | _ -> error ("Bad rectangle specification " ^ s)
  with
    _ -> error ("Bad rectangle specification " ^ s)

let parse_rectangles pdf s =
  try
    let pages = Pdfpage.pages_of_pagetree pdf in
    let groups = List.map (fun page -> parse_units_string pdf page s) pages in
      List.map
        (function
         | [x; y; w; h] -> (x, y, w, h)
         | _ -> error ("Bad rectangle specification " ^ s))
        groups
  with
    _ -> error ("Bad rectangle specification " ^ s)

let parse_coordinate pdf s =
  try
    match parse_units_string pdf emptypage s with
    | [dx; dy] ->
        (*Printf.printf "result = %f, %f\n" dx dy;*)
        dx, dy
    | _ -> error ("Bad coordinate specification " ^ s)
  with
    _ -> error ("Bad coordinate specification " ^ s)

let parse_coordinates pdf s =
  try
    let pages = Pdfpage.pages_of_pagetree pdf in
      let groups = List.map (fun page -> parse_units_string pdf page s) pages in
        List.map
          (function
            | [dx; dy] -> (dx, dy)
           | _ -> error ("Bad coordinate specification " ^ s))
          groups
  with
    _ -> error ("Bad coordinate specification " ^ s)

let parse_single_number pdf s =
  try
    match parse_units_string pdf emptypage s with
    | [x] -> x
    | _ -> error ("Bad number argument " ^ s)
  with
    _ -> error ("Bad number argument " ^ s)
