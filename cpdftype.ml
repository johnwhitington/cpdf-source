(* A typesetter for cpdf. A list of elements is manipulated zero or more times
   to lay it out, paginate it, and so on. It is then typeset to produce a list
   of pages *)

(* FIXME We need to make Pdfstandard14 width calculations much more efficient
   by caching so that we are not making a table up for each character! *)
(* FIXME We need to reintroduce kerning in Pdfstandard14. *)
(* FIXME Fix up charcode / text extractors to take fonts not fontdicts *)
open Pdfutil

(* Glue *)
type glue =
  {glen : float;
   gstretch : float}

(* Main type *)
type element =
  Text of string (* WinAnsiEncoding *)
| HGlue of glue
| VGlue of glue
| NewLine
| NewPage
| Font of (Pdftext.font * float)
| BeginDest of Pdfdest.t
| EndDest

let to_string_elt = function
  | Text t -> t
  | HGlue {glen} -> "HGLUE" ^ string_of_float glen
  | VGlue _ -> "VGLUE"
  | NewLine -> "NewLine"
  | NewPage -> "NewPage"
  | Font _ -> "Font"
  | BeginDest _ -> "BeginDest"
  | EndDest -> "EndDest"

let to_string es = fold_left (fun a b -> a ^ "\n" ^ b) "" (map to_string_elt es)

type t = element list

type state =
  {mutable font : Pdftext.font option;
   mutable fontsize : float;
   mutable width_table : float array; (* Widths for charcodes 0..255 *)
   mutable xpos : float;
   mutable ypos : float;
   mutable dest : Pdfdest.t option}

let initial_state () =
  {font = None;
   fontsize = 0.;
   width_table = [||];
   xpos = 0.;
   ypos = 0.;
   dest = None}

let font_widths f fontsize =
  let w = fontsize *. (600. /. 1000.) in
    Array.make 256 w

(* For now, split each text element into words, and lay them out ragged right
   on one long page. Words longer than a whole line just fall off the margin.
   Turn text newlines into real newlines. *)
let width_of_string ws s =
  fold_left ( +. ) 0. (map (fun s -> ws.(int_of_char s)) s) (* FIXME efficiency *)

(* Split into words on spaces. Find how many words (at least one, to make
   progress) fit into the available space. We set needs_newline if the next
   word would overflow. Return (text, needs_newline, remaining_text) *)
(* FIXME efficiency *)
let split_text space_left widths t =
  let chars = ref (explode t) in
  let words = ref [] in
  let space_left = ref space_left in
  let return needs_newline =
    (implode (flatten (rev !words)), needs_newline, implode !chars)
  in
    try
      while !chars <> [] do
        let word, rest = cleavewhile (neq ' ') !chars in
          let w = width_of_string widths word in 
          if !words = [] || w < !space_left
            then (words := (word @ [' '])::!words; space_left := !space_left -. w -. width_of_string widths [' '])
            else raise Exit;
          chars := if rest = [] then [] else tl rest;
      done;
      return false
    with
      Exit -> return true

let layout lmargin rmargin papersize i =
  let width =
    Pdfunits.convert 72. (Pdfpaper.unit papersize) Pdfunits.PdfPoint (Pdfpaper.width papersize)
  in
  let o = ref [] in
  let s = initial_state () in
  let xpos_max = width -. lmargin in
    s.xpos <- lmargin;
    let rec layout_element = function
    | Font (f, fontsize) ->
        s.width_table <- font_widths f fontsize;
        o := Font (f, fontsize) :: !o
    | Text text ->
        if text = "" then () else
          begin
            let this_line, needs_newline, remaining_text =
              split_text (xpos_max -. s.xpos) s.width_table text
            in
              o := Text this_line :: !o;
              s.xpos <- s.xpos +. width_of_string s.width_table (explode this_line);
              if needs_newline then layout_element NewLine;
              if remaining_text <> "" then layout_element (Text remaining_text)
          end
    | HGlue {glen} as glue ->
        s.xpos <- s.xpos +. glen;
        o := glue :: !o;
        if s.xpos >= xpos_max then layout_element NewLine
    | NewLine ->
        s.xpos <- lmargin;
        o := NewLine :: !o
    | x ->
        o := x :: !o
    in
    iter layout_element i;
    rev !o

(* Paginate, simply line-based. When ypos + lineheight exceeds max_ypos, we insert a page break. *)
let paginate tmargin bmargin papersize i =
  let height = Pdfunits.convert 72. (Pdfpaper.unit papersize) Pdfunits.PdfPoint (Pdfpaper.height papersize) in
  let o = ref [] in
  let s = initial_state () in
  s.ypos <- tmargin;
  let max_ypos = height -. tmargin -. bmargin in
  let rec process = function
   | VGlue {glen} as glue ->
       s.ypos <- s.ypos +. glen;
       o := glue :: !o;
       if s.ypos > max_ypos then process NewPage
   | NewLine ->
       s.ypos <- s.ypos +. s.fontsize *. 1.3;
       o := NewLine::!o;
       if s.ypos > max_ypos then process NewPage
   | Font (f, fs) ->
       s.font <- Some f;
       s.fontsize <- fs;
       o := Font (f, fs)::!o
   | NewPage ->
       s.ypos <- tmargin;
       o := NewPage::!o
   | x -> o := x::!o
  in
    iter process i;
    rev !o

let make_resources fontobjnums =
  Pdf.Dictionary
    [("/Font", Pdf.Dictionary (map (fun fo -> ("/F" ^ string_of_int fo, Pdf.Indirect fo)) fontobjnums))]

(* At this stage, just Font and Text and HGlue 0. and VGlue 0. and Newline and
   NewPage elements. Split on NewPages, typeset each page, add font
   dictionaries. New page only
   creates a page when that page has content. *)
let typeset lmargin rmargin tmargin bmargin papersize pdf i =
  print_endline "***input:\n\n";
  print_endline (to_string i);
  let i = layout lmargin rmargin papersize i in
  print_endline "***after layout:\n\n";
  print_endline (to_string i);
  let i = paginate tmargin bmargin papersize i in
  print_endline "***after pagination:\n\n";
  print_endline (to_string i);
  let height = Pdfunits.convert 72. (Pdfpaper.unit papersize) Pdfunits.PdfPoint (Pdfpaper.height papersize) in
  let s = initial_state () in
  s.xpos <- lmargin;
  s.ypos <- tmargin;
  let ops = ref [] in
  let fonts = ref [] in
  let thispagefontnums = ref [] in
  let pages = ref [] in
  let write_page () =
    if !ops <> [] then
      let page =
        {Pdfpage.content = [Pdfops.stream_of_ops (rev !ops)];
         Pdfpage.mediabox = Pdfpage.rectangle_of_paper papersize;
         Pdfpage.resources = make_resources !thispagefontnums;
         Pdfpage.rotate = Pdfpage.Rotate0;
         Pdfpage.rest = Pdf.Dictionary []}
      in
        pages := page :: !pages
  in
  let rec typeset_element = function
    | Text cps ->
        ops :=
          Pdfops.Op_Q
          ::Pdfops.Op_ET
          ::Pdfops.Op_Tj cps
          ::Pdfops.Op_BT
          ::Pdfops.Op_cm (Pdftransform.mktranslate s.xpos (height -. s.ypos))
          ::Pdfops.Op_q
          ::!ops
        (* If a destination, add the rectangle to the pile of rectangles for this annotation. *)
    | Font (f, fontsize) ->
        let name, objnum =
          match List.assoc_opt f !fonts with
          | Some objnum -> ("/F" ^ string_of_int objnum, objnum)
          | None ->
              let num = Pdftext.write_font pdf f in
              let n = "/F" ^ string_of_int num in
                fonts := (f, num) :: !fonts;
                (n, num)
        in
          s.font <- Some f;
          s.fontsize <- fontsize;
          thispagefontnums := objnum :: !thispagefontnums;
          ops := Pdfops.Op_Tf (name, fontsize)::!ops
    | HGlue {glen} ->
        s.xpos <- s.xpos +. glen
    | VGlue {glen} ->
        s.ypos <- s.ypos +. glen
    | NewLine ->
        s.xpos <- lmargin;
        typeset_element (VGlue {glen = s.fontsize *. 1.3; gstretch = 0.})
    | NewPage ->
        write_page ();
        thispagefontnums := [];
        ops := [];
        if s.font <> None then typeset_element (Font (unopt s.font, s.fontsize));
        s.xpos <- lmargin;
        s.ypos <- tmargin
    | BeginDest dest ->
        s.dest <- Some dest
    | EndDest ->
        (* Collect together the rectangles for this annotation, merge? and output *)
        s.dest <- None
  in
    iter typeset_element i;
    write_page ();
    rev !pages
