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

let indent x = HGlue {glen = x; gstretch = 0.}
let newpara x = VGlue {glen = x; gstretch = 0.}

type t = element list

let of_utf8 (f, fontsize) t =
  let pdf = Pdf.empty () in
  let fontdict = Pdftext.write_font pdf f in
  let extractor = Pdftext.charcode_extractor_of_font pdf (Pdf.Indirect fontdict) in
       Pdftext.codepoints_of_utf8 t
    |> option_map extractor
    |> map char_of_int
    |> implode

let times_roman_12 = (Pdftext.StandardFont (Pdftext.TimesRoman, Pdftext.WinAnsiEncoding), 12.)
let times_italic_10 = (Pdftext.StandardFont (Pdftext.TimesItalic, Pdftext.WinAnsiEncoding), 10.)
let times_bold_10 = (Pdftext.StandardFont (Pdftext.TimesBold, Pdftext.WinAnsiEncoding), 10.)

let example =
  [Font times_roman_12;
   newpara 12.; (* set up top of page correctly *)
   Text (of_utf8 times_roman_12 "Jackdaws love my Sphinx of Quartz. And this, this is the second sentence to provoke a line-break. We need rather more text than one might think in this diminutive font.");
   NewLine;
   Text (of_utf8 times_roman_12 "After the newline... ");
   newpara (12. *. 1.3);
   indent 32.;
   Font times_italic_10;
   Text (of_utf8 times_italic_10 "The second paragraph");
   NewPage;
   newpara 10.; (* set up top of page *)
   Font times_bold_10;
   BeginDest Pdfdest.NullDestination; 
   Text (of_utf8 times_bold_10 "A little too bold");
   EndDest
  ]

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
  fold_left ( +. ) 0. (map (fun s -> ws.(int_of_char s)) (explode s))

let split_text space_left ws t =
  (t, false, "", 0.)

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
        let this_line, needs_newline, remaining_text, space_used =
          split_text (xpos_max -. s.xpos) s.width_table text
        in
          o := Text this_line :: !o;
          s.xpos <- s.xpos +. width_of_string s.width_table this_line;
          if needs_newline then layout_element NewLine;
          if remaining_text <> "" then layout_element (Text remaining_text)
    | HGlue {glen} as glue ->
        s.xpos <- s.xpos +. glen;
        o := glue :: !o;
        if s.xpos >= xpos_max then layout_element NewLine
    | x -> o := x :: !o
    in
    iter layout_element i;
    rev !o

(* Resolve all hglue stretches, insert NewPage as needed. *)
let paginate tmargin bmargin papersize i = i

let make_resources fontobjnums =
  Pdf.Dictionary
    [("/Font", Pdf.Dictionary (map (fun fo -> ("/F" ^ string_of_int fo, Pdf.Indirect fo)) fontobjnums))]

(* At this stage, just Font and Text and HGlue 0. and VGlue 0. and Newline and
   NewPage elements. Split on NewPages, typeset each page, add font
   dictionaries. New page only
   creates a page when that page has content. *)
let typeset lmargin rmargin tmargin bmargin papersize pdf i =
  let i = layout lmargin rmargin papersize i in
  let i = paginate tmargin bmargin papersize i in
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

let example_pdf () =
  let pdf = Pdf.empty () in
  let pages = typeset 20. 20. 20. 20. Pdfpaper.a4 pdf example in
    let pdf, pageroot = Pdfpage.add_pagetree pages pdf in
      Pdfpage.add_root pageroot [] pdf

(*let _ =
  Pdfwrite.pdf_to_file (example_pdf ()) "out.pdf"*)
