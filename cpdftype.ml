(* A typesetter for cpdf. A list of elements is manipulated zero or more times
   to lay it out, paginate it, and so on. It is then typeset to produce a list
   of pages *)
open Pdfutil

(* Text is represented as a list of unicode code points *)
type text = int list

(* Glue *)
type glue =
  {glen : float;
   stretch : float}

(* Main type *)
type element =
  Text of text
| HGlue of glue
| VGlue of glue
| NewLine
| NewPage
| Font of Pdftext.font * float

let string_of_element = function
  | Text t -> Pdftext.utf8_of_codepoints t
  | HGlue _ -> "HGLUE"
  | VGlue _ -> "VGLUE"
  | NewLine -> "NewLine"
  | NewPage -> "NewPage"
  | Font _ -> "Font"

let indent x = HGlue {glen = x; stretch = 0.}
let newpara x = VGlue {glen = x; stretch = 0.}

type t = element list

let of_utf8 = Pdftext.codepoints_of_utf8

let example =
  [Font (Pdftext.StandardFont (Pdftext.TimesRoman, Pdftext.WinAnsiEncoding), 12.);
   Text (of_utf8 "Jackdaws love my Sphinx of Quartz. And this, this is the second sentence to provoke a line-break.");
   NewLine;
   newpara 10.;
   indent 72.;
   Font (Pdftext.StandardFont (Pdftext.TimesItalic, Pdftext.WinAnsiEncoding), 10.);
   Text (of_utf8 "The second paragraph");
   NewPage]

type state =
  {mutable font : Pdftext.font option;
   mutable xpos : float;
   mutable ypos : float}

let initial_state () =
  {font = None;
   xpos = 0.;
   ypos = 0.}

(* Split text into lines, resolve all hglue stretches to 0, remove Newlines. *)
let layout_element s xpos_max fo = function
  | e -> fo e

let layout lmargin rmargin papersize i =
  let width =
    Pdfunits.convert 72. (Pdfpaper.unit papersize) Pdfunits.PdfPoint (Pdfpaper.width papersize)
  in
  let o = ref [] in
  let s = initial_state () in
  let xpos_max = Pdfpaper.width papersize -. lmargin in
    s.xpos <- lmargin;
    iter (layout_element s xpos_max (fun e -> o := e::!o)) i;
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
  let typeset_element = function
    | Text cps ->
        let charcodestring =
          match s.font with
          | None -> failwith "font not set up"
          | Some f ->
              match List.assoc_opt f !fonts with
              | Some objnum ->
                  let extractor =
                    Pdftext.charcode_extractor_of_font pdf (Pdf.lookup_obj pdf objnum)
                  in
                    implode (map char_of_int (option_map extractor cps))
              | None -> failwith "font not found"
        in
          ops :=
            Pdfops.Op_Q
            ::Pdfops.Op_ET
            ::Pdfops.Op_Tj charcodestring
            ::Pdfops.Op_BT
            ::Pdfops.Op_cm (Pdftransform.mktranslate s.xpos (height -. s.ypos))
            ::Pdfops.Op_q
            ::!ops
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
          thispagefontnums := objnum :: !thispagefontnums;
          ops := Pdfops.Op_Tf (name, fontsize)::!ops
    | HGlue {glen} ->
        s.xpos <- s.xpos +. glen
    | VGlue {glen} ->
        s.ypos <- s.ypos +. glen
    | NewLine ->
        s.xpos <- 0. 
    | NewPage ->
        write_page ();
        ops := [];
        s.xpos <- 0.;
        s.ypos <- 0.
  in
    iter typeset_element i;
    write_page ();
    rev !pages

let example_pdf () =
  let pdf = Pdf.empty () in
  let pages = typeset 20. 20. 20. 20. Pdfpaper.a4 pdf example in
    let pdf, pageroot = Pdfpage.add_pagetree pages pdf in
      Pdfpage.add_root pageroot [] pdf

let _ =
  Pdfwrite.pdf_to_file (example_pdf ()) "out.pdf"
