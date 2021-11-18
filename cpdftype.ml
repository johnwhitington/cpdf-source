(* A typesetter for cpdf. A list of elements is manipulated zero or more times
   to lay it out, paginate it, and so on. It is then typeset to produce a list
   of pages *)

(* Text is represented as a list of unicode code points *)
type text = int list

(* Glue *)
type glue =
  {glen : float;
   stretch : float}

(* Main type *)
type code =
  Text of text
| HGlue of glue
| VGlue of glue
| NewLine
| NewPage
| Font of Pdftext.font

let indent x = HGlue {glen = x; stretch = 0.}
let newpara x = HGlue {glen = x; stretch = 0.}

type t = code list

let of_utf8 = Pdftext.codepoints_of_utf8

let example =
  [Text (of_utf8 "Jackdaws love my Sphinx of Quartz. And this, this is the second sentence to provoke a line-break.");
   newpara 10.;
   indent 72.;
   Text (of_utf8 "The second paragraph");
   NewPage]

(* Typesetter state. Origin at top left of page *)
type state =
  {mutable font : Pdftext.font;
   mutable xpos : float;
   mutable ypos : float}

let initial_state () =
  {font = Pdftext.StandardFont (Pdftext.TimesRoman, Pdftext.WinAnsiEncoding);
   xpos = 0.;
   ypos = 0.}

(* Split text into lines, resolve all hglue stretches to 0, remove Newlines. *)
let layout lmargin rmargin papersize i =
  i

(* Resolve all hglue stretches, insert NewPage as needed. *)
let paginate tmargin bmargin papersize i = i

(* Split on NewPages, typeset each page, add font dictionaries *)
let typeset papersize i =
  [Pdfpage.blankpage papersize]

