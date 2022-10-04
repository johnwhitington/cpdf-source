(* Parse a TrueType font *)
type t =
  {flags : int;
   minx : int;
   miny : int;
   maxx : int;
   maxy : int;
   italicangle : int;
   ascent : int;
   descent : int;
   capheight : int;
   stemv : int;
   xheight : int;
   avgwidth : int;
   maxwidth : int;
   firstchar : int;
   lastchar : int;
   widths : int array;
   subset : Pdfio.bytes;
   tounicode : Pdfio.bytes option}

(* Parse the font, given the list of Unicode codepoints required for the subset
   and optionally their PDF codepoint too. Returns the information required for
   embedding this font in a PDF. *)
val parse : ?subset:int list -> Pdfio.bytes -> encoding:Pdftext.encoding -> t list
