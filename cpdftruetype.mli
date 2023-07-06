(* Parse and subset TrueType fonts *)

(* The type of a single parsed font, including everything needed to build a PDF font. *)
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
   subset_fontfile : Pdfio.bytes;
   subset : int list;
   tounicode : (int, string) Hashtbl.t option}

(* Parse the given TrueType font file. It will return one or more fonts. The
   first, a plain Latin font in the given encoding. Others are for the
   additional characters in the font. For subsetting, or to return a full
   font-pack, you should supply a subset (a list of unicode codepoints whose
   corresponding glyphs are required). *)
val parse : subset:int list -> Pdfio.bytes -> Pdftext.encoding -> t list
