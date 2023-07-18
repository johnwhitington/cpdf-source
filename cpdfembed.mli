(* Embed a TrueType font for the given set of unicode codepoints in the given
   encoding, adding the fontfiles to the PDF and returning a list of font objects,
   together with a unicode codepoint --> (font number in list, charcode) table *)

type t = Pdftext.font list * (int, int * int) Hashtbl.t

type cpdffont =
  PreMadeFontPack of t
| EmbedInfo of {fontfile : Pdfio.bytes; fontname : string; encoding : Pdftext.encoding}
| ExistingNamedFont

val fontpack_of_standardfont : Pdftext.font -> t

(* Returns (charcode, fontnumber, font) *) 
val get_char : t -> int -> (int * int * Pdftext.font) option

val embed_truetype :
  Pdf.t -> fontfile:Pdfio.bytes -> fontname:string -> codepoints:int list ->
  encoding:Pdftext.encoding -> t

val collate_runs : ('a * 'b * 'c) list -> ('a * 'b * 'c) list list
