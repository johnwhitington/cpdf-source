(* Embed a TrueType font for the given set of unicode codepoints in the given
   encoding, adding the fontfiles to the PDF and returning a list of font objects,
   together with a unicode codepoint --> (font number in list, charcode) table *)

(* FIXME: Really we want to create an interactive fontpack which creates fonts
   when needed, but delays the actual production of the subset truetype data
   until later. This will mean we don't need to pre-calculate the USED set. For
   now, we just hack Cpdftoc, cpdfaddtext and cpdftextofpdf to pre-calculate
   the subset. *)

type t = Pdftext.font list * (int, int * int) Hashtbl.t

type cpdffont =
  PreMadeFontPack of t
| EmbedInfo of {fontfile : Pdfio.bytes; fontname : string; encoding : Pdftext.encoding}
| ExistingNamedFont

val get_char : t -> int -> (int * int * Pdftext.font) option

val embed_truetype :
  Pdf.t -> fontfile:Pdfio.bytes -> fontname:string -> codepoints:int list ->
  encoding:Pdftext.encoding -> t
