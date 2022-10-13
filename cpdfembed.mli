(* Embed a TrueType font for the given set of unicode codepoints in the given
   encoding, adding the fontfiles to the PDF and returning a list of font objects,
   together with a unicode codepoint --> (font number in list, charcode) table *)

type t = Pdftext.font list * (int, int * int) Hashtbl.t

val embed_truetype :
  Pdf.t -> fontfile:Pdfio.bytes -> fontname:string -> codepoints:int list ->
  encoding:Pdftext.encoding -> t
