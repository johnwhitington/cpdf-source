(* Embed a TrueType font for the given set of unicode codepoints in the given
   encoding, adding the fontfiles to the PDF and returning the font objects,
   together with the codepoints which appear in each. *)

val embed_truetype :
  Pdf.t -> fontfile:Pdfio.bytes -> fontname:string -> codepoints:int list ->
  encoding:Pdftext.encoding -> (int list * Pdftext.font) list
