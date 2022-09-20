(* Embed a TrueType font for the given set of unicode codepoints in the given
   encoding, adding the fontfile to the PDF and returning the font object. *)
val embed_truetype :
  Pdf.t -> fontfile:Pdfio.bytes -> fontname:string -> codepoints:int list ->
  encoding:Pdftext.encoding -> Pdftext.font
