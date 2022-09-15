(* Embed a TrueType font for the given set of UTF8 characters in the given
   encoding, adding it as an object to the PDF, and returning the number of
   that object. *)
val embed_truetype :
  Pdf.t -> fontfile:Pdfio.bytes -> fontname:string -> text:string ->
  encoding:Pdftext.encoding -> Pdftext.font
