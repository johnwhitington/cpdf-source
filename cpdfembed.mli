(* Embed a TrueType font for the given set of UTF8 characters in the given
   encoding, adding it as an object to the PDF, and returning the number of
   that object. *)
val embed_truetype :
  Pdf.t -> fontfile:Pdfio.bytes -> fontname:string -> text:string ->
  encoding:Pdftext.encoding -> int

(* Eventually, we will just have this, and a subsetting function. Then we will
   use Pdftext.write_font to write it. *)
val font_of_truetype :
  fontfile:Pdfio.bytes -> fontname:string -> encoding:Pdftext.encoding -> Pdftext.font
