val extract_images : string ->
           string ->
           Cpdfmetadata.encoding -> bool -> bool -> Pdf.t -> int list -> string -> unit
           
val image_resolution : Pdf.t -> int list -> float -> (int * string * int * int * float * float) list
