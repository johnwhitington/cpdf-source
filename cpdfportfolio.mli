type entry =
  {filename : string;
   relationship : Pdf.pdfobject;
   description : string}

val portfolio : Pdf.t -> entry list -> unit
