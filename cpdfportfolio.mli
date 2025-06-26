type entry =
  {filename : string;
   relationship : string option;
   description : string option}

val portfolio : Pdf.t -> entry list -> unit
