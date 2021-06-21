(* Parsing coordinates, numbers, positions etc. *)
val parse_rectangles : Pdf.t -> string -> (float * float * float * float) list

val parse_coordinate : Pdf.t -> string -> float * float

val parse_coordinates : Pdf.t -> string -> (float * float) list

val parse_single_number : Pdf.t -> string -> float
