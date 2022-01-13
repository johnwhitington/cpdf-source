(** Parsing coordinates, numbers, positions etc.*)

(** See cpdfmanual.pdf for examples of things these functions can parse, such as page sizes. *)

(** Read a list of rectangles from a string. *)
val parse_rectangles : Pdf.t -> string -> (float * float * float * float) list

(** Read a coordinate from a string *)
val parse_coordinate : Pdf.t -> string -> float * float

(** Read a list of coordinates from a string *)
val parse_coordinates : Pdf.t -> string -> (float * float) list

(** Read a single number from a string *)
val parse_single_number : Pdf.t -> string -> float
