(** Positions *)

(** Possible positions for adding text and other uses. See cpdfmanual.pdf *)
type position =
    PosCentre of float * float
  | PosLeft of float * float
  | PosRight of float * float
  | Top of float
  | TopLeft of float
  | TopRight of float
  | Left of float
  | BottomLeft of float
  | Bottom of float
  | BottomRight of float
  | Right of float
  | Diagonal
  | ReverseDiagonal
  | Centre

(** Produce a debug string of a [position] *)
val string_of_position : position -> string

(** Orientation of the string on the page *)
type orientation = Horizontal | Vertical | VerticalDown

(** [calculate_position ignore_d w (xmin, ymin, xmax, ymax) orientation pos] calculates
the absolute position of text given its width, bounding box, orientation and
position. If [ignore_d] is true, the distance from the position (e.g 10 in
TopLeft 10) is ignored (considered zero). *)
val calculate_position :
  bool ->
  float ->
  float * float * float * float ->
  orientation -> position -> float * float * float
