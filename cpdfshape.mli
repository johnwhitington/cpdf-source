(** Basic Shapes *)

type fpoint = float * float

type winding_rule = EvenOdd | NonZero

type segment =
  | Straight of fpoint * fpoint
  | Bezier of fpoint * fpoint * fpoint * fpoint

(* Each segment list may be marked as a hole or not. *)
type hole = Hole | Not_hole

(* A [subpath] is either closed or open. *)
type closure = Closed | Open

(* A [subpath] is the pair of a hole and a list of segments. *)
type subpath = hole * closure * segment list

(* A path is made from a number of subpaths. *)
type path = winding_rule * subpath list

(** The factor by which the radius of a circle is multiplied to find the length
of the bezier control lines when approximating quarter arcs to make circles. *)
val kappa : float

(** Calling [restrict_angle s a] restricts an angle [a] to one of those at [s,
2s, 3s...] returning the chosen one. *)
val restrict_angle : float -> float -> float

(** Calling [circle x y r] builds a path representing a circle at [(x, y)] with
radius [r]. *)
val circle : float -> float -> float -> path

(** Calling [rectangle x y w h] builds a path representing a rectangle with top
left [(x, y)], width [w] and height [h]. *)
val rectangle : float -> float -> float -> float -> path
