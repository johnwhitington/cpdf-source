(* Stroking lines and making shapes *)

(* This module provides for the stroking of lines, and production of shape
primitives (circles, regular polygons etc). *)
open Pdfutil

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
(* \section{Common geometric functions} *)

(* The factor by which we multiply the radius to find the length of the bezier
control lines when approximating quarter arcs to make semicircles and circles.
*)
let kappa = ((sqrt 2. -. 1.) /. 3.) *. 4.

(* Restrict an angle [a] to one of those at $s, 2s, 3s\ldots$. We find the two
candidate angles, and see which [a] is numerically closer to. The candidate
points are taken modulo $2\pi$ for this to work. *)
let restrict_angle s a =
  let p = mod_float (floor (a /. s) *. s) (2. *. pi) in
    let p' = mod_float (p +. s) (2. *. pi) in
      if abs_float (p -. a) < abs_float (p' -. a) then p else p'

(* Some Useful Shapes *)

(* Make a quarter-circle from a single bezier curve from [s] to $(s + \pi / 2)
\bmod 2\pi$ with centre [c] and radius [r]. We cheat by making the standard
quarter from [(1, 0)] to [(0, 1)] and rotating using the [Transform] module.
*)
let quarter s (cx, cy) r =
  let standard_quarter_points =
    [(1., 0.); (1., kappa); (kappa, 1.); (0., 1.)]
  and transform =
    [Pdftransform.Translate(cx, cy);
     Pdftransform.Scale((0., 0.), r, r);
     Pdftransform.Rotate((0., 0.), s)]
  in
    match
      map (Pdftransform.transform transform) standard_quarter_points
    with
    | [p; q; r; s] -> Bezier(p, q, r, s)
    | _ -> raise (Pdf.PDFError ("Shapes.quarter: inconsistency"))

(* Some of the following functions generate what is supposed to be a connected
list of segments. However, since they operate by calculating each segment
seperately, floating point inaccuracies can arise, making the end of one
segment misalign with the start of the next. This function corrects the defect
by copying the end of one segment to the beginning of the next. We only need to
deal with bezier segments for now. *)
let rec joinsegs segments =
  match segments with
  | [] -> []
  | [x] -> [x]
  | Bezier(_, _, _, d) as s::Bezier(_, b', c', d')::rest ->
      s::joinsegs (Bezier(d, b', c', d')::rest)
  | _ -> raise (Pdf.PDFError "PDFShapes.joinsegs: Segment not supported")

(* Approximate a circle using four bezier curves.*)
let circle x y r =
  NonZero,
    [(Not_hole,
      Closed,
     joinsegs
       [quarter 0. (x, y) r;
       quarter (pi /. 2.) (x, y) r;
       quarter pi (x, y) r;
       quarter (3. *. pi /. 2.) (x, y) r ])]

let rectangle x y w h =
  (EvenOdd,
    ([(Not_hole,
       Closed,
      [Straight ((x, y), (x +. w, y));
       Straight ((x +. w, y), (x +. w, y +. h));
       Straight ((x +. w, y +. h), (x, y +. h));
       Straight ((x, y +. h), (x, y))])]))
