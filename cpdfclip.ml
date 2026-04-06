(* Polygon clipping *)

(* An interface to Alan Murta's General Polygon Clipping Library. *)

(* Clip operations. *)
type gpc_clipop = Difference | Intersection | ExclusiveOR | Union

(* A vertex. *)
type gpc_vertex = {x : float; y : float}

(* A contour is a pair of the number of vertices and an array of those vertices. *)
type gpc_contour = int * gpc_vertex array

(* Number of contours, array of hole/not hole, array of countours, 0 = not a
hole, 1 = a hole. *)
type gpc_polygon = int * int array * gpc_contour array

(* A pointer to a polygon internal to the GPC library. *)
type gpc_pointer

(* Functions from gpcml.c *)
external gpcml_buildpolygon : gpc_polygon -> gpc_pointer =
  "gpcml_buildpolygon"

external gpcml_getpolygon : gpc_pointer -> gpc_polygon =
  "gpcml_getpolygon"

external gpcml_printnativepolygon : gpc_pointer -> unit =
  "gpcml_printpolygon"

external gpcml_clip : gpc_pointer -> gpc_pointer -> int -> gpc_pointer =
  "gpcml_clip"

(* The null GPC polygon *)
let nullpoly = 0, [||], [||]

(* Print a vertex. *)
let printvertex v =
  Printf.printf "    {x = %f, y = %f}\n" v.x v.y

(* Print a contour. *)
let printcontour hole (num_vertices, vertices) =
  Printf.printf "  Contour with %i vertices and hole %i\n" num_vertices hole;
  Array.iter printvertex vertices

(* Print a polygon. *)
let array_iter2 f a b =
  if Array.length a = Array.length b then
    if Array.length a = 0 then () else
      for x = 0 to (Array.length a) - 1 do
        f (Array.get a x) (Array.get b x)
      done
  else
    raise (Invalid_argument "Utility.array_iter2")

let array_iter2 f a b =
  if Array.length a = Array.length b then
    if Array.length a = 0 then () else
      for x = 0 to (Array.length a) - 1 do
        f (Array.get a x) (Array.get b x)
      done
  else
    raise (Invalid_argument "Utility.array_iter2")

let gpcml_printpolygon (num_contours, holes, contours) =
  Printf.printf "Contours: %i\n" num_contours;
  array_iter2 printcontour holes contours

(* Make a GPC polygon to represent a rectangle. *)
let gpc_polygon_of_box xmin xmax ymin ymax =
  (1, [|0|], [|(4, [|
    {x = xmin; y = ymin}; {x = xmax; y = ymin};
    {x = xmax; y = ymax}; {x = xmin; y = ymax}|])|])

(* Clip two polygons against one another using operation op. *)
let gpcml_clippolygon op a b =
  let a' = gpcml_buildpolygon a
  and b' = gpcml_buildpolygon b
  and clip_op =
    match op with
    | Difference -> 0
    | Intersection -> 1
    | ExclusiveOR -> 2
    | Union -> 3
  in
    gpcml_getpolygon (gpcml_clip a' b' clip_op)

(* Build a gpc polygon from an array of hole / not hole and array of gpc_vertex arrays *)
let make_gpcpolygon holes vertex_arrays =
  if Array.length holes <> Array.length vertex_arrays then
    raise (Failure "make_gpcpolygon: unequal length inputs")
  else
    let holes_bool =
      Array.map (function true -> 1 | false -> 0) holes
    and contours =
      Array.map
        (function vertices -> Array.length vertices, vertices)
        vertex_arrays
    in
      Array.length holes_bool, holes_bool, contours
