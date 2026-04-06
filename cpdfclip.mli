(** Boolean operations on polygons *)

(** The clip operators. *)
type gpc_clipop = Difference | Intersection | ExclusiveOR | Union

(** A vertex *)
type gpc_vertex = {x : float; y : float}

(** A contour is the number of vertices and an array of them. *)
type gpc_contour = int * gpc_vertex array

(** The type of polygons. Number of contours, array of hole/not hole, array of contours. 0 = not a hole, 1 = a hole. *)
type gpc_polygon = int * int array * gpc_contour array

(** The null [gpc_polygon]. *)
val nullpoly : gpc_polygon

(** Debug printing of a polygon. *)
val gpcml_printpolygon : gpc_polygon -> unit

(** [gpc_polygon_of_box xmin xmax ymin ymax] builds the obvious polygon. *)
val gpc_polygon_of_box : float -> float -> float -> float -> gpc_polygon

(** Build a polygon from an array of booleans (hole / not hole) and an array of vertex arrays *)
val make_gpcpolygon : bool array -> gpc_vertex array array -> gpc_polygon

(** [gpcml_clippolygon op a b] clips polygons [a] and [b] using operator [op],
returning the result. *)
val gpcml_clippolygon :
  gpc_clipop -> gpc_polygon -> gpc_polygon -> gpc_polygon
