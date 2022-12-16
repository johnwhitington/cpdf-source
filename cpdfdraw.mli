type drawops_colspec =
   NoCol
 | RGB of float * float * float
 | Grey of float
 | CYMK of float * float * float * float

type drawops =
  | Rect of float * float * float * float (* x, y, w, h *)
  | To of float * float
  | Line of float * float
  | Fill of drawops_colspec
  | Stroke of drawops_colspec
  | SetLineThickness of float
  | SetLineCap of int
  | SetLineJoin of int
  | SetMiterLimit of float
  | SetDashPattern of float list * float
  | Matrix of Pdftransform.transform_matrix
  | Push
  | Pop
  | EndPath

type state =
  {mutable fill : drawops_colspec;
   mutable stroke : drawops_colspec;
   mutable linewidth : float;
   mutable linecap : int;
   mutable linejoin : int;
   mutable miterlimit : float;
   mutable dashpattern : float list * float}

val draw : bool -> int list -> Pdf.t -> drawops list -> Pdf.t
