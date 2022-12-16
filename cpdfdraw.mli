type drawops_colspec =
   NoCol
 | RGB of float * float * float
 | Grey of float
 | CYMK of float * float * float * float

type drawops =
  | Rect of float * float * float * float (* x, y, w, h *)
  | To of float * float
  | Line of float * float
  | ClosePath
  | SetFill of drawops_colspec
  | SetStroke of drawops_colspec
  | SetLineThickness of float
  | SetLineCap of int
  | SetLineJoin of int
  | SetMiterLimit of float
  | SetDashPattern of float list * float
  | Matrix of Pdftransform.transform_matrix
  | Push
  | Pop
  | Fill
  | FillEvenOdd
  | Stroke
  | FillStroke
  | FillStrokeEvenOdd
  | SoftXObject of drawops list
  | HardXObject of drawops list

val draw : bool -> int list -> Pdf.t -> drawops list -> Pdf.t
