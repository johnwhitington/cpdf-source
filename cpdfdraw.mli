type colspec =
   NoCol
 | RGB of float * float * float
 | Grey of float
 | CYMK of float * float * float * float

type drawops =
  | Rect of float * float * float * float
  | Bezier of float * float * float * float * float * float
  | To of float * float
  | Line of float * float
  | ClosePath
  | SetFill of colspec
  | SetStroke of colspec
  | SetLineThickness of float
  | SetLineCap of int
  | SetLineJoin of int
  | SetMiterLimit of float
  | SetDashPattern of float list * float
  | Matrix of Pdftransform.transform_matrix
  | Qq of drawops list
  | Fill
  | FillEvenOdd
  | Stroke
  | FillStroke
  | FillStrokeEvenOdd
  | Clip
  | ClipEvenOdd
  | FormXObject of float * float * float * float * string * drawops list
  | Use of string
  | ImageXObject of string * Pdf.pdfobject
  | Image of string
  | NewPage
  | Opacity of float
  | SOpacity of float
  | Font of Pdftext.standard_font * float
  | TextSection of drawops list
  | Text of string
  | SpecialText of string
  | Newline
  | Leading of float
  | CharSpace of float
  | WordSpace of float
  | TextScale of float
  | RenderMode of int
  | Rise of float

val draw : filename:string ->
           bates:int ->
           batespad:int option ->
           bool -> int list -> Pdf.t -> drawops list -> Pdf.t
