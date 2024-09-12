(** Draw on PDFs *)

type colspec =
   NoCol
 | RGB of float * float * float
 | Grey of float
 | CYMK of float * float * float * float

type justification =
  Left | Right | Centre

type drawops =
  | Rect of float * float * float * float
  | Bezier of float * float * float * float * float * float
  | Bezier23 of float * float * float * float
  | Bezier13 of float * float * float * float
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
  | Image of string * string option
  | NewPage
  | Opacity of float
  | SOpacity of float
  | FontPack of string * Cpdfembed.cpdffont * (int, unit) Hashtbl.t
  | Font of string * float
  | TextSection of drawops list
  | Text of string
  | SpecialText of string
  | Para of justification * float * string
  | Newline
  | Leading of float
  | CharSpace of float
  | WordSpace of float
  | TextScale of float
  | RenderMode of int
  | Rise of float

(** Calling [draw fast underneath filename bates batespad range pdf drawops] draws on
    top of all the pages in the range. *)
val draw : struct_tree:bool ->
           fast:bool ->
           underneath:bool ->
           filename:string ->
           bates:int ->
           batespad:int option ->
           int list -> Pdf.t -> drawops list -> Pdf.t
