(** Representing page content as objects without loss. *)

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

(** Content item  *)
type content =
  | Glyph of int
  | InlineImage
  | Image
  | Path
  | Shading
  | Clip

type bounding_box =
  Quad of float * float * float * float * float * float * float * float

type t =
  {bounding_box : bounding_box;
   content : content}

(** Filter objects based on a predicate on [t]. *)
val filter :
  pdf:Pdf.t ->
  f:(t -> bool) ->
  mediabox:(float * float * float * float) ->
  resources:Pdf.pdfobject ->
  ops:Pdfops.t list ->
  Pdfops.t list

val to_json :
  pdf:Pdf.t ->
  mediabox:(float * float * float * float) ->
  resources:Pdf.pdfobject ->
  ops:Pdfops.t list ->
  Cpdfyojson.Safe.t
