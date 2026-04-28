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

(** The kind of content being returned. *)
type content = Glyph | InlineImage | Image | Path | Shading

(** Filter ops based on a bounding-box predicate. *)
val filter_ops :
  pdf:Pdf.t ->
  f:(content * (float * float * float * float * float * float * float * float) -> bool) ->
  mediabox:(float * float * float * float) ->
  resources:Pdf.pdfobject -> ops:Pdfops.t list -> Pdfops.t list
