(** Page content as objects *)

type fpoint = float * float

type winding_rule = EvenOdd | NonZero

type segment =
  | Straight of fpoint * fpoint
  | Bezier of fpoint * fpoint * fpoint * fpoint

type hole = Hole | Not_hole

type closure = Closed | Open

type subpath = hole * closure * segment list

type path = winding_rule * subpath list

type drawn_path =
  {stroked : bool;
   filled : bool;
   path : path}

type content =
  | Glyph of int
  | InlineImage of Pdf.pdfobject * Pdfio.bytes
  | Image of string
  | Path of drawn_path
  | Shading of string
  | Clip

type bounding_box =
  Quad of float * float * float * float * float * float * float * float

type overlap =
  | Encloses
  | Intersects of bounding_box
  | Nonintersecting

type partial =
  | NoPartial
  | PartialPath of fpoint * fpoint * segment list * subpath list 

type tiling = Tiling

type function_shading =
  {funshading_domain : float * float * float * float;
   funshading_matrix : Pdftransform.transform_matrix;
   funshading_function : Pdffun.t}

type radial_shading =
  {radialshading_coords : float * float * float * float * float * float;
   radialshading_domain : float * float;
   radialshading_function : Pdffun.t list;
   radialshading_extend : bool * bool}

type axial_shading =
  {axialshading_coords : float * float * float * float;
   axialshading_domain : float * float;
   axialshading_function : Pdffun.t list;
   axialshading_extend : bool * bool}

type shading_kind =
 | FunctionShading of function_shading
 | AxialShading of axial_shading
 | RadialShading of radial_shading
 | FreeFormGouraudShading
 | LatticeFormGouraudShading
 | CoonsPatchMesh
 | TensorProductPatchMesh

type shading =
 {shading_colourspace : Pdf.pdfobject;
  shading_background : Pdf.pdfobject option;
  shading_bbox : Pdf.pdfobject option;
  shading_antialias : bool;
  shading_matrix : Pdftransform.transform_matrix;
  shading_extgstate : Pdf.pdfobject;
  shading : shading_kind}

type pattern =
  | ColouredTilingPattern of tiling
  | UncolouredTilingPattern of tiling
  | ShadingPattern of shading

type colvals =
  | Floats of float list
  | Named of (string * float list)
  | Pattern of pattern

type font_data =
  {mutable fontobj : Pdftext.font;
   mutable extra_metrics : float * float;
   mutable table : (int, string) Hashtbl.t;
   mutable text_extractor : Pdftext.text_extractor option}

type text_state =
  {mutable character_spacing : float;
   mutable word_spacing : float;
   mutable horizontal_scaling : float;
   mutable leading : float;
   mutable font : string;
   mutable font_data : font_data;
   mutable font_cache : (string, font_data) Hashtbl.t;
   mutable font_size : float;
   mutable rendering_mode : int;
   mutable rise : float;
   mutable knockout : bool;
   mutable t_m : Pdftransform.transform_matrix;
   mutable t_lm : Pdftransform.transform_matrix}

type mode =
  | Graphics
  | Text

type state =
  {mutable mode : mode;
   mutable ctm : Pdftransform.transform_matrix;
   mutable partial_path : partial;
   mutable path : drawn_path;
   mutable clipping_path : path list;
   mutable colourspace_stroke : Pdfspace.t;
   mutable colourspace_non_stroke : Pdfspace.t;
   mutable colour_stroke : colvals;
   mutable colour_non_stroke : colvals;
   mutable text_state : text_state;
   mutable line_width : float;
   mutable line_cap : int;
   mutable line_join : int;
   mutable miter_limit : float;
   mutable dash_pattern : float list * float;
   mutable rendering_intent : string;
   mutable stroke_adjustment : bool;
   mutable blend_mode : Pdf.pdfobject;
   mutable soft_mask : Pdf.pdfobject;
   mutable alpha_constant_stroke : float;
   mutable alpha_constant_non_stroke : float;
   mutable alpha_source : bool;
   mutable black_point_compensation : string;
   mutable overprint_stroke : bool;
   mutable overprint_non_stroke : bool;
   mutable overprint_mode : int;
   mutable black_generation : Pdf.pdfobject;
   mutable undercolour_removal : Pdf.pdfobject;
   mutable transfer : Pdf.pdfobject;
   mutable halftone : Pdf.pdfobject;
   mutable flatness : float;
   mutable smoothness : float;
   mutable d0 : (float * float) option;
   mutable d1 : (float * float * float * float * float * float) option;
   mutable marked_content_point : (string * Pdf.pdfobject option) option;
   mutable marked_content : (string * Pdf.pdfobject option) list}

type t =
  {bounding_box : bounding_box;
   content : content;
   state : state}

type operation = Remove | Leave | Chop

type detection = Touching | Enclosing

type spec = operation * detection option

type helpers =
  {path_to_jbig2dec : string;
   path_to_convert : string;
   path_to_jbig2enc : string;
   color : Cpdfaddtext.colour;
   remove : string -> unit;
   text_spec : spec;
   image_spec : spec;
   inline_image_spec : spec;
   vector_spec : spec;
   annotation_spec : spec}

val empty_helpers : helpers

(** Filter objects based on a predicate on [t]. *)
val filter :
  pdf:Pdf.t ->
  helpers:helpers ->
  f:(t -> overlap) ->
  mediabox:(float * float * float * float) ->
  resources:Pdf.pdfobject ->
  ops:Pdfops.t list ->
  Pdfops.t list

val postprocess_remove_empty_path_ops : Pdfops.t list -> Pdfops.t list

(** Process graphics operations and output result as JSON. *)
val to_json :
  pdf:Pdf.t ->
  mediabox:(float * float * float * float) ->
  resources:Pdf.pdfobject ->
  ?graphics:bool ->
  ?text:bool ->
  ?images:bool ->
  Pdfops.t list ->
  Cpdfyojson.Safe.t

(**/**)
val test_extract_text : Pdf.t -> int list -> out_channel -> unit

val compress :
  pdf:Pdf.t ->
  mediabox:(float * float * float * float) ->
  resources:Pdf.pdfobject ->
  ops:Pdfops.t list ->
  Pdfops.t list
