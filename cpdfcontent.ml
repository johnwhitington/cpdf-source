(** Representing page content as objects without loss. *)

(* We run through the ops, doing all the work to process the page w.r.t
   graphics and text state.

   Initial aim: get bounding box of objects so we can redact them, outputting
   the stream with objects redacted.

   Final aim: page contents as objects free of graphics state, but without
   blow-ups (i.e keep xobjects) and fully round-trippable.

*)

(* Where are we in state diagram? Inline images already done in Pdfops, Shading
   and External are immediate and their states do not need representing. *)

type place = Path | Text | ClippingPath | Content

type text_state =
  {mutable character_spacing : float;
   mutable word_spacing : float;
   mutable horizontal_scaling : float;
   mutable leading : float;
   mutable font : string;
   mutable font_size : float;
   mutable rendering_mode : int;
   mutable rise : float;
   mutable knockout : bool;
   mutable t_m : Pdftransform.transform_matrix;
   mutable t_lm : Pdftransform.transform_matrix;
   mutable t_rm : Pdftransform.transform_matrix}

type state =
  {mutable ctm : Pdftransform.transform_matrix;
   mutable place : place;
   mutable clipping_path : float * float * float * float;
   mutable color_space : Pdf.pdfobject;
   mutable color : float list;
   mutable stroke_color : float list;
   mutable text_state : text_state;
   mutable line_width : float;
   mutable line_cap : int;
   mutable line_join : int;
   mutable miter_limit : float;
   mutable dash_pattern : float list * float;
   mutable rendering_intent : Pdf.pdfobject;
   mutable stroke_adjustment : bool;
   mutable blend_mode : Pdf.pdfobject;
   mutable soft_mask : Pdf.pdfobject;
   mutable alpha_constant : float;
   mutable alpha_source : bool;
   mutable black_point_compensation : Pdf.pdfobject;
   mutable overprint : bool;
   mutable overprint_mode : float;
   mutable black_generation : Pdf.pdfobject;
   mutable undercolor_removal : Pdf.pdfobject;
   mutable transfer : Pdf.pdfobject;
   mutable halftone : Pdf.pdfobject;
   mutable flatness : float;
   mutable smoothness : float}

let initial_text_state () =
  {character_spacing = 0.;
   word_spacing = 0.;
   horizontal_scaling = 100.;
   leading = 0.;
   font = "/Times-NewRoman";
   font_size = 12.;
   rendering_mode = 0;
   rise = 0.;
   knockout = true;
   t_m = Pdftransform.i_matrix;
   t_lm = Pdftransform.i_matrix;
   t_rm = Pdftransform.i_matrix}

let initial_state (minx, miny, maxx, maxy) =
  {ctm = Pdftransform.i_matrix;
   place = Content;
   clipping_path = (minx, miny, maxx, maxy);
   color_space = Pdf.Name "/DeviceGray";
   color = [1.];
   stroke_color = [1.];
   text_state = initial_text_state ();
   line_width = 1.;
   line_cap = 0;
   line_join = 0;
   miter_limit = 10.;
   dash_pattern = ([], 0.);
   rendering_intent = Pdf.Name "/RelativeColorimetric";
   stroke_adjustment = false;
   blend_mode = Pdf.Name "/Normal";
   soft_mask = Pdf.Null;
   alpha_constant = 1.;
   alpha_source = false;
   black_point_compensation = Pdf.Name "/Default";
   overprint = false;
   overprint_mode = 0.;
   black_generation = Pdf.Null;
   undercolor_removal = Pdf.Null;
   transfer = Pdf.Null;
   halftone = Pdf.Null;
   flatness = 1.;
   smoothness = 0.5}

(* An example, for redaction for now. We go through all the ops, then call f to
   see which objects to remove based on bounding box. This requires:

  a) Updating of matrices
  b) Calculation of bounding boxes for graphics (detecting graphics objects)
  c) Dealing with xobjects
  d) Calculation of bounding boxed for text (1. Simple - whole thing. Later,
  ability to split up text lines to remove just some glyphs.) *)
 
(* Return next object, list of ops consumed, remaining list *)
let rec next_object ~resources = function
  | Pdfops.Op_w f -> []
  | Pdfops.Op_J i -> []
  | Pdfops.Op_j i -> []
  | Pdfops.Op_M f -> []
  | Pdfops.Op_d (fl, f) -> []
  | Pdfops.Op_ri s -> []
  | Pdfops.Op_i i -> []
  | Pdfops.Op_gs s -> []
  | Pdfops.Op_q -> []
  | Pdfops.Op_Q -> []
  | Pdfops.Op_cm m -> []
  | Pdfops.Op_m (f1, f2) -> []
  | Pdfops.Op_l (f1, f2) -> []
  | Pdfops.Op_c (f1, f2, f3, f4, f5, f6) -> []
  | Pdfops.Op_v (f1, f2, f3, f4) -> []
  | Pdfops.Op_y (f1, f2, f3, f4) -> []
  | Pdfops.Op_h -> []
  | Pdfops.Op_re (f1, f2, f3, f4) -> []
  | Pdfops.Op_S -> []
  | Pdfops.Op_s -> []
  | Pdfops.Op_f -> []
  | Pdfops.Op_F -> []
  | Pdfops.Op_f' -> []
  | Pdfops.Op_B -> []
  | Pdfops.Op_B' -> []
  | Pdfops.Op_b -> []
  | Pdfops.Op_b' -> []
  | Pdfops.Op_n -> []
  | Pdfops.Op_W -> []
  | Pdfops.Op_W' -> []
  | Pdfops.Op_BT -> []
  | Pdfops.Op_ET -> []
  | Pdfops.Op_Tc f -> []
  | Pdfops.Op_Tw f -> []
  | Pdfops.Op_Tz f -> []
  | Pdfops.Op_TL f -> []
  | Pdfops.Op_Tf (s, f) -> []
  | Pdfops.Op_Tr i -> []
  | Pdfops.Op_Ts f -> []
  | Pdfops.Op_Td (f1, f2) -> []
  | Pdfops.Op_TD (f1, f2) -> []
  | Pdfops.Op_Tm m -> []
  | Pdfops.Op_T' -> []
  | Pdfops.Op_Tj s -> []
  | Pdfops.Op_TJ p -> []
  | Pdfops.Op_' s -> []
  | Pdfops.Op_'' (f1, f2, s) -> []
  | Pdfops.Op_d0 (f1, f2) -> []
  | Pdfops.Op_d1 (f1, f2, f3, f4, f5, f6) -> []
  | Pdfops.Op_CS s -> []
  | Pdfops.Op_cs s -> []
  | Pdfops.Op_SC fl -> []
  | Pdfops.Op_sc fl -> []
  | Pdfops.Op_SCN fl -> []
  | Pdfops.Op_scn fl -> []
  | Pdfops.Op_SCNName (s, fl) -> []
  | Pdfops.Op_scnName (s, fl) -> []
  | Pdfops.Op_G f -> []
  | Pdfops.Op_g f -> []
  | Pdfops.Op_RG (f1, f2, f3) -> []
  | Pdfops.Op_rg (f1, f2, f3) -> []
  | Pdfops.Op_K (f1, f2, f3, f4) -> []
  | Pdfops.Op_k (f1, f2, f3, f4) -> []
  | Pdfops.Op_sh s -> []
  | Pdfops.InlineImage i -> []
  | Pdfops.Op_Do s -> []
  | Pdfops.Op_MP s -> []
  | Pdfops.Op_DP (s, p) -> []
  | Pdfops.Op_BMC s -> []
  | Pdfops.Op_BDC (s, p) -> []
  | Pdfops.Op_EMC -> []
  | Pdfops.Op_BX -> []
  | Pdfops.Op_EX -> []
  | Pdfops.Op_Unknown s -> []
  | Pdfops.Op_Comment s -> []

let filter_ops ~f ~resources ~ops =
  let s = initial_state (0., 0., 500., 500.) in
  ops
