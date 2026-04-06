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
  {character_spacing : float;
   word_spacing : float;
   horizontal_scaling : float;
   leading : float;
   font : string;
   font_size : float;
   rendering_mode : int;
   rise : float;
   knockout : int;
   t_m : Pdftransform.transform_matrix;
   t_lm : Pdftransform.transform_matrix;
   t_rm : Pdftransform.transform_matrix}

type state =
  {ctm : Pdftransform.transform_matrix;
   place : place;
   clipping_path : unit;
   color_space : Pdf.pdfobject;
   color : unit;
   text_state : text_state;
   line_width : float;
   line_cap : int;
   line_join : int;
   miter_limit : int;
   dash_pattern : Pdf.pdfobject;
   rendering_intent : string;
   stroke_adjustment : bool;
   blend_mode : Pdf.pdfobject;
   soft_mask : Pdf.pdfobject;
   alpha_constant : float;
   alpha_source : float;
   black_point_compensation : string;
   overprint : bool;
   overprint_mode : float;
   black_generation : Pdf.pdfobject;
   undercolor_removal : Pdf.pdfobject;
   transfer : Pdf.pdfobject;
   halftone : Pdf.pdfobject;
   flatness : float;
   smoothness : float}

let filter_ops ~f ~resources ~ops = ops
