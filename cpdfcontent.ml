(** Representing page content as objects without loss. *)
open Pdfutil

(* We run through the ops, doing all the work to process the page w.r.t
   graphics and text state.

   Initial aim: get bounding box of objects so we can redact them, outputting
   the stream with objects redacted.

   Final aim: page contents as objects free of graphics state, but without
   blow-ups (i.e keep xobjects) and fully round-trippable.

*)

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

(* Where are we in state diagram? Inline images already done in Pdfops, Shading
   and External are immediate and their states do not need representing. *)

type place = Path | Text | ClippingPath | Content

type text_state =
  {mutable character_spacing : float;
   mutable word_spacing : float;
   mutable horizontal_scaling : float;
   mutable leading : float;
   mutable font : string;
   mutable fontobj : Pdftext.font;
   mutable font_size : float;
   mutable rendering_mode : int;
   mutable rise : float;
   mutable knockout : bool;
   mutable t_m : Pdftransform.transform_matrix;
   mutable t_lm : Pdftransform.transform_matrix}

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
   font = "";
   fontobj = Pdftext.StandardFont (Pdftext.TimesRoman, Pdftext.WinAnsiEncoding);
   font_size = 12.;
   rendering_mode = 0;
   rise = 0.;
   knockout = true;
   t_m = Pdftransform.i_matrix;
   t_lm = Pdftransform.i_matrix}

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

let copystate state =
  {state with ctm = state.ctm}

let push_statestack statestack state =
  statestack =| copystate state

let pop_statestack statestack state =
  match !statestack with
  | [] -> raise (Pdf.PDFError "Unbalanced q/Q Ops")
  | h::t -> statestack := t; state := h

type partial =
  | NoPartial
  | PartialPath of fpoint * fpoint * segment list * subpath list 

let rec initial_colour pdf resources = function
  | Pdf.Name "/DeviceGray"
  | Pdf.Array (Pdf.Name "/CalGray"::_) ->
      [0.]
  | Pdf.Name "/DeviceRGB"
  | Pdf.Array (Pdf.Name "/CalRGB"::_) ->
      [0.; 0.; 0.]
  | Pdf.Name "/DeviceCMYK" ->
      [0.; 0.; 0.; 1.]
  | Pdf.Name "/Pattern"
  | Pdf.Array [Pdf.Name "/Pattern"] ->
      [0.]
  | Pdf.Array elts as cs ->
      begin match elts with
        | [Pdf.Name "/ICCBased"; iccstream] ->
             begin match Pdf.lookup_direct pdf "/Alternate" iccstream with
             | Some space -> initial_colour pdf resources space
             | None ->
                 begin match Pdf.lookup_direct pdf "/N" iccstream with
                 | Some (Pdf.Integer 1) -> [0.]
                 | Some (Pdf.Integer 3) -> [0.; 0.; 0.]
                 | Some (Pdf.Integer 4) -> [0.; 0.; 0.; 0.]
                 | _ -> raise (Pdf.PDFError "Bad ICCBased Alternate")
                 end
             end
        | Pdf.Name "/DeviceN"::_::alternate::_ 
        | [Pdf.Name "/Separation"; _; alternate; _] ->
            initial_colour pdf resources alternate
        | [Pdf.Name "/Pattern"; alternate] ->
            initial_colour pdf resources alternate
        | _ -> Pdfe.log (Printf.sprintf "%s\n" (Pdfwrite.string_of_pdf cs)); raise (Pdf.PDFError "Unknown colourspace A")
      end
  | Pdf.Indirect _ as indirect ->
      initial_colour pdf resources (Pdf.direct pdf indirect)
  | _ -> raise (Pdf.PDFError "Unknown colourspace B")

(* An example, for redaction for now. We go through all the ops, then call f to
   see which objects to remove based on bounding box. This requires:

  a) Updating of matrices
  b) Calculation of bounding boxes for graphics (detecting graphics objects)
  c) Dealing with xobjects
  d) Calculation of bounding boxed for text (1. Simple - whole thing. Later,
  ability to split up text lines to remove just some glyphs.) *)

(* FIXME Move to Pdftext, get out of Cpdfaddtext. *)
let width_of_codepoint font codepoint =
  match font with
  | Pdftext.SimpleFont {Pdftext.fontmetrics = Some fontmetrics} ->
      fontmetrics.(codepoint)
  | Pdftext.StandardFont (f, _) ->
      let w = Pdfstandard14.textwidth false Pdftext.ImplicitInFontFile (* FIXME *) f (string_of_char (char_of_int codepoint)) in
        float_of_int w
  | f ->
    flprint (Pdftext.string_of_font f);
    Pdfe.log "\nwidth_of_codepoint: don't understand this font\n";
    0.

(* FIXME Move to Pdftext For finding the height for URL links, we try to find the Cap Height for the
   font. We fall back to using the font size alone if we cannot get the cap
   height. *)

(* Lex an integer from the table *)
let extract_num header s =
  match Pdfgenlex.lex_string (Hashtbl.find header s) with
    [Pdfgenlex.LexInt i] -> Pdf.Integer i
  | [Pdfgenlex.LexReal f] -> Pdf.Real f
  | _ -> raise (Failure ("extract_num: " ^ s))

let height = function
  | Pdftext.SimpleFont {fontdescriptor = Some {ascent}} ->
      ascent
  | Pdftext.StandardFont (font, _) ->
      let header, _, _, _ = Pdfstandard14.afm_data font in
        let height = try extract_num header "Ascender" with _ -> Pdf.Integer 0 in
          begin match height with Pdf.Integer i -> float_of_int i | Pdf.Real r -> r | _ -> 0. end
  | _ ->
      Pdfe.log "Height: unknown font";
      0.

let descender = function
  | Pdftext.SimpleFont {fontdescriptor = Some {descent}} ->
      descent
  | Pdftext.StandardFont (font, _) ->
      let header, _, _, _ = Pdfstandard14.afm_data font in
        let height = try extract_num header "Descender" with _ -> Pdf.Integer 0 in
          begin match height with Pdf.Integer i -> float_of_int i | Pdf.Real r -> r | _ -> 0. end
  | _ ->
      Pdfe.log "Height: unknown font";
      0.

let process_tj ~f ~stack ~state ~resources s =
  Printf.printf "process_tf %s\n" s;
  let text_extractor = Pdftext.text_extractor_of_font_real state.text_state.fontobj in
  let codepoints = Pdftext.codepoints_of_text text_extractor s in
  let widths = map (fun x -> (width_of_codepoint state.text_state.fontobj x *. state.text_state.font_size) /. 1000.) codepoints in
  let heights = map (fun x -> (height state.text_state.fontobj *. state.text_state.font_size) /. 1000.) codepoints in
  let descenders = map (fun x -> (descender state.text_state.fontobj *. state.text_state.font_size) /. 1000.) codepoints in
  (*iter (Printf.printf "%f ") widths; flprint "\n"; iter (Printf.printf "%f ") heights; flprint "\n"; iter (Printf.printf "%f ") descenders; flprint "\n";*)
    iter3
      (fun w h d ->
        let t_rm = Pdftransform.matrix_compose state.text_state.t_lm (Pdftransform.matrix_compose state.text_state.t_m state.ctm) in
        let (bl_x, bl_y) = Pdftransform.transform_matrix t_rm (0., d) in
        let (tr_x, tr_y) = Pdftransform.transform_matrix t_rm (w, h) in
          Printf.printf "Baseline position on page (%f, %f)\n" bl_x bl_y;
          f (bl_x, bl_y, tr_x, tr_y);
          state.text_state.t_m <- Pdftransform.matrix_compose (Pdftransform.mktranslate w 0.) state.text_state.t_m) 
      widths
      heights
      descenders

let process_capital_tj ~f ~stack ~state ~resources elts =
  iter
    (function
     | Pdf.String s ->
         process_tj ~f ~stack ~state ~resources s
     | Pdf.Real n ->
         state.text_state.t_m <- Pdftransform.matrix_compose (Pdftransform.mktranslate n 0.) state.text_state.t_m
     | _ -> ())
    elts

(* Return next object, list of ops consumed, remaining list *)
let rec process_op ~pdf ~f ~stack ~state ~resources = function
  | Pdfops.Op_w f -> ()
  | Pdfops.Op_J i -> ()
  | Pdfops.Op_j i -> ()
  | Pdfops.Op_M f -> ()
  | Pdfops.Op_d (fl, f) -> ()
  | Pdfops.Op_ri s -> ()
  | Pdfops.Op_i i -> ()
  | Pdfops.Op_gs s -> ()
  | Pdfops.Op_q -> ()
  | Pdfops.Op_Q -> ()
  | Pdfops.Op_cm m ->
      state.ctm <- Pdftransform.matrix_compose state.ctm m
  | Pdfops.Op_m (f1, f2) -> ()
  | Pdfops.Op_l (f1, f2) -> ()
  | Pdfops.Op_c (f1, f2, f3, f4, f5, f6) -> ()
  | Pdfops.Op_v (f1, f2, f3, f4) -> ()
  | Pdfops.Op_y (f1, f2, f3, f4) -> ()
  | Pdfops.Op_h -> ()
  | Pdfops.Op_re (f1, f2, f3, f4) -> ()
  | Pdfops.Op_S -> ()
  | Pdfops.Op_s -> ()
  | Pdfops.Op_f -> ()
  | Pdfops.Op_F -> ()
  | Pdfops.Op_f' -> ()
  | Pdfops.Op_B -> ()
  | Pdfops.Op_B' -> ()
  | Pdfops.Op_b -> ()
  | Pdfops.Op_b' -> ()
  | Pdfops.Op_n -> ()
  | Pdfops.Op_W -> ()
  | Pdfops.Op_W' -> ()
  | Pdfops.Op_BT ->
      state.text_state.t_m <- Pdftransform.i_matrix;
      state.text_state.t_lm <- Pdftransform.i_matrix
  | Pdfops.Op_ET ->
      ()
  | Pdfops.Op_Tc f -> ()
  | Pdfops.Op_Tw f -> ()
  | Pdfops.Op_Tz f -> ()
  | Pdfops.Op_TL f -> ()
  | Pdfops.Op_Tf (s, f) ->
      state.text_state.font <- s;
      state.text_state.font_size <- f;
      begin match Pdf.lookup_direct pdf "/Font" resources with
      | Some fontdict ->
          begin match Pdf.lookup_direct pdf s fontdict with
          | Some font -> state.text_state.fontobj <- Pdftext.read_font pdf font
          | None -> Pdfe.log "Font not found\n"
          end
      | None -> Pdfe.log "Font not found\n"
      end
  | Pdfops.Op_Tr i -> ()
  | Pdfops.Op_Ts f -> ()
  | Pdfops.Op_Td (f1, f2) -> ()
  | Pdfops.Op_TD (f1, f2) -> ()
  | Pdfops.Op_Tm m ->
      state.text_state.t_m <- m
  | Pdfops.Op_T' -> ()
  | Pdfops.Op_Tj s ->
      process_tj ~f ~stack ~state ~resources s
  | Pdfops.Op_TJ p ->
      process_capital_tj ~f ~stack ~state ~resources (match p with Pdf.Array a -> a | _ -> [])
  | Pdfops.Op_' s -> ()
  | Pdfops.Op_'' (f1, f2, s) -> ()
  | Pdfops.Op_d0 (f1, f2) -> ()
  | Pdfops.Op_d1 (f1, f2, f3, f4, f5, f6) -> ()
  | Pdfops.Op_CS s -> ()
  | Pdfops.Op_cs s -> ()
  | Pdfops.Op_SC fl -> ()
  | Pdfops.Op_sc fl -> ()
  | Pdfops.Op_SCN fl -> ()
  | Pdfops.Op_scn fl -> ()
  | Pdfops.Op_SCNName (s, fl) -> ()
  | Pdfops.Op_scnName (s, fl) -> ()
  | Pdfops.Op_G f -> ()
  | Pdfops.Op_g f -> ()
  | Pdfops.Op_RG (f1, f2, f3) -> ()
  | Pdfops.Op_rg (f1, f2, f3) -> ()
  | Pdfops.Op_K (f1, f2, f3, f4) -> ()
  | Pdfops.Op_k (f1, f2, f3, f4) -> ()
  | Pdfops.Op_sh s -> ()
  | Pdfops.InlineImage i -> ()
  | Pdfops.Op_Do s -> ()
  | Pdfops.Op_MP s -> ()
  | Pdfops.Op_DP (s, p) -> ()
  | Pdfops.Op_BMC s -> ()
  | Pdfops.Op_BDC (s, p) -> ()
  | Pdfops.Op_EMC -> ()
  | Pdfops.Op_BX -> ()
  | Pdfops.Op_EX -> ()
  | Pdfops.Op_Unknown s -> ()
  | Pdfops.Op_Comment s -> ()

(* Draft redactor. f is given the bbox and determines whether to delete or not. *)
let filter_ops ~pdf ~f ~mediabox ~resources ~ops =
  let stack : state list ref = ref [] in
  let state = ref (initial_state mediabox) in
    iter (fun op -> process_op ~pdf ~f ~stack ~state:!state ~resources op) ops;
    ops

let show_bounding_boxes pdf range =
  let show_bounding_boxes_page page =
    let ops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content in
    let page_boxes = ref [] in
      ignore (filter_ops ~pdf ~f:(fun box -> page_boxes =| box; false) ~mediabox:(Pdf.parse_rectangle pdf page.Pdfpage.mediabox) ~resources:page.Pdfpage.resources ~ops);
      !page_boxes
  in
  let bboxes = ref [] in
  let pdf =
    Cpdfpage.process_pages
      (Pdfpage.ppstub
        (fun pnum page -> if mem pnum range then (bboxes =| (pnum, show_bounding_boxes_page page); page) else page))
           pdf
           range
  in
    let pdf = ref pdf in
    let content_of_boxes boxes =
      [Pdfops.Op_w 0.5] @
      flatten (map (fun (minx, miny, maxx, maxy) -> [Pdfops.Op_re (minx, miny, maxx -. minx, maxy -. miny); Op_S]) boxes)
    in
      iter
        (fun (pnum, boxes) ->
           pdf := Cpdftweak.append_page_content (Pdfops.string_of_ops (content_of_boxes boxes)) false false [pnum] !pdf)
        !bboxes;
      !pdf
