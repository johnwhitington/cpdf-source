(* Processing page content. *)
open Pdfutil

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

type content =
  | Glyph of int
  | InlineImage of Pdf.pdfobject * Pdfio.bytes
  | Image of string
  | Path of drawn_path
  | Shading of string
  | Clip (* This is just for -show-bboxes. Clipping exists in the state otherwise. *)

type bounding_box =
  Quad of float * float * float * float * float * float * float * float

type overlap =
  | Encloses
  | Intersects of bounding_box
  | Nonintersecting

type t =
  {bounding_box : bounding_box;
   content : content;
   state : state}

let initial_text_state () =
  {character_spacing = 0.;
   word_spacing = 0.;
   horizontal_scaling = 1.;
   leading = 0.;
   font = "";
   font_data =
     {fontobj = Pdftext.StandardFont (Pdftext.TimesRoman, Pdftext.WinAnsiEncoding);
      extra_metrics = (0., 0.);
      table = null_hash ();
      text_extractor = Some (Pdftext.text_extractor_of_font_real (Pdftext.StandardFont (Pdftext.TimesRoman, Pdftext.WinAnsiEncoding)))};
   font_cache = null_hash ();
   font_size = 12.;
   rendering_mode = 0;
   rise = 0.;
   knockout = true;
   t_m = Pdftransform.i_matrix;
   t_lm = Pdftransform.i_matrix}

let initial_state (minx, miny, maxx, maxy) =
  {mode = Graphics;
   ctm = Pdftransform.i_matrix;
   clipping_path =
     [(EvenOdd, [(Not_hole, Closed,
                 [Straight ((minx, miny), (minx, maxy));
                  Straight ((minx, maxy), (maxx, maxy));
                  Straight ((maxx, maxy), (maxx, miny));
                  Straight ((maxx, miny), (minx, miny))])])];
   partial_path = NoPartial;
   path = {filled = false; stroked = false; path = (EvenOdd, [])};
   colourspace_stroke = Pdfspace.DeviceGray;
   colourspace_non_stroke = Pdfspace.DeviceGray;
   colour_non_stroke = Floats [1.];
   colour_stroke = Floats [1.];
   text_state = initial_text_state ();
   line_width = 1.;
   line_cap = 0;
   line_join = 0;
   miter_limit = 10.;
   dash_pattern = ([], 0.);
   rendering_intent = "/RelativeColorimetric";
   stroke_adjustment = false;
   blend_mode = Pdf.Name "/Normal";
   soft_mask = Pdf.Null;
   alpha_constant_stroke = 1.;
   alpha_constant_non_stroke = 1.;
   alpha_source = false;
   black_point_compensation = "/Default";
   overprint_stroke = false;
   overprint_non_stroke = false;
   overprint_mode = 0;
   black_generation = Pdf.Null;
   undercolour_removal = Pdf.Null;
   transfer = Pdf.Null;
   halftone = Pdf.Null;
   flatness = 1.;
   smoothness = 0.5;
   d0 = None;
   d1 = None;
   marked_content_point = None;
   marked_content = []}

let compare_state a b =
  let te_a = a.text_state.font_data.text_extractor in
  let te_b = b.text_state.font_data.text_extractor in
    a.text_state.font_data.text_extractor <- None;
    b.text_state.font_data.text_extractor <- None;
    let r = compare a b in
      a.text_state.font_data.text_extractor <- te_a;
      b.text_state.font_data.text_extractor <- te_b;
      r

let copystate state =
  let text_state = {state.text_state with leading = state.text_state.leading} in
    {state with text_state}

let push_statestack statestack state =
  statestack =| copystate state

let pop_statestack statestack state =
  match !statestack with
  | [] -> Pdfe.log "Warning: Unbalanced q/Q Ops"
  | h::t -> statestack := t; state := h

(* Calculate the bounding box (xmin, xmax, ymin, ymax) of a path. *)
let bbox_of_segment = function
  | Straight ((x1, y1), (x2, y2)) ->
      fmin x1 x2, fmax x1 x2, fmin y1 y2, fmax y1 y2
  | Bezier ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) ->
      fmin (fmin x1 x2) (fmin x3 x4), fmax (fmax x1 x2) (fmax x3 x4),
      fmin (fmin y1 y2) (fmin y3 y4), fmax (fmax y1 y2) (fmax y3 y4)

let bbox_of_path (_, subpaths) =
  let segments =
    flatten (map (function (_, _, l) -> l) subpaths)
  in
    fold_left
      box_union_float
      (max_float, min_float, max_float, min_float)
      (map bbox_of_segment segments)

(* Eventually this will be replaced with one operating on Pdfspace.t, of course. *)
let rec initial_colour pdf resources = function
  | Pdf.Name "/DeviceGray"
  | Pdf.Array (Pdf.Name "/CalGray"::_) ->
      Floats [0.]
  | Pdf.Name "/DeviceRGB"
  | Pdf.Array (Pdf.Name "/CalRGB"::_) ->
      Floats [0.; 0.; 0.]
  | Pdf.Name "/DeviceCMYK" ->
      Floats [0.; 0.; 0.; 1.]
  | Pdf.Name "/Pattern"
  | Pdf.Array [Pdf.Name "/Pattern"] ->
      Floats [0.]
  | Pdf.Array elts as cs ->
      begin match elts with
        | [Pdf.Name "/ICCBased"; iccstream] ->
             begin match Pdf.lookup_direct pdf "/Alternate" iccstream with
             | Some space -> initial_colour pdf resources space
             | None ->
                 begin match Pdf.lookup_direct pdf "/N" iccstream with
                 | Some (Pdf.Integer 1) -> Floats [0.]
                 | Some (Pdf.Integer 3) -> Floats [0.; 0.; 0.]
                 | Some (Pdf.Integer 4) -> Floats [0.; 0.; 0.; 0.]
                 | _ -> Pdfe.log "Bad ICCBased Alternate"; Floats [0.]
                 end
             end
        | Pdf.Name "/Lab"::_ -> Floats [0.; 0.; 0.; 0.]
        | Pdf.Name "/DeviceN"::_::alternate::_ 
        | [Pdf.Name "/Separation"; _; alternate; _] ->
            initial_colour pdf resources alternate
        | [Pdf.Name "/Pattern"; alternate] ->
            initial_colour pdf resources alternate
        | _ -> Pdfe.log (Printf.sprintf "%s\n" (Pdfwrite.string_of_pdf cs)); Floats [0.]
      end
  | Pdf.Indirect _ as indirect ->
      initial_colour pdf resources (Pdf.direct pdf indirect)
  | Pdf.Name n ->
      begin match Pdf.lookup_chain pdf resources ["/ColorSpace"; n] with
      | Some cs -> initial_colour pdf resources cs
      | None -> Pdfe.logf "Can't find colourspace %s\n" n; Floats [0.]
      end
  | p ->
      Pdfe.logf "Unknown colourspace %s" (Pdfwrite.string_of_pdf p);
      Floats [0.]

let vertical = function
  | Pdftext.CIDKeyedFont (_, _, Pdftext.Predefined "/Identity-V") -> true
  | Pdftext.CIDKeyedFont (_, _, Pdftext.Predefined "/Identity-H") -> false
  | Pdftext.CIDKeyedFont (_, _, Pdftext.CMap {wmode}) -> wmode = 1
  | Pdftext.CIDKeyedFont _ as f ->
      Pdfe.logf "Cannot determine font direction for font %s\n" (Pdftext.string_of_font f);
      false
  | _ ->
      false

let width_of_charcode font charcode table =
  match font with
  | Pdftext.SimpleFont {Pdftext.fontmetrics = Some fontmetrics} ->
      let matrix =
        match font with
        | Pdftext.SimpleFont {Pdftext.fonttype = Pdftext.Type3 {fontmatrix}} -> fontmatrix
        | _ -> Pdftransform.i_matrix
      in
      begin try (fst (Pdftransform.transform_matrix matrix (fontmetrics.(charcode), 0.)), 0., 0.) with
        e ->
          Pdfe.log (Printf.sprintf "Unable to get width (%s, %s, %i)\n" (Printexc.to_string e) (Pdftext.string_of_font font) charcode);
          (0., 0., 0.)
      end
  | Pdftext.StandardFont (f, encoding) ->
      begin try (float_of_int (Pdfstandard14.charwidth encoding table f charcode), 0., 0.) with
        e ->
          Pdfe.log (Printf.sprintf "Unable to get width - StandardFont (%s, %s, %i)\n" (Printexc.to_string e) (Pdftext.string_of_font font) charcode);
          (0., 0., 0.)
      end
  | Pdftext.CIDKeyedFont (_, {cid_widths; cid_widths2; cid_default_width; cid_default_width2}, _) ->
      if vertical font then
        begin match Hashtbl.find_opt cid_widths2 charcode with
        | Some (w1, pvx, pvy) -> (w1, pvx, pvy)
        | None ->
            match Hashtbl.find_opt cid_widths charcode with
            | Some w0 -> (w0 /. 2., fst cid_default_width2, snd cid_default_width2)
            | None ->
                Pdfe.log (Printf.sprintf "Unable to get width for font (%s, %i)\n" (Pdftext.string_of_font font) charcode);
                (0., 0., 0.)
        end
      else
        begin match Hashtbl.find_opt cid_widths charcode with
        | Some f -> (f, 0., 0.)
        | None -> (cid_default_width, 0., 0.)
        end
  | _ ->
    Pdfe.log (Printf.sprintf "Unable to get width for font (%s, %i)\n" (Pdftext.string_of_font font) charcode);
    (0., 0., 0.)

let extract_num header s =
  match Pdfgenlex.lex_string (Hashtbl.find header s) with
  | [Pdfgenlex.LexInt i] -> Pdf.Integer i
  | [Pdfgenlex.LexReal f] -> Pdf.Real f
  | _ ->
      Pdfe.logf "extract_num: %s\n" s;
      Pdf.Integer 0

let extract_rectangle header s =
  match Pdfgenlex.lex_string (Hashtbl.find header s) with
  | [Pdfgenlex.LexInt _; LexInt y0; LexInt _; LexInt y1] -> (Pdf.Integer (min y0 y1), Pdf.Integer (max y0 y1))
  | _ ->
      Pdfe.logf "extract_rectangle: %s\n" s;
      (Pdf.Integer 0, Pdf.Integer 0)

let extra_metrics = function
  | Pdftext.SimpleFont {fonttype = Pdftext.Type3 {fontmatrix; fontbbox = (minx, miny, maxx, maxy)}} ->
      (snd (Pdftransform.transform_matrix fontmatrix (0., maxy)),
       snd (Pdftransform.transform_matrix fontmatrix (0., miny)))
  | Pdftext.SimpleFont {fontdescriptor = Some {ascent; descent; fontbbox}}
  | Pdftext.CIDKeyedFont (_, {cid_fontdescriptor = {ascent; descent; fontbbox}}, _) ->
      begin match fontbbox with
      | (0., 0., 0., 0.) -> (ascent, descent)
      | (_, miny, _, maxy) -> (miny, maxy)
      end
  | Pdftext.StandardFont (font, _) ->
      let header, _, _, _ = Pdfstandard14.afm_data font in
        let ascender, descender = try extract_rectangle header "FontBBox" with _ -> Pdf.Integer 0, Pdf.Integer 0 in
        let ascender, descender =
          if ascender = Pdf.Integer 0 && descender = Pdf.Integer 0 then
            (begin try extract_num header "Ascender" with _ -> Pdf.Integer 0 end,
            begin try extract_num header "Descender" with _ -> Pdf.Integer 0 end)
          else
            (ascender, descender)
        in
          begin match ascender with Pdf.Integer i -> float_of_int i | Pdf.Real r -> r | _ -> 0. end,
          begin match descender with Pdf.Integer i -> float_of_int i | Pdf.Real r -> r | _ -> 0. end
  | Pdftext.SimpleFont _ ->
      Pdfe.log "Missing fontdescriptor in SimpleFont";
      (0., 0.)

let charcodes_of_string s = function
    Pdftext.StandardFont _ | Pdftext.SimpleFont _ -> map int_of_char (explode s)
  | Pdftext.CIDKeyedFont _ ->
      try map (fun (a, b) -> int_of_char a lsl 8 lor int_of_char b) (pairs_of_list (explode s)) with Invalid_argument _ -> []

let string_of_charcodes cc = function 
    Pdftext.StandardFont _ | Pdftext.SimpleFont _ -> implode (map char_of_int cc)
  | Pdftext.CIDKeyedFont _ -> implode (flatten (map (fun x -> [char_of_int (x lsr 8); char_of_int (x land 0xFF)]) cc))

let rec optimise_capital_tj l =
  match cleavewhile (function Pdf.String _ -> true | _ -> false) l with
  | [], [] -> []
  | [], more ->
      begin match cleavewhile (function Pdf.Real _ -> true | _ -> false) l with
      | [], more -> optimise_capital_tj more
      | reals, more -> Pdf.Real (fold_left ( +. ) 0. (map (function Pdf.Real r -> r | _ -> assert false) reals))::optimise_capital_tj more
      end
  | strings, more -> Pdf.String (String.concat "" (map (function Pdf.String s -> s | _ -> assert false) strings))::optimise_capital_tj more

let optimise_tj = function
  | Pdfops.Op_Tj s -> Pdfops.Op_Tj s
  | Pdfops.Op_TJ l ->
      begin match optimise_capital_tj l with
      | [Pdf.String s] -> Pdfops.Op_Tj s
      | tj -> Pdfops.Op_TJ tj
      end
  | _ -> assert false

let process_tj ~f ~stack ~state ~resources s =
  let fontobj = !state.text_state.font_data.fontobj in
  let character_spacing = !state.text_state.character_spacing in
  let word_spacing = !state.text_state.word_spacing in
  let font_size = !state.text_state.font_size in
  let vertical = vertical fontobj in
  let debug = false in
  if debug then Printf.printf "process_tj %S\n" s;
  let chars = charcodes_of_string s fontobj in
  if debug then begin flprint "CHARS: "; iter (Printf.printf "%i ") chars; flprint "\n" end;
  let divisor =
    match fontobj with
    | Pdftext.SimpleFont {fonttype = Pdftext.Type3 _ } -> 1.
    | _ -> 1000.
  in
  let widths = map (fun x -> let w, pvx, pvy = width_of_charcode fontobj x !state.text_state.font_data.table in w /. divisor, pvx /. divisor, pvy /. divisor) chars in
  let ascent, descent = let a, b = !state.text_state.font_data.extra_metrics in (a /. divisor, b /. divisor) in
    if debug then
      begin
        flprint "WIDTHS: "; iter (fun (a, b, c) -> Printf.printf "%f (%f %f) " a b c) widths; flprint "\n";
        Printf.printf "ascent = %f, descent = %f\n" ascent descent
      end;
    let op_of_triples triples =
      if List.for_all (function (_, _, (Intersects _ | Encloses)) -> true | _ -> false) triples then
        let total_width =
          ~-.(fold_left ( +. ) 0. (map (fun (c, w, _) -> w +. character_spacing /. !state.text_state.font_size +. (if c = 32 then 1. else 0.) *. word_spacing /. !state.text_state.font_size) triples) *. 1000.)
        in
          Pdfops.Op_TJ [Pdf.Real total_width]
      else if List.for_all (function (_, _, Nonintersecting) -> true | _ -> false) triples then
        Pdfops.Op_Tj s
      else
        let compose_tj_group = function
          | [] -> assert false
          | (_, _, Nonintersecting)::_ as l -> Pdf.String (string_of_charcodes (map (fun (c, _, _) -> c) l) fontobj)
          | (_, _, (Encloses | Intersects _))::_ as l ->
              Pdf.Real (~-.(fold_left ( +. ) 0. (map (fun (c, w, _) -> w +. character_spacing /. !state.text_state.font_size +. (if c = 32 then 1. else 0.) *. word_spacing /. !state.text_state.font_size) l)) *. 1000.)
        in
        let is_different a b =
          neq (eq a Nonintersecting) (eq b Nonintersecting)
        in
          let groups = split_around_two (fun (_, _, f_result) (_, _, f_result') -> is_different f_result f_result') triples in
            Pdfops.Op_TJ (map compose_tj_group groups)
    in
    let triples =
      map2
        (fun c (w, pvx, pvy) ->
          let t_params =
            {Pdftransform.a = font_size *. !state.text_state.horizontal_scaling;
             Pdftransform.b = 0.;
             Pdftransform.c = 0.;
             Pdftransform.d = font_size;
             Pdftransform.e = 0.;
             Pdftransform.f = !state.text_state.rise}
          in
          let t_rm = Pdftransform.matrix_compose !state.ctm (Pdftransform.matrix_compose !state.text_state.t_m t_params) in
          let (x0, y0) = Pdftransform.transform_matrix t_rm (0. +. pvx, descent -. pvy) in
          let (x1, y1) = Pdftransform.transform_matrix t_rm (0. +. pvx, ascent -. pvy) in
          let (x2, y2) = Pdftransform.transform_matrix t_rm (w +. pvx, ascent -. pvy) in
          let (x3, y3) = Pdftransform.transform_matrix t_rm (w +. pvx, descent -. pvy) in
            if debug then begin flprint "BOX: "; Printf.printf "%f %f %f %f %f %f %f %f\n" x0 y0 x1 y1 x2 y2 x3 y3 end;
            let f_result = f {state = copystate !state; content = Glyph c; bounding_box = Quad (x0, y0, x1, y1, x2, y2, x3, y3)} in
            let tx =
              if vertical then 0. else
                (w *. font_size +. character_spacing +.
                (if c = 32 then 1. else 0.) *. word_spacing) *. !state.text_state.horizontal_scaling
            in
            let ty =
              if vertical then
                w *. font_size +. character_spacing +. word_spacing
              else
                0.
            in
              !state.text_state.t_m <- Pdftransform.matrix_compose !state.text_state.t_m (Pdftransform.mktranslate tx ty);
              (c, w, f_result))
        chars
        widths
      in
        op_of_triples triples

let process_capital_tj ~f ~stack ~state ~resources elts =
  let vertical = vertical !state.text_state.font_data.fontobj in
  let debug = false (* !state.text_state.font = "/C0_0" && vertical *) in
  if debug then flprint "process_capital_tj...\n";
  flatten
    (map
      (function
       | Pdf.String s ->
           (match process_tj ~f ~stack ~state ~resources s with Pdfops.Op_Tj s -> [Pdf.String s] | Pdfops.Op_TJ l -> l | _ -> assert false)
       | Pdf.Real n ->
           let tx = if vertical then 0.  else (~-.n /. 1000.) *. !state.text_state.horizontal_scaling *. !state.text_state.font_size in
           let ty = if vertical then (~-.n /. 1000.) *. !state.text_state.font_size else 0. in
             !state.text_state.t_m <- Pdftransform.matrix_compose !state.text_state.t_m (Pdftransform.mktranslate tx ty);
             [Pdf.Real n]
       | _ -> [])
      elts)

let read_tiling_pattern _ =
  ColouredTilingPattern Tiling

let read_function_shading pdf shading =
  let funshading_domain =
    match Pdf.lookup_direct pdf "/Domain" shading with
    | Some (Pdf.Array [a; b; c; d]) -> Pdf.getnum pdf a, Pdf.getnum pdf b, Pdf.getnum pdf c, Pdf.getnum pdf d
    | _ -> 0., 1., 0., 1.
  and funshading_matrix =
    Pdf.parse_matrix pdf "/Matrix" shading
  and funshading_function =
    try
      Pdffun.parse_function pdf (Pdf.lookup_fail "No function found" pdf "/Function" shading)
    with
      _ -> {Pdffun.func = Calculator []; domain = []; range = None}
  in
    FunctionShading
      {funshading_domain;
       funshading_matrix;
       funshading_function}

let read_radial_shading pdf shading =
  let radialshading_coords =
    match Pdf.lookup_direct pdf "/Coords" shading with
    | Some (Pdf.Array [a; b; c; d; e; f]) ->
        Pdf.getnum pdf a, Pdf.getnum pdf b, Pdf.getnum pdf c, Pdf.getnum pdf d, Pdf.getnum pdf e, Pdf.getnum pdf f
    | _ ->
        (0., 0., 0., 0., 0., 0.)
  and radialshading_domain =
    match Pdf.lookup_direct pdf "/Domain" shading with
    | Some (Pdf.Array [a; b]) -> Pdf.getnum pdf a, Pdf.getnum pdf b
    | _ -> 0., 1.
  and radialshading_function =
    match Pdf.lookup_direct pdf "/Function" shading with
    | Some (Pdf.Array fs) -> map (Pdffun.parse_function pdf) fs
    | Some f -> [Pdffun.parse_function pdf f]
    | _ -> [{Pdffun.func = Calculator []; domain = []; range = None}]
  and radialshading_extend =
    match Pdf.lookup_direct pdf "/Extend" shading with
    | Some (Pdf.Array [Pdf.Boolean a; Pdf.Boolean b]) -> a, b
    | _ -> false, false
  in
    RadialShading
      {radialshading_coords;
       radialshading_domain;
       radialshading_function;
       radialshading_extend}

let read_axial_shading pdf shading =
  let axialshading_coords =
    match Pdf.lookup_direct pdf "/Coords" shading with
    | Some (Pdf.Array [a; b; c; d]) ->
        Pdf.getnum pdf a, Pdf.getnum pdf b, Pdf.getnum pdf c, Pdf.getnum pdf d
    | _ -> 0., 0., 0., 0.
  and axialshading_domain =
    match Pdf.lookup_direct pdf "/Domain" shading with
    | Some (Pdf.Array [a; b]) -> Pdf.getnum pdf a, Pdf.getnum pdf b
    | _ -> 0., 1.
  and axialshading_function =
    match Pdf.lookup_direct pdf "/Function" shading with
    | Some (Pdf.Array fs) -> map (Pdffun.parse_function pdf) fs
    | Some f -> [Pdffun.parse_function pdf f]
    | _ -> [{Pdffun.func = Calculator []; domain = []; range = None}]
  and axialshading_extend =
    match Pdf.lookup_direct pdf "/Extend" shading with
    | Some (Pdf.Array [Pdf.Boolean a; Pdf.Boolean b]) -> a, b
    | _ -> false, false
  in
    AxialShading
      {axialshading_coords;
       axialshading_domain;
       axialshading_function;
       axialshading_extend}

(* Read a shading pattern *)
let read_shading pdf shading_matrix shading_extgstate shading =
  let shading_colourspace =
    match Pdf.lookup_direct pdf "/colourSpace" shading with 
    | Some cs -> cs
    | None -> Pdf.Null
  and shading_background =
    Pdf.lookup_direct pdf "/Background" shading
  and shading_bbox =
    Pdf.lookup_direct pdf "/BBox" shading
  and shading_antialias =
    match Pdf.lookup_direct pdf "/BBox" shading with
    | Some (Pdf.Boolean true) -> true
    | _ -> false
  in
    let shading =
      match Pdf.lookup_direct pdf "/ShadingType" shading with
      | Some Pdf.Integer 1 -> read_function_shading pdf shading
      | Some Pdf.Integer 3 -> read_radial_shading pdf shading
      | Some Pdf.Integer 2 -> read_axial_shading pdf shading
      | Some Pdf.Integer 4 -> FreeFormGouraudShading
      | Some Pdf.Integer 5 -> LatticeFormGouraudShading
      | Some Pdf.Integer 6 -> CoonsPatchMesh
      | Some Pdf.Integer 7 -> TensorProductPatchMesh
      | _ -> TensorProductPatchMesh
    in
      {shading_colourspace;
       shading_background;
       shading_bbox;
       shading_antialias;
       shading_matrix;
       shading_extgstate;
       shading}

let read_shading_pattern pdf p =
  let matrix = Pdf.parse_matrix pdf "/Matrix" p
  and extgstate =
    match Pdf.lookup_direct pdf "/ExtGState" p with
    | Some (Pdf.Dictionary _ as d) -> d
    | _ -> Pdf.Dictionary []
  in
    match Pdf.lookup_direct pdf "/Shading" p with
    | Some shading ->
        ShadingPattern (read_shading pdf matrix extgstate shading)
    | _ ->
        raise (Pdf.PDFError "No shading dictionary")

let read_pattern pdf resources name =
  match Pdf.lookup_direct pdf "/Pattern" resources with
  | None -> None
  | Some patterndict ->
      match Pdf.lookup_direct pdf name patterndict with
      | None -> None
      | Some pattern ->
          match Pdf.lookup_direct pdf "/PatternType" pattern with
          | Some (Pdf.Integer 1) -> Some (read_tiling_pattern pattern)
          | Some (Pdf.Integer 2) -> Some (read_shading_pattern pdf pattern)
          | _ -> None

let bbox_of_segment = function
  | Straight ((x1, y1), (x2, y2)) ->
      fmin x1 x2, fmax x1 x2, fmin y1 y2, fmax y1 y2
  | Bezier ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) ->
      fmin (fmin x1 x2) (fmin x3 x4), fmax (fmax x1 x2) (fmax x3 x4),
      fmin (fmin y1 y2) (fmin y3 y4), fmax (fmax y1 y2) (fmax y3 y4)

let bbox_of_path (_, subpaths) =
  let segments = flatten (map (function (_, _, l) -> l) subpaths) in
    if segments = [] then None else
      let boxes = map bbox_of_segment segments in
        Some (fold_left box_union_float (hd boxes) (tl boxes))

let transform_path m (w, subpaths) =
  let transform_point (x, y) = Pdftransform.transform_matrix m (x, y) in
  let transform_segment = function
  | Straight (a, b) -> Straight (transform_point a, transform_point b)
  | Bezier (a, b, c, d) -> Bezier (transform_point a, transform_point b, transform_point c, transform_point d)
  in
    (w, map (fun (h, c, segments) -> (h, c, map transform_segment segments)) subpaths)

(* Find the intersection of all the bboxes of all the clipping paths, to find the BBOX for bad shadings.*)
let smallest_clip_bbox paths =
  let bboxes = map bbox_of_path paths in
  let overlap = ref (hd bboxes) in
    iter
      (function
       | None -> overlap := None 
       | Some (minx, maxx, miny, maxy) ->
           overlap :=
             match !overlap with
             | None -> Some (minx, maxx, miny, maxy)
             | Some (ominx, omaxx, ominy, omaxy) ->
                 match box_overlap_float minx miny maxx maxy ominx ominy omaxx omaxy with
                 | None -> None
                 | Some (nminx, nminy, nmaxx, nmaxy) -> Some (nminy, nmaxx, nminy, nmaxy))
      (tl bboxes);
    !overlap

let emit_path_bounding_box ~content ~stroking ~f ~state =
  let bbox =
    match content with
    | Clip | Shading _ -> smallest_clip_bbox (map (transform_path !state.ctm) !state.clipping_path)
    | _ -> bbox_of_path (transform_path !state.ctm !state.path.path)
  in
    match bbox with
    | None -> 
        (*flprint "No bbox found in emit_path_bounding_box\n";*)
        Nonintersecting
    | Some (minx, maxx, miny, maxy) ->
        (*Printf.printf "found bbox in emit_path_bounding_box: %f %f %f %f\n" minx maxx miny maxy;*)
        let minx, maxx, miny, maxy =
          if stroking then
             let dx0, dy0 = Pdftransform.transform_matrix !state.ctm (0., 0.) in
             let dx1, dy1 = Pdftransform.transform_matrix !state.ctm (!state.line_width, !state.line_width) in
             let dx, dy = fabs (dx1 -. dx0), fabs (dy1 -. dy0) in
               (minx -. dx /. 2., maxx +. dx /. 2., miny -. dy /. 2., maxy +. dy /. 2.)
          else
            (minx, maxx, miny, maxy)
        in
          f ({state = copystate !state; content; bounding_box = Quad (minx, miny, minx, maxy, maxx, maxy, maxx, miny)})

(* Given an image object number and coordinates within that image, call out to imagemagick to redact the given rectangle. *)
let rec chop_image pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc imageobjnum ctm (x0, y0, x1, y1, x2, y2, x3, y3) =
  let image = Pdf.lookup_obj pdf imageobjnum in
  let w =
    match Pdf.lookup_direct pdf "/Width" image with
    | Some (Pdf.Integer i) -> i
    | _ -> raise Exit
  in
  let h =
    match Pdf.lookup_direct pdf "/Height" image with
    | Some (Pdf.Integer i) -> i
    | _ -> raise Exit
  in
  let minx, miny, maxx, maxy =
    let inverse = Pdftransform.matrix_invert ctm in
      let minx, miny = Pdftransform.transform_matrix inverse (x0, y0) in
      let maxx, maxy = Pdftransform.transform_matrix inverse (x2, y2) in
         minx *. float w, miny *. float h, maxx *. float w, maxy *. float h
  in
    Cpdfimage.redact pdf imageobjnum ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc (minx, miny, maxx, maxy)
  &&
    match Pdf.lookup_immediate "/Mask" image, Pdf.lookup_immediate "/SMask" image with
    | Some (Pdf.Indirect i), _ | _, Some (Pdf.Indirect i) ->
        chop_image pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc i ctm (x0, y0, x1, y1, x2, y2, x3, y3) 
    | _ -> true

(* TODO Allow this to expand operations, optionally e.g for -remove-xobjects.
   TODO Allow filtering on object, but also on ops (only one of these options at a time though).
   TODO Allow iter and map to save creating all the sublists?
   TODO Change to take all ops not just one op? Does this help with the compressor? *)
let rec process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources op =
  let remove : string -> unit = remove in
  match op with
  | Pdfops.Op_w f ->
      !state.line_width <- f;
      [op]
  | Pdfops.Op_J i ->
      !state.line_cap <- i;
      [op]
  | Pdfops.Op_j i ->
      !state.line_join <- i;
      [op]
  | Pdfops.Op_M f ->
      !state.miter_limit <- f;
      [op]
  | Pdfops.Op_d (fl, f) ->
      !state.dash_pattern <- (fl, f);
      [op]
  | Pdfops.Op_ri s ->
      !state.rendering_intent <- s;
      [op]
  | Pdfops.Op_i f ->
      !state.flatness <- f;
      [op]
  | Pdfops.Op_gs s ->
      read_graphics_state_dictionary ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources s;
      [op]
  | Pdfops.Op_q ->
      push_statestack stack !state;
      [op]
  | Pdfops.Op_Q ->
      pop_statestack stack state;
      [op]
  | Pdfops.Op_cm m ->
      !state.ctm <- Pdftransform.matrix_compose !state.ctm m;
      [op]
  | Pdfops.Op_m (x, y) ->
      begin match !state.partial_path with
      | PartialPath (sp, cp, segs, subpaths) ->
          !state.partial_path <-
            if segs = []
              then PartialPath ((x, y), (x, y), [], subpaths)
              else PartialPath ((x, y), (x, y), [], (Not_hole, Open, rev segs)::subpaths)
      | _ ->
          !state.partial_path <- PartialPath ((x, y), (x, y), [], [])
      end;
      [op]
  | Pdfops.Op_l (x, y) ->
      begin match !state.partial_path with 
      | PartialPath (sp, cp, segs, subpaths) ->
          !state.partial_path <- PartialPath (sp, (x, y), Straight (cp, (x, y))::segs, subpaths)
      | _ -> ()
      end;
      [op]
  | Pdfops.Op_c (a, b, c, d, e, f) ->
      begin match !state.partial_path with 
      | PartialPath (sp, cp, segs, subpaths) ->
          let ep = (e, f) in
          let curve = Bezier (cp, (a, b), (c, d), ep) in
            !state.partial_path <- PartialPath (sp, ep, curve::segs, subpaths)
      | _ -> ()
      end;
      [op]
  | Pdfops.Op_v (a, b, c, d) ->
      begin match !state.partial_path with 
      | PartialPath (sp, cp, segs, subpaths) ->
          let ep = (c, d) in
          let curve = Bezier (cp, cp, (a, b), ep) in
            !state.partial_path <- PartialPath (sp, ep, curve::segs, subpaths)
      | _ -> ()
      end;
      [op]
  | Pdfops.Op_y (a, b, c, d) ->
      begin match !state.partial_path with 
      | PartialPath (sp, cp, segs, subpaths) ->
          let ep = (c, d) in
          let curve = Bezier (cp, (a, b), ep, ep) in
            !state.partial_path <- PartialPath (sp, ep, curve::segs, subpaths)
      | _ -> ()
      end;
      [op]
  | Pdfops.Op_h ->
      begin match !state.partial_path with 
      | PartialPath (sp, cp, segs, subpaths) ->
          !state.partial_path <- PartialPath (sp, cp, [], (Not_hole, Closed, rev segs)::subpaths)
      | _ -> ()
      end;
      [op]
  | Pdfops.Op_s ->
      ignore (map (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources) [Pdfops.Op_h]);
      process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources Pdfops.Op_S
  | Pdfops.Op_b ->
      ignore (map (process_op ~pdf ~f ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~remove ~stack ~state ~resources) [Pdfops.Op_h]);
      process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources Pdfops.Op_B
  | Pdfops.Op_b' ->
      ignore (map (process_op ~pdf ~f ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~remove ~stack ~state ~resources) [Pdfops.Op_h]);
      process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources Pdfops.Op_B'
  | Pdfops.Op_f | Pdfops.Op_F ->
      ignore (process_op ~pdf ~f ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~remove ~stack ~state ~resources Pdfops.Op_h);
      begin match !state.partial_path with
      | PartialPath (sp, cp, segs, subpaths) ->
          (* segs is empty, due to [Op_h] *)
          !state.partial_path <- PartialPath (sp, cp, [], []);
          !state.path <- {filled = true; stroked = false; path = (NonZero, rev subpaths)};
          begin match emit_path_bounding_box ~content:(Path !state.path) ~stroking:false ~f ~state with
          | Nonintersecting -> [op]
          | Encloses | Intersects _ -> [Pdfops.Op_n]
          end
      | _ -> [op]
      end
  | Pdfops.Op_S ->
      begin match !state.partial_path with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = [] then
            begin
              !state.partial_path <- PartialPath (sp, cp, [], []);
              !state.path <- {filled = false; stroked = true; path = (EvenOdd, rev subpaths)}
            end
          else
            begin
              !state.partial_path <- PartialPath (sp, cp, [], []);
              !state.path <- {filled = false; stroked = true; path = (EvenOdd, rev ((Not_hole, Open, rev segs)::subpaths))}
            end;
          begin match emit_path_bounding_box ~content:(Path !state.path) ~stroking:true ~f ~state with
          | Nonintersecting -> [op]
          | Encloses | Intersects _ -> [Pdfops.Op_n]
          end
      | _ -> [op]
      end
  | Pdfops.Op_B ->
      begin match !state.partial_path with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = [] then
            begin
              !state.partial_path <- PartialPath (sp, cp, [], []);
              !state.path <- {filled = true; stroked = true; path = (NonZero, rev subpaths)}
            end
          else
            begin
              !state.partial_path <- PartialPath (sp, cp, [], []);
              !state.path <- {filled = true; stroked = true; path = (NonZero, rev ((Not_hole, Open, rev segs)::subpaths))}
            end;
          begin match emit_path_bounding_box ~content:(Path !state.path) ~stroking:true ~f ~state with
          | Nonintersecting -> [op]
          | Encloses | Intersects _ -> [Pdfops.Op_n]
          end
      | _ -> [op]
      end
  | Pdfops.Op_B' ->
      ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources Pdfops.Op_h);
      begin match !state.partial_path with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = [] then
            begin
              !state.partial_path <- PartialPath (sp, cp, [], []);
              !state.path <- {filled = true; stroked = true; path = (EvenOdd, rev subpaths)}
            end
          else
            begin
              !state.partial_path <- PartialPath (sp, cp, [], []);
              !state.path <- {filled = true; stroked = true; path = (EvenOdd, rev ((Not_hole, Open, rev segs)::subpaths))}
            end;
          begin match emit_path_bounding_box ~content:(Path !state.path) ~stroking:true ~f ~state with
          | Nonintersecting -> [op]
          | Encloses | Intersects _ -> [Pdfops.Op_n]
          end
      | _ -> [op]
      end
  | Pdfops.Op_f' ->
      begin match !state.partial_path with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = [] then
            begin
              !state.partial_path <- PartialPath (sp, cp, [], []);
              !state.path <- {filled = true; stroked = false; path = (EvenOdd, rev subpaths)}
            end
          else
            begin
              !state.partial_path <- PartialPath (sp, cp, [], []);
              !state.path <- {filled = true; stroked = false; path = (EvenOdd, rev ((Not_hole, Open, rev segs)::subpaths))}
            end;
          begin match emit_path_bounding_box ~content:(Path !state.path) ~stroking:false ~f ~state with
          | Nonintersecting -> [op]
          | Encloses | Intersects _ -> [Pdfops.Op_n]
          end
      | _ -> [op]
      end
  | Pdfops.Op_n ->
      ignore (emit_path_bounding_box ~content:Clip ~stroking:false ~f ~state);
      !state.partial_path <- NoPartial;
      [op]
  | Pdfops.Op_re (x, y, w, h) ->
      ignore
        (map
          (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources)
          [Pdfops.Op_m (x, y); Pdfops.Op_l (x +. w, y); Pdfops.Op_l (x +. w, y +. h); Pdfops.Op_l (x, y +. h); Pdfops.Op_h]);
      [op]
  | Pdfops.Op_W ->
      (* TODO Should we be removing clipping paths? For example, in 6408.pdf the path betrays the bicycle shape... Remove if wholly within rectangle? *)
      begin match !state.partial_path with
      | PartialPath (_, _, segments, subpaths) ->
          if segments = [] && subpaths = [] then () else
            let path =
              if segments <> []
                then (Not_hole, Closed, rev segments)::subpaths
                else subpaths
            in
              !state.clipping_path <- (NonZero, path)::!state.clipping_path
      | _ -> ()
      end;
      [op]
  | Pdfops.Op_W' ->
      begin match !state.partial_path with
      | PartialPath (_, _, segments, subpaths) ->
          if segments = [] && subpaths = [] then () else
            let path =
              if segments <> []
                then (Not_hole, Closed, rev segments)::subpaths
                else subpaths
            in
              !state.clipping_path <- (EvenOdd, path)::!state.clipping_path
      | _ -> ()
      end;
      [op]
  | Pdfops.Op_BT ->
      !state.text_state.t_m <- Pdftransform.i_matrix;
      !state.text_state.t_lm <- Pdftransform.i_matrix;
      !state.mode <- Text;
      [op]
  | Pdfops.Op_ET ->
      !state.mode <- Graphics;
      [op]
  | Pdfops.Op_Tc f ->
      !state.text_state.character_spacing <- f;
      [op]
  | Pdfops.Op_Tw f ->
      !state.text_state.word_spacing <- f;
      [op]
  | Pdfops.Op_Tz f ->
      !state.text_state.horizontal_scaling <- (f /. 100.);
      [op]
  | Pdfops.Op_TL f ->
      !state.text_state.leading <- f;
      [op]
  | Pdfops.Op_Tf (s, f) ->
      !state.text_state.font <- s;
      !state.text_state.font_size <- f;
      (* File redact/text.pdf fails with -show-bboxes and /F1 in main and
         XObject, so we remove caching for now. To be investigated... Code for
         Op_Do below looks correct, copying and clearing the cache, and copying
         back, so why? *)
      begin match None (*Hashtbl.find_opt !state.text_state.font_cache s*) with
      | Some font_data ->
          !state.text_state.font_data <- font_data
      | None ->
          begin match Pdf.lookup_direct pdf "/Font" resources with
          | Some fontdict ->
              begin match Pdf.lookup_direct pdf s fontdict with
              | Some font ->
                  let fontobj = Pdftext.read_font pdf font in
                  let font_data =
                    {fontobj;
                     extra_metrics = extra_metrics fontobj;
                     table =
                       begin match fontobj with
                       | Pdftext.StandardFont (_, encoding) -> Pdftext.table_of_encoding encoding
                       | _ -> null_hash ()
                       end;
                     text_extractor = Some (Pdftext.text_extractor_of_font_real fontobj)}
                  in
                    !state.text_state.font_data <- font_data;
                    Hashtbl.add !state.text_state.font_cache s font_data
              | None -> Pdfe.log "Font not found\n"
              end
          | None -> Pdfe.log "Font not found\n"
      end
      end;
      [op]
  | Pdfops.Op_Tr i ->
      !state.text_state.rendering_mode <- i;
      [op]
  | Pdfops.Op_Ts f ->
      !state.text_state.rise <- f;
      [op]
  | Pdfops.Op_Td (f1, f2) ->
      !state.text_state.t_lm <- Pdftransform.matrix_compose !state.text_state.t_lm (Pdftransform.mktranslate f1 f2);
      !state.text_state.t_m <- !state.text_state.t_lm;
      [op]
  | Pdfops.Op_TD (f1, f2) ->
      ignore (map (process_op ~pdf ~f ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~remove ~stack ~state ~resources) [(Pdfops.Op_TL ~-.f2); (Pdfops.Op_Td (f1, f2))]);
      [op]
  | Pdfops.Op_Tm m ->
      !state.text_state.t_m <- m;
      !state.text_state.t_lm <- m;
      [op]
  | Pdfops.Op_T' ->
      ignore (process_op ~pdf ~f ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~remove ~stack ~state ~resources (Pdfops.Op_TD (0., ~-.(!state.text_state.leading))));
      [op]
  | Pdfops.Op_Tj s ->
      [optimise_tj (process_tj ~f ~stack ~state ~resources s)]
  | Pdfops.Op_TJ l ->
      [Pdfops.Op_TJ (optimise_capital_tj (process_capital_tj ~f ~stack ~state ~resources l))]
  | Pdfops.Op_' s ->
      ignore (map (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources) [Pdfops.Op_T']);
      process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_Tj s)
  | Pdfops.Op_'' (f1, f2, s) ->
      ignore (map (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources) [(Pdfops.Op_Tw f1); (Pdfops.Op_Tc f2)]);
      process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_' s)
  | Pdfops.Op_d0 (f1, f2) ->
      !state.d0 <- Some (f1, f2);
      [op]
  | Pdfops.Op_d1 (f1, f2, f3, f4, f5, f6) ->
      !state.d1 <- Some (f1, f2, f3, f4, f5, f6);
      [op]
  | Pdfops.Op_CS s ->
      !state.colourspace_stroke <- Pdfspace.read_colourspace pdf resources (Pdf.Name s);
      !state.colour_stroke <- initial_colour pdf resources (Pdf.Name s);
      [op]
  | Pdfops.Op_cs s ->
      !state.colourspace_non_stroke <- Pdfspace.read_colourspace pdf resources (Pdf.Name s);
      !state.colour_non_stroke <- initial_colour pdf resources (Pdf.Name s);
      [op]
  | Pdfops.Op_SC fl | Pdfops.Op_SCN fl ->
      !state.colour_stroke <- Floats fl;
      [op]
  | Pdfops.Op_sc fl | Pdfops.Op_scn fl ->
      !state.colour_non_stroke <- Floats fl;
      [op]
  | Pdfops.Op_SCNName (s, fl) ->
      begin match !state.colourspace_non_stroke with
      | Pdfspace.Pattern | Pdfspace.PatternWithBaseColourspace _ ->
          begin match read_pattern pdf resources s with
          | Some pattern -> !state.colour_non_stroke <- Pattern pattern
          | None -> ()
          end
      | _ -> 
          !state.colour_non_stroke <- Named (s, fl)
      end;
      [op]
  | Pdfops.Op_scnName (s, fl) ->
      begin match !state.colourspace_stroke with
      | Pdfspace.Pattern | Pdfspace.PatternWithBaseColourspace _ ->
          begin match read_pattern pdf resources s with
          | Some pattern -> !state.colour_stroke <- Pattern pattern
          | None -> ()
          end
      | _ -> 
          !state.colour_stroke <- Named (s, fl)
      end;
      [op]
  | Pdfops.Op_G f ->
      !state.colourspace_stroke <- Pdfspace.DeviceGray;
      !state.colour_stroke <- Floats [f];
      [op]
  | Pdfops.Op_g f ->
      !state.colourspace_non_stroke <- Pdfspace.DeviceGray;
      !state.colour_non_stroke <- Floats [f];
      [op]
  | Pdfops.Op_RG (f1, f2, f3) ->
      !state.colourspace_stroke <- Pdfspace.DeviceRGB;
      !state.colour_stroke <- Floats [f1; f2; f3];
      [op]
  | Pdfops.Op_rg (f1, f2, f3) ->
      !state.colourspace_non_stroke <- Pdfspace.DeviceRGB;
      !state.colour_non_stroke <- Floats [f1; f2; f3];
      [op]
  | Pdfops.Op_K (f1, f2, f3, f4) ->
      !state.colourspace_stroke <- Pdfspace.DeviceCMYK;
      !state.colour_stroke <- Floats [f1; f2; f3; f4];
      [op]
  | Pdfops.Op_k (f1, f2, f3, f4) ->
      !state.colourspace_non_stroke <- Pdfspace.DeviceCMYK;
      !state.colour_non_stroke <- Floats [f1; f2; f3; f4];
      [op]
  | Pdfops.Op_sh s ->
      begin try
        let shadingdict = Pdf.lookup_fail "no /Shading" pdf "/Shading" resources in
        let shading = Pdf.lookup_fail "named shading not found" pdf s shadingdict in
        let shading = read_shading pdf Pdftransform.i_matrix Pdf.Null shading in
          begin match shading.shading_bbox with
          | Some r ->
              begin try
                let minx, miny, maxx, maxy = Pdf.parse_rectangle pdf r in
                  match f {state = copystate !state; content = Shading s; bounding_box = Quad (minx, miny, minx, maxy, maxx, maxy, maxx, miny)} with
                  | Nonintersecting -> [op]
                  | Intersects _ | Encloses -> []
              with
                _ -> [op]
              end
          | None ->
              (* This is an unbounded shading, not recommended. So we use the current clipping path. *)
              match emit_path_bounding_box ~content:(Shading s) ~stroking:false ~f ~state with
              | Nonintersecting -> [op]
              | Intersects _ | Encloses -> []
          end
      with
        _ -> [op]
      end
  | Pdfops.InlineImage (dict, _, data) ->
      let x0, y0 = Pdftransform.transform_matrix !state.ctm (0., 0.) in
      let x1, y1 = Pdftransform.transform_matrix !state.ctm (0., 1.) in
      let x2, y2 = Pdftransform.transform_matrix !state.ctm (1., 1.) in
      let x3, y3 = Pdftransform.transform_matrix !state.ctm (1., 0.) in
        begin match f {state = copystate !state; content = InlineImage (dict, data); bounding_box = Quad (x0, y0, x1, y1, x2, y2, x3, y3)} with
        | Nonintersecting -> [op]
        | _ -> []
        end
  | Pdfops.Op_Do s ->
      begin match Pdf.lookup_direct pdf "/XObject" resources with
      | Some d ->
          begin match Pdf.indirect_number pdf s d, Pdf.lookup_direct pdf s d with
          | Some xobjnum, Some xobj ->
              begin match Pdf.lookup_direct pdf "/Subtype" xobj with
              | Some (Pdf.Name "/Image") ->
                  let x0, y0 = Pdftransform.transform_matrix !state.ctm (0., 0.) in
                  let x1, y1 = Pdftransform.transform_matrix !state.ctm (0., 1.) in
                  let x2, y2 = Pdftransform.transform_matrix !state.ctm (1., 1.) in
                  let x3, y3 = Pdftransform.transform_matrix !state.ctm (1., 0.) in
                    begin match f {state = copystate !state; content = Image s; bounding_box = Quad (x0, y0, x1, y1, x2, y2, x3, y3)} with
                    | Encloses -> remove s; []
                    | Intersects (Quad (x0, y0, x1, y1, x2, y2, x3, y3)) ->
                        if chop_image pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc xobjnum !state.ctm (x0, y0, x1, y1, x2, y2, x3, y3) then [op] else
                          begin
                            Pdfe.log "Failed to chop image, removing whole image instead\n";
                            []
                          end
                    | Nonintersecting -> [op]
                    end
              | Some (Pdf.Name "/Form") ->
                  let matrix = Pdf.parse_matrix pdf "/Matrix" xobj in
                  let minx, miny, maxx, maxy =
                    match Pdf.lookup_direct pdf "/BBox" xobj with
                    | Some x ->
                        begin try Pdf.parse_rectangle pdf x with _ -> (min_float, min_float, max_float, max_float) end
                    | None -> 
                        (min_float, min_float, max_float, max_float)
                  in
                  let saved_font_cache = Hashtbl.copy !state.text_state.font_cache in
                    ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources Pdfops.Op_q);
                    !state.ctm <- Pdftransform.matrix_compose !state.ctm matrix;
                    ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_re (minx, miny, maxx -. minx, maxy -. miny)));
                    ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_W));
                    ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_n));
                    Hashtbl.clear !state.text_state.font_cache;
                    let to_remove = ref [] in
                    let ops = process_form_xobject ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove:(fun s -> to_remove := s::!to_remove) ~stack ~state ~resources xobj in
                    (* TODO Not clear what to do here in general. Content of
                       xobject will be processed each time. For redaction, we
                       might need to redact each time, so that is correct: any
                       use intersecting the area needs removal even when
                       shared. We need to return to the question of shared
                       content. Perhaps any redacted one needs automatically
                       copying to a new name? But what about xobjects of
                       xobjects and so on? *)
                    (*if !to_remove <> [] then
                      begin
                        Printf.printf "To remove at xobject level... ";
                        iter (Printf.printf "%s ") !to_remove;
                        flprint "\n";
                      end;*)
                    let resources' =
                      let xobjects =
                        match Pdf.lookup_chain pdf xobj ["/Resources"; "/XObject"] with
                        | Some (Pdf.Dictionary d) -> d
                        | _ -> []
                      in
                        let xobjects' = ref xobjects in
                          iter
                            (fun x ->
                              (* If it's in the resources, remove it. If it's not
                               there, it's an old-fashioned xobject with a resource
                               in the page. We must call the main page removal procedure
                               then! *)
                               if List.exists (function (k, _) -> k = x) !xobjects' then
                                 xobjects' := lose (function (k, _) -> k = x) !xobjects'
                               else
                                 remove x)
                            !to_remove;
                        let resources =
                          match Pdf.lookup_direct pdf "/Resources" xobj with
                          | Some d -> d
                          | _ -> Pdf.Dictionary []
                        in
                          Pdf.add_dict_entry resources "/XObject" (Pdf.Dictionary !xobjects')
                    in
                    let ops =
                      lose (function Pdfops.Op_Do n when mem n !to_remove -> true | _ -> false) ops
                    in
                    begin match xobj with
                    | Pdf.Stream ({contents = (dict, _)} as r) ->
                       begin match Pdfops.stream_of_ops ops with
                       | Pdf.Stream {contents = (_, Pdf.Got bytes)} ->
                           r := (Pdf.add_dict_entry dict "/Resources" resources', Pdf.Got bytes)
                       | _ -> assert false
                       end
                    | _ -> Pdfe.log "malformed stream"
                    end;
                    ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources Pdfops.Op_Q);
                    !state.text_state.font_cache <- saved_font_cache;
                    [op]
              | _ -> Pdfe.log "Unknown kind of xobject"; [op]
              end
          | _ -> Pdfe.log "Unknown xobject"; [op]
          end
      | None -> Pdfe.log "xobject not found"; [op]
      end
  | Pdfops.Op_MP s ->
      !state.marked_content_point <- Some (s, None);
      [op]
  | Pdfops.Op_DP (s, p) ->
      !state.marked_content_point <- Some (s, Some p);
      [op]
  | Pdfops.Op_BMC s ->
      !state.marked_content <- (s, None)::!state.marked_content;
      [op]
  | Pdfops.Op_BDC (s, p) ->
      !state.marked_content <- (s, Some p)::!state.marked_content;
      [op]
  | Pdfops.Op_EMC ->
      begin match !state.marked_content with
      | _::t -> !state.marked_content <- t
      | _ -> ()
      end;
      [op]
  | Pdfops.Op_BX ->
      [op]
  | Pdfops.Op_EX ->
      [op]
  | Pdfops.Op_Unknown _ ->
      [op]
  | Pdfops.Op_Comment _ ->
      [op]

and process_form_xobject ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources obj =
  let content = [Pdf.direct pdf obj] in
    let pagedict =
      match Pdf.direct pdf resources with
      | Pdf.Dictionary rs -> rs
      | _ -> []
    in
    let xobjdict =
      match Pdf.direct pdf obj with
      | Pdf.Stream {contents = (dict, _)} ->
          begin match Pdf.lookup_direct pdf "/Resources" dict with
          | Some (Pdf.Dictionary rs) -> rs
          | _ -> []
          end
      | _ -> []
    in
      let total_resources = Pdf.Dictionary (mergedict pagedict xobjdict) in
        flatten
          (map
            (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources:total_resources)
            (Pdfops.parse_operators pdf total_resources content))

and read_graphics_state_dictionary ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources s =
  let extgstate_dict =
    match Pdf.lookup_direct pdf "/ExtGState" resources with
    | Some (Pdf.Dictionary _ as d) -> d
    | _ -> Pdf.Dictionary []
  in
    begin match Pdf.lookup_direct pdf "/LW" extgstate_dict with
    | Some (Pdf.Real lw) -> ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_w lw))
    | Some (Pdf.Integer lw) -> ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_w (float_of_int lw)))
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/LC" extgstate_dict with
    | Some (Pdf.Integer lc) -> ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_J lc))
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/LJ" extgstate_dict with
    | Some (Pdf.Integer lj) -> ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_j lj))
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/ML" extgstate_dict with
    | Some (Pdf.Real ml) -> ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_M ml))
    | Some (Pdf.Integer ml) -> ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_M (float_of_int ml)))
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/D" extgstate_dict with
    | Some (Pdf.Array [a; phase]) ->
        let fs =
          try
            match Pdf.direct pdf a with | Pdf.Array a -> a | _ -> []
          with
            _ -> []
        in
          ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_d (map (Pdf.getnum pdf) fs, Pdf.getnum pdf phase)))
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/RI" extgstate_dict with
    | Some (Pdf.Name ri) -> ignore (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources (Pdfops.Op_ri ri))
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/OP" extgstate_dict with
    | Some (Pdf.Boolean op) ->
        begin match Pdf.lookup_direct pdf "/op" extgstate_dict with
        | Some (Pdf.Boolean _) ->
            !state.overprint_stroke <- op
        | _ ->
            !state.overprint_stroke <- op;
            !state.overprint_non_stroke <- op
        end
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/op" extgstate_dict with
    | Some (Pdf.Boolean op) -> !state.overprint_non_stroke <- op
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/op" extgstate_dict with
    | Some (Pdf.Integer opm) -> !state.overprint_mode <- opm
    | _ -> ()
    end;
    (* Font. Sadly this code is essentially copied from Op_Tf (minus the
    caching). At some point think if this can be combined (and cached). But
    really we need to find or craft an example first. *)
    begin match Pdf.lookup_direct pdf "/Font" extgstate_dict with
    | Some (Pdf.Array [Pdf.Indirect font; size]) ->
        begin match Pdf.getnum_opt pdf size with
        | Some f -> !state.text_state.font_size <- f
        | None -> ()
        end;
        !state.text_state.font <- "__EXTGSTATE__" ^ string_of_int font;
        let font = Pdf.direct pdf (Pdf.Indirect font) in
        let fontobj = Pdftext.read_font pdf font in
        let font_data =
          {fontobj;
           extra_metrics = extra_metrics fontobj;
           table =
             begin match fontobj with
             | Pdftext.StandardFont (_, encoding) -> Pdftext.table_of_encoding encoding
             | _ -> null_hash ()
             end;
           text_extractor = Some (Pdftext.text_extractor_of_font_real fontobj)}
        in
          !state.text_state.font_data <- font_data;
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/BG" extgstate_dict with
    | Some bg -> !state.black_generation <- bg
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/BG2" extgstate_dict with
    | Some bg2 -> !state.black_generation <- bg2
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/UCR" extgstate_dict with
    | Some ucr -> !state.undercolour_removal <- ucr
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/UCR2" extgstate_dict with
    | Some ucr2 -> !state.undercolour_removal <- ucr2
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/TR" extgstate_dict with
    | Some tr -> !state.transfer <- tr
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/TR2" extgstate_dict with
    | Some tr2 -> !state.transfer <- tr2
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/HT" extgstate_dict with
    | Some ht -> !state.halftone <- ht
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/FL" extgstate_dict with
    | Some (Pdf.Real fl) -> !state.flatness <- fl
    | Some (Pdf.Integer fl) -> !state.flatness <- float_of_int fl
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/SM" extgstate_dict with
    | Some (Pdf.Real sm) -> !state.smoothness <- sm
    | Some (Pdf.Integer sm) -> !state.smoothness <- float_of_int sm
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/SA" extgstate_dict with
    | Some (Pdf.Boolean sa) -> !state.stroke_adjustment <- sa
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/BM" extgstate_dict with
    | Some bm -> !state.blend_mode <- bm
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/SMask" extgstate_dict with
    | Some smask -> !state.halftone <- smask
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/CA" extgstate_dict with
    | Some (Pdf.Real ca) -> !state.alpha_constant_stroke <- ca
    | Some (Pdf.Integer ca) -> !state.alpha_constant_stroke <- float_of_int ca
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/ca" extgstate_dict with
    | Some (Pdf.Real ca) -> !state.alpha_constant_non_stroke <- ca
    | Some (Pdf.Integer ca) -> !state.alpha_constant_non_stroke <- float_of_int ca
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/AIS" extgstate_dict with
    | Some (Pdf.Boolean ais) -> !state.alpha_source <- ais
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/TK" extgstate_dict with
    | Some (Pdf.Boolean tk) -> !state.text_state.knockout <- tk
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/UseBlackPtComp" extgstate_dict with
    | Some (Pdf.Name bpc) -> !state.black_point_compensation <- bpc
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/HTO" extgstate_dict with
    | Some hto -> !state.halftone <- hto
    | _ -> ()
    end

(* Filter page content, given a predicate on page content. *)
let filter ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~mediabox ~resources ~ops =
  let stack = ref [] in
  let state = ref (initial_state mediabox) in
    flatten (map (process_op ~pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~f ~remove ~stack ~state ~resources) ops)

let rec postprocess_remove_empty_path_ops_inner a ops =
  match
    cleavewhile
      (function Pdfops.Op_m _ | Pdfops.Op_l _ | Pdfops.Op_c _ | Pdfops.Op_v _ | Pdfops.Op_y _ | Pdfops.Op_h | Pdfops.Op_re _  -> true | _ -> false)
      ops
  with
  | ([], []) -> rev a
  | ([], r) -> postprocess_remove_empty_path_ops_inner (hd r::a) (tl r)
  | (_, Pdfops.Op_n::r) -> postprocess_remove_empty_path_ops_inner a r
  | (pathops, l) -> postprocess_remove_empty_path_ops_inner (rev pathops @ a) l

let postprocess_remove_empty_path_ops =
  postprocess_remove_empty_path_ops_inner []

(* We run process_op over each op, losing any operation which doesn't alter the state.
   This is used, for example, to clean up redacted paths. And, of course, for efficiency. *)
(* Experimental, do not use. *)
(* FIXME: The problem here (and elsewhere) is that Form Xobjects are treated differently, rather than recursively using 'filter'. We should have filter take a copy of itself to call itself? For example, in 'compress' Form Xobjects would not be compressed....*)
(* FIXME: Check the fake 'f' here with Nonintersecting really is a no-op *)
(* FIXME: Other kinds of compression are available - for example this does not compress 'm l l l l S' into 're S'. See also the PDF 1.0 spec for additional ideas. *)
(* FIXME: Cleaning up paths - when an OP_n is removed, we have to scroll back and remove any path-creation ops. So this is not quite so simple.... *)
let compress ~pdf ~mediabox ~resources ~ops =
  let stack = ref [] in
  let state = ref (initial_state mediabox) in
  let opsout = ref [] in
    iter
      (fun op ->
         let effective_op = function
         | Pdfops.Op_Do _ | Op_sh _ | InlineImage _ | Op_MP _ | Op_DP _ | Op_BMC _ | Op_BDC _ | Op_EMC | Op_BX | Op_EX | Op_Unknown _ -> true
         | _ -> false
         in
         let old_state = copystate !state in
         let stack_length = length !stack in
           ignore (process_op ~pdf ~path_to_jbig2dec:"" ~path_to_convert:"" ~path_to_jbig2enc:"" ~f:(fun _ -> Nonintersecting) ~remove:(fun _ -> ()) ~stack ~state ~resources op);
           if compare_state !state old_state <> 0 || stack_length <> length !stack || effective_op op then opsout := op::!opsout)
      ops;
    (rev !opsout)

(* Export page content to JSON. One day this will be round-trippable. *)
let json_of_fpoint (x, y) =
  `Assoc [("x", `Float x); ("y", `Float y)]

let json_of_segment = function
  | Straight (a, b) -> `Assoc [("line", `List [json_of_fpoint a; json_of_fpoint b])]
  | Bezier (a, b, c, d) -> `Assoc [("bezier", `List [json_of_fpoint a; json_of_fpoint b; json_of_fpoint c; json_of_fpoint d])]

let json_of_subpath (_, closure, segments) =
  `Assoc [("closed", `Bool (closure = Closed));
          ("segments", `List (map json_of_segment segments))]

let json_of_path {filled; stroked; path = (winding, subpaths)} =
  `Assoc [("winding", `String ((function EvenOdd -> "even-odd" | NonZero -> "non-zero") winding));
          ("subpaths", `List (map json_of_subpath subpaths));
          ("stroked", `Bool stroked);
          ("filled", `Bool filled)]

let json_of_inline_image (dict, data) = `Null

let json_of_state_glyph state =
  `Assoc [("rendering mode", `Int state.text_state.rendering_mode);
          ("knockout", `Bool state.text_state.knockout);
          ("font", `String state.text_state.font);
          ("font size", `Float state.text_state.font_size);
          ("clipping path", `List (map json_of_path (map (function path -> {filled = false; stroked = false; path}) state.clipping_path)))]

let json_of_state_inline_image state =
  `Assoc [("clipping path", `List (map json_of_path (map (function path -> {filled = false; stroked = false; path}) state.clipping_path)))]

let json_of_state_image state =
  `Assoc [("clipping path", `List (map json_of_path (map (function path -> {filled = false; stroked = false; path}) state.clipping_path)))]

let json_of_colourspace = function
  | Pdfspace.DeviceGray -> `String "DeviceGray"
  | DeviceRGB -> `String "DeviceRGB"
  | DeviceCMYK -> `String "DeviceCMYK"
  | CalGray _ -> `String "CalGray"
  | CalRGB _ -> `String "CalRGB"
  | Lab _ -> `String "Lab"
  | ICCBased _ -> `String "ICCBased"
  | Indexed _ -> `String "Indexed"
  | Pattern -> `String "Pattern"
  | PatternWithBaseColourspace _ -> `String "PatternWithBaseColourspace"
  | Separation _ -> `String "Separation"
  | DeviceN _ -> `String "DeviceN"

let json_of_colvals = function
  | Floats fl -> `List (map (fun x -> `Float x) fl)
  | Named (s, fl) -> `List (`String s::map (fun x -> `Float x) fl)
  | Pattern _ -> `String "pattern"

let json_of_state_path state =
  `Assoc [("clipping path", `List (map json_of_path (map (function path -> {filled = false; stroked = false; path}) state.clipping_path)));
          ("colourspace stroke", json_of_colourspace state.colourspace_stroke);
          ("colourspace non stroke", json_of_colourspace state.colourspace_non_stroke);
          ("colour stroke", json_of_colvals state.colour_stroke);
          ("colour non stroke", json_of_colvals state.colour_non_stroke);
          ("line width", `Float state.line_width);
          ("line cap", `Int state.line_cap);
          ("line join", `Int state.line_join);
          ("dash pattern",
             let arr, phase = state.dash_pattern in
               `Assoc [("array", `List (map (fun x -> `Float x) arr));
                       ("phase", `Float phase)]
          );
          ("rendering intent", `String state.rendering_intent)]

let json_of_state_shading state =
  `Assoc [("clipping path", `List (map json_of_path (map (function path -> {filled = false; stroked = false; path}) state.clipping_path)))]

let json_of_state state = function
  | Glyph _ -> json_of_state_glyph state
  | InlineImage _ -> json_of_state_inline_image state
  | Image _ -> json_of_state_image state
  | Path _ -> json_of_state_path state
  | Shading _ -> json_of_state_shading state
  | Clip -> assert false (* Clipping path is part of the state. It exists in the content type only because we use it for -show-bboxes. *)

let json_of_object state = function
  | Glyph charcode ->
      let bytes =
        match state.text_state.font_data.fontobj with
        | Pdftext.CIDKeyedFont _ -> implode [char_of_int (charcode lsr 8); char_of_int (charcode land 0xFF)]
        | _ -> string_of_char (char_of_int charcode)
      in
        `Assoc
          [("obj", `String "glyph");
           ("charcode", `Int charcode);
           ("bytes", `String bytes);
           ("text", `String (Pdftext.utf8_of_codepoints (Pdftext.codepoints_of_text (unopt state.text_state.font_data.text_extractor) bytes)))]
  | InlineImage (dict, data) -> `Assoc [("obj", `String "inline image"); ("inline image", json_of_inline_image (dict, data))]
  | Image i -> `Assoc [("obj", `String "image"); ("image", `String i)]
  | Path p -> `Assoc [("obj", `String "path"); ("path", json_of_path p)]
  | Shading s -> `Assoc [("obj", `String "shading"); ("shading", `String s)]
  | Clip -> assert false (* Clipping path is part of the state. It exists in the content type only because we use it for -show-bboxes. *)

let to_json ~pdf ~mediabox ~resources ?(graphics=true) ?(text=true) ?(images=true) ops =
  let jsons = ref [] in
  let including = function
    | Clip -> false
    | Glyph _ -> text
    | InlineImage (_, _) | Image _ -> images
    | Path _ | Shading _ -> graphics
  in
  let f {state; content; bounding_box = Quad (x0, y0, x1, y1, x2, y2, x3, y3)} =
    if including content then
      jsons =|
        `Assoc ([("object", json_of_object state content);
                 ("state", json_of_state state content);
                 ("bbox", `List [`Float x0; `Float y0; `Float x1; `Float y1; `Float x2; `Float y2; `Float x3; `Float y3])]);
    Encloses
  in
    ignore (filter ~pdf ~path_to_jbig2dec:"" ~path_to_convert:"" ~path_to_jbig2enc:"" ~f ~remove:(fun _ -> ()) ~mediabox ~resources ~ops);
    `List (rev !jsons)

let charcodes_of_string s = function
    Pdftext.StandardFont _ | Pdftext.SimpleFont _ -> map int_of_char (explode s)
  | Pdftext.CIDKeyedFont _ ->
      try map (fun (a, b) -> int_of_char a lsl 8 lor int_of_char b) (pairs_of_list (explode s)) with Invalid_argument _ -> []

(* A very simple text extractor just for testing the text extraction we will need for -page-content and proof files. *)
let test_extract_text pdf range ch =
  let codepoints = ref [] in
    iter2
      (fun page pnum ->
         codepoints := [int_of_char '\n']::!codepoints;
         let f = function
           | {content = Glyph charcode; state} ->
               let bytes =
                 match state.text_state.font_data.fontobj with
                 | Pdftext.CIDKeyedFont _ -> implode [char_of_int (charcode lsr 8); char_of_int (charcode land 0xFF)]
                 | _ -> string_of_char (char_of_int charcode)
               in
               codepoints := Pdftext.codepoints_of_text (unopt state.text_state.font_data.text_extractor) bytes::!codepoints;
               Encloses
           | _ -> Encloses
         in
           if mem pnum range then
             ignore
               (filter
                 ~pdf ~path_to_jbig2dec:"" ~path_to_convert:"" ~path_to_jbig2enc:"" ~f ~remove:(fun _ -> ()) ~mediabox:(Pdf.parse_rectangle pdf page.Pdfpage.mediabox)
                 ~resources:page.Pdfpage.resources ~ops:(Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content)))
      (Pdfpage.pages_of_pagetree pdf)
      (ilist 1 (Pdfpage.endpage pdf));
      let utf8 = Pdftext.utf8_of_codepoints (flatten (rev !codepoints)) in
        output_string ch utf8;
        close_out ch
