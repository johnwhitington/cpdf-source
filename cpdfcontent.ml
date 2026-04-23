(** Processing page content. *)
open Pdfutil

(* Initial aim: get bounding box of objects so we can redact them, outputting
   the stream with objects redacted.

   Future aim: page contents as objects free of graphics state, but without
   blow-ups (i.e keep xobjects) and fully round-trippable. *)

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

type content = Glyph | InlineImage | Image | Path

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
   mutable rendering_intent : string;
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
   horizontal_scaling = 1.;
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
   rendering_intent = "/RelativeColorimetric";
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
  let text_state = {state.text_state with leading = state.text_state.leading} in
    {state with text_state}

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

let width_of_charcode font charcode =
  match font with
  | Pdftext.SimpleFont {Pdftext.fontmetrics = Some fontmetrics} ->
      let matrix =
        match font with
        | Pdftext.SimpleFont {Pdftext.fonttype = Pdftext.Type3 {fontmatrix}} ->
            (*Printf.printf "charcode = %i\n" charcode;*)
            fontmatrix
        | _ -> Pdftransform.i_matrix
      in
      begin try
        (*Printf.printf "width = %f\n" fontmetrics.(charcode);*)
        let final = fst (Pdftransform.transform_matrix matrix (fontmetrics.(charcode), 0.)) in
          (*Printf.printf "final width = %f\n" final;*) final
      with
        e -> Pdfe.log (Printf.sprintf "Unable to get width (%s, %s, %i)\n" (Printexc.to_string e) (Pdftext.string_of_font font) charcode); 0.
      end
  | Pdftext.StandardFont (f, encoding) ->
      begin try
        let w = Pdfstandard14.textwidth false encoding f (string_of_char (char_of_int charcode)) in
          float_of_int w
      with
        e ->
          Pdfe.log (Printf.sprintf "Unable to get width - StandardFont (%s, %s, %i)\n" (Printexc.to_string e) (Pdftext.string_of_font font) charcode);
          0.
      end
  | Pdftext.CIDKeyedFont (_, {cid_widths; cid_default_width}, _) ->
      begin match lookup charcode cid_widths with
      | Some f -> f
      | None -> cid_default_width
      end
  | f ->
    Pdfe.log (Printf.sprintf "Unable to get width for font (%s, %i)\n" (Pdftext.string_of_font f) charcode);
    0.

let extract_num header s =
  match Pdfgenlex.lex_string (Hashtbl.find header s) with
  | [Pdfgenlex.LexInt i] -> Pdf.Integer i
  | [Pdfgenlex.LexReal f] -> Pdf.Real f
  | _ -> raise (Failure ("extract_num: " ^ s))

let extract_rectangle header s =
  match Pdfgenlex.lex_string (Hashtbl.find header s) with
  | [Pdfgenlex.LexInt _; LexInt y0; LexInt _; LexInt y1] -> (Pdf.Integer (min y0 y1), Pdf.Integer (max y0 y1))
  | _ -> raise (Failure ("extract_rectangle: " ^ s))

let extra_metrics = function
  | Pdftext.SimpleFont {fonttype = Pdftext.Type3 {fontbbox = (minx, miny, maxx, maxy)}} ->
      (*Printf.printf "Finding ascent, descent for Type 3 char %f %f %f %f\n" minx miny maxx maxy;*)
      (miny, maxy)
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

let tx ~state w c tj =
  ((w -. tj /. 1000.) *. !state.text_state.font_size +. !state.text_state.character_spacing +.
  (if c = 32 then 1. else 0.) *. !state.text_state.word_spacing) *. !state.text_state.horizontal_scaling

let tx2 ~state tj =
  (~-.tj /. 1000.) *. !state.text_state.horizontal_scaling *. !state.text_state.font_size

let charcodes_of_string font s =
  match font with
  | Pdftext.StandardFont _ | Pdftext.SimpleFont _ -> map int_of_char (explode s)
  | Pdftext.CIDKeyedFont _ -> map int_of_char (pair (fun a b -> b) (explode s)) (* Just Identity H for now *)

let process_tj ~f ~stack ~state ~resources s =
  (*Printf.printf "process_tj %S\n" s;*)
  let chars = charcodes_of_string !state.text_state.fontobj s in
  (*flprint "CHARS: "; iter (Printf.printf "%i ") chars; flprint "\n";*)
  let widths = map (fun x -> (width_of_charcode !state.text_state.fontobj x) /. 1000.) chars in
  let ascent, descent = let a, b = extra_metrics !state.text_state.fontobj in (a /. 1000., b /. 1000.) in
  (*flprint "WIDTHS: "; iter (Printf.printf "%f ") widths; flprint "\n";
  Printf.printf "ascent = %f, descent = %f\n" ascent descent;*)
    iter2
      (fun c w ->
        let t_params =
          {Pdftransform.a = !state.text_state.font_size *. !state.text_state.horizontal_scaling;
           Pdftransform.b = 0.;
           Pdftransform.c = 0.;
           Pdftransform.d = !state.text_state.font_size;
           Pdftransform.e = 0.;
           Pdftransform.f = !state.text_state.rise}
        in
        let t_rm = Pdftransform.matrix_compose !state.ctm (Pdftransform.matrix_compose !state.text_state.t_m t_params) in
        let (x0, y0) = Pdftransform.transform_matrix t_rm (0., descent) in
        let (x1, y1) = Pdftransform.transform_matrix t_rm (0., ascent) in
        let (x2, y2) = Pdftransform.transform_matrix t_rm (w, ascent) in
        let (x3, y3) = Pdftransform.transform_matrix t_rm (w, descent) in
          (*Printf.printf "Baseline position on page (%f, %f)\n" bl_x bl_y;*)
          (*flprint "BOX: "; Printf.printf "%f %f %f %f %f %f %f %f\n" x0 y0 x1 y1 x2 y2 x3 y3;*)
          f (Glyph, (x0, y0, x1, y1, x2, y2, x3, y3));
          !state.text_state.t_m <- Pdftransform.matrix_compose !state.text_state.t_m (Pdftransform.mktranslate (tx ~state w c 0.) 0.))
      chars
      widths

let process_capital_tj ~f ~stack ~state ~resources elts =
  iter
    (function
     | Pdf.String s ->
         process_tj ~f ~stack ~state ~resources s
     | Pdf.Real n ->
         !state.text_state.t_m <- Pdftransform.matrix_compose !state.text_state.t_m (Pdftransform.mktranslate (tx2 ~state n) 0.)
     | _ -> ())
    elts

let read_graphics_state_dictionary ~pdf ~state s =
  ()

(* Return next object, list of ops consumed, remaining list *)
let rec process_op ~pdf ~f ~stack ~state ~resources = function
  | Pdfops.Op_w f ->
      !state.line_width <- f
  | Pdfops.Op_J i ->
      !state.line_cap <- i
  | Pdfops.Op_j i ->
      !state.line_join <- i
  | Pdfops.Op_M f ->
      !state.miter_limit <- f
  | Pdfops.Op_d (fl, f) ->
      !state.dash_pattern <- (fl, f)
  | Pdfops.Op_ri s ->
      !state.rendering_intent <- s
  | Pdfops.Op_i f ->
      !state.flatness <- f
  | Pdfops.Op_gs s ->
      read_graphics_state_dictionary ~pdf ~state s
  | Pdfops.Op_q ->
      push_statestack stack !state
  | Pdfops.Op_Q ->
      pop_statestack stack state
  | Pdfops.Op_cm m ->
      !state.ctm <- Pdftransform.matrix_compose !state.ctm m
  | Pdfops.Op_m (x, y) ->
      (* Begin a new subpath. Get into path mode if not already there. If the last op was an
      [Op_m], it should have no effect. *)
      (*(!state).objectclass <- PathObject;
      begin match partial with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = []
            then PartialPath ((x, y), (x, y), [], subpaths), graphic
            else PartialPath ((x, y), (x, y), [], (Not_hole, Open, rev segs)::subpaths), graphic
      | _ ->
          PartialPath ((x, y), (x, y), [], []), graphic
      end*)()
  | Pdfops.Op_l (x, y) ->
      (*if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_l");
      begin match partial with 
      | PartialPath (sp, cp, segs, subpaths) ->
          PartialPath (sp, (x, y), Straight (cp, (x, y))::segs, subpaths), graphic
      | _ ->
          raise (Pdf.PDFError "Pdfgraphics: Op_l")
      end*)()
  | Pdfops.Op_c (a, b, c, d, e, f) ->
      (*if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_c");
      begin match partial with 
      | PartialPath (sp, cp, segs, subpaths) ->
          let ep = (e, f) in
            let curve = Bezier (cp, (a, b), (c, d), ep) in
              PartialPath (sp, ep, curve::segs, subpaths), graphic
      | _ ->
          raise (Pdf.PDFError "Pdfgraphics: Op_c")
      end*)()
  | Pdfops.Op_v (a, b, c, d) ->
      (*if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_v");
      begin match partial with 
      | PartialPath (sp, cp, segs, subpaths) ->
          let ep = (c, d) in
            let curve = Bezier (cp, cp, (a, b), ep) in
              PartialPath (sp, ep, curve::segs, subpaths), graphic
      | _ ->
          raise (Pdf.PDFError "Pdfgraphics: Op_v")
      end*)()
  | Pdfops.Op_y (a, b, c, d) ->
      (*if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_y");
      begin match partial with 
      | PartialPath (sp, cp, segs, subpaths) ->
          let ep = (c, d) in
            let curve = Bezier (cp, (a, b), ep, ep) in
              PartialPath (sp, ep, curve::segs, subpaths), graphic
      | _ ->
          raise (Pdf.PDFError "Pdfgraphics: Op_y")
      end*)()
  | Pdfops.Op_h ->
      (*if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_h - not in PathObject");
      begin match partial with 
      | PartialPath (sp, cp, segs, subpaths) ->
          PartialPath (sp, cp, [], (Not_hole, Closed, rev segs)::subpaths), graphic
      | _ ->
          raise (Pdf.PDFError "Pdfgraphics: Op_h - not a partial path")
      end*)()
  | Pdfops.Op_s ->
      (* Close and stroke. Equivalent to h S *)
      (*process_ops pdf page ret [Pdfops.Op_h; Pdfops.Op_S]*)()
  | Pdfops.Op_b ->
      (* Close, fill, stroke, nonzero. Equivalent to h B *)
      (*process_ops pdf page ret [Pdfops.Op_h; Pdfops.Op_B]*)()
  | Pdfops.Op_b' ->
      (* Close, fill, stroke, evenodd. Equivalent to h B* *)
      (*process_ops pdf page ret [Pdfops.Op_h; Pdfops.Op_B']*)()
  | Pdfops.Op_f | Pdfops.Op_F ->
      (*(* Close and Fill non-zero *)
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_f");
      let partial, graphic = process_op pdf page (partial, graphic) Pdfops.Op_h in
        (!state).objectclass <- PageDescriptionLevel;
        begin match partial with
        | PartialPath (sp, cp, segs, subpaths) ->
            (* segs is empty, due to [Op_h] *)
            PartialPath (sp, cp, [], []),
            Path ((NonZero, rev subpaths), path_attributes_fill ())::graphic
        | _ ->
           raise (Pdf.PDFError "Pdfgraphics: Op_f")
        end*)()
  | Pdfops.Op_S ->
      (* Stroke *)
      (*if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_S");
      (!state).objectclass <- PageDescriptionLevel;
      begin match partial with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = [] then
            PartialPath (sp, cp, [], []),
            Path ((EvenOdd, rev subpaths), path_attributes_stroke ())::graphic
          else
            PartialPath (sp, cp, [], []),
            Path ((EvenOdd, rev ((Not_hole, Open, rev segs)::subpaths)), path_attributes_stroke ())::graphic
      | _ ->
         raise (Pdf.PDFError "Pdfgraphics: Op_S")
      end*)()
  | Pdfops.Op_B ->
      (* Fill and stroke, non-zero. *)
      (*if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_B");
      (!state).objectclass <- PageDescriptionLevel;
      begin match partial with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = [] then
            PartialPath (sp, cp, [], []),
            Path ((NonZero, rev subpaths), path_attributes_fill_and_stroke ())::graphic
          else
            PartialPath (sp, cp, [], []),
            Path ((NonZero, rev ((Not_hole, Open, rev segs)::subpaths)), path_attributes_fill_and_stroke ())
            ::graphic
      | _ ->
        raise (Pdf.PDFError "Pdfgraphics: Op_B")
      end*)()
  | Pdfops.Op_B' ->
      (* Fill and stroke, even-odd. *)
      (*if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_B*");
      let partial, graphic = process_op pdf page (partial, graphic) Pdfops.Op_h in
        (!state).objectclass <- PageDescriptionLevel;
        begin match partial with
        | PartialPath (sp, cp, segs, subpaths) ->
            if segs = [] then
              PartialPath (sp, cp, [], []),
              Path ((EvenOdd, rev subpaths), path_attributes_fill_and_stroke ())::graphic
            else
              PartialPath (sp, cp, [], []),
              Path ((EvenOdd, rev ((Not_hole, Open, rev segs)::subpaths)), path_attributes_fill_and_stroke ())
              ::graphic
        | _ ->
           raise (Pdf.PDFError "Pdfgraphics: Op_B*")
        end*)
      ()
  | Pdfops.Op_f' ->
      (* Fill, even-odd *)
      (*if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_f*");
      (!state).objectclass <- PageDescriptionLevel;
      begin match partial with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = [] then
            PartialPath (sp, cp, [], []),
            Path ((EvenOdd, rev subpaths), path_attributes_fill ())::graphic
          else
            PartialPath (sp, cp, [], []),
            Path ((EvenOdd, rev ((Not_hole, Open, rev segs)::subpaths)), path_attributes_fill ())
            ::graphic
      | _ ->
         raise (Pdf.PDFError "Pdfgraphics: Op_f*")
      end*)()
  | Pdfops.Op_n ->
      (*(* no-op *)
      (!state).objectclass <- PageDescriptionLevel;
      (* for now, until we support clipviews, clean up the polygon *)
      (NoPartial, graphic)*)()
  | Pdfops.Op_re (x, y, w, h) ->
      (* Rectangle. *)
      (*let ops =
        [Pdfops.Op_m (x, y);
         Pdfops.Op_l (x +. w, y);
         Pdfops.Op_l (x +. w, y +. h);
         Pdfops.Op_l (x, y +. h);
         Pdfops.Op_h]
      in
        process_ops pdf page (partial, graphic) ops*)()
  | Pdfops.Op_W ->
      (* Move the current partial path into Clip, and return *)
      (*begin match partial with
      | PartialPath (_, _, segments, subpaths) ->
          if segments = [] && subpaths = [] then ret else
            let path =
              if segments <> []
                then (Not_hole, Closed, rev segments)::subpaths
                else subpaths
            in
              (!state).clip <- Some (NonZero, path); ret
      | _ -> ret
      end*)()
      (* FIXME: In NextClip needs to support possibly several clips, since we can do W n W n W n f, for instance? *)
  | Pdfops.Op_W' ->
      (*begin match partial with
      | PartialPath (_, _, segments, subpaths) ->
          if segments = [] && subpaths = [] then ret else
            let path =
              if segments <> []
                then (Not_hole, Closed, rev segments)::subpaths
                else subpaths
            in
              (!state).clip <- Some (EvenOdd, path); ret
      | _ -> ret
      end*)()
  | Pdfops.Op_BT ->
      !state.text_state.t_m <- Pdftransform.i_matrix;
      !state.text_state.t_lm <- Pdftransform.i_matrix
  | Pdfops.Op_ET ->
      ()
  | Pdfops.Op_Tc f ->
      !state.text_state.character_spacing <- f
  | Pdfops.Op_Tw f ->
      !state.text_state.word_spacing <- f
  | Pdfops.Op_Tz f ->
      !state.text_state.horizontal_scaling <- (f /. 100.)
  | Pdfops.Op_TL f ->
      !state.text_state.leading <- f
  | Pdfops.Op_Tf (s, f) ->
      !state.text_state.font <- s;
      !state.text_state.font_size <- f;
      begin match Pdf.lookup_direct pdf "/Font" resources with
      | Some fontdict ->
          begin match Pdf.lookup_direct pdf s fontdict with
          | Some font -> !state.text_state.fontobj <- Pdftext.read_font pdf font
          | None -> Pdfe.log "Font not found\n"
          end
      | None -> Pdfe.log "Font not found\n"
      end
  | Pdfops.Op_Tr i ->
      !state.text_state.rendering_mode <- i
  | Pdfops.Op_Ts f ->
      !state.text_state.rise <- f
  | Pdfops.Op_Td (f1, f2) ->
      !state.text_state.t_lm <- Pdftransform.matrix_compose !state.text_state.t_lm (Pdftransform.mktranslate f1 f2);
      !state.text_state.t_m <- !state.text_state.t_lm
  | Pdfops.Op_TD (f1, f2) ->
      process_op ~pdf ~f ~stack ~state ~resources (Pdfops.Op_TL ~-.f2);
      process_op ~pdf ~f ~stack ~state ~resources (Pdfops.Op_Td (f1, f2))
  | Pdfops.Op_Tm m ->
      !state.text_state.t_m <- m;
      !state.text_state.t_lm <- m
  | Pdfops.Op_T' ->
      process_op ~pdf ~f ~stack ~state ~resources (Pdfops.Op_TD (0., ~-.(!state.text_state.leading)))
  | Pdfops.Op_Tj s ->
      process_tj ~f ~stack ~state ~resources s
  | Pdfops.Op_TJ p ->
      process_capital_tj ~f ~stack ~state ~resources (match p with Pdf.Array a -> a | _ -> [])
  | Pdfops.Op_' s ->
      process_op ~pdf ~f ~stack ~state ~resources Pdfops.Op_T';
      process_op ~pdf ~f ~stack ~state ~resources (Pdfops.Op_Tj s)
  | Pdfops.Op_'' (f1, f2, s) ->
      process_op ~pdf ~f ~stack ~state ~resources (Pdfops.Op_Tw f1);
      process_op ~pdf ~f ~stack ~state ~resources (Pdfops.Op_Tc f2);
      process_op ~pdf ~f ~stack ~state ~resources (Pdfops.Op_' s)
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
  | Pdfops.InlineImage i ->
      let x0, y0 = Pdftransform.transform_matrix !state.ctm (0., 0.) in
      let x1, y1 = Pdftransform.transform_matrix !state.ctm (0., 1.) in
      let x2, y2 = Pdftransform.transform_matrix !state.ctm (1., 1.) in
      let x3, y3 = Pdftransform.transform_matrix !state.ctm (1., 0.) in
        ignore (f (InlineImage, (x0, y0, x1, y1, x2, y2, x3, y3)))
  | Pdfops.Op_Do s ->
      begin match Pdf.lookup_direct pdf "/XObject" resources with
      | Some d ->
          begin match Pdf.lookup_direct pdf s d with
          | Some xobj ->
              begin match Pdf.lookup_direct pdf "/Subtype" xobj with
              | Some (Pdf.Name "/Image") ->
                  let x0, y0 = Pdftransform.transform_matrix !state.ctm (0., 0.) in
                  let x1, y1 = Pdftransform.transform_matrix !state.ctm (0., 1.) in
                  let x2, y2 = Pdftransform.transform_matrix !state.ctm (1., 1.) in
                  let x3, y3 = Pdftransform.transform_matrix !state.ctm (1., 0.) in
                    ignore (f (Image, (x0, y0, x1, y1, x2, y2, x3, y3)))
              | Some (Pdf.Name "/Form") ->
                  let matrix = Pdf.parse_matrix pdf "/Matrix" xobj in
                  let minx, miny, maxx, maxy =
                    match Pdf.lookup_direct pdf "/BBox" xobj with
                    | Some x ->
                        begin try Pdf.parse_rectangle pdf x with _ -> (min_float, min_float, max_float, max_float) end
                    | None -> 
                        (min_float, min_float, max_float, max_float)
                  in
                    process_op ~pdf ~f ~stack ~state ~resources Pdfops.Op_q;
                    !state.ctm <- Pdftransform.matrix_compose !state.ctm matrix;
                    process_op ~pdf ~f ~stack ~state ~resources (Pdfops.Op_re (minx, miny, maxx, maxy));
                    process_op ~pdf ~f ~stack ~state ~resources (Pdfops.Op_W);
                    process_op ~pdf ~f ~stack ~state ~resources (Pdfops.Op_n);
                    process_form_xobject ~pdf ~f ~stack ~state ~resources xobj;
                    process_op ~pdf ~f ~stack ~state ~resources Pdfops.Op_Q;
              | _ -> raise (Pdf.PDFError "Unknown kind of xobject")
              end
          | _ -> raise (Pdf.PDFError "Unknown xobject")
          end
      | None -> raise (Pdf.PDFError "xobject not found")
      end
  | Pdfops.Op_MP s -> ()
  | Pdfops.Op_DP (s, p) -> ()
  | Pdfops.Op_BMC s -> ()
  | Pdfops.Op_BDC (s, p) -> ()
  | Pdfops.Op_EMC -> ()
  | Pdfops.Op_BX -> ()
  | Pdfops.Op_EX -> ()
  | Pdfops.Op_Unknown s -> ()
  | Pdfops.Op_Comment s -> ()

and process_form_xobject ~pdf ~f ~stack ~state ~resources pdfobject =
  let content = [Pdf.direct pdf pdfobject] in
    let pagedict =
      match Pdf.direct pdf resources with
      | Pdf.Dictionary rs -> rs
      | _ -> []
    in
    let xobjdict =
      match Pdf.direct pdf pdfobject with
      | Pdf.Stream {contents = (dict, _)} ->
          begin match Pdf.lookup_direct pdf "/Resources" dict with
          | Some (Pdf.Dictionary rs) -> rs
          | _ -> []
          end
      | _ -> raise (Pdf.PDFError "bad stream in process_form_xobject")
    in
      let total_resources = Pdf.Dictionary (mergedict pagedict xobjdict) in
        iter
          (process_op ~pdf ~f ~stack ~state ~resources:total_resources)
          (Pdfops.parse_operators pdf total_resources content)

(* Draft redactor. f is given the bbox and determines whether to delete or not. *)
let filter_ops ~pdf ~f ~mediabox ~resources ~ops =
  let stack = ref [] in
  let state = ref (initial_state mediabox) in
    iter (fun op -> process_op ~pdf ~f ~stack ~state ~resources op) ops;
    ops
