(* Drawing operations. *)
open Pdfutil
open Cpdferror

let embed_font = ref (fun () -> Printf.printf "BAD *\n%!"; Cpdfembed.ExistingNamedFont)
let setdrawing = ref (fun () -> Printf.printf "BAD **\n%!")
let getfontname = ref (fun () -> Printf.printf "BAD ***\n%!"; "")
let getfontsize = ref (fun () -> Printf.printf "BAD ****\n%!"; 0.)
let setfontname = ref (fun _ -> Printf.printf "BAD *****\n%!")
let setfontsize = ref (fun _ -> Printf.printf "BAD ******\n%!")
let loadttf = ref (fun _ -> Printf.printf "BAD *******\n%!")
let setembedstd14 = ref (fun _ _ -> Printf.printf "BAD ********\n%!")
let getindent = ref (fun () -> Printf.printf "BAD *********\n%!"; None)

let ttfs = null_hash ()

let loadttfseparate name filename = !loadttf (name ^ "=" ^ filename)

let fontpack_initialised = ref false

let drawops = ref [("_MAIN", [])]

let startxobj n =
  drawops := (n, [])::!drawops

let xobj_bbox = ref (0., 0., 1000., 1000.)

let xobjbbox s =
  xobj_bbox := Cpdfcoord.parse_rectangle (Pdf.empty ()) s 

let addop o =
  match !drawops with
  | (n, ops)::t -> drawops := (n, (o::ops))::t
  | [] -> error "no drawops"

let endxobj () =
  match !drawops with
  | (n, ops)::t ->
      drawops := t;
      let a, b, c, d = !xobj_bbox in
        addop (Cpdfdraw.FormXObject (a, b, c, d, n, rev ops))
  | [] ->
      error "too many -end-xobj or -et"

let addbt () =
  drawops := ("_TEXT", [])::!drawops

let addet () =
  match !drawops with
  | ("_TEXT", ops)::t ->
      drawops := t;
      addop (Cpdfdraw.TextSection (rev ops))
  | _ -> error "not in a text section at -et"

let push () =
  drawops := ("_PUSH", [])::!drawops

let pop () =
  match !drawops with
  | ("_PUSH", ops)::t ->
      drawops := t;
      addop (Cpdfdraw.Qq (rev ops))
  | _ -> error "not in a pushed section at -pop"

let readfloats s = map float_of_string (String.split_on_char ' ' s)

let parse_colour s =
  match lookup (String.lowercase_ascii s) Cpdfcolours.colours with
  | Some c ->
      let r = float_of_int ((c land 0xFF0000) lsr 16) /. 255. in
      let g = float_of_int ((c land 0x00FF00) lsr 8) /. 255. in
      let b = float_of_int (c land 0x0000FF) /. 255. in
        Cpdfaddtext.RGB (r, g, b)
  | None ->
      let getnum = function
        | Pdfgenlex.LexInt i -> float i
        | Pdfgenlex.LexReal f -> f
        | _ -> error "Bad color"
      in
        match Pdfgenlex.lex_string s with
        | [g] -> Cpdfaddtext.Grey (getnum g)
        | [r; g; b] -> Cpdfaddtext.RGB (getnum r, getnum g, getnum b)
        | [c; y; m; k] -> Cpdfaddtext.CYMK (getnum c, getnum y, getnum m, getnum k)
        | _ -> error "Bad color"

let col_of_string s =  
  match parse_colour s with
  | Cpdfaddtext.RGB (r, g, b) -> Cpdfdraw.RGB (r, g, b)
  | Cpdfaddtext.Grey g -> Cpdfdraw.Grey g
  | Cpdfaddtext.CYMK (c, y, m, k) -> Cpdfdraw.CYMK (c, y, m, k)
  | exception _ -> Cpdfdraw.NoCol

let setstroke s =
  addop (Cpdfdraw.SetStroke (col_of_string s))

let setfill s =
  addop (Cpdfdraw.SetFill (col_of_string s))

let addtag t =
  addop (Cpdfdraw.Tag t)

let addstag t =
  addop (Cpdfdraw.STag t)

let endtag () =
  addop Cpdfdraw.EndTag

let endstag () =
  addop Cpdfdraw.EndSTag

let autotags b =
  Cpdfdraw.do_auto_tag := b

let autoartifacts b =
  Cpdfdraw.do_add_artifacts := b

let artifact () =
  addop Cpdfdraw.BeginArtifact

let endartifact () =
  addop Cpdfdraw.EndArtifact

let addnamespace s =
  addop (Cpdfdraw.Namespace s)

let eltinfo s =
  let k, v =
    match String.split_on_char '=' s with
    | [h; t] -> (h, t)
    | _ -> error "Bad -eltinfo format"
  in
    addop (Cpdfdraw.EltInfo (k, v))

let endeltinfo s =
  addop (Cpdfdraw.EndEltInfo s)

let setrolemap s =
  Cpdfdraw.rolemap := s

let addrect s =
  let x, y, w, h = Cpdfcoord.parse_rectangle (Pdf.empty ()) s in
    addop (Cpdfdraw.Rect (x, y, w, h))

let addto s =
  let x, y = Cpdfcoord.parse_coordinate (Pdf.empty ()) s in
    addop (Cpdfdraw.To (x, y))

let addline s =
  let x, y = Cpdfcoord.parse_coordinate (Pdf.empty ()) s in
    addop (Cpdfdraw.Line (x, y))

let addbezier s =
  match readfloats s with
  | [a; b; c; d; e; f] -> addop (Cpdfdraw.Bezier (a, b, c, d, e, f))
  | _ -> error "-bez requires six numbers"
  | exception _ -> error "malformed -bez"

let addbezier23 s =
  match readfloats s with
  | [a; b; c; d] -> addop (Cpdfdraw.Bezier23 (a, b, c, d))
  | _ -> error "-bez23 requires four numbers"
  | exception _ -> error "malformed -bez23"

let addbezier13 s =
  match readfloats s with
  | [a; b; c; d] -> addop (Cpdfdraw.Bezier13 (a, b, c, d))
  | _ -> error "-bez13 requires four numbers"
  | exception _ -> error "malformed -bez13"

let addcircle s =
  match readfloats s with
  | [x; y; r] ->
      let _, _, segs = hd (snd (Cpdfshape.circle x y r)) in
        (match segs with
         | Cpdfshape.Bezier ((a, b), _, _, _)::_ -> addop (Cpdfdraw.To (a, b))
         | _ -> assert false);
        iter
          (function
           | Cpdfshape.Bezier (_, (c, d), (e, f), (g, h)) -> addop (Cpdfdraw.Bezier (c, d, e, f, g, h))
           | Cpdfshape.Straight _ -> assert false)
          segs
  | _ -> error "-circle requires three numbers"
  | exception _ -> error "malformed -circle"

let stroke () =
  addop Cpdfdraw.Stroke

let fill () =
  addop Cpdfdraw.Fill

let fillevenodd () =
  addop Cpdfdraw.FillEvenOdd

let strokefill () =
  addop Cpdfdraw.FillStroke

let strokefillevenodd () =
  addop Cpdfdraw.FillStrokeEvenOdd

let clip () =
  addop Cpdfdraw.Clip

let clipevenodd () =
  addop Cpdfdraw.ClipEvenOdd

let closepath () =
  addop Cpdfdraw.ClosePath

let setthickness s =
  try addop (Cpdfdraw.SetLineThickness (float_of_string s)) with
    _ -> error "Thickness must be a number"

let setcap s =
  let num =
    match s with
    | "butt" -> 0
    | "round" -> 1
    | "square" -> 2
    | _ -> error "Unknown cap type"
  in
    addop (Cpdfdraw.SetLineCap num)

let setjoin s =
  let num =
    match s with
    | "miter" -> 0
    | "round" -> 1
    | "bevel" -> 2
    | _ -> error "Unknown join type"
  in
    addop (Cpdfdraw.SetLineJoin num)

let setmiter s = 
  try addop (Cpdfdraw.SetMiterLimit (float_of_string s)) with
    _ -> error "Miter limit must be a number"

let setdash s =
  try
    let x, y =
      let nums = readfloats s in all_but_last nums, last nums
    in
      addop (Cpdfdraw.SetDashPattern (x, y))
  with
   _ -> error "Dash pattern elements must one or more numbers"

let setmatrix s =
  match readfloats s with
  | [a; b; c; d; e; f] ->
      addop (Cpdfdraw.Matrix {Pdftransform.a = a; Pdftransform.b = b; Pdftransform.c = c;
                              Pdftransform.d = d; Pdftransform.e = e; Pdftransform.f = f})
  | _ -> error "Matrix must have six numbers"
  | exception _ -> error "Matrix elements must be numbers"

let setmtranslate s =
  match readfloats s with
  | [a; b] -> addop (Cpdfdraw.Matrix (Pdftransform.matrix_of_transform [Pdftransform.Translate (a, b)]))
  | _ | exception _ -> error "-mtrans takes two numbers"

let setmrotate s =
  match readfloats s with
  | [a; b; c] -> addop (Cpdfdraw.Matrix (Pdftransform.matrix_of_transform [Pdftransform.Rotate ((a, b), c)]))
  | _ | exception _ -> error "-mrot takes three numbers"

let setmscale s =
  match readfloats s with
  | [a; b; c; d] -> addop (Cpdfdraw.Matrix (Pdftransform.matrix_of_transform [Pdftransform.Scale ((a, b), c, d)]))
  | _ | exception _ -> error "-mscale takes four numbers"

let setmshearx s =
  match readfloats s with
  | [a; b; c] -> addop (Cpdfdraw.Matrix (Pdftransform.matrix_of_transform [Pdftransform.ShearX ((a, b), c)]))
  | _ | exception _ -> error "-mshearx takes three numbers"

let setmsheary s = 
  match readfloats s with
  | [a; b; c] -> addop (Cpdfdraw.Matrix (Pdftransform.matrix_of_transform [Pdftransform.ShearY ((a, b), c)]))
  | _ | exception _ -> error "-msheary takes three numbers"

let usexobj s =
  addop (Cpdfdraw.Use s)

let addjpeg ?data n =
  match data with
  | Some d ->
      addop
        (Cpdfdraw.ImageXObject
           (n, fst (Cpdfimage.obj_of_jpeg_data (Pdfio.bytes_of_raw d))))
  | None ->
      let name, filename =
        match String.split_on_char '=' n with
        | [name; filename] -> name, filename
        | _ -> error "addjpeg: bad file specification"
      in
        try
          let data = Pdfio.bytes_of_string (contents_of_file filename) in
            addop (Cpdfdraw.ImageXObject (name, fst (Cpdfimage.obj_of_jpeg_data data)))
        with
          _ -> error "addjpeg: could not load JPEG"

let addpng ?data n =
  match data with
  | Some d ->
      addop
        (Cpdfdraw.ImageXObject
           (n, fst (Cpdfimage.obj_of_png_data (Pdfio.bytes_of_raw d))))
  | None ->
      let name, filename =
        match String.split_on_char '=' n with
        | [name; filename] -> name, filename
        | _ -> error "addpng: bad file specification"
      in
        let data = Pdfio.bytes_of_string (contents_of_file filename) in
          addop (Cpdfdraw.ImageXObject (name, fst (Cpdfimage.obj_of_png_data data)))

let addimage ?title s =
  addop (Cpdfdraw.Image (s, title))

let addnewpage s =
  addop Cpdfdraw.NewPage

let addopacity f =
  addop (Cpdfdraw.Opacity f)

let addsopacity f =
  addop (Cpdfdraw.SOpacity f)

let addleading f =
  addop (Cpdfdraw.Leading f)

let addcharspace f =
  addop (Cpdfdraw.CharSpace f)

let addwordspace f =
  addop (Cpdfdraw.WordSpace f)

let addtextscale f = 
  addop (Cpdfdraw.TextScale f)

let addrendermode i =
  addop (Cpdfdraw.RenderMode i)

let addrise f =
  addop (Cpdfdraw.Rise f)

let addnewline () = 
  addop Cpdfdraw.Newline

let add_default_fontpack fontname =
  if not !fontpack_initialised then
    begin
      addop (Cpdfdraw.FontPack (fontname, !embed_font (), null_hash ()));
      set fontpack_initialised
    end
   
let addtext s =
  begin match !drawops with _::_::_ -> () | _ -> error "-text must be in a -bt / -et section" end;
    add_default_fontpack (!getfontname ());
    addop (Cpdfdraw.Font (!getfontname (), !getfontsize ()));
    addop (Cpdfdraw.Text s)

let addspecialtext s =
  begin match !drawops with _::_::_ -> () | _ -> error "-stext must be in a -bt / -et section" end;
    add_default_fontpack (!getfontname ());
    addop (Cpdfdraw.Font (!getfontname (), !getfontsize ()));
    addop (Cpdfdraw.SpecialText s)

(* "L200pt=....." *)
let jws s =
  let j, rest =
    match explode s with
    | 'L'::t -> (Cpdfdraw.Left, t)
    | 'R'::t -> (Cpdfdraw.Right, t)
    | 'C'::t -> (Cpdfdraw.Centre, t)
    | _ -> error "Unknown justification specification"
  in
  let w, s =
      match String.split_on_char '=' (implode rest) with
      | [w; s] -> (Cpdfcoord.parse_single_number (Pdf.empty ()) w, s)
      | _ -> error "addjpeg: bad file specification"
  in
    j, w, s

let addpara s =
  begin match !drawops with _::_::_ -> () | _ -> error "-para must be in a -bt / -et section" end;
  add_default_fontpack (!getfontname ());
  addop (Cpdfdraw.Font (!getfontname (), !getfontsize ()));
  let j, w, s = jws s in
    addop (Cpdfdraw.Para (None, j, w, [s]))

let rec split_on_newlines a = function
  | 0x005c::0x006e::t -> rev a::split_on_newlines [] t
  | h::t -> split_on_newlines (h::a) t
  | [] -> if a = [] then [] else [rev a]

let split_on_newlines s =
  map Pdftext.utf8_of_codepoints (split_on_newlines [] (Pdftext.codepoints_of_utf8 s))

let addparas s =
  begin match !drawops with _::_::_ -> () | _ -> error "-paras must be in a -bt / -et section" end;
  add_default_fontpack (!getfontname ());
  addop (Cpdfdraw.Font (!getfontname (), !getfontsize ()));
  let j, w, s = jws s in
  let splits = split_on_newlines s in
  let indent = !getindent () in
    addop (Cpdfdraw.Para (indent, j, w, splits))
