open Pdfutil
open Cpdferror

type colspec =
   NoCol
 | RGB of float * float * float
 | Grey of float
 | CYMK of float * float * float * float

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
  | Image of string
  | NewPage
  | Opacity of float
  | SOpacity of float
  | Font of Pdftext.standard_font * float
  | TextSection of drawops list
  | Text of string
  | SpecialText of string
  | Newline
  | Leading of float
  | CharSpace of float
  | WordSpace of float
  | TextScale of float
  | RenderMode of int
  | Rise of float

let rec string_of_drawop = function
  | Qq o -> "Qq (" ^ string_of_drawops o ^ ")"
  | FormXObject (_, _, _, _, _, o) -> "FormXObject (" ^ string_of_drawops o ^ ")"
  | TextSection o -> "TextSection (" ^ string_of_drawops o ^ ")"
  | Rect _ -> "Rect" | Bezier _ -> "Bezier" | Bezier23 _ -> "Bezier23"
  | Bezier13 _ -> "Bezier13" | To _ -> "To" | Line _ -> "Line"
  | ClosePath -> "ClosePath" | SetFill _ -> "SetFill" | SetStroke _ -> "SetStroke"
  | SetLineThickness _ -> "SetLineThickness" | SetLineCap _ -> "SetLineCap"
  | SetLineJoin _ -> "SetLineJoin" | SetMiterLimit _ -> "SetMiterLimit"
  | SetDashPattern _ -> "SetDashPattern" | Matrix _ -> "SetMatrix"
  | Fill -> "Fill" | FillEvenOdd -> "FillEvenOdd" | Stroke -> "Stroke"
  | FillStroke -> "FillStroke" | FillStrokeEvenOdd -> "FillStrokeEvenOdd"
  | Clip -> "Clip" | ClipEvenOdd -> "ClipEvenOdd" | Use _ -> "Use"
  | ImageXObject _ -> "ImageXObject" | Image _ -> "Image" | NewPage -> "NewPage"
  | Opacity _ -> "Opacity" | SOpacity _ -> "SOpacity" | Font _ -> "Font" | Text _ -> "Text"
  | SpecialText _ -> "SpecialText" | Newline -> "Newline" | Leading _ -> "Leading"
  | CharSpace _ -> "CharSpace" | WordSpace _ -> "WordSpace" | TextScale _ -> "TextScale"
  | RenderMode _ -> "RenderMode" | Rise _ -> "Rise"

and string_of_drawops l =
  fold_left (fun x y -> x ^ " " ^ y) "" (map string_of_drawop l)

(* Per page / xobject resources *)
type res = 
  {images : (string, (string * int)) Hashtbl.t; (* (name, (pdf name, objnum)) *)
   extgstates : ((string * float), string) Hashtbl.t; (* (kind, value), name *)
   fonts : (Pdftext.font, (string * int)) Hashtbl.t; (* (font, (objnum, pdf name)) *)
   form_xobjects : (string, (string * int)) Hashtbl.t; (* (name, (pdf name, objnum)) *)
   mutable page_names : string list;
   mutable time : Cpdfstrftime.t;
   mutable current_font : Pdftext.font;
   mutable num : int}

let empty_res () =
  {images = null_hash ();
   extgstates = null_hash ();
   fonts = null_hash ();
   form_xobjects = null_hash ();
   page_names = [];
   time = Cpdfstrftime.dummy;
   current_font = Pdftext.StandardFont (Pdftext.TimesRoman, Pdftext.WinAnsiEncoding);
   num = 0}

let resstack =
  ref [empty_res ()]

let res () =
  try hd !resstack with _ -> error "graphics stack empty"

let rescopy r =
  {r with
    images = Hashtbl.copy r.images;
    fonts = Hashtbl.copy r.fonts;
    extgstates = Hashtbl.copy r.extgstates;
    form_xobjects = Hashtbl.copy r.form_xobjects}

let respush () =
  resstack := (rescopy (res ()))::!resstack

let respop () =
  let n = (res ()).num in
    resstack := tl !resstack;
    (* not necessary, since names are isolated in the xobject, but it makes
       manual debugging of PDF files easier if we don't re-use numbers *)
    (res ()).num <- max n (res ()).num

let fresh_name s =
  (res ()).num <- (res ()).num + 1;
  s ^ string_of_int (res ()).num

(* At end of page, we keep things for which we have indirects - but ExtGStates
   aren't indirect, so they go. *)
let reset_state () =
  Hashtbl.clear (res ()).extgstates;
  (res ()).page_names <- []

let process_specials pdf endpage filename bates batespad num page s =
  let pairs =
    Cpdfaddtext.replace_pairs pdf endpage None filename bates batespad num page
  in
    Cpdfaddtext.process_text (res ()).time s pairs

let charcodes_of_utf8 s =
  implode (map char_of_int (option_map (Pdftext.charcode_extractor_of_font_real (res ()).current_font) (Pdftext.codepoints_of_utf8 s)))

let extgstate kind v =
  try Hashtbl.find (res ()).extgstates (kind, v) with
    Not_found ->
      let n = fresh_name "/G" in
        Hashtbl.add (res ()).extgstates (kind, v) n;
        n

let read_resource pdf n res =
  match Pdf.lookup_direct pdf n res with
  | Some (Pdf.Dictionary d) -> d
  | _ -> []

let update_resources pdf old_resources =
  let gss_resources = map (fun ((kind, v), n) -> (n, Pdf.Dictionary [(kind, Pdf.Real v)])) (list_of_hashtbl (res ()).extgstates) in
  let select_resources t =
    option_map (fun (_, (n, o)) -> if mem n (res ()).page_names then Some (n, Pdf.Indirect o) else None) (list_of_hashtbl t)
  in
  let update = fold_right (fun (k, v) d -> add k v d) in
  let new_gss = update gss_resources (read_resource pdf "/ExtGState" old_resources) in
  let new_xobjects = update (select_resources (res ()).form_xobjects @ select_resources (res ()).images) (read_resource pdf "/XObject" old_resources) in
  let new_fonts = update (select_resources (res ()).fonts) (read_resource pdf "/Font" old_resources) in
  let add_if_non_empty dict name newdict =
    if newdict = Pdf.Dictionary [] then dict else
      Pdf.add_dict_entry dict name newdict
  in
    add_if_non_empty
    (add_if_non_empty
      (add_if_non_empty old_resources "/XObject" (Pdf.Dictionary new_xobjects))
      "/ExtGState"
      (Pdf.Dictionary new_gss))
    "/Font"
    (Pdf.Dictionary new_fonts)

let rec ops_of_drawop pdf endpage filename bates batespad num page = function
  | Qq ops -> [Pdfops.Op_q] @ ops_of_drawops pdf endpage filename bates batespad num page ops @ [Pdfops.Op_Q]
  | Matrix m -> [Pdfops.Op_cm m] 
  | Rect (x, y, w, h) -> [Pdfops.Op_re (x, y, w, h)]
  | Bezier (a, b, c, d, e, f) -> [Pdfops.Op_c (a, b, c, d, e, f)]
  | Bezier23 (a, b, c, d) -> [Pdfops.Op_v (a, b, c, d)]
  | Bezier13 (a, b, c, d) -> [Pdfops.Op_y (a, b, c, d)]
  | To (x, y) -> [Pdfops.Op_m (x, y)]
  | Line (x, y) -> [Pdfops.Op_l (x, y)]
  | SetFill x ->
      begin match x with
      | RGB (r, g, b) -> [Op_rg (r, g, b)]
      | Grey g -> [Op_g g]
      | CYMK (c, y, m, k) -> [Op_k (c, y, m, k)]
      | NoCol -> []
      end
  | SetStroke x ->
      begin match x with
      | RGB (r, g, b) -> [Op_RG (r, g, b)]
      | Grey g -> [Op_G g]
      | CYMK (c, y, m, k) -> [Op_K (c, y, m, k)]
      | NoCol -> []
      end
  | ClosePath -> [Pdfops.Op_h]
  | Fill -> [Pdfops.Op_f]
  | FillEvenOdd -> [Pdfops.Op_f']
  | Stroke -> [Pdfops.Op_S]
  | FillStroke -> [Pdfops.Op_B]
  | FillStrokeEvenOdd -> [Pdfops.Op_B']
  | Clip -> [Pdfops.Op_W; Pdfops.Op_n]
  | ClipEvenOdd -> [Pdfops.Op_W'; Pdfops.Op_n]
  | SetLineThickness t -> [Pdfops.Op_w t]
  | SetLineCap c -> [Pdfops.Op_J c]
  | SetLineJoin j -> [Pdfops.Op_j j]
  | SetMiterLimit m -> [Pdfops.Op_M m]
  | SetDashPattern (x, y) -> [Pdfops.Op_d (x, y)]
  | FormXObject (a, b, c, d, n, ops) ->
      create_form_xobject a b c d pdf endpage filename bates batespad num page n ops;
      []
  | Use n ->
      let pdfname = try fst (Hashtbl.find (res ()).form_xobjects n) with _ -> error ("Form XObject not found: " ^ n) in
        (res ()).page_names <- pdfname::(res ()).page_names;
        [Pdfops.Op_Do pdfname]
  | Image s ->
      let pdfname = try fst (Hashtbl.find (res ()).images s) with _ -> error ("Image not found: " ^ s) in
        (res ()).page_names <- pdfname::(res ()).page_names;
        [Pdfops.Op_Do pdfname]
  | ImageXObject (s, obj) ->
      Hashtbl.add (res ()).images s (fresh_name "/I", Pdf.addobj pdf obj); 
      []
  | NewPage -> Pdfe.log ("NewPage remaining in graphic stream"); assert false
  | Opacity v -> [Pdfops.Op_gs (extgstate "/ca" v)]
  | SOpacity v -> [Pdfops.Op_gs (extgstate "/CA" v)]
  | Font (s, f) ->
      let font = Pdftext.StandardFont (s, Pdftext.WinAnsiEncoding) in
      let (n, _) =
        try Hashtbl.find (res ()).fonts font with
          Not_found ->
            let o = Pdftext.write_font pdf font in
            let n = fresh_name "/F" in
              Hashtbl.add (res ()).fonts font (n, o);
              (n, o)
      in
        (res ()).current_font <- font;
        (res ()).page_names <- n::(res ()).page_names;
        [Pdfops.Op_Tf (n, f)]
  | TextSection ops -> [Pdfops.Op_BT] @ ops_of_drawops pdf endpage filename bates batespad num page ops @ [Pdfops.Op_ET]
  | Text s -> [Pdfops.Op_Tj (charcodes_of_utf8 s)]
  | SpecialText s ->
      let s = process_specials pdf endpage filename bates batespad num page s in
        [Pdfops.Op_Tj (charcodes_of_utf8 s)]
  | Leading f -> [Pdfops.Op_TL f]
  | CharSpace f -> [Pdfops.Op_Tc f]
  | WordSpace f -> [Pdfops.Op_Tw f]
  | TextScale f -> [Pdfops.Op_Tz f]
  | RenderMode i -> [Pdfops.Op_Tr i]
  | Rise f -> [Pdfops.Op_Ts f]
  | Newline -> [Pdfops.Op_T']

and ops_of_drawops pdf endpage filename bates batespad num page drawops =
  flatten (map (ops_of_drawop pdf endpage filename bates batespad num page) drawops)

and create_form_xobject a b c d pdf endpage filename bates batespad num page n ops =
  respush ();
  reset_state ();
  let data =
    Pdfio.bytes_of_string (Pdfops.string_of_ops (ops_of_drawops pdf endpage filename bates batespad num page ops))
  in
  let obj =
    Pdf.Stream
      {contents =
          (Pdf.Dictionary
             [("/Length", Pdf.Integer (Pdfio.bytes_size data));
              ("/Subtype", Pdf.Name "/Form");
              ("/Resources", update_resources pdf (Pdf.Dictionary []));
              ("/BBox", Pdf.Array [Pdf.Real a; Pdf.Real b; Pdf.Real c; Pdf.Real d])
             ],
           Pdf.Got data)}
  in
    respop ();
    Hashtbl.add (res ()).form_xobjects n (fresh_name "/X", (Pdf.addobj pdf obj))

let minimum_resource_number pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pages_in_range =
    option_map2 (fun p n -> if mem n range then Some p else None) pages (indx pages) in
  let number_of_name s =
    match implode (rev (takewhile (function '0'..'9' -> true | _ -> false) (rev (explode s)))) with
    | "" -> None
    | s -> Some (int_of_string s)
  in
  let resource_names_page p =
    let names n =
      match Pdf.lookup_direct pdf n p.Pdfpage.resources with
      | Some (Pdf.Dictionary d) -> map fst d
      | _ -> []
    in
      names "/XObject" @ names "/ExtGState" @ names "/Font"
  in
    match
      sort
        (fun a b -> compare b a)
        (option_map number_of_name (flatten (map resource_names_page pages_in_range)))
    with
    | [] -> 0
    | n::_ -> n + 1

let rec contains_specials_drawop = function
  | SpecialText _ -> true
  | Qq l | TextSection l | FormXObject (_, _, _, _, _, l) -> contains_specials l
  | _ -> false

and contains_specials l =
  List.exists contains_specials_drawop l

let draw_single ~fast ~underneath ~filename ~bates ~batespad fast range pdf drawops =
  (res ()).num <- max (res ()).num (minimum_resource_number pdf range);
  let endpage = Pdfpage.endpage pdf in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let ops =
    if contains_specials drawops
      then None
      else Some (ops_of_drawops pdf endpage filename bates batespad 0 (hd pages) drawops)
  in
  let ss =
    map2
      (fun n p ->
         if mem n range
           then (match ops with Some x -> x | None -> ops_of_drawops pdf endpage filename bates batespad n p drawops)
           else [])
      (ilist 1 endpage)
      pages
  in
  let pages =
    map3
      (fun n p ops ->
        if not (mem n range) then p else
          let page = {p with Pdfpage.resources = update_resources pdf p.Pdfpage.resources} in
            (if underneath then Pdfpage.prepend_operators else Pdfpage.postpend_operators) pdf ops ~fast page)
      (ilist 1 endpage)
      (Pdfpage.pages_of_pagetree pdf)
      ss
  in
    Pdfpage.change_pages true pdf pages

let draw ?(fast=false) ?(underneath=false) ~filename ~bates ~batespad fast range pdf drawops =
  resstack := [empty_res ()];
  (res ()).time <- Cpdfstrftime.current_time ();
  let pdf = ref pdf in
  let range = ref range in
  (* Double up a trailing NewPage so it actually does something... *)
  let drawops = match rev drawops with NewPage::t -> rev (NewPage::NewPage::t) | _ -> drawops in
  let chunks = ref (split_around (eq NewPage) drawops) in
    while !chunks <> [] do
      reset_state ();
      if hd !chunks <> [] then pdf := draw_single ~fast ~underneath ~filename ~bates ~batespad fast !range !pdf (hd !chunks);
      chunks := tl !chunks;
      if !chunks <> [] then begin
        let endpage = Pdfpage.endpage !pdf in
          pdf := Cpdfpad.padafter [endpage] !pdf;
          range := [endpage + 1]
      end
    done;
    !pdf
