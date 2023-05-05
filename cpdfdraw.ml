open Pdfutil

type colspec =
   NoCol
 | RGB of float * float * float
 | Grey of float
 | CYMK of float * float * float * float

type drawops =
  | Rect of float * float * float * float
  | Bezier of float * float * float * float * float * float
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
  | Push
  | Pop
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
  | BT
  | ET
  | Text of string
  | SpecialText of string
  | Newline
  | Leading of float
  | CharSpace of float
  | WordSpace of float
  | TextScale of float
  | RenderMode of int
  | Rise of float
  | URL of string
  | EndURL

(* Per page resources *)
type res = 
  {images : (string, (string * int)) Hashtbl.t; (* (name, (pdf name, objnum)) *)
   extgstates : ((string * float), string) Hashtbl.t; (* (kind, value), name *)
   fonts : (Pdftext.font, (string * int)) Hashtbl.t; (* (font, (objnum, pdf name)) *)
   form_xobjects : (string, (string * int)) Hashtbl.t; (* (name, (pdf name, objnum)) *)
   mutable time : Cpdfstrftime.t;
   mutable current_url : string option;
   mutable current_font : Pdftext.font;
   mutable num : int}

let res =
  {images = null_hash ();
   extgstates = null_hash ();
   fonts = null_hash ();
   form_xobjects = null_hash ();
   time = Cpdfstrftime.dummy;
   current_url = None;
   current_font = Pdftext.StandardFont (Pdftext.TimesRoman, Pdftext.WinAnsiEncoding);
   num = 0}

let fresh_name s =
  res.num <- res.num + 1;
  s ^ string_of_int res.num

(* At end of page, we keep things for which we have indirects - we may use them on another page. *)
let reset_state () =
  Hashtbl.clear res.extgstates

let process_specials pdf endpage filename bates batespad num page s =
  let pairs =
    Cpdfaddtext.replace_pairs pdf endpage None filename bates batespad num page
  in
    Cpdfaddtext.process_text res.time s pairs

let charcodes_of_utf8 s =
  implode (map char_of_int (option_map (Pdftext.charcode_extractor_of_font_real res.current_font) (Pdftext.codepoints_of_utf8 s)))

let extgstate kind v =
  try Hashtbl.find res.extgstates (kind, v) with
    Not_found ->
      let n = fresh_name "/gs" in
        Hashtbl.add res.extgstates (kind, v) n;
        n

let rec ops_of_drawop pdf endpage filename bates batespad num page = function
  | Push -> [Pdfops.Op_q]
  | Pop -> [Pdfops.Op_Q]
  | Matrix m -> [Pdfops.Op_cm m] 
  | Rect (x, y, w, h) -> [Pdfops.Op_re (x, y, w, h)]
  | Bezier (a, b, c, d, e, f) -> [Pdfops.Op_c (a, b, c, d, e, f)]
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
  | ClosePath
  | Fill -> [Pdfops.Op_f]
  | FillEvenOdd -> [Pdfops.Op_f']
  | Stroke -> [Pdfops.Op_S]
  | FillStroke -> [Pdfops.Op_B]
  | FillStrokeEvenOdd -> [Pdfops.Op_B']
  | Clip -> [Pdfops.Op_W; Pdfops.Op_n]
  | ClipEvenOdd -> [Pdfops.Op_W']
  | SetLineThickness t -> [Pdfops.Op_w t; Pdfops.Op_n]
  | SetLineCap c -> [Pdfops.Op_J c]
  | SetLineJoin j -> [Pdfops.Op_j j]
  | SetMiterLimit m -> [Pdfops.Op_M m]
  | SetDashPattern (x, y) -> [Pdfops.Op_d (x, y)]
  | FormXObject (a, b, c, d, n, ops) -> create_form_xobject a b c d pdf endpage filename bates batespad num page n ops; []
  | Use n -> [Pdfops.Op_Do (try fst (Hashtbl.find res.form_xobjects n) with _ -> Cpdferror.error ("Form XObject not found: " ^ n))]
  | Image s -> [Pdfops.Op_Do (try fst (Hashtbl.find res.images s) with _ -> Cpdferror.error ("Image not found: " ^ s))]
  | ImageXObject (s, obj) ->
      Hashtbl.add res.images s (fresh_name "/XObj", Pdf.addobj pdf obj); 
      []
  | NewPage -> Pdfe.log ("NewPage remaining in graphic stream"); assert false
  | Opacity v -> [Pdfops.Op_gs (extgstate "/ca" v)]
  | SOpacity v -> [Pdfops.Op_gs (extgstate "/CA" v)]
  | URL s ->
      res.current_url <- Some s;
      []
  | EndURL ->
      res.current_url <- None;
      []
  | Font (s, f) ->
      let font = Pdftext.StandardFont (s, Pdftext.WinAnsiEncoding) in
      let (n, _) =
        try Hashtbl.find res.fonts font with
          Not_found ->
            let o = Pdftext.write_font pdf font in
            let n = fresh_name "/F" in
              Hashtbl.add res.fonts font (n, o);
              (n, o)
      in
        res.current_font <- font;
        [Pdfops.Op_Tf (n, f)]
  | BT -> [Pdfops.Op_BT]
  | ET -> [Pdfops.Op_ET]
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
  let data =
    Pdfio.bytes_of_string (Pdfops.string_of_ops (ops_of_drawops pdf endpage filename bates batespad num page ops))
  in
  let obj =
    Pdf.Stream
      {contents =
          (Pdf.Dictionary
             [("/Length", Pdf.Integer (Pdfio.bytes_size data));
              ("/Subtype", Pdf.Name "/Form");
              ("/BBox", Pdf.Array [Pdf.Real a; Pdf.Real b; Pdf.Real c; Pdf.Real d])
             ],
           Pdf.Got data)}
  in
    Hashtbl.add res.form_xobjects n (fresh_name "/Fm", (Pdf.addobj pdf obj))

let read_resource pdf n p =
  match Pdf.lookup_direct pdf n p.Pdfpage.resources with
  | Some (Pdf.Dictionary d) -> d
  | _ -> []

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

let contains_specials drawops =
  List.exists (function SpecialText _ -> true | _ -> false) drawops

let draw_single ~filename ~bates ~batespad fast range pdf drawops =
  res.num <- minimum_resource_number pdf range;
  let endpage = Pdfpage.endpage pdf in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let str =
    if contains_specials drawops then None else Some (Pdfops.string_of_ops (ops_of_drawops pdf endpage filename bates batespad 0 (hd pages) drawops))
  in
  let ss =
    map2
      (fun n p ->
         if mem n range then (match str with Some x -> x | None -> Pdfops.string_of_ops (ops_of_drawops pdf endpage filename bates batespad n p drawops)) else "")
      (ilist 1 endpage)
      pages
  in
  let pdf = ref pdf in
    iter2
      (fun n s ->
        if mem n range then pdf := Cpdftweak.append_page_content s false fast [n] !pdf)
      (ilist 1 endpage)
      ss;
  let pdf = !pdf in
  let image_resources = map (fun (_, (n, o)) -> (n, Pdf.Indirect o)) (list_of_hashtbl res.images) in
  let gss_resources = map (fun ((kind, v), n) -> (kind, Pdf.Real v)) (list_of_hashtbl res.extgstates) in
  let font_resources = map (fun (_, (n, o)) -> (n, Pdf.Indirect o)) (list_of_hashtbl res.fonts) in
  let form_resources = map (fun (_, (n, o)) -> (n, Pdf.Indirect o)) (list_of_hashtbl res.form_xobjects) in 
  let pages =
    map
      (fun p ->
        let new_resources =
          let update = fold_right (fun (k, v) d -> add k v d) in
          let new_xobjects = update (form_resources @ image_resources) (read_resource pdf "/XObject" p) in
          let new_gss = update gss_resources (read_resource pdf "/ExtGState" p) in
          let new_fonts = update font_resources (read_resource pdf "/Font" p) in
            Pdf.add_dict_entry
              (Pdf.add_dict_entry
                (Pdf.add_dict_entry p.Pdfpage.resources "/XObject" (Pdf.Dictionary new_xobjects))
                "/ExtGState"
                (Pdf.Dictionary new_gss))
              "/Font"
              (Pdf.Dictionary new_fonts)
        in
         {p with resources = new_resources})
      (Pdfpage.pages_of_pagetree pdf)
  in
    Pdfpage.change_pages true pdf pages

let draw ~filename ~bates ~batespad fast range pdf drawops =
  res.time <- Cpdfstrftime.current_time ();
  let pdf = ref pdf in
  let range = ref range in
  let chunks = ref (split_around (eq NewPage) drawops) in
    while !chunks <> [] do
      reset_state ();
      pdf := draw_single ~filename ~bates ~batespad fast !range !pdf (hd !chunks);
      chunks := tl !chunks;
      if !chunks <> [] then begin
        let endpage = Pdfpage.endpage !pdf in
          pdf := Cpdfpad.padafter [endpage] !pdf;
          range := [endpage + 1]
      end
    done;
    !pdf
