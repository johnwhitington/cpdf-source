open Pdfutil
open Cpdferror

let do_add_artifacts = ref true

let do_auto_tag = ref true

let rolemap = ref ""

type colspec =
   NoCol
 | RGB of float * float * float
 | Grey of float
 | CYMK of float * float * float * float

type justification =
  Left | Right | Centre

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
  | FontPack of string * Cpdfembed.cpdffont * (int, unit) Hashtbl.t
  | Font of string * float
  | TextSection of drawops list
  | Text of string
  | SpecialText of string
  | Para of float option * justification * float * string list
  | Newline
  | Leading of float
  | CharSpace of float
  | WordSpace of float
  | TextScale of float
  | RenderMode of int
  | Rise of float
  | Tag of string
  | EndTag
  | STag of string
  | EndSTag
  | BeginArtifact
  | EndArtifact
  | Namespace of string
  | EltInfo of string * Pdf.pdfobject
  | EndEltInfo of string
  | AutoTag of bool

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
  | Opacity _ -> "Opacity" | SOpacity _ -> "SOpacity" | FontPack (n, _, _) -> "FontPack " ^ n ^ " "
  | Font (f, _) -> "Font " ^ f ^ " " | Text _ -> "Text" | SpecialText _ -> "SpecialText"
  | Newline -> "Newline" | Leading _ -> "Leading" | CharSpace _ -> "CharSpace"
  | WordSpace _ -> "WordSpace" | TextScale _ -> "TextScale"
  | RenderMode _ -> "RenderMode" | Rise _ -> "Rise"
  | EndTag -> "EndTag" | Tag s -> "Tag " ^ s | EndSTag  -> "EndSTag" | STag s -> "Tag " ^ s
  | BeginArtifact -> "BeginArtifact" | EndArtifact -> "EndArtifact"
  | Para (_, _, _, _) -> "Para" | Namespace s -> "Namespace " ^ s 
  | EltInfo (_, _) -> "EltInfo" | EndEltInfo _ -> "EndEltInfo"
  | AutoTag _ -> "AutoTag"

and string_of_drawops l =
  fold_left (fun x y -> x ^ " " ^ y) "" (map string_of_drawop l)

(* Per page / xobject resources *)
type res = 
  {images : (string, (string * int)) Hashtbl.t; (* (name, (pdf name, objnum)) *)
   extgstates : ((string * float), string) Hashtbl.t; (* (kind, value), name *)
   fonts : (string * int, (string * int)) Hashtbl.t; (* (font, (objnum, pdf name)) *)
   form_xobjects : (string, (string * int)) Hashtbl.t; (* (name, (pdf name, objnum)) *)
   mutable page_names : string list;
   mutable time : Cpdfstrftime.t;
   mutable current_fontpack : string * Cpdfembed.t;
   mutable current_fontpack_codepoints : (int, unit) Hashtbl.t;
   mutable font_size : float;
   mutable num : int}

let default_fontpack =
  Cpdfembed.fontpack_of_standardfont
    (Pdftext.StandardFont (Pdftext.TimesRoman, Pdftext.WinAnsiEncoding))

let fontpacks = ref (null_hash ())

let empty_res () =
  {images = null_hash ();
   extgstates = null_hash ();
   fonts = null_hash ();
   form_xobjects = null_hash ();
   page_names = [];
   time = Cpdfstrftime.dummy;
   current_fontpack = ("Times-Roman", default_fontpack);
   current_fontpack_codepoints = null_hash ();
   font_size = 12.;
   num = 0}

let resstack =
  ref [empty_res ()]

let rescopy r =
  {r with
    images = Hashtbl.copy r.images;
    fonts = Hashtbl.copy r.fonts;
    extgstates = Hashtbl.copy r.extgstates;
    form_xobjects = Hashtbl.copy r.form_xobjects}

let res () =
  try hd !resstack with _ -> error "graphics stack empty"

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
  Hashtbl.clear (res ()).extgstates(*;
  (res ()).page_names <- []*)

let process_specials pdf endpage filename bates batespad num page s =
  let pairs =
    Cpdfaddtext.replace_pairs pdf endpage None filename bates batespad num page
  in
    Cpdfaddtext.process_text (res ()).time s pairs

(* FIXME cache (just for paragraph) *)
let font_widths f fontsize =
  match f with
  | Pdftext.StandardFont (sf, encoding) ->
      Array.init
        256
        (fun x ->
             fontsize
          *. float_of_int
               (Pdfstandard14.textwidth false encoding sf (string_of_char (char_of_int x)))
          /. 1000.)
  | Pdftext.SimpleFont {fontmetrics = Some m} ->
      Array.map (fun x -> fontsize *. x /. 1000. ) m
  | _ -> raise (Pdf.PDFError "Cpdfdraw: Unsupported font")

let runs_of_utf8 s =
  let widthcache = null_hash () in
  let identifier, fontpack = (res ()).current_fontpack in
  let codepoints = Pdftext.codepoints_of_utf8 s in
  let triples = option_map (Cpdfembed.get_char fontpack) codepoints in
  let collated = Cpdfembed.collate_runs triples in
  let font_widths fontnum font font_size =
    (* Need to cache font widths here. TODO need to cache futher up too for
       more speed. Check -typeset speed now we need widths. Or, make width
       calculation optional? *)
    match Hashtbl.find_opt widthcache (fontnum, font_size) with
    | Some table -> table
    | None ->
        let widths = font_widths font font_size in
          Hashtbl.add widthcache (fontnum, font_size) widths;
          widths
  in
  let w =
    fold_left ( +. ) 0.
      (map
        (fun (charcode, fontnum, font) ->
           let widths = font_widths fontnum font (res ()).font_size in
             widths.(charcode))
        triples)
  in
  let output =
    flatten
      (map
       (fun l ->
         if l = [] then [] else
           let f, n = match l with (_, n, f)::_ -> f, n | _ -> assert false in
           let fontname = fst (Hashtbl.find (res ()).fonts (identifier, n)) in
           let charcodes = map (fun (c, _, _) -> char_of_int c) l in
             [Pdfops.Op_Tf (fontname, (res ()).font_size);
              Pdfops.Op_Tj (implode charcodes)])
       collated)
  in
    (output, w)

let extgstate kind v =
  try Hashtbl.find (res ()).extgstates (kind, v) with
    Not_found ->
      let n = fresh_name "/G" in
        Hashtbl.replace (res ()).extgstates (kind, v) n;
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

let mcidr = ref ~-1
let mcid () = (incr mcidr; !mcidr)
let mcpage = ref ~-1

let standard_namespace = "http://iso.org/pdf/ssn"
let pdf2_namespace = "http://iso.org/pdf2/ssn"

(* namespace, object number pair. *)
let namespaces = null_hash ()

(* Add the object, add its number and this namespace to the hash. *)
let add_namespace pdf s =
  if s = standard_namespace then () else
    match Hashtbl.find_opt namespaces s with
    | Some _ -> ()
    | None ->
        let objnum = Pdf.addobj pdf (Pdf.Dictionary [("/NS", Pdf.String s)]) in
          Hashtbl.add namespaces s objnum

(* The structure data, as it is created, in flat form. Later on, this will be
   reconstructed into a structure tree. *)
type structdata =
  | StDataBeginTree of string
  | StDataEndTree
  | StDataMCID of string * int
  | StDataPage of int
  | StDataNamespace of string
  | StEltInfo of string * Pdf.pdfobject
  | StEndEltInfo of string

let structdata = ref []

(* TODO: Use Uuseg for proper unicode segmentation. *)
let format_paragraph indent j w s =
  let ss = String.split_on_char ' ' s in
  let rs_and_widths = ref (map runs_of_utf8 ss) in
  let space_runs, space_width = runs_of_utf8 " " in
  let remaining = ref w in
  let allops = ref [] in
  let ops = ref [Pdfops.Op_Comment "Format paragraph"] in
  let first = ref true in
  let firstloop = ref true in
  let justify ops =
    match j with
    | Left -> (if !first then [Pdfops.Op_Td (~-.indent, 0.)] else []) @ ops @ (if !first then [Pdfops.Op_Td (indent, 0.)] else [])
    | Right -> [Pdfops.Op_Td (~-.(!remaining), 0.)] @ ops @ [Pdfops.Op_Td (!remaining, 0.)]
    | Centre -> [Pdfops.Op_Td (~-.(!remaining) /. 2., 0.)] @ ops @ [Pdfops.Op_Td (!remaining /. 2., 0.)]
  in
    while !rs_and_widths <> [] do
      if !firstloop then (remaining -.= indent; clear firstloop);
      let word, word_width = hd !rs_and_widths in
        if !remaining = w then
          (* If current line empty, output word. *)
          begin
            ops := rev word @ !ops;
            remaining := !remaining -. word_width;
            rs_and_widths := tl !rs_and_widths
          end
        else if word_width +. space_width <= !remaining then
          (* If current line not empty, and space for space char and word, emit them. *)
          begin
            ops := rev space_runs @ !ops;
            ops := rev word @ !ops;
            remaining := !remaining -. word_width -. space_width;
            rs_and_widths := tl !rs_and_widths
          end
        else
          (* If current line not empty, and not enough space, emit newline. *)
          begin
            allops =| rev (Pdfops.Op_T'::justify !ops);
            clear first;
            ops := [];
            remaining := w;
          end;
    done;
  allops =| rev (Pdfops.Op_T'::justify !ops);
  flatten (rev !allops)

let current_eltinfo = null_hash ()

let rec ops_of_drawop struct_tree dryrun pdf endpage filename bates batespad num page = function
  | Qq ops ->
      [Pdfops.Op_q] @ ops_of_drawops struct_tree dryrun pdf endpage filename bates batespad num page ops @ [Pdfops.Op_Q]
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
      create_form_xobject struct_tree dryrun a b c d pdf endpage filename bates batespad num page n ops;
      []
  | Use n ->
      let pdfname = try fst (Hashtbl.find (res ()).form_xobjects n) with _ -> error ("Form XObject not found: " ^ n) in
        (res ()).page_names <- pdfname::(res ()).page_names;
        [Pdfops.Op_Do pdfname]
  | Image s ->
      let m = mcid () in
        if not dryrun then structdata := StDataMCID ("/Figure", m)::!structdata;
      let pdfname = try fst (Hashtbl.find (res ()).images s) with _ -> error ("Image not found: " ^ s) in
        (res ()).page_names <- pdfname::(res ()).page_names;
            (if struct_tree && !do_auto_tag then [Pdfops.Op_BDC ("/Figure", Pdf.Dictionary ["/MCID", Pdf.Integer m])] else [])
          @ [Pdfops.Op_Do pdfname]
          @ (if struct_tree && !do_auto_tag then [Pdfops.Op_EMC] else [])
  | ImageXObject (s, obj) ->
      Hashtbl.replace (res ()).images s (fresh_name "/I", Pdf.addobj pdf obj); 
      []
  | NewPage -> Pdfe.log ("NewPage remaining in graphic stream"); assert false
  | Opacity v -> [Pdfops.Op_gs (extgstate "/ca" v)]
  | SOpacity v -> [Pdfops.Op_gs (extgstate "/CA" v)]
  | FontPack (identifier, cpdffont, codepoints) ->
      (*Printf.printf "FontPack op: |%s|\n%!" identifier;*)
      let fontpack =
        match Hashtbl.find !fontpacks identifier with
        | (fontpack, _) ->
            (*Printf.printf "Cpdfdraw FontPack op: using existing fontpack |%s|\n%!" identifier;*)
            fontpack
        | exception Not_found ->
            (*Printf.printf "Cpdfdraw FontPack op: storing new fontpack |%s|\n%!" identifier;*)
            let fontpack =
              match cpdffont with
              | PreMadeFontPack fp ->
                  (*Printf.printf "it's a pre-made font pack\n%!";*)
                  fp
              | EmbedInfo {fontfile; fontname; encoding} ->
                  let codepoints = map fst (list_of_hashtbl codepoints) in
                    (*Printf.printf "%i codepoints to embed\n%!" (length codepoints);*)
                    if codepoints = [] then default_fontpack else 
                      Cpdfembed.embed_truetype pdf ~fontfile ~fontname ~codepoints ~encoding
              | ExistingNamedFont ->
                  error "-draw does not support using an existing named font"
            in
              Hashtbl.replace !fontpacks identifier (fontpack, codepoints);
              fontpack
      in
        let ns =
          map2
            (fun font n ->
              try fst (Hashtbl.find (res ()).fonts (identifier, n)) with
                Not_found ->
                  let o = if dryrun then 0 else Pdftext.write_font pdf font in
                  let name = fresh_name "/F" in
                    (*Printf.printf "Adding font %s as %s\n%!" identifier name;*)
                    Hashtbl.replace (res ()).fonts (identifier, n) (name, o);
                    name)
            (fst fontpack)
            (indx0 (fst fontpack))
        in
          (res ()).page_names <- ns @ (res ()).page_names;
          []
  | Font (identifier, size) ->
      (*Printf.printf "Cpdfdraw Font op: Changing to stored font %s\n%!" identifier;*)
      let fontpack, codepoints = Hashtbl.find !fontpacks identifier in
        (res ()).current_fontpack <- (identifier, fontpack);
        if dryrun then (res ()).current_fontpack_codepoints <- codepoints;
        (res ()).font_size <- size;
        []
  | TextSection ops ->
      let m = mcid () in
        if not dryrun && !do_auto_tag then structdata := StDataMCID ("/P", m)::!structdata;
          (if struct_tree && !do_auto_tag then [Pdfops.Op_BDC ("/P", Pdf.Dictionary ["/MCID", Pdf.Integer m])] else [])
        @ [Pdfops.Op_BT]
        @ ops_of_drawops struct_tree dryrun pdf endpage filename bates batespad num page ops
        @ [Pdfops.Op_ET] 
        @ (if struct_tree && !do_auto_tag then [Pdfops.Op_EMC] else [])
  | Text s ->
      if dryrun then iter (fun c -> Hashtbl.replace (res ()).current_fontpack_codepoints c ()) (Pdftext.codepoints_of_utf8 s);
      fst (runs_of_utf8 s)
  | SpecialText s ->
      let s = process_specials pdf endpage filename bates batespad num page s in
      if dryrun then iter (fun c -> Hashtbl.replace (res ()).current_fontpack_codepoints c ()) (Pdftext.codepoints_of_utf8 s);
        fst (runs_of_utf8 s)
  | Para (indent, j, w, s) ->
      if dryrun then iter (iter (fun c -> Hashtbl.replace (res ()).current_fontpack_codepoints c ())) (map Pdftext.codepoints_of_utf8 s);
      let first = ref true in
        flatten
          (map
            (function para ->
               (if not !first && indent = None then ([Pdfops.Op_T']) else (clear first; [])) @ format_paragraph (if indent <> None && not !first then unopt indent else 0.) j w para)
            s)
  | Leading f -> [Pdfops.Op_TL f]
  | CharSpace f -> [Pdfops.Op_Tc f]
  | WordSpace f -> [Pdfops.Op_Tw f]
  | TextScale f -> [Pdfops.Op_Tz f]
  | RenderMode i -> [Pdfops.Op_Tr i]
  | Rise f -> [Pdfops.Op_Ts f]
  | Newline -> [Pdfops.Op_T']
  | Tag s ->
      let m = mcid () in
        if not dryrun then structdata := StDataMCID ("/" ^ s, m)::!structdata;
        [Pdfops.Op_BDC ("/" ^ s, Pdf.Dictionary ["/MCID", Pdf.Integer m])]
  | EndTag -> [Pdfops.Op_EMC]
  | STag s -> if not dryrun then structdata =| StDataBeginTree ("/" ^ s); []
  | EndSTag -> if not dryrun then structdata =| StDataEndTree; []
  | BeginArtifact -> [Pdfops.Op_BMC "/BeginArtifact"]
  | EndArtifact -> [Pdfops.Op_BMC "/EndArtifact"]
  | Namespace s ->
      if not dryrun then
        begin
          add_namespace pdf s;
          structdata =| StDataNamespace s
        end;
        []
  | EltInfo (k, v) ->
      if not dryrun then structdata =| StEltInfo (k, v);
      []
  | EndEltInfo s ->
      if not dryrun then structdata =| StEndEltInfo s;
      []
  | AutoTag b ->
      do_auto_tag := b;
      []

and ops_of_drawops struct_tree dryrun pdf endpage filename bates batespad num page drawops =
  flatten (map (ops_of_drawop struct_tree dryrun pdf endpage filename bates batespad num page) drawops)

and create_form_xobject struct_tree dryrun a b c d pdf endpage filename bates batespad num page n ops =
  respush ();
  reset_state ();
  let data =
    Pdfio.bytes_of_string (Pdfops.string_of_ops (ops_of_drawops struct_tree dryrun pdf endpage filename bates batespad num page ops))
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
    Hashtbl.replace (res ()).form_xobjects n (fresh_name "/X", (if dryrun then 0 else Pdf.addobj pdf obj))

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

let save_whole_stack () =
  map (fun r -> rescopy r) !resstack

let restore_whole_stack r =
  resstack := r

(* When no automatic artifacting, we still need to fix our backchannel manual artifacts. *)
let fixup_manual_artifacts =
  map (function Pdfops.Op_BMC "/BeginArtifact" -> Pdfops.Op_BMC "/Artifact"
              | Pdfops.Op_BMC "/EndArtifact" -> Pdfops.Op_EMC
              | x -> x)

let draw_single ~struct_tree ~fast ~underneath ~filename ~bates ~batespad range pdf drawops =
  (res ()).num <- max (res ()).num (minimum_resource_number pdf range);
  let endpage = Pdfpage.endpage pdf in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let ops =
    if contains_specials drawops
      then None
      else Some (ops_of_drawops struct_tree false pdf endpage filename bates batespad 0 (hd pages) drawops)
  in
  let ss =
    map2
      (fun n p ->
         if mem n range
           then
             (match ops with
              | Some x -> x
              | None -> ops_of_drawops struct_tree false pdf endpage filename bates batespad n p drawops)
           else [])
      (ilist 1 endpage)
      pages
  in
  let pages =
    map3
      (fun n p ops ->
        if not (mem n range) then p else
          let ops = if struct_tree && !do_add_artifacts then Cpdftype.add_artifacts ops else fixup_manual_artifacts ops in
          let page = {p with Pdfpage.resources = update_resources pdf p.Pdfpage.resources} in
            (if underneath then Pdfpage.prepend_operators else Pdfpage.postpend_operators) pdf ops ~fast page)
      (ilist 1 endpage)
      (Pdfpage.pages_of_pagetree pdf)
      ss
  in
    Pdfpage.change_pages true pdf pages

(* Do a dry run of all the drawing to collect subset information. *)
let dryrun ~struct_tree ~filename ~bates ~batespad range pdf chunks =
  let endpage = Pdfpage.endpage pdf in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let r = save_whole_stack () in
  let saved_fontpacks = Hashtbl.copy !fontpacks in
  let pagenum = ref (hd range) in
    iter
      (fun chunk ->
         ignore (ops_of_drawops struct_tree true pdf endpage filename bates batespad !pagenum (hd pages) chunk);
         match range with
         | [x] when endpage > x -> pagenum := x + 1
         | _ -> pagenum := endpage + 1)
      chunks;
    restore_whole_stack r;
    fontpacks := saved_fontpacks

type st =
   StMCID of int
 | StItem of {kind : string; namespace : string; pageobjnum : int option; alt : (string * Pdf.pdfobject) list; children : st list}

(* Build a tree from the MCIDs and structure tree instructions gathered *)
let rec find_tree_contents a level = function
  | [] -> error "not enough -end-stag"
  | StDataBeginTree _ as h::t ->
      find_tree_contents (h::a) (level + 1) t
  | StDataEndTree::t ->
      if level = 1 then (rev a, t) else find_tree_contents a (level - 1) t
  | h::t -> find_tree_contents (h::a) level t

let mstdebug = ref false

let rec make_structure_tree pageobjnums (pn, ns, ei) pdf = function
  | [] -> []
  | StDataMCID (n, mcid)::t ->
      if !mstdebug then Printf.printf "StDataMCID, type = %s pagenum = %i, pageobjnum = %i\n" n !pn (unopt (lookup !pn pageobjnums));
      let item =
        StItem {kind = n; namespace = !ns; alt = list_of_hashtbl ei; pageobjnum = lookup !pn pageobjnums; children = [StMCID mcid]}
      in
        item::make_structure_tree pageobjnums (pn, ns, ei) pdf t
  | StDataPage n::t ->
      if !mstdebug then Printf.printf "StDataPage %i\n" n;
      pn := n;
      make_structure_tree pageobjnums (pn, ns, ei) pdf t
  | StDataNamespace s::t ->
      if !mstdebug then Printf.printf "StDataNamespace %s\n" s;
      ns := s;
      make_structure_tree pageobjnums (pn, ns, ei) pdf t
  | StEltInfo (k, v)::t ->
      if !mstdebug then Printf.printf "StEltInfo %s, %s\n" k (Pdfwrite.string_of_pdf v);
      Hashtbl.replace ei k v;
      make_structure_tree pageobjnums (pn, ns, ei) pdf t
  | StEndEltInfo s::t ->
      if !mstdebug then Printf.printf "StEndEltInfo %s\n" s;
      Hashtbl.remove ei s;
      make_structure_tree pageobjnums (pn, ns, ei) pdf t
  | StDataBeginTree s::t ->
      if !mstdebug then Printf.printf "StBeginTree %s, namespace = %s\n" s !ns;
      let tree_contents, rest = find_tree_contents [] 1 t in
        let item =
          let namespace = !ns in
          let alt = list_of_hashtbl ei in
          let children = make_structure_tree pageobjnums (pn, ns, ei) pdf tree_contents in
            StItem {kind = s; namespace; alt; pageobjnum = None; children;}
        in
          item::make_structure_tree pageobjnums (pn, ns, ei) pdf rest
  | StDataEndTree::t ->
      error "Too many -end-tags"

let make_structure_tree pdf items =
  let pageobjnums =
    let objnums = Pdf.page_reference_numbers pdf in
      combine (indx objnums) objnums
  in
    make_structure_tree pageobjnums (ref 0, ref standard_namespace, null_hash ()) pdf items

(* Write such a structure tree to a PDF. *)
let write_structure_tree pdf st =
  let parentmap = ref [] in
  let add_parentmap pon this_objnum =
    match lookup pon !parentmap with
    | None -> parentmap =| (pon, [this_objnum])
    | Some objnums -> parentmap := add pon (this_objnum::objnums) !parentmap
  in
  let struct_tree_root = Pdf.addobj pdf Pdf.Null in
  let rec mktree struct_tree_parent  = function
    | StItem {kind; namespace; pageobjnum; alt; children} ->
        let this_objnum = Pdf.addobj pdf Pdf.Null in
          begin match pageobjnum with
          | Some p -> add_parentmap p this_objnum
          | _ -> ()
          end;
          let alt = map (fun (k, v) -> ("/" ^ k, v)) alt in
          let page =
            match pageobjnum with
            | Some i -> [("/Pg", Pdf.Indirect i)]
            | None -> []
          in
          let namespace =
            if namespace = standard_namespace then [] else
              [("/NS", Pdf.Indirect (Hashtbl.find namespaces namespace))]
          in
          let this_obj =
            Pdf.Dictionary
              (alt
               @ page
               @ namespace
               @ [("/S", Pdf.Name kind);
                  ("/P", Pdf.Indirect struct_tree_parent);
                  ("/K", Pdf.Array (map (mktree this_objnum) children))])
          in
            Pdf.addobj_given_num pdf (this_objnum, this_obj);
            Pdf.Indirect this_objnum
    | StMCID x ->
        Pdf.Integer x
  in
  let items = map (mktree struct_tree_root) st in
  iter
    (fun (pon, _) ->
       Pdf.addobj_given_num pdf (pon, Pdf.add_dict_entry (Pdf.lookup_obj pdf pon) "/StructParents" (Pdf.Integer pon)))
    !parentmap;
  let parentmap =
    map (fun (pon, items) -> (string_of_int pon, Pdf.Array (map (fun x -> Pdf.Indirect x) (rev items)))) !parentmap
  in
  let st =
    let namespaces =
      match list_of_hashtbl namespaces with
      | [] -> []
      | ns -> [("/Namespaces", Pdf.Array (map (function (_, objnum) -> Pdf.Indirect objnum) ns))]
    in
    let rolemap =
      match !rolemap with
      | "" -> []
      | s -> [("/RoleMap", Pdfread.parse_single_object ("<<" ^ s ^ ">>"))]
    in
      Pdf.Dictionary
        (rolemap @ namespaces @
          [("/Type", Pdf.Name "/StructTreeRoot");
           ("/ParentTree", Pdf.Indirect (Pdf.addobj pdf (Pdftree.build_name_tree true pdf parentmap))); 
           ("/K", Pdf.Array items)])
  in
    Pdf.addobj_given_num pdf (struct_tree_root, st);
    Pdf.replace_chain pdf ["/Root"] ("/StructTreeRoot", Pdf.Indirect struct_tree_root)

let draw ~struct_tree ~fast ~underneath ~filename ~bates ~batespad range pdf drawops =
  (*Printf.printf "%s\n" (string_of_drawops drawops);*)
  if not struct_tree then clear do_add_artifacts;
  resstack := [empty_res ()];
  Hashtbl.clear !fontpacks;
  (res ()).time <- Cpdfstrftime.current_time ();
  let pdf = ref pdf in
  let range = ref range in
  (* Double up a trailing NewPage so it actually does something... *)
  let drawops = match rev drawops with NewPage::t -> rev (NewPage::NewPage::t) | _ -> drawops in
  let chunks = ref (split_around (eq NewPage) drawops) in
  dryrun ~struct_tree ~filename ~bates ~batespad !range !pdf !chunks;
  mcpage := 0;
    while !chunks <> [] do
      mcidr := -1;
      mcpage += 1;
      structdata =| StDataPage !mcpage;
      reset_state ();
      if hd !chunks <> [] then pdf := draw_single ~struct_tree ~fast ~underneath ~filename ~bates ~batespad !range !pdf (hd !chunks);
      chunks := tl !chunks;
      if !chunks <> [] then begin
        (* If the range is just a single page, and there is a next page, move to it. Otherwise,
           add a blank page at the end of the document. *)
        let endpage = Pdfpage.endpage !pdf in
          match !range with
          | [x] when endpage > x -> range := [x + 1]
          | _ ->
              pdf := Cpdfpad.padafter [endpage] !pdf;
              range := [endpage + 1]
      end
    done;
    if struct_tree then write_structure_tree !pdf (make_structure_tree !pdf (rev !structdata));
    !pdf
