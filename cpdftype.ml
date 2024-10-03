(* A typesetter for cpdf. A list of elements is manipulated zero or more times
   to lay it out, paginate it, and so on. It is then typeset to produce a list
   of pages.

   For now, this is just an experiment for -table-of-contents and -typeset. To
   be continued... *)

open Pdfutil

(* Main type *)
type element =
  Text of char list (* charcodes 0..255 *)
| HGlue of float
| VGlue of float
| NewLine
| NewPage
| Font of string * Pdftext.font * float
| BeginDest of Pdfdest.t
| EndDest
| BeginDocument
| Tag of string * int
| EndTag

let to_string_elt = function
  | Text t -> implode t
  | HGlue len -> "HGLUE" ^ string_of_float len
  | VGlue _ -> "VGLUE"
  | NewLine -> "NewLine"
  | NewPage -> "NewPage"
  | Font _ -> "Font"
  | BeginDest _ -> "BeginDest"
  | EndDest -> "EndDest"
  | BeginDocument -> "BeginDocument"
  | Tag (s, i) -> "Tag " ^ s ^ " " ^ string_of_int i
  | EndTag -> "EndTag"

let to_string es = fold_left (fun a b -> a ^ "\n" ^ b) "" (map to_string_elt es)

type t = element list

type state =
  {mutable font : Pdftext.font option;
   mutable fontid : string option;
   mutable fontsize : float;
   mutable width_table : float array; (* Widths for charcodes 0..255 *)
   mutable xpos : float;
   mutable ypos : float;
   mutable dest : Pdfdest.t option}

let width_table_cache = null_hash ()

let initial_state () =
  {font = None;
   fontid = None;
   fontsize = 0.;
   width_table = [||];
   xpos = 0.;
   ypos = 0.;
   dest = None}

(* Mark as an artifact anything not already marked. *)
let add_artifacts ops =
  let content = ref false in
  let artifact = ref false in
  let rec loop a = function
  | [] ->
      (* The end. Must end artifact if in artifact. *)
      if !artifact then rev (Pdfops.Op_EMC::a) else rev a
  | Pdfops.Op_BMC "/BeginArtifact"::t ->
      (* Convert back-channel artifact beginning. *)
      set artifact;
      loop (Pdfops.Op_BMC "/Artifact"::a) t
  | Pdfops.Op_BMC "/EndArtifact"::t ->
      (* Convert back-channel artifact ending. *)
      clear artifact;
      loop (Pdfops.Op_EMC::a) t
  | Pdfops.Op_BDC _ as h::t -> 
      (* Entering content. If in artifact, must end artifact. *)
      let a' = if !artifact then h::Pdfops.Op_EMC::a else h::a in
        set content; clear artifact; loop a' t
  | Pdfops.Op_EMC as h::t ->
      (* Exiting content. *)
      clear content;
      loop (h::a) t
  | h::t -> 
      (* A normal operation. If not in content or artifact must start artifact. *)
      let a' =
        if not (!content || !artifact) then (set artifact; h::Pdfops.Op_BMC "/Artifact"::a) else h::a
      in
        loop a' t
  in
    loop [] ops

let font_widths id f fontsize =
  match Hashtbl.find width_table_cache (id, fontsize) with
  | x -> x
  | exception Not_found ->
      let newtable =
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
        | _ -> raise (Pdf.PDFError "Cpdftype: Unsupported font")
      in
        Hashtbl.add width_table_cache (id, fontsize) newtable;
        newtable

let width_of_string ws s =
  let w = ref 0. in
    iter (fun s -> w := !w +. ws.(int_of_char s)) s;
    !w

(* For now, split each text element into words, and lay them out ragged right
   on one long page. Words longer than a whole line just fall off the margin.
   Turn text newlines into real newlines. *)
(* Split into words on spaces. Find how many words (at least one, to make
   progress) fit into the available space. We set needs_newline if the next
   word would overflow. Return (text, needs_newline, remaining_text) *)
let split_text space_left widths t =
  let chars = ref t in
  let words = ref [] in
  let space_left = ref space_left in
  let return needs_newline =
    (flatten (rev !words), needs_newline, !chars)
  in
    try
      while !chars <> [] do
        let word, rest = cleavewhile (neq ' ') !chars in
          let w = width_of_string widths word in 
          if !words = [] || w < !space_left
            then
              let is_last_word = rest = [] in
              let new_word = if is_last_word then word else word @ [' '] in
                begin
                  words := new_word::!words;
                  space_left := !space_left -. w -. (if is_last_word then 0. else width_of_string widths [' '])
                end
            else raise Exit;
          chars := if rest = [] then [] else tl rest;
      done;
      return false
    with
      Exit -> return true

let layout lmargin rmargin papersize i =
  let width = Pdfunits.points (Pdfpaper.width papersize) (Pdfpaper.unit papersize) in
  let o = ref [] in
  let s = initial_state () in
  let xpos_max = width -. lmargin in
    s.xpos <- lmargin;
    let rec layout_element = function
    | Font (id, f, fontsize) ->
        s.width_table <- font_widths id f fontsize;
        o := Font (id, f, fontsize) :: !o
    | Text text ->
        if text = [] then () else
          begin
            let this_line, needs_newline, remaining_text =
              split_text (xpos_max -. s.xpos) s.width_table text
            in
              o := Text this_line :: !o;
              s.xpos <- s.xpos +. width_of_string s.width_table this_line;
              if needs_newline then layout_element NewLine;
              if remaining_text <> [] then layout_element (Text remaining_text)
          end
    | HGlue len as glue ->
        s.xpos <- s.xpos +. len;
        o := glue :: !o;
        if s.xpos >= xpos_max then layout_element NewLine
    | NewLine ->
        s.xpos <- lmargin;
        o := NewLine :: !o
    | x ->
        o := x :: !o
    in
    iter layout_element i;
    rev !o

(* Paginate, simply line-based. When ypos + lineheight exceeds max_ypos, we
   insert a page break. In addition, we re-write any paragraph tag/endtag to
   make sure they appear on both pages. *)
let paginate tmargin bmargin papersize i =
  let height = Pdfunits.points (Pdfpaper.height papersize) (Pdfpaper.unit papersize) in
  let o = ref [] in
  let s = initial_state () in
  s.ypos <- tmargin;
  let max_ypos = height -. bmargin in
  let tag = ref None in
  let rec process = function
   | VGlue len as glue ->
       s.ypos <- s.ypos +. len;
       o := glue :: !o;
       if s.ypos > max_ypos then process NewPage
   | NewLine ->
       s.ypos <- s.ypos +. s.fontsize *. 1.3;
       o := NewLine::!o;
       if s.ypos > max_ypos then process NewPage
   | Font (id, f, fs) ->
       s.font <- Some f;
       s.fontid <- Some id;
       s.fontsize <- fs;
       o := Font (id, f, fs)::!o
   | NewPage ->
       s.ypos <- tmargin +. s.fontsize;
       begin match !tag with Some (s, i) -> o := EndTag::!o | None -> () end;
       o := NewPage::!o;
       begin match !tag with Some (s, i) -> o := Tag (s, i)::!o | None -> () end
   | BeginDocument ->
       s.ypos <- tmargin +. s.fontsize;
       o := BeginDocument::!o
   | Tag (s, i) ->
       tag := Some (s, i);
       o := Tag (s, i)::!o
   | EndTag ->
       tag := None;
       o := EndTag::!o
   | x -> o := x::!o
  in
    iter process i;
    rev !o

let make_resources fontobjnums =
  Pdf.Dictionary
    [("/Font", Pdf.Dictionary (map (fun fo -> ("/F" ^ string_of_int fo, Pdf.Indirect fo)) (setify fontobjnums)))]

let make_annotations pdf annots =
  if annots = [] then Pdf.Dictionary [] else
    Pdf.Dictionary ["/Annots", Pdf.Array (map (function a -> Pdf.Indirect (Pdf.addobj pdf a)) annots)]

let rec number_tags n = function
  | Tag (s, _)::t -> Tag (s, n)::number_tags (n + 1) t
  | h::t -> h::number_tags n t
  | [] -> []

(* At this stage, just Font and Text and HGlue 0. and VGlue 0. and Newline and
   NewPage elements. Split on NewPages, typeset each page, add font
   dictionaries. New page only creates a page when that page has content. *)
let typeset ~process_struct_tree lmargin rmargin tmargin bmargin papersize pdf i =
  Hashtbl.clear width_table_cache;
  let debug = false in
  if debug then (print_endline "***input:\n\n"; print_endline (to_string i));
  let i = number_tags 1 i in
  let i = layout lmargin rmargin papersize i in
  if debug then (print_endline "***after layout:\n\n"; print_endline (to_string i));
  let i = paginate tmargin bmargin papersize i in
  if debug then (print_endline "***after pagination:\n\n"; print_endline (to_string i));
  let height = Pdfunits.points (Pdfpaper.height papersize) (Pdfpaper.unit papersize) in
  let s = initial_state () in
  s.xpos <- lmargin;
  s.ypos <- tmargin;
  let mcidr = ref ~-1 in
  let mcid () = incr mcidr; !mcidr in
  let ops = ref [] in
  let fonts = ref [] in
  let thispagefontnums = ref [] in
  let thispageannotations = ref [] in
  let thisdestrectangles = ref [] in
  let pages = ref [] in
  let tags = ref [] in
  let tagsout = ref [] in
  let write_page () =
    let ops = if process_struct_tree then add_artifacts (rev !ops) else rev !ops in
    let page =
      {Pdfpage.content = if ops = [] then [] else [Pdfops.stream_of_ops ops];
       Pdfpage.mediabox = Pdfpage.rectangle_of_paper papersize;
       Pdfpage.resources = make_resources !thispagefontnums;
       Pdfpage.rotate = Pdfpage.Rotate0;
       Pdfpage.rest = make_annotations pdf !thispageannotations}
    in
      pages := page::!pages;
      tagsout := rev !tags::!tagsout
  in
  let rec typeset_element = function
    | Text cps ->
        ops :=
          Pdfops.Op_Q
          ::Pdfops.Op_ET
          ::Pdfops.Op_Tj (implode cps)
          ::Pdfops.Op_BT
          ::Pdfops.Op_cm (Pdftransform.mktranslate s.xpos (height -. s.ypos))
          ::Pdfops.Op_q
          ::!ops;
        (* If a destination, add the rectangle to the pile of rectangles for this annotation. *)
        if s.dest <> None then
          begin
            let minx, miny = s.xpos, height -. s.ypos in
              thisdestrectangles := (minx, miny, minx +. width_of_string s.width_table cps, miny +. s.fontsize)::!thisdestrectangles
          end;
        s.xpos <- s.xpos +. width_of_string s.width_table cps
    | Font (id, f, fontsize) ->
        let name, objnum =
          match List.assoc_opt id !fonts with
          | Some objnum -> ("/F" ^ string_of_int objnum, objnum)
          | None ->
              let num = Pdftext.write_font pdf f in
              let n = "/F" ^ string_of_int num in
                fonts := (id, num) :: !fonts;
                (n, num)
        in
          s.width_table <- font_widths id f fontsize;
          s.font <- Some f;
          s.fontid <- Some id;
          s.fontsize <- fontsize;
          thispagefontnums := objnum :: !thispagefontnums;
          ops := Pdfops.Op_Tf (name, fontsize)::!ops
    | HGlue len ->
        s.xpos <- s.xpos +. len
    | VGlue len ->
        s.ypos <- s.ypos +. len
    | NewLine ->
        s.xpos <- lmargin;
        typeset_element (VGlue (s.fontsize *. 1.3))
    | NewPage ->
        write_page ();
        thispagefontnums := [];
        thispageannotations := [];
        ops := [];
        mcidr := -1;
        if s.font <> None && s.fontid <> None then typeset_element (Font (unopt s.fontid, unopt s.font, s.fontsize));
        s.xpos <- lmargin;
        s.ypos <- tmargin +. s.fontsize
    | BeginDocument ->
        s.ypos <- tmargin +. s.fontsize
    | BeginDest dest ->
        s.dest <- Some dest
    | EndDest ->
        if !thisdestrectangles <> [] && s.dest <> None then
          let annot (minx, miny, maxx, maxy) =
            Pdf.Dictionary
              [("/Type", Pdf.Name "/Annot");
               ("/Subtype", Pdf.Name "/Link");
               ("/Border", Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real 0.]);
               ("/Rect", Pdf.Array [Pdf.Real minx; Pdf.Real miny; Pdf.Real maxx; Pdf.Real maxy]);
               ("/Dest", Pdfdest.pdfobject_of_destination (unopt s.dest))]
          in
            thispageannotations := map annot !thisdestrectangles @ !thispageannotations;
        s.dest <- None;
        thisdestrectangles := []
   | Tag (s, i) ->
       tags := (s, i)::!tags;
       ops := Pdfops.Op_BDC ("/" ^ s, Pdf.Dictionary [("/MCID", Pdf.Integer (mcid ()))])::!ops
   | EndTag -> ops := Pdfops.Op_EMC::!ops
  in
    iter typeset_element i;
    write_page ();
    (rev !pages, rev !tagsout)
