open Pdfutil
open Cpdferror

(* Implements most Matterhorn checks except for:

   Partially implemented:
     31-009 31-027 Can require looking inside font files
     
     10-002 Doesn't check CID keyed fonts

     11-001 11-002 11-003 11-004 11-005 Natural Language (We just check for
     top-level document language - PDF/UA-2 requires it, and we have no example
     files without it.)

   Unimplemented:
     31-007 31-008 31-011 31-012 31-013 31-014 31-015 31-016 31-018 31-030
     Require looking inside font files *)

type subformat =
  | PDFUA1
  | PDFUA2

let subformat_of_string = function
  | "PDF/UA-1" -> PDFUA1
  | "PDF/UA-2" -> PDFUA2
  | _ -> error "Unknown subformat"

exception MatterhornError of Cpdfyojson.Safe.t

exception MatterhornUnimplemented

let merror () = raise (MatterhornError `Null)
let merror_str s = raise (MatterhornError (`String s))
let unimpl () = raise MatterhornUnimplemented

(* A simple type for structure trees, for doing structure checks. For now just
   the element name, and its children. *)
type st = E of string * st list

(* Now one which contains the attributes too. *)
type st2 = E2 of string * string list * st2 list

let print_children (E (n, cs)) =
  Printf.printf "%S: " n;
  iter (fun (E (n, _)) -> Printf.printf "%S " n) cs;
  flprint "\n"

(* Read attributes. *)
let rec read_single pdf d =
  match Pdf.direct pdf d with
  | Pdf.Dictionary d -> map fst d
  | Pdf.Stream s -> read_single pdf (fst !s)
  | Pdf.Name n -> [n]
  | x -> error ("read_single: " ^ Pdfwrite.string_of_pdf x)

let read_a pdf n stnode =
  match Pdf.lookup_direct pdf n stnode with
  | Some (Pdf.Array attrs) ->
      let attrs = keep (function Pdf.Integer _ -> false | _ -> true) attrs in
        flatten (map (read_single pdf) attrs)
  | Some (Pdf.Name n) -> [n]
  | Some (Pdf.Dictionary d) ->
      read_single pdf (Pdf.Dictionary d)
  | Some (Pdf.Stream s) ->
      read_single pdf (Pdf.Stream s)
  | Some _ -> []
  | None -> []

let read_attributes pdf stnode =
  let from_a = read_a pdf "/A" stnode in
  let from_c = read_a pdf "/C" stnode in
  (* Prefer entries from a, but we are just testing for presence, so merely setify *)
  let attrs = setify (from_a @ from_c) in
  (* For now, stick /ID, /Alt, /ActualText in here too. Eventually, move to prevent crashes. *)
  let alt =
    match Pdf.lookup_direct pdf "/Alt" stnode with | Some _ -> ["/Alt"] | None -> []
  in
  let id =
    match Pdf.lookup_direct pdf "/ID" stnode with | Some _ -> ["/ID"] | None -> []
  in
  let at =
    match Pdf.lookup_direct pdf "/ActualText" stnode with | Some _ -> ["/ActualText"] | None -> []
  in
  let pageref =
    match Pdf.direct pdf stnode with
    | Pdf.Dictionary d ->
        begin match lookup "/Pg" d with
        | Some (Pdf.Indirect i) ->
            ["_" ^ string_of_int i]
        | _ -> []
        end
    | _ -> []
  in
    attrs @ id @ at @ alt @ pageref

let rec read_st_inner pdf stnode =
  let s =
    match Pdf.lookup_direct pdf "/S" stnode with
    | Some (Pdf.Name s) -> s
    | _ -> ""
  in
    match Pdf.lookup_direct pdf "/K" stnode with
    | None -> E2 (s, read_attributes pdf stnode, [])
    | Some (Pdf.Dictionary d) -> E2 (s, read_attributes pdf stnode, [read_st_inner pdf (Pdf.Dictionary d)])
    | Some (Pdf.Integer mcd) -> E2 (s, read_attributes pdf stnode, []) (* marked content identifier, we drop. *)
    | Some (Pdf.Array a) -> E2 (s, read_attributes pdf stnode, read_st_inner_array pdf a)
    | _ -> error "malformed st node"

and read_st_inner_array pdf nodes =
  map (read_st_inner pdf) nodes

let read_st_basic pdf =
  match Pdf.lookup_obj pdf pdf.Pdf.root with
  | Pdf.Dictionary d ->
      begin match lookup "/StructTreeRoot" d with
      | None -> E2 ("/StructTreeRoot", [], [])
      | Some st ->
          match read_st_inner pdf st with
          | E2 (_, a, b) -> E2 ("/StructTreeRoot", a, b)
      end
  | _ -> error "read_st no root"

(* Rewrite a tree according to a rolemap. *)
let rec rewrite_st rolemap (E2 (n, attr, cs)) =
  let rec rewrite_st_name rolemap n =
    match List.assoc_opt n rolemap with
    | Some n' -> rewrite_st_name rolemap n'
    | None -> n
  in
    E2 (rewrite_st_name rolemap n, attr, map (rewrite_st rolemap) cs)

let read_rolemap pdf = function
  | Pdf.Dictionary d ->
      option_map (function (k, Pdf.Name v) -> Some (k, v) | _ -> None) d
  | _ -> error "read_rolemap: not a rolemap"

let read_st2 pdf =
  let rolemap =
    match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/RoleMap"] with
    | Some rm -> read_rolemap pdf rm
    | None -> []
  in
    rewrite_st rolemap (read_st_basic pdf)

let rec st_of_st2 = function E2 (a, _, cs) -> E (a, map st_of_st2 cs)

let rec st_mem p = function
  | E (s, _) when p s -> true
  | E (_, cs) -> List.exists (st_mem p) cs

let string_of_st st =
  let rec convert (E (s, ks)) = `Tuple [`String s; `List (map convert ks)] in
    Cpdfyojson.Safe.pretty_to_string (convert st)

(* Return a list of ops for all pages and form xobjects in a document. *)
let all_ops pdf =
  let form_xobject_ops =
    let objnums =
      Pdf.objselect
        (function Pdf.Stream s when Pdf.lookup_direct pdf "/Subtype" (Pdf.Stream s) = Some (Pdf.Name "/Form") -> true | _ -> false)
        pdf
    in
      map
       (fun streamnum ->
          let stream = Pdf.lookup_obj pdf streamnum in
          let resources = match Pdf.lookup_direct pdf "/Resources" stream with Some d -> d | None -> Pdf.Dictionary [] in
            Pdfops.parse_operators pdf resources [stream])
        objnums
  in
  let page_ops =
    map
      (fun objnum ->
         let stream = Pdf.lookup_obj pdf objnum in
         let resources = match Pdf.lookup_direct pdf "/Resources" stream with Some d -> d | None -> Pdf.Dictionary [] in
         let content = match Pdf.lookup_direct pdf "/Contents" stream with Some (Pdf.Array a) -> a | Some x -> [x] | None -> [] in
           Pdfops.parse_operators pdf resources content)
      (Pdf.page_reference_numbers pdf)
  in
    form_xobject_ops @ page_ops

let rec artifact_in_content c a = function
  | [] -> ()
  (* Start content. *)
  | Pdfops.Op_BDC _::t -> artifact_in_content true a t
  (* Start artifact. If already in content, this is an error. *)
  | Pdfops.Op_BMC "/Artifact"::t -> if c then merror () else artifact_in_content c true t
  (* End artifact or content, whichever is true. If both true, an error. *)
  | Pdfops.Op_EMC::t -> if c && a then merror () else if c then artifact_in_content false a t else artifact_in_content c false t
  (* Anything else *)
  | h::t -> artifact_in_content c a t

let rec content_in_artifact c a = function
  | [] -> ()
  (* Start content. If already in artifact, this is an error. *)
  | Pdfops.Op_BDC _::t -> if a then merror () else content_in_artifact true a t
  (* Start artifact. *)
  | Pdfops.Op_BMC "/Artifact"::t -> content_in_artifact c true t
  (* End artifact or content, whichever is true. If both true, an error. *)
  | Pdfops.Op_EMC::t -> if c && a then merror () else if c then content_in_artifact false a t else content_in_artifact c false t
  (* Anything else *)
  | h::t -> content_in_artifact c a t

(* Content marked as Artifact is present inside tagged content. *)
let matterhorn_01_003 _ _ pdf =
  iter (fun ops -> artifact_in_content false false ops) (all_ops pdf)

(* Tagged content is present inside content marked as Artifact. *)
let matterhorn_01_004 _ _ pdf =
  iter (fun ops -> content_in_artifact false false ops) (all_ops pdf)

(* Content is neither marked as Artifact nor tagged as real content. *)

(* Which operations are real? *)
let op_is_real = function
  | Pdfops.(  Op_m _ | Op_l _ | Op_c _ | Op_v _ | Op_y _ | Op_h | Op_re _ | Op_S | Op_s | Op_f | Op_F | Op_f'
            | Op_B | Op_B' | Op_b | Op_b' | Op_n | Op_W | Op_W' | Op_BT | Op_ET | Op_Tj _ | Op_TJ _ | Op_' _ 
            | Op_'' _ | Op_sh _ | InlineImage _ | Op_Do _) -> true
  | _ -> false

(* Look at a list of ops and return operators neither marked as neither artifect nor content *)
let naked_ops ops = []

let matterhorn_01_005 _ _ pdf =
  iter (fun ops -> if List.exists op_is_real (naked_ops ops) then merror ()) (all_ops pdf)

(* Suspects entry has a value of true. *)
let matterhorn_01_007 _ _ pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/MarkInfo"; "/Suspects"] with
  | Some (Pdf.Boolean true) -> merror ()
  | _ -> ()

let standard_structure_types_2008 =
  ["/Document"; "/Part"; "/Sect"; "/Div"; "/NonStruct"; "/P"; "/H1"; "/H2";
  "/H3"; "/H4"; "/H5"; "/H6"; "/H"; "/Lbl"; "/Span"; "/Link"; "/Annot";
  "/Form"; "/Ruby"; "/RB"; "/RT"; "/RP"; "/Warichu"; "/WT"; "/WP"; "/L"; "/LI";
  "/LBody"; "/Table"; "/TR"; "/TH"; "/TD"; "/THead"; "/TBody"; "/TFoot";
  "/Caption"; "/Figure"; "/Formula"; "/Art"; "/BlockQuote"; "/TOC"; "/TOCI";
  "/Index"; "/Private"; "/Quote"; "/Note"; "/Reference"; "/Code"; "/BibEntry"]

let rec follow_standard rm n =
  match List.assoc_opt n rm with
  | None -> raise Exit
  | Some x when mem x standard_structure_types_2008 -> ()
  | Some x -> follow_standard rm x

let circular rm =
  let rec circular n k rm =
    n < 0 || match List.assoc_opt k rm with None -> false | Some k' -> circular (n - 1) k' rm
  in
    List.exists (fun k -> circular (length rm) k rm) (map fst rm)

(* One or more non-standard tag’s mapping does not terminate with a standard
type. *)
let matterhorn_02_001 _ _ pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/RoleMap"] with
  | Some rm ->
      let rolemap = read_rolemap pdf rm in
        if circular rolemap then () else (* Will be reported below *)
          iter (fun x -> try follow_standard rolemap x with Exit -> merror_str x) (map fst rolemap)
  | None -> ()

(* A circular mapping exists. *)
let matterhorn_02_003 _ _ pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/RoleMap"] with
  | Some rm ->
      let rolemap = read_rolemap pdf rm in
        if circular rolemap then
          merror_str "STOP. If rolemap circular, cannot proceed with other checks."
          (* We never reach here in normal use. This is pre-checked. *)
  | None -> ()

(* One or more standard types are remapped. *)
let matterhorn_02_004 _ _ pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/RoleMap"] with
  | Some rm ->
      let rolemap = read_rolemap pdf rm in
        iter (function k -> if mem k standard_structure_types_2008 then merror_str k) (map fst rolemap)
  | None -> ()

(* Document does not contain an XMP metadata stream *)
let matterhorn_06_001 _ _ pdf =
  match Cpdfmetadata.get_metadata pdf with
  | Some _ -> ()
  | None -> merror ()

(* The XMP metadata stream in the Catalog dictionary does not include the
   PDF/UA identifier. *)
let matterhorn_06_002 _ _ pdf =
  match Cpdfmetadata.get_metadata pdf with
  | Some metadata ->
      let _, tree = Cpdfmetadata.xmltree_of_bytes metadata in
        begin match Cpdfmetadata.get_data_for Cpdfmetadata.pdfuaid "part" tree with
        | Some _ -> ()
        | None -> merror ()
        end
  | None -> () (* case covered by test 06_001 above, no need for two failures *)

(* XMP metadata stream does not contain dc:title *)
let matterhorn_06_003 _ _ pdf =
  match Cpdfmetadata.get_metadata pdf with
  | Some metadata ->
      let _, tree = Cpdfmetadata.xmltree_of_bytes metadata in
        begin match Cpdfmetadata.get_data_for Cpdfmetadata.dc "title" tree with
        | Some _ -> ()
        | None -> merror ()
        end
  | None -> () (* case covered by test 06_001 above, no need for two failures *)

(* ViewerPreferences dictionary of the Catalog dictionary does not contain a
   DisplayDocTitle entry. *)
let matterhorn_07_001 _ _ pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/ViewerPreferences"; "/DisplayDocTitle"] with
  | None -> merror ()
  | _ -> ()

(* ViewerPreferences dictionary of the Catalog dictionary contains a
   DisplayDocTitle entry with a value of false. *)
let matterhorn_07_002 _ _ pdf = 
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/ViewerPreferences"; "/DisplayDocTitle"] with
  | Some (Pdf.Boolean false) -> merror ()
  | _ -> ()

(* A table-related structure element is used in a way that does not conform to
   the syntax defined in ISO 32000-1, Table 337. We assume no nesting of whole
   tables, since it is not excplicitly mentioned in the spec. *)
let matterhorn_09_004 st st2 pdf =
  let rec check_table = function
  | E ("/Table", cs) ->
      let cs =
        begin match cs with
        | E ("/Caption", _)::cs -> cs
        | l ->
            begin match rev cs with
            | E ("/Caption", _)::cs -> rev cs
            | cs -> rev cs
            end
        end
      in
        (* A) 1 or more /TRs is ok *)
        if List.for_all (function E ("/TR", cs) -> iter check_tr cs; true | _ -> false) cs then () else
        (* B) 0 or 1 /THead followed by 1 or n /TBody followed by 0 or 1 /TFoot *)
        begin
          check_thead_tbody_tfoot cs;
          let without_thead =
            match cs with
            | E ("/THead", _)::cs -> cs
            | cs -> cs
          in
          let without_tfoot =
            match rev without_thead with
            | E ("/TFoot", _)::cs -> cs
            | cs -> cs
          in
            if List.exists (function E ("/TBody", _) -> false | _ -> true) without_tfoot then
              merror_str "Top-level /Table not of required form"
        end
  | E (_, cs) -> iter check_table cs
  and check_tr = function
    | E (("/TH" | "/TD"), _) -> ()
    | _ -> merror_str "Every /TR element must be a /TH or /TD"
  and check_thead_tbody_tfoot cs =
    iter
      (fun node ->
         if List.exists (function E ("/TR", _) -> false | _ -> true) node then
           merror_str "Element in /THead | /TBody | /TFoot not a /TR")
      (map (function (E (_, cs')) -> cs') cs)
  in
    check_table st

(* A list-related structure element is used in a way that does not conform to
   Table 336 in ISO 32000-1. *)
let matterhorn_09_005 st st2 pdf =
  let rec check_l = function
    | E ("/L", cs) ->
        (* 0 or 1 captions *)
        let cs = match cs with E ("/Caption", _)::cs | cs -> cs in
          (* 1 or n /LI *)
          begin match cs with
          | [] -> merror_str "No /LI in /L"
          | cs -> iter check_li cs
          end
    | E (_, cs) ->
        iter check_l cs
  and check_li = function
    (* for each /LI, 1 or n /Lbl or /LBody or both *)
    | E ("/LI", []) -> merror_str "Empty /LI"
    | E ("/LI", cs) -> iter check_li_child cs
    | E ("/L", cs) -> check_l (E ("/L", cs))
    | E (n, _) -> merror_str ("Unknown child of /LI: " ^ n)
    (* need to check all children of /LBody too, to see if any is /L *)
  and check_li_child = function
    | E (("/LBody"| "/Lbl"), cs) -> iter check_l cs
    | E (_, _) -> merror_str "Child of /LI must be /Lbl or /LBody"
  in
    check_l st

(* A TOC-related structure element is used in a way that does not conform to
   Table 333 in ISO 32000-1. *)


(* We test two things: a) everything under a TOC is correct; and b) There is no
TOCI except under a TOC. *)
let matterhorn_09_006 st st2 pdf =
  let seen_toc = ref false in
    let rec check_toplevel_TOCI n =
      begin match n with
      | E ("/TOC", cs) -> set seen_toc
      | E ("/TOCI", cs) -> if not !seen_toc then merror_str "TOCI without TOC above"
      | E (_, cs) -> ()
      end;
      begin match n with
      | E (_, cs) -> iter check_toplevel_TOCI cs
      end
   in
   let rec check_toc_toci = function
     | E ("/TOC", cs) ->
         if
           List.exists (function E (("/TOC" | "/TOCI"), _) -> false | _ -> true) cs
         then
           merror_str "/TOC children must be /TOC or /TOCI";
         iter check_toc_toci cs
     | E ("/TOCI", cs) ->
         if
           List.exists (function E (("/TOC" | "/Lbl" | "/Reference" | "/P" | "/NonStruct"), _) -> false | _ -> true) cs
         then
           merror_str "Bad child of /TOCI";
         iter check_toc_toci cs
     | E (_, cs) ->
         iter check_toc_toci cs
   in
     check_toplevel_TOCI st;
     check_toc_toci st

(* A Ruby-related structure element is used in a way that does not conform to
   Table 338 in ISO 32000-1. *)
let matterhorn_09_007 st st2 pdf =
  let rec check_ruby = function
    | E ("/Ruby", cs) ->
        if List.exists (function (E (("/RB" | "/RT" | "RP"), _)) -> false | _ -> true) cs then merror () 
    | E (_, cs) ->
        iter check_ruby cs
  in
    check_ruby st

(* A Warichu-related structure element is used in a way that does not conform
   to Table 338 in ISO 32000-1. *)
let matterhorn_09_008 st st2 pdf =
  let rec check_warichu = function
    | E ("/Ruby", cs) ->
        if List.exists (function (E (("/WT" | "/WP"), _)) -> false | _ -> true) cs then merror () 
    | E (_, cs) ->
        iter check_warichu cs
  in
    check_warichu st

(* Character code cannot be mapped to Unicode. *)
let matterhorn_10_001 _ _ pdf =
  (* Each font in the PDF must either, per 9.10.2 in the standard
      a) Have a /ToUnicode entry; or
      b) Be a simple font with a simple encoding; or
      c) Be a CIDFont matching certain parameters *)
  let check_diffs diffs =
    let allowed_names =
      map fst
        (Pdfglyphlist.name_to_symbol
       @ Pdfglyphlist.name_to_win
       @ Pdfglyphlist.name_to_standard
       @ Pdfglyphlist.name_to_pdf
       @ Pdfglyphlist.name_to_macroman)
  in
    let names =
      match diffs with
      | Pdf.Array a -> option_map (function Pdf.Name n -> Some n | _ -> None) a
      | _ -> []
    in
      if not (List.for_all (mem' allowed_names) names) then merror ()
  in
  let check_font font =
    (*Printf.printf "Check font: %s\n" (Pdfwrite.string_of_pdf font);*)
    match Pdf.lookup_direct pdf "/ToUnicode" font with
    | Some _ -> (* a) *) ()
    | _ ->
        match Pdf.lookup_direct pdf "/Encoding" font with
        | Some (Pdf.Name ("/MacRomanEncoding" | "/MacExpertEncoding" | "/WinAnsiEncoding")) -> () (* b) 1 *)
        | Some d ->
            begin match Pdf.lookup_direct pdf "/Differences" d with
            | Some diffs -> check_diffs diffs (* b) 2 *)
            | None -> merror ()
            end
        | None ->
            match Pdf.lookup_direct pdf "/Subtype" font with
            | Some (Pdf.Name "/Type0") ->
              (* c) *)
              unimpl ()
            | _ -> merror ()
  in
    let fonts = map (fun (_, _, _, _, _, x) -> x) (Cpdffont.list_fonts pdf (ilist 1 (Pdfpage.endpage pdf))) in
      iter
        (fun o ->
             match Pdf.lookup_direct pdf "/Type" o, Pdf.lookup_direct pdf "/Subtype" o with
             | Some (Pdf.Name "/Font"), Some (Pdf.Name ("/CIDFontType0" | "/CIDFontType2")) -> ()
             | Some (Pdf.Name "/Font"), _ -> check_font o
             | _ -> ())
        fonts

(* If the top-level /Lang is present, that rules all and is sufficient. *)

(* Natural language for text in page content cannot be determined. *)
let matterhorn_11_001 _ _ pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/Lang"] with
  | Some (Pdf.String "") | None -> merror_str "No top-level /Lang"
  | Some _ -> ()

(* Natural language for text in Alt, ActualText and E attributes cannot be
   determined. *)
let matterhorn_11_002 _ _ pdf = unimpl ()

(* Natural language in the Outline entries cannot be determined. *)
let matterhorn_11_003 _ _ pdf = unimpl ()

(* Natural language in the Contents entry for annotations cannot be determined.
 *)
let matterhorn_11_004 _ _ pdf = unimpl ()

(* Natural language in the TU entry for form fields cannot be determined. *)
let matterhorn_11_005 _ _ pdf = unimpl ()

(* Natural language for document metadata cannot be determined. *)
let matterhorn_11_006 _ _ pdf = unimpl ()

(* <Figure> tag alternative or replacement text missing. *)
let matterhorn_13_004 _ st2 pdf =
  let rec check_fig = function
  | E2 ("/Figure", attrs, cs) ->
      if not (mem "/Alt" attrs || mem "/ActualText" attrs) then merror ();
      iter check_fig cs
  | E2 (_, _, cs) ->
      iter check_fig cs
  in
    check_fig st2

let is_hnum s =
  match explode s with
  | ['/'; 'H'] -> false
  | '/'::'H'::cs ->
      begin try ignore (int_of_string (implode cs)); true with Failure _ -> false end
  | _ -> false

let num s = int_of_string (implode (tl (tl (explode s))))

let rec headings_list_of_tree (E (n, cs)) =
  (if is_hnum n then [n] else []) @ flatten (map headings_list_of_tree cs)

(* Does use numbered headings, but the first heading tag is not <H1>. *)
let matterhorn_14_002 st _ _ =
  match headings_list_of_tree st with
  | [] | "/H1"::_ -> ()
  | _ -> merror ()

(* Numbered heading levels in descending sequence are skipped (Example: <H3>
   follows directly after <H1>). *)
let matterhorn_14_003 st _ _ =
  let rec check l = function
  | [] -> ()
  | n::ns ->
      let nm = num n in
        if nm > l + 1 then merror_str (Printf.sprintf "%i -> %i" l nm) else check nm ns
  in
    check 1 (headings_list_of_tree st)
  
(* A node contains more than one <H> tag. *)
let matterhorn_14_006 st st2 pdf =
  let found = ref false in
  let rec check_hs (E (_, cs)) =
    if length (option_map (function E ("/H", _) -> Some () | _ -> None) cs) > 1 then set found;
    iter check_hs cs
  in
    check_hs st;
    if !found then merror ()

(* Document uses both <H> and <H#> tags. *)
let matterhorn_14_007 st st2 pdf =
  if st_mem (eq "/H") st && st_mem is_hnum st then merror ()

(* In a table not organized with Headers attributes and IDs, a <TH> cell does
   not contain a Scope attribute. *)
let matterhorn_15_003 st st2 pdf =
  (* For now, we complain any time a <TH> does not have a scope. The 2008 PDF
     spec, 2014 PDF/UA spec and Matterhorn protocol combined do not quite give
     enough information to know what is required. To be returned to. *)
  let rec check_th = function
  | E2 ("/TH", attr, _) ->
      if not (List.mem "/Scope" attr) then merror_str "No scope, table organization not checked."
  | E2 (_, _, cs) -> iter check_th cs
  in
    check_th st2

(* <Formula> tag is missing an Alt attribute. *)
let matterhorn_17_002 _ st2 pdf =
  let rec check_fm = function
  | E2 ("/Formula", attr, _) ->
      if not (List.mem "/Alt" attr) then merror ()
  | E2 (_, _, cs) -> iter check_fm cs
  in
    check_fm st2

(* Unicode mapping requirements are not met. *)
let matterhorn_17_003 _ _ pdf =
  (* Covered elsewhere, since all text in the PDF must meet these requirements,
     mathematical expressions need not be considered separately. *)
  ()

(* ID entry of the <Note> tag is not present. *)
let matterhorn_19_003 st st2 pdf =
  let rec check_note = function
  | E2 ("/Note", attr, _) ->
      if not (List.mem "/ID" attr) then merror ()
  | E2 (_, _, cs) -> iter check_note cs
  in
    check_note st2

(* ID entry of the <Note> tag is non-unique. *)
let matterhorn_19_004 _ _ pdf =
  (* Looking for /Type /StructElem /N /Note /ID to exist. *)
  let ids = ref [] in
    Pdf.objiter
      (fun _ x ->
         match Pdf.lookup_direct pdf "/Type" x, Pdf.lookup_direct pdf "/S" x, Pdf.lookup_direct pdf "/ID" x with
         | Some (Pdf.Name "/StructElem"), Some (Pdf.Name "/Note"), Some (Pdf.String s) -> ids := s::!ids
         | _ -> ())
      pdf;
    if length (setify_large !ids) < length !ids then merror ()

(* Name entry is missing or has an empty string as its value in an Optional
   Content Configuration Dictionary in the Configs entry in the OCProperties
   entry in the Catalog dictionary. *)
let matterhorn_20_001 _ _ pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/OCProperties"; "/Configs"] with
  | Some (Pdf.Array occds) ->
      iter (function x -> match Pdf.lookup_direct pdf "/Name" x with None | Some (Pdf.Name "") -> merror () | _ -> ()) occds
  | _ -> ()

(* Name entry is missing or has an empty string as its value in an Optional
   Content Configuration Dictionary that is the value of the D entry in the
   OCProperties entry in the Catalog dictionary. *)
let matterhorn_20_002 _ _ pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/OCProperties"; "/D"],
        Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/OCProperties"; "/D"; "/Name"]
  with
  | Some _, (Some (Pdf.String "") | None) -> merror ()
  | _ -> ()

(* An AS entry appears in an Optional Content Configuration Dictionary. *)
let matterhorn_20_003 _ _ pdf =
  begin match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/OCProperties"; "/D"; "/AS"] with
  | Some _ -> merror ()
  | _ -> ()
  end;
  begin match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/OCProperties"; "/Configs"] with
  | Some (Pdf.Array occds) ->
      iter (function x -> match Pdf.lookup_direct pdf "/AS" x with Some _ -> merror () | _ -> ()) occds
  | _ -> ()
  end

(* The file specification dictionary for an embedded file does not contain F
   and UF entries. *)
let matterhorn_21_001 _ _ pdf =
  let from_nametree =
    match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/Names"; "/EmbeddedFiles"] with
    | Some embeddedfiles -> map snd (Pdf.contents_of_nametree pdf embeddedfiles)
    | _ -> []
  in
  let from_annots =
    option_map
      (fun x ->
         if x.Pdfannot.subtype = Pdfannot.FileAttachment
           then Pdf.lookup_direct pdf "/FS" x.Pdfannot.annotrest
           else None)
      (flatten (map (Pdfannot.annotations_of_page pdf) (Pdfpage.pages_of_pagetree pdf)))
  in
  if
    List.exists
      (fun x ->
         match Pdf.lookup_direct pdf "/F" x, Pdf.lookup_direct pdf "/UF" x with
         | Some _, Some _ -> false
         | _ -> true)
      (from_nametree @ from_annots)
  then
    merror ()

(* File contains the dynamicRender element with value “required”. *)
let matterhorn_25_001 _ _ pdf =
  let rec contains_required_dynamicRender = function
    | Cpdfmetadata.E (((_, "dynamicRender"), _), [Cpdfmetadata.D "required"]) -> true
    | Cpdfmetadata.E (_, children) -> List.exists contains_required_dynamicRender children
    | Cpdfmetadata.D _ -> false
  in
    match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/AcroForm"; "/XFA"] with
    | Some (Pdf.Array xfa) ->
        begin match option_map (function (Pdf.String "config", x) -> Some x | _ -> None) (pairs xfa) with
        | [config] ->
            begin match Pdf.direct pdf config with
            | Pdf.Stream _ as s ->
              Pdfcodec.decode_pdfstream pdf s;
              begin match s with
              | Pdf.Stream {contents = _, Pdf.Got xmlstream} ->
                  let _, tree = Cpdfmetadata.xmltree_of_bytes xmlstream in
                    if contains_required_dynamicRender tree then merror ()
              | _ -> assert false
              end
            | _ -> ()
            end
        | _ -> ()
        end
    | _ -> ()

(* The file is encrypted but does not contain a P entry in its encryption
   dictionary. *)
let matterhorn_26_001 _ _ pdf = ()
  (* Would already have failed at this point, because CamlPDF does not allow
  the decryption of a file with no /P. So this is never reported. A file without
  a /P is simply reported as malformed upon reading. *)

(* The file is encrypted and does contain a P entry but the 10th bit position
   of the P entry is false. *)
let matterhorn_26_002 _ _ pdf =
  match pdf.Pdf.saved_encryption with
  | None -> ()
  | Some {Pdf.from_get_encryption_values = (_, _, _, p, _, _, _)} ->
      if mem Pdfcrypt.NoExtract (Pdfcrypt.banlist_of_p p) then merror ()

let read_parent_tree pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/ParentTree"] with
  | Some t -> Pdftree.read_number_tree pdf t
  | None -> [] 

(* An annotation, other than of subtype Widget, Link and PrinterMark, is not a
   direct child of an <Annot> structure element. *)
let matterhorn_28_002 _ _ pdf =
  let parent_tree = read_parent_tree pdf in
    (* Find object numbers of all annotations which are not Widget, Link, or Printermark. *)
    Pdf.objiter
      (fun n obj -> match Pdf.lookup_direct pdf "/Subtype" obj with
      | Some (Pdf.Name
                ("/Stamp" | "/Line" | "Square" | "/Circle" | "/Polygon" | "/PolyLine" |
                 "/Highlight" | "/Underline" | "/Squiggly" | "/StrikeOut" | "/Caret" |
                 "/Ink" | "/FileAttachment" | "/Sound" | "/Movie" | "/Screen" | "/TrapNet" |
                 "/Watermark" | "/3D")) ->
        (* Check that every /StructParent entry for each of these points to something
        with /S /Annot. No need to worry about rolemapping, because PDF/UA docs
        aren't allowed to remap standard types. *)
        begin match Pdf.lookup_direct pdf "/StructParent" obj with
        | Some (Pdf.Integer i) ->
            begin match List.assoc_opt (string_of_int i) parent_tree with
            | Some d ->
                begin match Pdf.lookup_direct pdf "/S" d with
                | Some (Pdf.Name "/Annot") -> ()
                | _ -> merror ()
                end
            | None -> merror ()
            end
        | _ -> merror ()
        end
      | _ -> ())
      pdf

(* An annotation, other than of subtype Widget, does not have a Contents entry
   and does not have an alternative description (in the form of an Alt entry in
   the enclosing structure element). *)
(* NB for future: "The requirements of this clause shall not apply to
   annotations whose hidden flag is set or whose rectangle is outside the
   CropBox or whose Subtype is Popup." *)
let matterhorn_28_004 _ _ pdf =
  let parent_tree = read_parent_tree pdf in
    Pdf.objiter
      (fun n obj ->
       (*flprint (Pdfwrite.string_of_pdf obj ^ "\n");*)
      match Pdf.lookup_direct pdf "/Subtype" obj with
      | Some (Pdf.Name
                ("/Stamp" | "/Line" | "Square" | "/Circle" | "/Polygon" | "/PolyLine" |
                 "/Highlight" | "/Underline" | "/Squiggly" | "/StrikeOut" | "/Caret" |
                 "/Ink" | "/FileAttachment" | "/Sound" | "/Movie" | "/Screen" | "/TrapNet" |
                 "/Watermark" | "/3D" | "/Link" | "/PrinterMark")) ->
        begin match Pdf.lookup_direct pdf "/Contents" obj with
        | Some _ -> ()
        | None ->
          begin match Pdf.lookup_direct pdf "/StructParent" obj with
          | Some (Pdf.Integer i) ->
              begin match List.assoc_opt (string_of_int i) parent_tree with
              | Some d ->
                  begin match Pdf.lookup_direct pdf "/Alt" d with
                  | Some _ -> ()
                  | _ -> merror ()
                  end
              | None -> merror ()
              end
          | _ -> () (* Ok, since not part of structure tree. *)
          end
        end
      | _ -> ())
      pdf

(* A form field does not have a TU entry and does not have an alternative
   description (in the form of an Alt entry in the enclosing structure
   element). *)
let get_field_object_numbers pdf =
  let rec get_field_object_numbers_inner obj =
    match obj with
    | Pdf.Indirect i ->
        (* Is this referenced item a field (rather than an annotation alone?).
           If so, count it, and recurse on /Kids. *)
        begin match Pdf.lookup_direct pdf "/T" (Pdf.Indirect i) with
        | Some _ ->
            begin match Pdf.lookup_direct pdf "/Kids" (Pdf.Indirect i) with
            | Some (Pdf.Array kids) -> i::flatten (map get_field_object_numbers_inner kids)
            | _ -> [i]
            end
        | None -> []
        end
    | x ->
       Pdfe.log (Printf.sprintf "get_field_object_numbers_inner: non-indirect Kid %S\n" (Pdfwrite.string_of_pdf x));
       []
  in
    match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/AcroForm"; "/Fields"] with
    | Some (Pdf.Array toplevelfields) ->
        flatten (map get_field_object_numbers_inner toplevelfields) 
    | _ ->
        []

let matterhorn_28_005 _ _ pdf =
  let missing_tu =
    option_map
      (function objnum ->
        match Pdf.lookup_direct pdf "/T" (Pdf.Indirect objnum) with
        | Some _ -> None
        | None -> Some objnum)
      (get_field_object_numbers pdf)
  in
   if missing_tu = [] then () else
     (* Check for alts in enclosing. We look for /StructParent (from merged annotation, not field) *)
     let parent_tree = read_parent_tree pdf in
       iter
         (fun objnum ->
           let obj = Pdf.lookup_obj pdf objnum in
              match Pdf.lookup_direct pdf "/StructParent" obj with
              | Some (Pdf.Integer i) ->
                  begin match List.assoc_opt (string_of_int i) parent_tree with
                  | Some d ->
                      begin match Pdf.lookup_direct pdf "/Alt" d with
                      | None -> merror ()
                      | _ -> ()
                      end
                  | _ -> merror ()
                  end
              | _ -> merror ())
         missing_tu

(* An annotation with subtype undefined in ISO 32000 does not meet 7.18.1. *)
let matterhorn_28_006 _ _ pdf =
  if
    List.exists
      (fun x -> match x.Pdfannot.subtype with Pdfannot.Unknown _ -> true | _ -> false)
      (flatten (map (Pdfannot.annotations_of_page pdf) (Pdfpage.pages_of_pagetree pdf)))
  then
    merror ()

(* An annotation of subtype TrapNet exists. *)
let matterhorn_28_007 _ _ pdf =
  if
    List.exists
      (fun x -> x.Pdfannot.subtype = Pdfannot.TrapNet)
      (flatten (map (Pdfannot.annotations_of_page pdf) (Pdfpage.pages_of_pagetree pdf)))
  then
    merror ()

(* A page containing an annotation does not contain a Tabs entry *)
let matterhorn_28_008 _ _ pdf =
  if
    List.exists
      (fun p ->
         Pdfannot.annotations_of_page pdf p <> [] && Pdf.lookup_direct pdf "/Tabs" p.Pdfpage.rest = None)
      (Pdfpage.pages_of_pagetree pdf)
  then
    merror ()

(* A page containing an annotation has a Tabs entry with a value other than S.
 *)
let matterhorn_28_009 _ _ pdf =
  if
    List.exists
      (fun p ->
         Pdfannot.annotations_of_page pdf p <> [] &&
         Pdf.lookup_direct pdf "/Tabs" p.Pdfpage.rest <> None && (* already covered by 28_008 above. *)
         Pdf.lookup_direct pdf "/Tabs" p.Pdfpage.rest <> Some (Pdf.Name "/S"))
      (Pdfpage.pages_of_pagetree pdf)
  then
    merror ()

(* A widget annotation is not nested within a <Form> tag. *)
let matterhorn_28_010 _ _ pdf =
  let parent_tree = read_parent_tree pdf in
    Pdf.objiter
      (fun _ o ->
         match Pdf.lookup_direct pdf "/Subtype" o with
         | Some (Pdf.Name "/Widget") ->
             begin match Pdf.lookup_direct pdf "/StructParent" o with
             | Some (Pdf.Integer i) ->
                 begin match List.assoc_opt (string_of_int i) parent_tree with
                 | Some d ->
                     begin match Pdf.lookup_direct pdf "/S" d with
                     | Some (Pdf.Name "/Form") -> ()
                     | _ -> merror ()
                     end
                 | _ -> merror ()
                 end
             | _ -> merror ()
             end
         | _ -> ())
      pdf

(* A link annotation is not nested within a <Link> tag. *)
let matterhorn_28_011 _ _ pdf =
  let parent_tree = read_parent_tree pdf in
    Pdf.objiter
      (fun _ o ->
         match Pdf.lookup_direct pdf "/Subtype" o with
         | Some (Pdf.Name "/Link") ->
             begin match Pdf.lookup_direct pdf "/StructParent" o with
             | Some (Pdf.Integer i) ->
                 begin match List.assoc_opt (string_of_int i) parent_tree with
                 | Some d ->
                     begin match Pdf.lookup_direct pdf "/S" d with
                     | Some (Pdf.Name "/Link") -> ()
                     | _ -> merror ()
                     end
                 | _ -> merror ()
                 end
             | _ -> () (* Not part of structure tree. That's ok. *)
             end
         | _ -> ())
      pdf

(* A link annotation does not include an alternate description in its Contents
   entry. *)
let matterhorn_28_012 _ _ pdf =
  if
    List.exists
      (fun x -> x.Pdfannot.subtype = Pdfannot.Link && x.Pdfannot.annot_contents = None )
      (flatten (map (Pdfannot.annotations_of_page pdf) (Pdfpage.pages_of_pagetree pdf)))
  then
    merror ()

(* CT entry is missing from the media clip data dictionary. *)
let matterhorn_28_014 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Type" o, Pdf.lookup_direct pdf "/S" o, Pdf.lookup_direct pdf "/CT" o with
       | Some (Pdf.Name "/MediaClip"), Some (Pdf.Name "/MCD"), None -> merror ()
       | _ -> ())
    pdf

(* Alt entry is missing from the media clip data dictionary. *)
let matterhorn_28_015 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Type" o, Pdf.lookup_direct pdf "/S" o, Pdf.lookup_direct pdf "/CT" o with
       | Some (Pdf.Name "/MediaClip"), Some (Pdf.Name "/MCD"), None -> merror ()
       | _ -> ())
    pdf

(* File attachment annotations do not conform to 7.11. *)
let matterhorn_28_016 _ _ pdf =
  (* Covered by 21_001 above *)
  ()

(* A PrinterMark annotation is included in the logical structure. *)
let matterhorn_28_017 _ _ pdf =
  Pdf.objiter
    (fun _ o -> match Pdf.lookup_direct pdf "/Subtype" o, Pdf.lookup_direct pdf "/StructParent" o with
     | Some (Pdf.Name "/PrinterMark"), Some _ -> merror ()
     | _ -> ())
    pdf

(* The appearance stream of a PrinterMark annotation is not marked as Artifact.
 *)
let matterhorn_28_018 _ _ pdf =
  let annotations =
    map
      (Pdf.lookup_obj pdf)
      (Pdf.objselect (fun o -> match Pdf.lookup_direct pdf "/Subtype" o with Some (Pdf.Name "/PrinterMark") -> true | _ -> false) pdf)
  in
  let form_xobjects =
    let ns = option_map (fun a -> Pdf.lookup_chain pdf a ["/AP"; "/N"]) annotations in
      flatten
        (map
           (function Pdf.Dictionary d -> (map (Pdf.direct pdf) (map snd d)) | x -> [x])
           ns)
  in
  iter
    (fun stream ->
       let resources = match Pdf.lookup_direct pdf "/Resources" stream with Some d -> d | None -> Pdf.Dictionary [] in
       let ops = Pdfops.parse_operators pdf resources [stream] in
         if Cpdftype.add_artifacts ops <> ops then merror ())
    form_xobjects

(* A reference XObject is present. *)
let matterhorn_30_001 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o, Pdf.lookup_direct pdf "/Ref" o with
       | Some (Pdf.Name "/Form"), Some _ -> merror ()
       | _ -> ())
    pdf

(* Form XObject contains MCIDs and is referenced more than once. *)
let matterhorn_30_002 _ _ pdf =
  let contains_mcid n =
    let obj = Pdf.lookup_obj pdf n in
    let ops = Pdfops.parse_operators pdf (match Pdf.lookup_direct pdf "/Resources" obj with Some r -> r | None -> Pdf.Dictionary []) [obj] in
     keep (function Pdfops.Op_BDC (n, d) when Pdf.lookup_direct pdf "/MCID" d <> None -> true | _ -> false) ops <> []
  in
  (* 0. Regularize inheritance amongst pages. *)
  Pdfpage.replace_inherit pdf (Pdf.page_reference_numbers pdf);
  (* 1. Find list of xobject object numbers *)
  let xobj_objnums =
    Pdf.objselect (function o -> match Pdf.lookup_direct pdf "/Subtype" o with Some (Pdf.Name "/Form") -> true | _ -> false) pdf
  in
  (* 2. Trim to only ones containing MCIDs *)
  let containing_mcids = keep contains_mcid xobj_objnums in
  (* 3. Find from which place each of these is referenced. For pages, it's from
     a page. For Xobjects, it's from an xobject. But, two pages (or xobjects)
     could reference an xobject which references an xobject with an MCID - so
     there is transitivity to deal with! *)
    ()

(* A Type 0 font dictionary with encoding other than Identity-H and Identity-V
   has values for Registry in both CIDSystemInfo dictionaries that are not
   identical. *)
let matterhorn_31_001 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o, Pdf.lookup_direct pdf "/Encoding" o with
       | Some (Pdf.Name "/Type0"), Some (Pdf.Name ("/Identity-H" | "/Identity-V")) -> ()
       | Some (Pdf.Name "/Type0"), _ ->
           merror_str
             "Advisory: contains composite font with non-identity encoding. Cpdf\
              cannot check the CIDSystemInfo entries are identical automatically."
       | _ -> ())
    pdf

(* A Type 0 font dictionary with encoding other than Identity-H and Identity-V
   has values for Ordering in both CIDSystemInfo dictionaries that are not
   identical. *)
let matterhorn_31_002 st st2 pdf =
  matterhorn_31_001 st st2 pdf

(* A Type 0 font dictionary with encoding other than Identity-H and Identity-V
   has a value for Supplement in the CIDSystemInfo dictionary of the CID font
   that is less than the value for Supplement in the CIDSystemInfo dictionary
   of the CMap. *)
let matterhorn_31_003 st st2 pdf =
  matterhorn_31_001 st st2 pdf

(* A Type 2 CID font contains neither a stream nor the name Identity as the
   value of the CIDToGIDMap entry. *)
let matterhorn_31_004 _ _ pdf =
  Pdf.objiter
    (fun _ n ->
       match Pdf.lookup_direct pdf "/Subtype" n with
       | Some (Pdf.Name "/CIDFontType2") ->
           begin match Pdf.lookup_direct pdf "/CIDToGIDMap" n with
           | Some (Pdf.Name "/Identity" | Pdf.Stream _) -> ()
           | _ -> merror_str (Pdfwrite.string_of_pdf n)
           end
       | _ -> ())
    pdf

(* A Type 2 CID font does not contain a CIDToGIDMap entry. *)
let matterhorn_31_005 _ _ pdf =
  Pdf.objiter
    (fun _ n ->
       match Pdf.lookup_direct pdf "/Subtype" n with
       | Some (Pdf.Name "/CIDFontType2") ->
           begin match Pdf.lookup_direct pdf "/CIDToGIDMap" n with
           | Some _ -> ()
           | _ -> merror ()
           end
       | _ -> ())
    pdf

(* A CMap is neither listed as described in ISO 32000-1:2008, 9.7.5.2, Table
   118 nor is it embedded. *)
let cmap_names =
  ["/GB-EUC-H";
   "/GB-EUC-V";
   "/GBpc-EUC-H";
   "/GBpc-EUC-V";
   "/GBK-EUC-H";
   "/GBK-EUC-V";
   "/GBKp-EUC-H";
   "/GBKp-EUC-V";
   "/GBK2K-H";
   "/GBK2K-V";
   "/UniGB-UCS32-H";
   "/UniGB-UCS32-V";
   "/UniGB-UTF16-H";
   "/UniGB-UTF16-V";
   "/B5pc-H";
   "/B5pc-V";
   "/HKscs-B5-H";
   "/HKscs-B5-V";
   "/ETen-B5-H";
   "/ETen-B5-V";
   "/ETenms-B5-H";
   "/ETenms-B5-V";
   "/CNS-EUC-H";
   "/CNS-EUC-V";
   "/UniCNS-UCS2-H";
   "/UniCNS-UCS2-V";
   "/UniCNS-UTF16-H";
   "/UniCNS-UTF16-V";
   "/83pv-RKSJ-H";
   "/90ms-RKSJ-H";
   "/90ms-RKSJ-V";
   "/90msp-RKSJ-H";
   "/90msp-RKSJ-V";
   "/90pv-RKSJ-H";
   "/Add-RKSJ-H";
   "/Add-RKSJ-V";
   "/EUC-H";
   "/EUC-V";
   "/Ext-RKSJ-H";
   "/Ext-RKSJ-V";
   "/H";
   "/V";
   "/UniJIS-UCS2-H";
   "/UniJIS-UCS2-V";
   "/UniJIS-UCS2-HW-H";
   "/UniJIS-UCS2-HW-V";
   "/UniJIS-UTF16-H";
   "/UniJIS-UTF16-V";
   "/KSC-EUC-H";
   "/KSC-EUC-V";
   "/KSCms-UHC-H";
   "/KSCms-UHC-V";
   "/KSCms-UHC-HW-H";
   "/KSCms-UHS-HW-V";
   "/KSCpc-EUC-H";
   "/UniKS-UCS2-H";
   "/UniKS-UCS2-V";
   "/Identity-H";
   "/Identity-V"]

let matterhorn_31_006 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
     match Pdf.lookup_direct pdf "/Subtype" o with
     | Some (Pdf.Name "/Type0") ->
         begin match Pdf.lookup_direct pdf "/Encoding" o with
         | Some (Pdf.Name n) when not (List.mem n cmap_names) -> merror_str n
         | _ -> ()
         end
     | _ -> ())
    pdf

(* The WMode entry in a CMap dictionary is not identical to the WMode value in
the CMap stream. *)
let matterhorn_31_007 _ _ pdf =
  unimpl ()

(* A CMap references another CMap which is not listed in ISO 32000-1:2008,
   9.7.5.2, Table 118. *)
let matterhorn_31_008 _ _ pdf =
  unimpl ()

(* For a font used by text intended to be rendered the font program is not
   embedded. *)
(* NB This, for now, reports all unembedded fonts, save for Type 3 ones... *)
let matterhorn_31_009 _ _ pdf =
  let l = Cpdffont.missing_fonts_return pdf (ilist 1 (Pdfpage.endpage pdf)) in
    if l <> [] then
      raise (MatterhornError (`List (map (fun (a, b, c, d, e) -> `String (Printf.sprintf "%i %s %s %s %s" a b c d e)) l)))

(* For a font used by text the font program is embedded but it does not contain
   glyphs for all of the glyphs referenced by the text used for rendering. *)
let matterhorn_31_011 _ _ pdf =
  unimpl ()

(* The FontDescriptor dictionary of an embedded Type 1 font contains a CharSet
   string, but at least one of the glyphs present in the font program is not
   listed in the CharSet string. *)
let matterhorn_31_012 _ _ pdf =
  unimpl ()

(* The FontDescriptor dictionary of an embedded Type 1 font contains a CharSet
   string, but at least one of the glyphs listed in the CharSet string is not
   present in the font program. *)
let matterhorn_31_013 _ _ pdf =
  unimpl ()

(* The FontDescriptor dictionary of an embedded CID font contains a CIDSet
   string, but at least one of the glyphs present in the font program is not
   listed in the CIDSet string. *)
let matterhorn_31_014 _ _ pdf =
  unimpl ()

(* The FontDescriptor dictionary of an embedded CID font contains a CIDSet
   string, but at least one of the glyphs listed in the CIDSet string is not
   present in the font program. *)
let matterhorn_31_015 _ _ pdf =
  unimpl ()

(* For one or more glyphs, the glyph width information in the font dictionary
   and in the embedded font program differ by more than 1/1000 unit. *)
let matterhorn_31_016 _ _ pdf =
  unimpl ()

let is_non_symbolic pdf o =
  match Pdf.lookup_direct pdf "/FontDescriptor" o with
  | Some fd ->
      begin match Pdf.lookup_direct pdf "/Flags" fd with
      | Some (Pdf.Integer i) -> not (i land 0b100 > 0)
      | _ -> true
      end
  | None -> true 

let truetype_fontfile pdf o =
  match Pdf.lookup_chain pdf o ["/FontDescriptor"; "/FontFile2"] with
  | Some (Pdf.Stream s) ->
      Pdfcodec.decode_pdfstream_until_unknown pdf (Pdf.Stream s);
      begin match s with
      | {contents = (_, Pdf.Got bs)} -> Some bs
      | _ -> None
      end
  | _ -> None

(* A non-symbolic TrueType font is used for rendering, but none of the cmap
   entries in the embedded font program is a non-symbolic cmap. *)
let matterhorn_31_017 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           if not (is_non_symbolic pdf o) then
             let fontfile = truetype_fontfile pdf o in
               if fontfile = None then () else
                 let cmaps = Cpdftruetype.cmaps (unopt fontfile) in
                   (*iter (fun (x, y) -> Printf.printf "%i, %i\n" x y) cmaps;*)
                   (* Must all be symbolic *)
                   iter (function (1, 8) | (3, 0) -> () | (a, b) -> merror_str (Printf.sprintf "(%i, %i) cmap" a b)) cmaps
           else
             ()
       | _ -> ())
    pdf

(* A non-symbolic TrueType font is used for rendering, but for at least one
   glyph to be rendered the glyph cannot be looked up by any of the
   non-symbolic cmap entries in the embedded font program. *)
let matterhorn_31_018 _ _ pdf =
  unimpl ()

(* The font dictionary for a non-symbolic TrueType font does not contain an
   Encoding entry. *)
let matterhorn_31_019 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           begin match is_non_symbolic pdf o, Pdf.lookup_direct pdf "/Encoding" o with
           | true, None -> merror ()
           | _ -> ()
           end
       | _ -> ())
    pdf

(* The font dictionary for a non-symbolic TrueType font contains an Encoding
   dictionary which does not contain a BaseEncoding entry. *)
let matterhorn_31_020 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           begin match is_non_symbolic pdf o, Pdf.lookup_direct pdf "/Encoding" o with
           | true, Some ((Pdf.Dictionary _) as d) ->
               begin match Pdf.lookup_direct pdf "/BaseEncoding" d with
               | None -> merror ()
               | Some _ -> ()
               end
           | _ -> ()
           end
       | _ -> ())
    pdf

(* The value for either the Encoding entry or the BaseEncoding entry in the
   Encoding dictionary in a non-symbolic TrueType font dictionary is neither
   MacRomanEncoding nor WinAnsiEncoding. *)
let matterhorn_31_021 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           begin match is_non_symbolic pdf o, Pdf.lookup_direct pdf "/Encoding" o with
           | true, Some (Pdf.Name ("/MacRomanEncoding" | "/WinAnsiEncoding")) -> ()
           | true, Some (Pdf.Name _) -> merror ()
           | true, Some ((Pdf.Dictionary _) as d) ->
               begin match Pdf.lookup_direct pdf "/BaseEncoding" d with
               | Some (Pdf.Name ("/MacRomanEncoding" | "/WinAnsiEncoding")) -> ()
               | Some x -> merror_str (Pdfwrite.string_of_pdf x)
               | _ -> ()
               end
           | _ -> ()
           end
       | _ -> ())
    pdf

(* The Differences array in the Encoding entry in a non-symbolic TrueType font
   dictionary contains one or more glyph names which are not listed in the
   Adobe Glyph List. *)
let matterhorn_31_022 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           begin match is_non_symbolic pdf o, Pdf.lookup_direct pdf "/Encoding" o with
           | true, Some d ->
               begin match Pdf.lookup_direct pdf "/Differences" d with
               | Some (Pdf.Array a) ->
                   let glyphs = Pdfglyphlist.glyph_hashes () in
                   let names = option_map (function Pdf.Name n -> Some n | _ -> None) a in
                     iter (fun n -> if not (Hashtbl.mem glyphs n) then merror_str n) names
               | _ -> ()
               end
           | _ -> ()
           end
       | _ -> ())
    pdf
 
(* The Differences array is present in the Encoding entry in a non-symbolic
   TrueType font dictionary but the embedded font program does not contain a
   (3,1) Microsoft Unicode cmap. *)
let matterhorn_31_023 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o, Pdf.lookup_chain pdf o ["/Encoding"; "/Differences"] with
       | Some (Pdf.Name "/TrueType"), Some _ ->
           if is_non_symbolic pdf o then
             let fontfile = truetype_fontfile pdf o in
               if fontfile = None then () else
                 let cmaps = Cpdftruetype.cmaps (unopt fontfile) in
                   if mem (3, 1) cmaps then () else merror ()
           else
             ()
       | _ -> ())
    pdf

(* The Encoding entry is present in the font dictionary for a symbolic TrueType
   font. *)
let matterhorn_31_024 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           begin match is_non_symbolic pdf o, Pdf.lookup_direct pdf "/Encoding" o with
           | false, Some _ -> merror ()
           | _ -> ()
           end
       | _ -> ())
    pdf

(* The embedded font program for a symbolic TrueType font contains no cmap. *)
let matterhorn_31_025 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           if not (is_non_symbolic pdf o) then
             let fontfile = truetype_fontfile pdf o in
               if fontfile = None then () else
                 let cmaps = Cpdftruetype.cmaps (unopt fontfile) in
                   (*iter (fun (x, y) -> Printf.printf "%i, %i\n" x y) cmaps;*)
                   if cmaps = [] then merror ()
           else
             ()
       | _ -> ())
    pdf

(* The embedded font program for a symbolic TrueType font contains more than
   one cmap, but none of the cmap entries is a (3,0) Microsoft Symbol cmap. *)
let matterhorn_31_026 _ _ pdf =
  Pdf.objiter
    (fun _ o ->
       match Pdf.lookup_direct pdf "/Subtype" o with
       | Some (Pdf.Name "/TrueType") ->
           if not (is_non_symbolic pdf o) then
             let fontfile = truetype_fontfile pdf o in
               if fontfile = None then () else
                 let cmaps = Cpdftruetype.cmaps (unopt fontfile) in
                   (*iter (fun (x, y) -> Printf.printf "%i, %i\n" x y) cmaps;*)
                   if length cmaps > 1 && not (mem (3, 0) cmaps) then merror ()
           else
             ()
       | _ -> ())
    pdf

(* A font dictionary does not contain the ToUnicode entry and none of the
   following is true: the font uses MacRomanEncoding, MacExpertEncoding or
   WinAnsiEncoding; the font is a Type 1 or Type 3 font and the glyph names of
   the glyphs referenced are all contained in the Adobe Glyph List or the set
   of named characters in the Symbol font, as defined in ISO 32000-1:2008,
   Annex D; the font is a Type 0 font, and its descendant CIDFont uses
   Adobe-GB1, Adobe-CNS1, Adobe-Japan1 or Adobe-Korea1 character collections;
   the font is a non-symbolic TrueType font. *)
let matterhorn_31_027 _ _ pdf =
  (* Here, we implement most of this one, but can't check the set of referenced
     glyphs for Type1 / Type3. *)
  let c1 o =
    match Pdf.lookup_direct pdf "/Encoding" o with
    | Some (Pdf.Name ("/MacRomanEncoding" | "/MacExpertEncoding" | "/WinAnsiEncoding")) -> true
    | _ -> false
  in
  let c3 o =
    match Pdf.lookup_direct pdf "/Subtype" o with
    | Some (Pdf.Name "/Type0") ->
        begin match Pdf.lookup_direct pdf "/DescendantFonts" o with
        | Some (Pdf.Array [df]) ->
            begin match Pdf.lookup_direct pdf "/CIDSystemInfo" df with
            | Some cidinfo ->
                begin match Pdf.lookup_direct pdf "/Registry" cidinfo, Pdf.lookup_direct pdf "/Ordering" cidinfo with
                | Some (Pdf.Name "/Adobe"), Some (Pdf.Name ("/GB1" | "/CNS1" | "/Japan1" | "/Korea1")) -> true
                | _ -> false
                end
            | _ -> false
            end
        | _ -> false
        end
    | _ -> false
  in
  let c4 o = is_non_symbolic pdf o in
    Pdf.objiter
      (fun _ o ->
         match Pdf.lookup_direct pdf "/ToUnicode" o with
         | Some _ -> ()
         | None ->
             begin match Pdf.lookup_direct pdf "/Type" o with
             | Some (Pdf.Name "/Font") ->
                 begin match c1 o, c3 o, c4 o with
                 | false, false, false ->
                     begin match Pdf.lookup_direct pdf "/Subtype" o with
                     | Some (Pdf.Name ("/Type0" | "/CIDFontType0" | "/CIDFontType2")) -> ()
                     | Some (Pdf.Name ("/Type1" | "/MMType1" | "/Type3")) -> unimpl ()
                     | _ -> merror_str (Pdfwrite.string_of_pdf o)
                     end
                 | _ -> ()
                 end
             | _ -> ()
             end)
      pdf

let all_tounicodes pdf = 
  let tus = ref [] in
    Pdf.objiter
      (fun _ o ->
        match Pdf.indirect_number pdf "/ToUnicode" o with
        | Some i -> tus := i::!tus
        | None -> ())
      pdf;
      (setify !tus)

let check_unicode tu n =
  mem n (flatten (map (fun x -> Pdftext.codepoints_of_utf16be (snd x)) tu))

(* One or more Unicode values specified in the ToUnicode CMap are zero (0). *)
let matterhorn_31_028 _ _ pdf =
  iter
    (fun i ->
      let tu = Pdftext.parse_tounicode pdf (Pdf.lookup_obj pdf i) in
        if check_unicode tu 0 then merror ())
    (all_tounicodes pdf)

(* One or more Unicode values specified in the ToUnicode CMap are equal to
   either U+FEFF or U+FFFE. *)
let matterhorn_31_029 _ _ pdf =
  iter
    (fun i ->
      let tu = Pdftext.parse_tounicode pdf (Pdf.lookup_obj pdf i) in
        if check_unicode tu 0xFEFF || check_unicode tu 0xFFFE then merror ())
    (all_tounicodes pdf)

(* One or more characters used in text showing operators reference the .notdef
   glyph. *)
let matterhorn_31_030 _ _ pdf =
  unimpl ()

let matterhorn =
  [("01-003", "Content marked as Artifact is present inside tagged content.", "UA1:7.1-1", matterhorn_01_003);
   ("01-004", "Tagged content is present inside content marked as Artifact.", "UA1:7.1-1", matterhorn_01_004);
   ("01-005", "Content is neither marked as Artifact nor tagged as real content.", "UA1:7-1-2", matterhorn_01_005);
   ("01-007", "Suspects entry has a value of true.", "UA1:7-1-11", matterhorn_01_007);
   ("02-001", "One or more non-standard tag’s mapping does not terminate with a standard type.", "UA1:7.1-3", matterhorn_02_001);
   ("02-003", "A circular mapping exists.", "UA1:7.1-3", matterhorn_02_003);
   ("02-004", "One or more standard types are remapped.", "UA1:7.1-4", matterhorn_02_004);
   ("06-001", "Document does not contain an XMP metadata stream", "UA1:7.1-8", matterhorn_06_001);
   ("06-002", "The XMP metadata stream in the Catalog dictionary does not include the PDF/UA identifier.", "UA1:5", matterhorn_06_002);
   ("06-003", "XMP metadata stream does not contain dc:title", "UA1:7.1-8", matterhorn_06_003);
   ("07-001", "ViewerPreferences dictionary of the Catalog dictionary does not contain a DisplayDocTitle entry", "UA1:7.1-9", matterhorn_07_001);
   ("07-002", "ViewerPreferences dictionary of the Catalog dictionary contains a DisplayDocTitle entry with a value of false", "UA1:7.1-9", matterhorn_07_002);
   ("09-004", "A table-related structure element is used in a way that does not conform to the syntax defined in ISO 32000-1, Table 337.", "UA1-7.2-1", matterhorn_09_004);
   ("09-005", "A list-related structure element is used in a way that does not conform to Table 336 in ISO 32000-1.", "UA1-7.2-1", matterhorn_09_005);
   ("09-006", "A TOC-related structure element is used in a way that does not conform to Table 333 in ISO 32000-1.", "UA1-7.2-1", matterhorn_09_006);
   ("09-007", "A Ruby-related structure element is used in a way that does not conform to Table 338 in ISO 32000-1.", "UA1-7.2-1", matterhorn_09_007);
   ("09-008", "A Warichu-related structure element is used in a way that does not conform to Table 338 in ISO 32000-1.", "UA1-7.2-1", matterhorn_09_008);
   ("10-001", "Character code cannot be mapped to Unicode.", "UA1:7.2-2", matterhorn_10_001);
   ("11-001", "Natural language for text in page content cannot be determined.", "UA1:7.2-3", matterhorn_11_001);
   ("11-002", "Natural language for text in Alt, ActualText and E attributes cannot be determined.", "UA1:7.2-3", matterhorn_11_002);
   ("11-003", "Natural language in the Outline entries cannot be determined.", "UA1:7.2-3", matterhorn_11_003);
   ("11-004", "Natural language in the Contents entry for annotations cannot be determined.", "UA1:7.2-3", matterhorn_11_004);
   ("11-005", "Natural language in the TU entry for form fields cannot be determined.", "UA1:7.2-3", matterhorn_11_005);
   ("11-006", "Natural language for document metadata cannot be determined.", "UA1:7.2-3", matterhorn_11_006);
   ("13-004", "<Figure> tag alternative or replacement text missing.", "UA1:7.3-3", matterhorn_13_004);
   ("14-002", "Does use numbered headings, but the first heading tag is not <H1>.", "UA1:7.4.2-1", matterhorn_14_002);
   ("14-003", "Numbered heading levels in descending sequence are skipped (Example: <H3> follows directly after <H1>).", "UA1:7.4-1", matterhorn_14_003);
   ("14-006", "A node contains more than one <H> tag.", "UA1:7.4.4-1", matterhorn_14_006);
   ("14-007", "Document uses both <H> and <H#> tags.", "UA1:7.4.4-3", matterhorn_14_007);
   ("15-003", "In a table not organized with Headers attributes and IDs, a <TH> cell does not contain a Scope attribute.", "UA1:7.5-2", matterhorn_15_003);
   ("17-002", "<Formula> tag is missing an Alt attribute.", "UA1:7.7-1", matterhorn_17_002);
   ("17-003", "Unicode mapping requirements are not met.", "UA1:7.7-2", matterhorn_17_003);
   ("19-003", "ID entry of the <Note> tag is not present.", "UA1:7.9-2", matterhorn_19_003);
   ("19-004", "ID entry of the <Note> tag is non-unique.", "UA1:7.9-2", matterhorn_19_004);
   ("20-001", "Name entry is missing or has an empty string as its value in an Optional Content Configuration Dictionary in the Configs entry in the OCProperties entry in the Catalog dictionary.", "UA1:7.10-1", matterhorn_20_001);
   ("20-002", "Name entry is missing or has an empty string as its value in an Optional Content Configuration Dictionary that is the value of the D entry in the OCProperties entry in the Catalog dictionary.", "UA1:7.10-1", matterhorn_20_002);
   ("20-003", "An AS entry appears in an Optional Content Configuration Dictionary.", "UA1:7.10-2", matterhorn_20_003);
   ("21-001", "The file specification dictionary for an embedded file does not contain F and UF entries.", "UA1:7.11-1", matterhorn_21_001);
   ("25-001", "File contains the dynamicRender element with value “required”.", "UA1:7.15-1", matterhorn_25_001);
   ("26-001", "The file is encrypted but does not contain a P entry in its encryption dictionary.", "UA1:7.16-1", matterhorn_26_001);
   ("26-002", "The file is encrypted and does contain a P entry but the 10th bit position of the P entry is false.", "UA1:7.16-1", matterhorn_26_002);
   ("28-002", "An annotation, other than of subtype Widget, Link and PrinterMark, is not a direct child of an <Annot> structure element.", "UA1:7.18.1-2", matterhorn_28_002);
   ("28-004", "An annotation, other than of subtype Widget, does not have a Contents entry and does not have an alternative description (in the form of an Alt entry in the enclosing structure element).", "UA1:7.18.1-4", matterhorn_28_004);
   ("28-005", "A form field does not have a TU entry and does not have an alternative description (in the form of an Alt entry in the enclosing structure element).", "UA1:7.18.1-4", matterhorn_28_005);
   ("28-006", "An annotation with subtype undefined in ISO 32000 does not meet 7.18.1.", "UA1:7.18.2-1", matterhorn_28_006);
   ("28-007", "An annotation of subtype TrapNet exists.", "UA1:7.18.2-2", matterhorn_28_007);
   ("28-008", "A page containing an annotation does not contain a Tabs entry", "UA1:7.18.3-1", matterhorn_28_008);
   ("28-009", "A page containing an annotation has a Tabs entry with a value other than S.", "UA1:7.18.3-1", matterhorn_28_009);
   ("28-010", "A widget annotation is not nested within a <Form> tag.", "UA1:7.18.4-1", matterhorn_28_010);
   ("28-011", "A link annotation is not nested within a <Link> tag.", "UA1:7.18.5-1", matterhorn_28_011);
   ("28-012", "A link annotation does not include an alternate description in its Contents entry.", "UA1:7.18.5-2", matterhorn_28_012);
   ("28-014", "CT entry is missing from the media clip data dictionary.", "", matterhorn_28_014);
   ("28-015", "Alt entry is missing from the media clip data dictionary.", "UA1:7.18.6.2-1", matterhorn_28_015);
   ("28-016", "File attachment annotations do not conform to 7.11.", "UA1:7.18.7-1", matterhorn_28_016);
   ("28-017", "A PrinterMark annotation is included in the logical structure.", "UA1:7.18.8-1", matterhorn_28_017);
   ("28-018", "The appearance stream of a PrinterMark annotation is not marked as Artifact.", "UA1:7.18.8-2", matterhorn_28_018);
   ("30-001", "A reference XObject is present.", "UA1:7.2", matterhorn_30_001);
   ("30-002", "Form XObject contains MCIDs and is referenced more than once.", "UA1:7.21.3.1-1",  matterhorn_30_002);
   ("31-001", "A Type 0 font dictionary with encoding other than Identity-H and Identity-V has values for Registry in both CIDSystemInfo dictionaries that are not identical.", "UA1:7.21.3-1", matterhorn_31_001);
   ("31-002", "A Type 0 font dictionary with encoding other than Identity-H and Identity-V has values for Ordering in both CIDSystemInfo dictionaries that are not identical.", "UA1:7.21.3.1-1", matterhorn_31_002);
   ("31-003", "A Type 0 font dictionary with encoding other than Identity-H and Identity-V has a value for Supplement in the CIDSystemInfo dictionary of the CID font that is less than the value for Supplement in the CIDSystemInfo dictionary of the CMap.", "UA1:7.21.3.1-1", matterhorn_31_003);
   ("31-004", "A Type 2 CID font contains neither a stream nor the name Identity as the value of the CIDToGIDMap entry.", "UA1:7.21.3.2-1", matterhorn_31_004);
   ("31-005", "A Type 2 CID font does not contain a CIDToGIDMap entry.", "UA1:7.21.3.2-1", matterhorn_31_005);
   ("31-006", "A CMap is neither listed as described in ISO 32000- 1:2008, 9.7.5.2, Table 118 nor is it embedded.", "UA1:7.21.3.3-1", matterhorn_31_006);
   ("31-007", "The WMode entry in a CMap dictionary is not identical to the WMode value in the CMap stream.", "UA1:7.21.3.3-1", matterhorn_31_007);
   ("31-008", "A CMap references another CMap which is not listed in ISO 32000-1:2008, 9.7.5.2, Table 118.", "UA1:7.21.3.3-2", matterhorn_31_008);
   ("31-009", "For a font used by text intended to be rendered the font program is not embedded.", "UA1:7.21.4.1-1", matterhorn_31_009);
   ("31-011", "For a font used by text the font program is embedded but it does not contain glyphs for all of the glyphs referenced by the text used for rendering.", "UA1:7.21.4.1-3", matterhorn_31_011);
   ("31-012", "The FontDescriptor dictionary of an embedded Type 1 font contains a CharSet string, but at least one of the glyphs present in the font program is not listed in the CharSet string.", "UA1:7.21.4.2-1", matterhorn_31_012);
   ("31-013", "The FontDescriptor dictionary of an embedded Type 1 font contains a CharSet string, but at least one of the glyphs listed in the CharSet string is not present in the font program.", "UA1:7.21.4.2-2", matterhorn_31_013);
   ("31-014", "The FontDescriptor dictionary of an embedded CID font contains a CIDSet string, but at least one of the glyphs present in the font program is not listed in the CIDSet string.", "UA1:7.21.4.2-3", matterhorn_31_014);
   ("31-015", "The FontDescriptor dictionary of an embedded CID font contains a CIDSet string, but at least one of the glyphs listed in the CIDSet string is not present in the font program.", "UA1:7.21.4.2-4", matterhorn_31_015);
   ("31-016", "For one or more glyphs, the glyph width information in the font dictionary and in the embedded font program differ by more than 1/1000 unit.", "UA1:7.21.5-1", matterhorn_31_016);
   ("31-017", "A non-symbolic TrueType font is used for rendering, but none of the cmap entries in the embedded font program is a non-symbolic cmap.", "UA1:7.21.6-1", matterhorn_31_017);
   ("31-018", "A non-symbolic TrueType font is used for rendering, but for at least one glyph to be rendered the glyph cannot be looked up by any of the non-symbolic cmap entries in the embedded font program.", "UA1:7.21.6-2", matterhorn_31_018);
   ("31-019", "The font dictionary for a non-symbolic TrueType font does not contain an Encoding entry.", "UA1:7.21.6-3", matterhorn_31_019);
   ("31-020", "The font dictionary for a non-symbolic TrueType font contains an Encoding dictionary which does not contain a BaseEncoding entry.", "UA1:7.21.6-4", matterhorn_31_020);
   ("31-021", "The value for either the Encoding entry or the BaseEncoding entry in the Encoding dictionary in a non-symbolic TrueType font dictionary is neither MacRomanEncoding nor WinAnsiEncoding.", "UA1:7.21.6-5", matterhorn_31_021);
   ("31-022", "The Differences array in the Encoding entry in a non-symbolic TrueType font dictionary contains one or more glyph names which are not listed in the Adobe Glyph List.", "UA1:7.21.6-6", matterhorn_31_022);
   ("31-023", "The Differences array is present in the Encoding entry in a non-symbolic TrueType font dictionary but the embedded font program does not contain a (3,1) Microsoft Unicode cmap.", "UA1:7.21.6-7", matterhorn_31_023);
   ("31-024", "The Encoding entry is present in the font dictionary for a symbolic TrueType font.", "UA1:7.21.6-8", matterhorn_31_024);
   ("31-025", "The embedded font program for a symbolic TrueType font contains no cmap.", "UA1:7.21.6-9", matterhorn_31_025);
   ("31-026", "The embedded font program for a symbolic TrueType font contains more than one cmap, but none of the cmap entries is a (3,0) Microsoft Symbol cmap.", "UA1:7.21.6-10", matterhorn_31_026);
   ("31-027", "A font dictionary does not contain the ToUnicode entry and none of the following is true: the font uses MacRomanEncoding, MacExpertEncoding or WinAnsiEncoding; the font is a Type 1 or Type 3 font and the glyph names of the glyphs referenced are all contained in the Adobe Glyph List or the set of named characters in the Symbol font, as defined in ISO 32000-1:2008, Annex D; the font is a Type 0 font, and its descendant CIDFont uses Adobe-GB1, Adobe-CNS1, Adobe-Japan1 or Adobe-Korea1 character collections; the font is a non-symbolic TrueType font.", "UA1:7.21.7-1", matterhorn_31_027);
   ("31-028", "One or more Unicode values specified in the ToUnicode CMap are zero (0).", "UA1:7.21.7-2", matterhorn_31_028);
   ("31-029", "One or more Unicode values specified in the ToUnicode CMap are equal to either U+FEFF or U+FFFE.", "UA1:7.21.7-3", matterhorn_31_029);
   ("31-030", "One or more characters used in text showing operators reference the .notdef glyph.", "UA1:7.21.8-1", matterhorn_31_030);
  ]

let test_matterhorn pdf testname =
  let tests =
    match testname with
    | "" -> matterhorn
    | n -> 
        match keep (fun (n', _, _, _) -> n' = n) matterhorn with
        | [] -> error "test not found"
        | [t] -> [t]
        | _ -> error "duplicate test"
  in
  (* A circularity in the role map prevents all structure checks, so we do it first at stop if it fails. *)
  let circularity_error =
    try matterhorn_02_003 0 0 pdf; [] with
      MatterhornError (`String s) ->
        [("02-003", "A circular mapping exists.", "UA1:7.1-3", `String s)]
  in
  if circularity_error <> [] then circularity_error else
    let st2 = read_st2 pdf in
    let st = st_of_st2 st2 in
      option_map
        (fun (name, error, section, test) ->
          try test st st2 pdf; None with
           | MatterhornError extra -> Some (name, error, section, extra)
           | MatterhornUnimplemented -> None
           | e -> Some (name, "Incomplete", section, `String ("ERROR: " ^ Printexc.to_string e)))
        tests

let test_matterhorn_print pdf testname =
  iter
    (fun (name, error, section, extra) ->
       Printf.eprintf "%s %s %s %s\n" name section error
       (if extra = `Null then "" else "(" ^ Cpdfyojson.Safe.to_string extra ^ ")"))
    (test_matterhorn pdf testname)

let test_matterhorn_json pdf testname =
  `List
    (map
      (fun (name, error, section, extra) ->
        `Assoc [("name", `String name); ("section", `String section); ("error", `String error); ("extra", extra)])
      (test_matterhorn pdf testname))

let pdfua_marker =
  Cpdfmetadata.(E (((rdf, "Description"), [((rdf, "about"), ""); ((Cpdfxmlm.ns_xmlns, "pdfuaid"), pdfuaid)]), [E (((pdfuaid, "part"), []), [D "1"])])) 

let pdfua2_marker year =
  Cpdfmetadata.(E (((rdf, "Description"), [((rdf, "about"), ""); ((Cpdfxmlm.ns_xmlns, "pdfuaid"), pdfuaid)]), [E (((pdfuaid, "part"), []), [D "2"]);
                                                                                                               E (((pdfuaid, "rev"), []), [D (string_of_int year)])])) 

let rec insert_as_rdf_description fragment = function
  | Cpdfmetadata.E (((_, "RDF"), _) as rdftag, rdfs) ->
      Cpdfmetadata.E (rdftag, fragment::rdfs)
  | Cpdfmetadata.E (((_, "xmpmeta"), _) as xmptag, cs) ->
      Cpdfmetadata.E (xmptag, map (insert_as_rdf_description fragment) cs)
  | _ -> error "insert_as_rdf_description: could not find insertion point."

let rec delete_pdfua_marker tree =
  let is_pdfuaid = function
  | Cpdfmetadata.E (((pdfuaid, ("part" | "rev" | "amd" | "corr")), _), _) when pdfuaid = Cpdfmetadata.pdfuaid -> true
  | _ -> false
  in
    match tree with
    | Cpdfmetadata.E (((rdf, "Description"), x), c) when rdf = Cpdfmetadata.rdf && List.exists is_pdfuaid c ->
        Cpdfmetadata.E (((rdf, "Description"), x), keep (notpred is_pdfuaid) c)
    | Cpdfmetadata.E (x, children) ->
        Cpdfmetadata.E (x, map delete_pdfua_marker children)
    | x -> x

let mark_inner pdfua_marker pdf =
  let pdf2 = if Cpdfmetadata.get_metadata pdf = None then Cpdfmetadata.create_metadata pdf else pdf in
    pdf.Pdf.objects <- pdf2.Pdf.objects;
    pdf.Pdf.trailerdict <- pdf2.Pdf.trailerdict;
    pdf.Pdf.root <- pdf2.Pdf.root;
    match Cpdfmetadata.get_metadata pdf with
    | Some metadata ->
        let dtd, tree = Cpdfmetadata.xmltree_of_bytes metadata in
        let newtree =
          match Cpdfmetadata.get_data_for Cpdfmetadata.pdfuaid "part" tree with
          | Some _ -> insert_as_rdf_description pdfua_marker (delete_pdfua_marker tree)
          | None -> insert_as_rdf_description pdfua_marker tree
        in
          let newbytes = Cpdfmetadata.bytes_of_xmltree (dtd, newtree) in
          let pdf3 = Cpdfmetadata.set_metadata_from_bytes true newbytes pdf in
            pdf.Pdf.objects <- pdf3.Pdf.objects;
            pdf.Pdf.trailerdict <- pdf3.Pdf.trailerdict;
            pdf.Pdf.root <- pdf3.Pdf.root
    | None -> assert false

let mark = mark_inner pdfua_marker

let mark2 year = mark_inner (pdfua2_marker year)

let remove_mark pdf =
  match Cpdfmetadata.get_metadata pdf with
  | Some metadata ->
      let dtd, tree = Cpdfmetadata.xmltree_of_bytes metadata in
      let newtree =
        match Cpdfmetadata.get_data_for Cpdfmetadata.pdfuaid "part" tree with
        | Some _ -> delete_pdfua_marker tree
        | None -> tree
      in
        let newbytes = Cpdfmetadata.bytes_of_xmltree (dtd, newtree) in
        let pdf3 = Cpdfmetadata.set_metadata_from_bytes true newbytes pdf in
          pdf.Pdf.objects <- pdf3.Pdf.objects;
          pdf.Pdf.trailerdict <- pdf3.Pdf.trailerdict;
          pdf.Pdf.root <- pdf3.Pdf.root
  | None -> ()

let extract_struct_tree pdf =
  match Pdf.lookup_obj pdf pdf.Pdf.root with
  | Pdf.Dictionary d ->
      let zero =
        `List [`Int 0;
                `Assoc [("/CPDFJSONformatversion", `Int 1);
                        ("/CPDFJSONpageobjnumbers", `List (map (fun x -> `Int (unopt (Pdfpage.page_object_number pdf x))) (ilist 1 (Pdfpage.endpage pdf))))]]
      in
        begin match lookup "/StructTreeRoot" d with
        | None -> `List [zero]
        | Some x ->
            let objs = Pdf.objects_referenced ["/Pg"; "/Obj"; "/Stm"; "/StmOwn"] [] pdf x in
              `List
                 (zero::map
                    (fun objnum ->
                       let jsonobj =
                         Cpdfjson.json_of_object ~utf8:true ~no_stream_data:false ~parse_content:false pdf (function _ -> ()) (Pdf.lookup_obj pdf objnum)
                       in
                         `List [`Int objnum; jsonobj])
                    objs)
        end
  | _ -> error "extract_struct_tree: no root"

(* Use JSON data to replace objects in a file. Negative objects are new ones,
   we make them positive and renumber them not to clash. Everything else must
   remain unrenumbered. *)
let replace_struct_tree pdf json =
  try
    let rec rewrite_indirects negobjnummap = function
      | Pdf.Indirect i ->
          begin match lookup i negobjnummap with
          | Some x -> Pdf.Indirect x
          | None -> Pdf.Indirect i
          end
      | Pdf.Dictionary d -> Pdf.recurse_dict (rewrite_indirects negobjnummap) d
      | Pdf.Array a -> Pdf.recurse_array (rewrite_indirects negobjnummap) a
      | x -> x
    in
      match json with
      | `List (`List [`Int 0; _]::xs) ->
          let pos, neg = List.partition (function (`List [`Int x; _]) -> x > 0 | _ -> error "structure 1") xs in
          let pos = map (function `List [`Int x; j] -> (x, Cpdfjson.object_of_json j) | _ -> error "structure 2") pos in
          let neg = map (function `List [`Int x; j] -> (x, Cpdfjson.object_of_json j) | _ -> error "structure 3") neg in
          let nextnum = Pdf.objcard pdf + 1 in
          let negobjnummap = if length neg = 0 then [] else map2 (fun n n' -> (n, n')) (map fst neg) (ilist nextnum (nextnum + length neg - 1)) in
          let pos = map (fun (objnum, obj) -> (objnum, rewrite_indirects negobjnummap obj)) pos in
          let neg = map (fun (objnum, obj) -> (objnum, rewrite_indirects negobjnummap obj)) neg in
            iter (Pdf.addobj_given_num pdf) (pos @ neg)
      | _ -> error "Top level JSON wrong. Must be list with 0 first."
  with
    e -> error (Printf.sprintf "replace_struct_tree: %s" (Printexc.to_string e))

let rec remove_empty = function
  E2 (n, attrs, cs) ->
    let cs' = map remove_empty cs in
      E2 (n, attrs, lose (function E2 ("", _, []) -> true | _ -> false) cs')

let rec remove_slashes = function
  E2 (n, attrs, cs) -> E2 ((match n with "" -> "" | n -> implode (tl (explode n))), attrs, map remove_slashes cs)

let print_struct_tree pdf =
  let page_lookup =
    hashtable_of_dictionary (combine (Pdf.page_reference_numbers pdf) (ilist 1 (Pdfpage.endpage pdf)))
  in
  let get_page attrs =
    match option_map (fun x -> match explode x with '_'::more -> Some (implode more) | _ -> None) attrs with
    | [i] -> string_of_int (try Hashtbl.find page_lookup (int_of_string i) with _ -> 0)
    | _ -> "0"
  in
    let st = read_st2 pdf in
      match st with E2 ("/StructTreeRoot", _, []) -> () | _ ->
        flprint
          (Cpdfprinttree.to_string
            ~get_name:(fun (E2 (x, a, _)) -> if int_of_string (get_page a) > 0 then x ^ " (" ^ get_page a ^ ")" else x)
            ~get_children:(fun (E2 (_, _, cs)) -> cs)
            (remove_empty (remove_slashes st)))

let create_pdfua1 title pagesize pages =
  let pdf = Cpdfcreate.blank_document_paper pagesize pages in
  let pdf = Cpdfmetadata.set_pdf_info ~xmp_also:false ~xmp_just_set:false ("/Title", Pdf.String title, 0) pdf in
  let pdf = Cpdfmetadata.create_metadata pdf in
    Cpdfmetadata.set_language pdf "en-US";
    let pdf = Cpdfmetadata.set_viewer_preference ("/DisplayDocTitle", Pdf.Boolean true, 0) pdf in
      Pdf.replace_chain pdf ["/Root"; "/MarkInfo"; "/Marked"] (Pdf.Boolean true);
      Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"; "/Type"] (Pdf.Name "/StructTreeRoot");
      let pdf = {pdf with Pdf.major = 1; Pdf.minor =  7} in
        mark pdf;
        pdf

let create_pdfua2 title pagesize pages =
  let pdf = Cpdfcreate.blank_document_paper pagesize pages in
  let pdf = Cpdfmetadata.set_pdf_info ~xmp_also:false ~xmp_just_set:false ("/Title", Pdf.String title, 0) pdf in
  let pdf = Cpdfmetadata.create_metadata pdf in
    Cpdfmetadata.set_language pdf "en-US";
    let pdf = Cpdfmetadata.set_viewer_preference ("/DisplayDocTitle", Pdf.Boolean true, 0) pdf in
      Pdf.replace_chain pdf ["/Root"; "/MarkInfo"; "/Marked"] (Pdf.Boolean true);
      Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"; "/Type"] (Pdf.Name "/StructTreeRoot");
      let pdf = {pdf with Pdf.major = 2; Pdf.minor =  0} in
        mark2 2024 pdf;
        pdf
