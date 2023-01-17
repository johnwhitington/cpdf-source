open Pdfutil
open Pdfio
open Cpdferror

(* Blacken text *)

(*
    Algorithm: Change
     BT
     <ops>
    ET
 
    ...to...
 
    BT
    Op_g 0.
    <ops minus any color, shading or gs operators>
    ET
    <ops minus any text positioning or text rendering ones>
*)
let blacktext_ops colour pdf resources content =
  let not_text = function
    | Pdfops.Op_Tj _ | Pdfops.Op_TJ _
    | Pdfops.Op_' _ | Pdfops.Op_'' (_, _, _)
    | Pdfops.Op_Td (_, _) | Pdfops.Op_TD (_, _)
    | Pdfops.Op_Tm _ | Pdfops.Op_T'
    | Pdfops.Op_Tc _
    | Pdfops.Op_Tw _
    | Pdfops.Op_Tz _
    | Pdfops.Op_TL _
    | Pdfops.Op_Tf (_, _)
    | Pdfops.Op_Tr _
    | Pdfops.Op_Ts _ -> false
    | _ -> true
  in let textlevel = ref 0
  in let removed = ref []
  in let operators =
    Pdfops.parse_operators pdf resources content
  in
    let rec remove_colourops prev = function
      | [] -> rev prev
      | Pdfops.Op_BT::more ->
          incr textlevel;
          remove_colourops
            (Cpdfaddtext.colour_op colour::Pdfops.Op_BT::prev)
            more
      | Pdfops.Op_ET::more ->
          decr textlevel;
          let prev' = !removed @ Pdfops.Op_ET::prev in
            removed := [];
            remove_colourops prev' more
      | (Pdfops.Op_G _
         | Pdfops.Op_g _ 
         | Pdfops.Op_RG (_, _, _)
         | Pdfops.Op_rg (_, _, _)
         | Pdfops.Op_k (_, _, _, _)
         | Pdfops.Op_K (_, _, _, _)
         | Pdfops.Op_SCN _
         | Pdfops.Op_SC _
         | Pdfops.Op_scn _
         | Pdfops.Op_sc _
         | Pdfops.Op_SCNName (_, _)
         | Pdfops.Op_scnName (_, _)
         | Pdfops.Op_CS _
         | Pdfops.Op_cs _
         | Pdfops.Op_sh _
         | Pdfops.Op_gs _)
        as op::more ->
          if !textlevel > 0
            then
              begin
                removed =| op;
                remove_colourops prev more
              end
            else remove_colourops (op::prev) more
      | op::more ->
          if !textlevel > 0 && not_text op then removed =| op;
          remove_colourops (op::prev) more
    in
      let operators' = remove_colourops [] operators in
        [Pdfops.stream_of_ops operators']

(* Blacken a form xobject, writing it to the same object. *)

let blacktext c range pdf =
  let blacktext_page _ page =
    let content' =
      blacktext_ops c pdf page.Pdfpage.resources page.Pdfpage.content
    in
      Cpdfutil.process_xobjects pdf page (blacktext_ops c);
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (Cpdfutil.ppstub blacktext_page) pdf range

(* Blacken lines *)
let blacklines_ops c pdf resources content =
  let rec blacken_strokeops prev = function
    | [] -> rev prev
    | Pdfops.Op_CS _::t ->
        blacken_strokeops (Pdfops.Op_CS "/DeviceRGB"::prev) t
    | (Pdfops.Op_SC _ | Pdfops.Op_SCN _ | Pdfops.Op_SCNName _ | Pdfops.Op_G _
       | Pdfops.Op_RG _ | Pdfops.Op_K _)::t ->
           blacken_strokeops (Cpdfaddtext.colour_op_stroke c::prev) t
    | h::t -> blacken_strokeops (h::prev) t
  and operators =
    Pdfops.parse_operators pdf resources content
  in
    let operators' = blacken_strokeops [] operators in
      [Pdfops.stream_of_ops operators']

let blacklines c range pdf =
  let blacklines_page _ page =
    let content' =
      blacklines_ops c pdf page.Pdfpage.resources page.Pdfpage.content
    in
      Cpdfutil.process_xobjects pdf page (blacklines_ops c);
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (Cpdfutil.ppstub blacklines_page) pdf range

(* Blacken Fills *)
let blackfills_ops c pdf resources content =
  let rec blacken_fillops prev = function
    | [] -> rev prev
    | Pdfops.Op_cs _::t ->
        blacken_fillops (Pdfops.Op_cs "/DeviceRGB"::prev) t
    | (Pdfops.Op_sc _ | Pdfops.Op_scn _ | Pdfops.Op_scnName _ | Pdfops.Op_g _
       | Pdfops.Op_rg _ | Pdfops.Op_k _)::t ->
           blacken_fillops (Cpdfaddtext.colour_op c::prev) t
    | h::t -> blacken_fillops (h::prev) t
  and operators =
    Pdfops.parse_operators pdf resources content
  in
    let operators' = blacken_fillops [] operators in
      [Pdfops.stream_of_ops operators']

let blackfills c range pdf =
  let blackfills_page _ page =
    let content' =
      blackfills_ops c pdf page.Pdfpage.resources page.Pdfpage.content
    in
      Cpdfutil.process_xobjects pdf page (blackfills_ops c);
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (Cpdfutil.ppstub blackfills_page) pdf range

(* Set a minimum line width to avoid dropout *)
let thinlines range width pdf =
  let thinpage _ page =
    let operators =
      Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content
    in
      let ctmstack = ref [ref Pdftransform.i_matrix] in
        let scaleof_ctm () =
          try
            match Pdftransform.decompose (!(hd !ctmstack)) with
              (scale, _, _, _, _, _) ->
                 scale
          with
            Failure _ (*"hd"*) -> 1.
        in
          let rec replace_operators prev = function
            | [] -> rev prev
            | (Pdfops.Op_w w)::more ->
              (* Alter width. *)
              let width' = width /. scaleof_ctm () in
                let w' =
                  if w >= width' then Pdfops.Op_w w else Pdfops.Op_w width'
                in
                  replace_operators (w'::prev) more
            | (Pdfops.Op_cm m)::more ->
                (* Update CTM *)
                begin try 
                  let top = hd !ctmstack in
                    top := Pdftransform.matrix_compose !top m
                with
                  Failure _ (*"hd"*) -> error "Malformed file."
                end;
                replace_operators ((Pdfops.Op_cm m)::prev) more
            | Pdfops.Op_q::more ->
                (* Push stack *)
                begin try
                  ctmstack =| ref (!(hd !ctmstack))
                with
                  Failure _ (*"hd"*) -> error "Malformed file"
                end;
                replace_operators (Pdfops.Op_q::prev) more
            | Pdfops.Op_Q::more ->
                (* Pop stack *)
                begin try
                  ctmstack := tl !ctmstack
                with
                  Failure _ (*"tl"*) -> error "Malformed file"
                end;
                replace_operators (Pdfops.Op_Q::prev) more
            | (Pdfops.Op_gs gsname)::more ->
                (* Perhaps insert [Op_w]. *)
                let opw =
                  match Pdf.lookup_direct pdf "/ExtGState" page.Pdfpage.resources with
                  | None -> []
                  | Some ext_state_dict ->
                      match Pdf.lookup_direct pdf gsname ext_state_dict with
                      | None -> []
                      | Some gdict ->
                          match Pdf.lookup_direct pdf "/LW" gdict with
                          | Some s -> (try [Pdfops.Op_w (Pdf.getnum pdf s)] with _ -> [])
                          | None -> []
                in
                  replace_operators (opw @ ((Pdfops.Op_gs gsname)::prev)) more
            | x::more -> replace_operators (x::prev) more
          in
            let operators = replace_operators [] operators in
              (* 2. Add an initial 'w' if width more than default width *)
              let operators =
                if width > 1. then (Pdfops.Op_w width)::operators else operators
              in
                let content' = [Pdfops.stream_of_ops operators] in
                  {page with Pdfpage.content = content'} 
  in
    Cpdfpage.process_pages (Cpdfutil.ppstub thinpage) pdf range

(* Parse the new content to make sure syntactically ok, append
 * as required. Rewrite the content *)
let append_page_content_page fast s before pdf n page =
  let ops =
    Pdfops.parse_stream pdf page.Pdfpage.resources [bytes_of_string s] 
  in
    (if before then Pdfpage.prepend_operators else Pdfpage.postpend_operators)
    pdf ops ~fast page

let append_page_content s before fast range pdf =
  Cpdfpage.process_pages (Cpdfutil.ppstub (append_page_content_page fast s before pdf)) pdf range

let rec dict_entry_single_object f pdf = function
  | (Pdf.Dictionary d) -> f (Pdf.recurse_dict (dict_entry_single_object f pdf) d)
  | (Pdf.Stream {contents = (Pdf.Dictionary dict, data)}) ->
      f (Pdf.Stream {contents = (Pdf.recurse_dict (dict_entry_single_object f pdf) dict, data)})
  | Pdf.Array a -> Pdf.recurse_array (dict_entry_single_object f pdf) a
  | x -> x

(* FIXME are we sure that functional values can never appear in the equality here? *)
let remove_dict_entry pdf key search =
  let f d =
    match search with
    | None -> Pdf.remove_dict_entry d key
    | Some s ->
        match Pdf.lookup_direct pdf key d with
        | Some v when v = s -> Pdf.remove_dict_entry d key
        | _ -> d
  in
    Pdf.objselfmap (dict_entry_single_object f pdf) pdf;
    pdf.Pdf.trailerdict <- dict_entry_single_object f pdf pdf.Pdf.trailerdict

let replace_dict_entry pdf key value search =
  let f d =
    match search with
    | None -> begin try Pdf.replace_dict_entry d key value with _ -> d end
    | Some s ->
        match Pdf.lookup_direct pdf key d with
        | Some v when v = s -> Pdf.replace_dict_entry d key value
        | _ -> d
  in
    Pdf.objselfmap (dict_entry_single_object f pdf) pdf;
    pdf.Pdf.trailerdict <- dict_entry_single_object f pdf pdf.Pdf.trailerdict

(* FIXME no need to self map here, since nothing changes *)
let print_dict_entry ~utf8 pdf key =
  let f d =
    match Pdf.lookup_direct pdf key d with
    | Some v ->
        (* We use a double newline as a separator. *)
        Printf.printf "%s\n\n" (Cpdfyojson.Safe.to_string (Cpdfjson.json_of_object ~utf8 ~clean_strings:true pdf (fun _ -> ()) ~no_stream_data:false ~parse_content:false v));
        d
    | None -> d
  in
    Pdf.objselfmap (dict_entry_single_object f pdf) pdf;
    pdf.Pdf.trailerdict <- dict_entry_single_object f pdf pdf.Pdf.trailerdict

(* For cpdflib. *)
let get_dict_entries ~utf8 pdf key =
  let es = ref [] in
  let f d =
    match Pdf.lookup_direct pdf key d with
    | Some v -> es := Cpdfjson.json_of_object ~utf8 ~clean_strings:true pdf (fun _ -> ()) ~no_stream_data:false ~parse_content:false v::!es; d
    | None -> d
  in
    Pdf.objselfmap (dict_entry_single_object f pdf) pdf;
    pdf.Pdf.trailerdict <- dict_entry_single_object f pdf pdf.Pdf.trailerdict;
    let arr = `List (rev !es) in
      (Pdfio.bytes_of_string (Cpdfyojson.Safe.to_string arr))

let remove_clipping_ops pdf resources content =
  let ops = Pdfops.parse_operators pdf resources content in
    let rec process a = function
      Pdfops.Op_W::Pdfops.Op_n::t -> process (Pdfops.Op_n::a) t
    | h::t -> process (h::a) t
    | [] -> rev a
    in
      [Pdfops.stream_of_ops (process [] ops)] 

let remove_clipping pdf range =
  let remove_clipping_page _ page =
    let content' =
      remove_clipping_ops pdf page.Pdfpage.resources page.Pdfpage.content
    in
      Cpdfutil.process_xobjects pdf page remove_clipping_ops;
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (Cpdfutil.ppstub remove_clipping_page) pdf range

let remove_unused_resources_page pdf n page =
  let xobjects, all_names =
    match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
    | Some (Pdf.Dictionary d) -> Pdf.Dictionary d, map fst d
    | _ -> Pdf.Dictionary [], []
  in
    let names_to_keep =
      option_map
        (function Pdfops.Op_Do n -> Some n | _ -> None)
        (Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content)
    in
      let names_to_remove = lose (mem' names_to_keep) all_names in
        let xobjdict = fold_left (Pdf.remove_dict_entry) xobjects names_to_remove in
          {page with Pdfpage.resources = Pdf.add_dict_entry page.Pdfpage.resources  "/XObject" xobjdict}

let remove_unused_resources pdf =
  Cpdfpage.process_pages (Cpdfutil.ppstub (remove_unused_resources_page pdf)) pdf (ilist 1 (Pdfpage.endpage pdf))
