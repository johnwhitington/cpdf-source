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
      Pdfpage.process_xobjects pdf page (blacktext_ops c);
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (Pdfpage.ppstub blacktext_page) pdf range

(* Blacken lines. FIXME Why doesn't this do xobjects like the other two? *)
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
      Pdfpage.process_xobjects pdf page (blacklines_ops c);
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (Pdfpage.ppstub blacklines_page) pdf range

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
      Pdfpage.process_xobjects pdf page (blackfills_ops c);
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (Pdfpage.ppstub blackfills_page) pdf range

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
    Cpdfpage.process_pages (Pdfpage.ppstub thinpage) pdf range

(* Parse the new content to make sure syntactically ok, append
 * as required. Rewrite the content *)
let append_page_content_page fast s before pdf n page =
  let ops =
    Pdfops.parse_stream pdf page.Pdfpage.resources [bytes_of_string s] 
  in
    (if before then Pdfpage.prepend_operators else Pdfpage.postpend_operators)
    pdf ops ~fast page

let append_page_content s before fast range pdf =
  Cpdfpage.process_pages (Pdfpage.ppstub (append_page_content_page fast s before pdf)) pdf range

let rec dict_entry_single_object f pdf = function
  | (Pdf.Dictionary d) -> f (Pdf.recurse_dict (dict_entry_single_object f pdf) d)
  | (Pdf.Stream {contents = (Pdf.Dictionary dict, data)}) ->
      f (Pdf.Stream {contents = (Pdf.recurse_dict (dict_entry_single_object f pdf) dict, data)})
  | Pdf.Array a -> Pdf.recurse_array (dict_entry_single_object f pdf) a
  | x -> x

let print_dict_entry ~utf8 pdf key =
  let f d =
    match Pdf.lookup_direct pdf key d with
    | Some v -> Printf.printf "%s\n" (Pdfwrite.string_of_pdf v); d
    | None -> d
  in
    Pdf.objselfmap (dict_entry_single_object f pdf) pdf;
    pdf.Pdf.trailerdict <- dict_entry_single_object f pdf pdf.Pdf.trailerdict

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
      Pdfpage.process_xobjects pdf page remove_clipping_ops;
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (Pdfpage.ppstub remove_clipping_page) pdf range

(* Empty string is trailerdict. Begins with / and it's a chain separated by
   commas. Begins with P and it's a page number then a (possibly empty) chain.
   Otherwise it's an object number (0 = trailerdict) then a (possibly empty)
   chain. *)
let split_chain str =
  map (fun x -> "/" ^ x) (tl (String.split_on_char '/' str))

let find_obj pdf objspec =
  let simple_obj obj =
    if obj = 0 then pdf.Pdf.trailerdict else Pdf.lookup_obj pdf obj
  in
  let chain_obj objnum chain =
    let obj = if objnum = 0 then pdf.Pdf.trailerdict else Pdf.lookup_obj pdf objnum in
    match Pdf.lookup_chain pdf obj chain with
    | Some x -> x
    | None -> raise (Pdf.PDFError "Chain not found")
  in
    match explode objspec with
    | 'P'::more ->
        let number, chain =
          let digits, rest = cleavewhile isdigit more in
            List.nth (Pdf.page_reference_numbers pdf) (int_of_string (implode digits) - 1),
            begin match split_chain (implode rest) with [""] -> [] | x -> x end
        in
          chain_obj number chain
    | '/'::more -> chain_obj 0 (split_chain (implode ('/'::more)))
    | [] -> simple_obj 0
    | l ->
        let digits, rest = cleavewhile isdigit l in
          chain_obj (int_of_string (implode digits)) (split_chain (implode rest))

let replace_obj pdf objspec obj =
  try Pdf.replace_chain pdf (split_chain objspec) obj with
    _ -> raise (Pdf.PDFError "Chain not found")

let remove_obj pdf objspec = ()

(* Replace a stream from a file e.g 4=data.dat replaces contents of object 4.
   The stream dictionary is altered only to correct the length. *)
let replace_stream pdf objspec filename =
  let data = Pdfio.bytes_of_string (contents_of_file filename) in
    begin match find_obj pdf objspec with
    | Pdf.Stream ({contents = dict, stream} as s) ->
        s := (Pdf.add_dict_entry dict "/Length" (Pdf.Integer (bytes_size data)), Pdf.Got data)
    | _ -> error "not a stream"
    end
