open Pdfutil
open Cpdferror

(*module J = Tjjson
module P = Pdf
module O = Pdfops

let sof = Printf.sprintf "%f" (* To prevent "0." *)
let soi = string_of_int
let string_of_float _ = error "use sof"
let string_of_int _ = error "use soi"

let opf = function
  | J.Object ["F", J.Number f] -> float_of_string f
  | _ -> error "num: not a float"

let opi = function
  | J.Object ["I", J.Number i] -> int_of_string i
  | _ -> error "num: not a float"

let rec op_of_json = function
  | J.Array [J.String "S"] -> O.Op_S
  | J.Array [J.String "s"] -> O.Op_s
  | J.Array [J.String "f"] -> O.Op_f
  | J.Array [J.String "F"] -> O.Op_F
  | J.Array [J.String "f*"] -> O.Op_f'
  | J.Array [J.String "B"] -> O.Op_B
  | J.Array [J.String "B*"] -> O.Op_B'
  | J.Array [J.String "b"] -> O.Op_b
  | J.Array [J.String "b*"] -> O.Op_b'
  | J.Array [J.String "n"] -> O.Op_n
  | J.Array [J.String "W"] -> O.Op_W
  | J.Array [J.String "W*"] -> O.Op_W'
  | J.Array [J.String "BT"] -> O.Op_BT
  | J.Array [J.String "ET"] -> O.Op_ET
  | J.Array [J.String "q"] -> O.Op_q
  | J.Array [J.String "Q"] -> O.Op_Q
  | J.Array [J.String "h"] -> O.Op_h
  | J.Array [J.String "T*"] -> O.Op_T'
  | J.Array [J.String "EMC"] -> O.Op_EMC
  | J.Array [J.String "BX"] -> O.Op_BX
  | J.Array [J.String "EX"] -> O.Op_EX
  | J.Array [a; b; c; d; J.String "re"] -> O.Op_re (opf a, opf b, opf c, opf d)
  | J.Array [a; b; c; d; J.String "k"] -> O.Op_k (opf a, opf b, opf c, opf d)
  | J.Array [a; b; J.String "m"] -> O.Op_m (opf a, opf b)
  | J.Array [a; b; J.String "l"] -> O.Op_l (opf a, opf b)
  | J.Array [J.String s; obj; J.String "BDC"] -> O.Op_BDC (s, object_of_json obj)
  | J.Array [J.String s; J.String "gs"] -> O.Op_gs s
  | J.Array [J.String s; J.String "Do"] -> O.Op_Do s
  | J.Array [J.String s; J.String "CS"] -> O.Op_CS s
  | J.Array [i; J.String "j"] -> O.Op_j (opi i)
  | J.Array [a; b; c; d; e; f; J.String "cm"] ->
      O.Op_cm
        {Pdftransform.a = opf a; Pdftransform.b = opf b; Pdftransform.c = opf c;
         Pdftransform.d = opf d; Pdftransform.e = opf e; Pdftransform.f = opf f}
  | J.Array [J.Array fls; y; J.String "d"] -> O.Op_d (map opf fls, opf y)
  | J.Array [a; J.String "w"] -> O.Op_w (opf a)
  | J.Array [a; J.String "J"] -> O.Op_J (opi a)
  | J.Array [a; J.String "M"] -> O.Op_M (opf a)
  | J.Array [J.String s; J.String "ri"] -> O.Op_ri s
  | J.Array [a; J.String "i"] -> O.Op_i (opi a)
  | J.Array [a; b; c; d; e; f; J.String "c"] -> O.Op_c (opf a, opf b, opf c, opf d, opf e, opf f)
  | J.Array [a; b; c; d; J.String "v"] -> O.Op_v (opf a, opf b, opf c, opf d)
  | J.Array [a; b; c; d; J.String "y"] -> O.Op_y (opf a, opf b, opf c, opf d)
  | J.Array [a; J.String "Tc"] -> O.Op_Tc (opf a)
  | J.Array [a; J.String "Tw"] -> O.Op_Tw (opf a)
  | J.Array [a; J.String "Tz"] -> O.Op_Tz (opf a)
  | J.Array [a; J.String "TL"] -> O.Op_TL (opf a)
  | J.Array [J.String k; n; J.String "Tf"] -> O.Op_Tf (k, opf n)
  | J.Array [a; J.String "Tr"] -> O.Op_Tr (opi a)
  | J.Array [a; J.String "Ts"] -> O.Op_Ts (opf a)
  | J.Array [a; b; J.String "Td"] -> O.Op_Td (opf a, opf b)
  | J.Array [a; b; J.String "TD"] -> O.Op_TD (opf a, opf b)
  | J.Array [a; b; c; d; e; f; J.String "Tm"] ->
        O.Op_Tm
          {Pdftransform.a = opf a; Pdftransform.b = opf b; Pdftransform.c = opf c;
           Pdftransform.d = opf d; Pdftransform.e = opf e; Pdftransform.f = opf f}
  | J.Array [J.String s; J.String "Tj"] -> Op_Tj s
  | J.Array [obj; J.String "TJ"] -> Op_TJ (object_of_json obj)
  | J.Array [J.String s; J.String "'"] -> Op_' s
  | J.Array [a; b; J.String s; J.String "''"] -> Op_'' (opf a, opf b, s)
  | J.Array [a; b; J.String "d0"] -> Op_d0 (opf a, opf b)
  | J.Array [a; b; c; d; e; f; J.String "d1"] -> Op_d1 (opf a, opf b, opf c, opf d, opf e, opf f)
  | J.Array [J.String s; J.String "cs"] -> Op_cs s 
  | J.Array [a; J.String "G"] -> Op_G (opf a);
  | J.Array [a; J.String "g"] -> Op_g (opf a);
  | J.Array [a; b; c; J.String "RG"] -> Op_RG (opf a, opf b, opf c);
  | J.Array [a; b; c; J.String "rg"] -> Op_rg (opf a, opf b, opf c);
  | J.Array [a; b; c; d; J.String "K"] -> Op_K (opf a, opf b, opf c, opf d);
  | J.Array [J.String s; J.String "sh"] -> Op_sh s;
  | J.Array [J.String s; J.String "MP"] -> Op_MP s;
  | J.Array [J.String s; J.String "BMC"] -> Op_BMC s;
  | J.Array [J.String s; J.String "Unknown"] -> O.Op_Unknown s
  | J.Array [J.String s; obj; J.String "DP"] -> O.Op_DP (s, object_of_json obj)
  | J.Array [a; J.String b; J.String "InlineImage"] ->
      O.InlineImage (object_of_json a, Pdfio.bytes_of_string b)
  | J.Array torev ->
      begin match rev torev with
      | J.String "SCN"::ns -> O.Op_SCN (map opf (rev ns))
      | J.String "SC"::ns -> O.Op_SCN (map opf (rev ns))
      | J.String "sc"::ns -> O.Op_SCN (map opf (rev ns))
      | J.String "scn"::ns -> O.Op_SCN (map opf (rev ns))
      | J.String "SCNName"::J.String s::ns -> O.Op_SCNName (s, map opf (rev ns))
      | J.String "scnName"::J.String s::ns -> O.Op_scnName (s, map opf (rev ns))
      | j ->
          Printf.eprintf "Unable to read reversed op from %s\n" (J.show (J.Array j));
          error "op reading failed"
      end
  | j ->
      Printf.eprintf "Unable to read op from %s\n" (J.show j);
      error "op reading failed"

and object_of_json = function
  | J.Null -> P.Null
  | J.Bool b -> P.Boolean b
  | J.Number n -> Pdf.Indirect (int_of_string n)
  | J.String s -> P.String s
  | J.Array objs -> P.Array (map object_of_json objs)
  | J.Object ["I", J.Number i] -> P.Integer (int_of_string i)
  | J.Object ["F", J.Number f] -> P.Real (float_of_string f)
  | J.Object ["N", J.String n] -> P.Name n
  | J.Object ["S", J.Array [dict; J.String data]] ->
      (* Fix up the length, in case it's been edited. *)
      let d' =
        P.add_dict_entry (object_of_json dict) "/Length" (P.Integer (String.length data))
      in
        P.Stream (ref (d', P.Got (Pdfio.bytes_of_string data)))
  | J.Object ["S", J.Array [dict; J.Array parsed_ops]] ->
      Pdfops.stream_of_ops (List.map op_of_json parsed_ops)
  | J.Object elts -> P.Dictionary (map (fun (n, o) -> (n, object_of_json o)) elts)

let rec json_of_object pdf fcs no_stream_data = function
  | P.Null -> J.Null
  | P.Boolean b -> J.Bool b
  | P.Integer i -> J.Object [("I", J.Number (soi i))]
  | P.Real r -> J.Object [("F", J.Number (sof r))]
  | P.String s -> J.String s
  | P.Name n -> J.Object [("N", J.String n)]
  | P.Array objs -> J.Array (map (json_of_object pdf fcs no_stream_data) objs)
  | P.Dictionary elts ->
      iter
        (function
            ("/Contents", P.Indirect i) ->
               begin match Pdf.lookup_obj pdf i with
               | Pdf.Array is -> iter (function Pdf.Indirect i -> fcs i | _ -> ()) is
               | _ -> fcs i
               end
          | ("/Contents", P.Array elts) -> iter (function P.Indirect i -> fcs i | _ -> ()) elts
          | _ -> ())
        elts;
      J.Object (map (fun (k, v) -> (k, json_of_object pdf fcs no_stream_data v)) elts)
  | P.Stream ({contents = (P.Dictionary dict as d, stream)} as mut) as thestream ->
      P.getstream thestream;
      let str =
        begin match P.lookup_direct pdf "/FunctionType" d with
        | Some _ ->
            Pdfcodec.decode_pdfstream_until_unknown pdf thestream;
            begin match !mut with (_, P.Got b) -> Pdfio.string_of_bytes b | _ -> "failure: decomp" end
        | None ->
            if no_stream_data then "<<stream data elided>>" else
              match stream with P.Got b -> Pdfio.string_of_bytes b | P.ToGet _ -> "failure: toget"
       end
      in
        json_of_object pdf fcs no_stream_data (P.Dictionary [("S", P.Array [P.Dictionary dict; P.String str])])
  | P.Stream _ -> J.String "error: stream with not-a-dictionary"
  | P.Indirect i ->
      begin match P.lookup_obj pdf i with
      | P.Stream {contents = (P.Dictionary dict as d, _)} ->
          begin match P.lookup_direct pdf "/Subtype" d with
          | Some (P.Name "/Form") -> fcs i
          | _ -> ()
          end
      | _ -> ()
      end;
      J.Number (soi i)

let json_of_op pdf no_stream_data = function
  | O.Op_S -> J.Array [J.String "S"]
  | O.Op_s -> J.Array [J.String "s"]
  | O.Op_f -> J.Array [J.String "f"]
  | O.Op_F -> J.Array [J.String "F"]
  | O.Op_f' ->J.Array [J.String "f*"]
  | O.Op_B -> J.Array [J.String "B"]
  | O.Op_B' -> J.Array [J.String "B*"]
  | O.Op_b -> J.Array [J.String "b"]
  | O.Op_b' -> J.Array [J.String "b*"]
  | O.Op_n -> J.Array [J.String "n"]
  | O.Op_W -> J.Array [J.String "W"]
  | O.Op_W' -> J.Array [J.String "W*"]
  | O.Op_BT -> J.Array [J.String "BT"]
  | O.Op_ET -> J.Array [J.String "ET"]
  | O.Op_q -> J.Array [J.String "q"]
  | O.Op_Q -> J.Array [J.String "Q"]
  | O.Op_h -> J.Array [J.String "h"]
  | O.Op_T' -> J.Array [J.String "T*"]
  | O.Op_EMC -> J.Array [J.String "EMC"]
  | O.Op_BX -> J.Array [J.String "BX"]
  | O.Op_EX -> J.Array [J.String "EX"]
  | O.Op_re (a, b, c, d) ->
      J.Array [J.Number (sof a); J.Number (sof b); J.Number (sof c); J.Number (sof d); J.String "re"]
  | O.Op_k (c, m, y, k) ->
      J.Array [J.Number (sof c); J.Number (sof m); J.Number (sof y); J.Number (sof k); J.String "k"]
  | O.Op_m (a, b) -> J.Array [J.Number (sof a); J.Number (sof b); J.String "m"]
  | O.Op_l (a, b) -> J.Array [J.Number (sof a); J.Number (sof b); J.String "l"]
  | O.Op_BDC (s, obj) -> J.Array [J.String s; json_of_object pdf (fun _ -> ()) no_stream_data obj; J.String "BDC"]
  | O.Op_gs s -> J.Array [J.String s; J.String "gs"]
  | O.Op_Do s -> J.Array [J.String s; J.String "Do"]
  | O.Op_CS s -> J.Array [J.String s; J.String "CS"]
  | O.Op_SCN fs -> J.Array ((map (fun x -> J.Number (sof x)) fs) @ [J.String "SCN"])
  | O.Op_j j -> J.Array [J.Number (soi j); J.String "j"] 
  | O.Op_cm t ->
      J.Array
        [J.Number (sof t.Pdftransform.a); J.Number (sof t.Pdftransform.b); J.Number (sof t.Pdftransform.c);
         J.Number (sof t.Pdftransform.d); J.Number (sof t.Pdftransform.e); J.Number (sof t.Pdftransform.f);
         J.String "cm"]
  | O.Op_d (fl, y) ->
      J.Array [J.Array (map (fun x -> J.Number (sof x)) fl); J.Number (sof y); J.String "d"]
  | O.Op_w w -> J.Array [J.Number (sof w); J.String "w"]
  | O.Op_J j -> J.Array [J.Number (soi j); J.String "J"]
  | O.Op_M m -> J.Array [J.Number (sof m); J.String "M"]
  | O.Op_ri s -> J.Array [J.String s; J.String "ri"]
  | O.Op_i i -> J.Array [J.Number (soi i); J.String "i"]
  | O.Op_c (a, b, c, d, e, k) ->
      J.Array
        [J.Number (sof a); J.Number (sof b); J.Number (sof c);
         J.Number (sof d); J.Number (sof e); J.Number (sof k); J.String "c"]
  | O.Op_v (a, b, c, d) ->
      J.Array
        [J.Number (sof a); J.Number (sof b); J.Number (sof c);
         J.Number (sof d); J.String "v"]
  | O.Op_y (a, b, c, d) ->
      J.Array
        [J.Number (sof a); J.Number (sof b); J.Number (sof c);
         J.Number (sof d); J.String "y"]
  | O.Op_Tc c -> J.Array [J.Number (sof c); J.String "Tc"]
  | O.Op_Tw w -> J.Array [J.Number (sof w); J.String "Tw"]
  | O.Op_Tz z -> J.Array [J.Number (sof z); J.String "Tz"]
  | O.Op_TL l -> J.Array [J.Number (sof l); J.String "TL"]
  | O.Op_Tf (k, s) -> J.Array [J.String k; J.Number (sof s); J.String "Tf"]
  | O.Op_Tr i -> J.Array [J.Number (soi i); J.String "Tr"]
  | O.Op_Ts k -> J.Array [J.Number (sof k); J.String "Ts"]
  | O.Op_Td (k, k') -> J.Array [J.Number (sof k); J.Number (sof k'); J.String "Td"]
  | O.Op_TD (k, k') -> J.Array [J.Number (sof k); J.Number (sof k'); J.String "TD"]
  | O.Op_Tm t ->
      J.Array
        [J.Number (sof t.Pdftransform.a); J.Number (sof t.Pdftransform.b); J.Number (sof t.Pdftransform.c);
         J.Number (sof t.Pdftransform.d); J.Number (sof t.Pdftransform.e); J.Number (sof t.Pdftransform.f);
         J.String "Tm"]
  | O.Op_Tj s -> J.Array [J.String s; J.String "Tj"]
  | O.Op_TJ pdfobject -> J.Array [json_of_object pdf (fun _ -> ()) no_stream_data pdfobject; J.String "TJ"]
  | O.Op_' s -> J.Array [J.String s; J.String "'"]
  | O.Op_'' (k, k', s) -> J.Array [J.Number (sof k); J.Number (sof k'); J.String s; J.String "''"]
  | O.Op_d0 (k, k') -> J.Array [J.Number (sof k); J.Number (sof k'); J.String "d0"]
  | O.Op_d1 (a, b, c, d, e, k) ->
      J.Array
        [J.Number (sof a); J.Number (sof b); J.Number (sof c);
         J.Number (sof d); J.Number (sof e); J.Number (sof k); J.String "d1"]
  | O.Op_cs s -> J.Array [J.String s; J.String "cs"]
  | O.Op_SC fs -> J.Array (map (fun x -> J.Number (sof x)) fs @ [J.String "SC"])
  | O.Op_sc fs -> J.Array (map (fun x -> J.Number (sof x)) fs @ [J.String "sc"])
  | O.Op_scn fs -> J.Array (map (fun x -> J.Number (sof x)) fs @ [J.String "scn"])
  | O.Op_G k -> J.Array [J.Number (sof k); J.String "G"]
  | O.Op_g k -> J.Array [J.Number (sof k); J.String "g"]
  | O.Op_RG (r, g, b) -> J.Array [J.Number (sof r); J.Number (sof g); J.Number (sof b); J.String "RG"]
  | O.Op_rg (r, g, b) -> J.Array [J.Number (sof r); J.Number (sof g); J.Number (sof b); J.String "rg"]
  | O.Op_K (c, m, y, k) -> J.Array [J.Number (sof c); J.Number (sof m); J.Number (sof y); J.Number (sof k); J.String "K"]
  | O.Op_sh s -> J.Array [J.String s; J.String "sh"]
  | O.Op_MP s -> J.Array [J.String s; J.String "MP"]
  | O.Op_BMC s -> J.Array [J.String s; J.String "BMC"]
  | O.Op_Unknown s -> J.Array [J.String s; J.String "Unknown"]
  | O.Op_SCNName (s, fs) ->
      J.Array (map (fun x -> J.Number (sof x)) fs @ [J.String s; J.String "SCNName"])
  | O.Op_scnName (s, fs) ->
      J.Array (map (fun x -> J.Number (sof x)) fs @ [J.String s; J.String "scnName"])
  | O.InlineImage (dict, data) -> J.Array [json_of_object pdf (fun _ -> ()) no_stream_data dict; J.String (Pdfio.string_of_bytes data); J.String "InlineImage"]
  | O.Op_DP (s, obj) -> J.Array [J.String s; json_of_object pdf (fun _ -> ()) no_stream_data obj; J.String "DP"]

(* parse_stream needs pdf and resources. These are for lexing of inline images,
 * looking up the colourspace. We do not need to worry about inherited
 * resources, though? For now, don't worry about inherited resources: check in
 * PDF standard. *)

let parse_content_stream pdf resources bs =
  let ops = O.parse_stream pdf resources [bs] in
    J.Array (map (json_of_op pdf false) ops)

(* We need to make sure each page only has one page content stream. Otherwise,
   if not split on op boundaries, each one would fail to parse on its own. *)
(* Future improvement. Don't blow up shared content streams. *)
let precombine_page_content pdf =
  let pages' =
    map
      (fun page ->
         match page.Pdfpage.content with
           [] | [_] -> page
         | _ ->
           let operators =
             Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content
           in
             {page with Pdfpage.content = [Pdfops.stream_of_ops operators]}
      )
      (Pdfpage.pages_of_pagetree pdf)
  in
    Pdfpage.change_pages true pdf pages'

let json_of_pdf parse_content no_stream_data decompress_streams pdf =
  let pdf = if parse_content then precombine_page_content pdf else pdf in
  if decompress_streams then
    Pdf.objiter
      (fun n obj ->
        Printf.eprintf "obj %i\n" n;
        match obj with
        | Pdf.Stream _ -> Printf.eprintf "decompressing...\n"; Pdfcodec.decode_pdfstream_until_unknown pdf obj
        | _ -> ())
      pdf;
  Pdf.remove_unreferenced pdf;
  let trailerdict = (0, json_of_object pdf (fun x -> ()) no_stream_data pdf.P.trailerdict) in
  let parameters =
    (-1, json_of_object pdf (fun x -> ()) false
      (Pdf.Dictionary [("/CPDFJSONformatversion", Pdf.Integer 2);
                       ("/CPDFJSONcontentparsed", Pdf.Boolean parse_content);
                       ("/CPDFJSONstreamdataincluded", Pdf.Boolean (not no_stream_data));
                       ("/CPDFJSONmajorpdfversion", Pdf.Integer pdf.Pdf.major);
                       ("/CPDFJSONminorpdfversion", Pdf.Integer pdf.Pdf.minor);
                      ]))
  in
  let content_streams = ref [] in
  let fcs n =
    content_streams := n::!content_streams;
    if parse_content then Pdfcodec.decode_pdfstream_until_unknown pdf (P.lookup_obj pdf n)
  in
  let pairs =
    let ps = ref [] in
      P.objiter
        (fun i pdfobj ->
          ps := (i, json_of_object pdf fcs no_stream_data pdfobj)::!ps)
        pdf;
      parameters::trailerdict::!ps
  in
    let pairs_parsed =
      if not parse_content then pairs else
        map
          (fun (objnum, obj) ->
             if mem objnum !content_streams then
               begin match obj with
               | J.Object ["S", J.Array [dict; J.String _]] ->
                   (* FIXME Proper resources here for reasons explained above? *)
                   let streamdata =
                     match P.lookup_obj pdf objnum with
                     | P.Stream {contents = (_, P.Got b)} -> b
                     | _ -> error "JSON: stream not decoded"
                   in
                     (objnum, J.Object ["S", J.Array [dict; parse_content_stream pdf (P.Dictionary []) streamdata]])
               | _ -> error "json_of_pdf: stream parsing inconsistency"
               end
             else
               (objnum, obj))
          pairs
    in
      J.Array
        (map
          (fun (objnum, jsonobj) -> J.Array [J.Number (soi objnum); jsonobj])
          pairs_parsed)

let pdf_of_json json =
  (*flprint (J.show json); flprint "\n";*)
  let objs = match json with J.Array objs -> objs | _ -> error "bad json top level" in
  let params = ref Pdf.Null in
  let trailerdict = ref Pdf.Null in
    let objects =
      option_map
        (function
         | J.Array [J.Number n; o] ->
             let objnum = int_of_string n in
               begin match objnum with
               | -1 -> params := object_of_json o; None 
               | 0 -> trailerdict := object_of_json o; None
               | n when n < 0 -> None
               | n ->  Some (n, object_of_json o)
               end
         | _ -> error "json bad obj")
        objs
    in
  (*List.  iter (fun (i, o) -> flprint (soi i); flprint "\n"; flprint (Pdfwrite.string_of_pdf o); flprint "\n") objects;*)
  begin match Pdf.lookup_direct (Pdf.empty ()) "/CPDFJSONstreamdataincluded" !params with
  | Some (Pdf.Boolean false) -> error "no stream data; cannot reconstruct PDF" 
  | _ -> ()  
  end;
  let major =
    match Pdf.lookup_direct (Pdf.empty ()) "/CPDFJSONmajorpdfversion" !params with 
      Some (Pdf.Integer i) -> i | _ -> error "bad major version"
  in
  let minor =
    match Pdf.lookup_direct (Pdf.empty ()) "/CPDFJSONminorpdfversion" !params with 
      Some (Pdf.Integer i) -> i | _ -> error "bad minor version"
  in
  (*flprint (Pdfwrite.string_of_pdf !trailerdict);*)
  let root =
    match !trailerdict with Pdf.Dictionary d ->
      begin match lookup "/Root" d with
        Some (Pdf.Indirect i) -> i | _ -> error "bad root"
      end
    | _ -> error "bad root 2"
  in
  let objmap = P.pdfobjmap_empty () in
  List.iter (fun (k, v) -> Hashtbl.add objmap k (ref (P.Parsed v), 0)) objects;
  let objects =
    {P.maxobjnum = 0;
     P.parse = None;
     P.pdfobjects = objmap;
     P.object_stream_ids = Hashtbl.create 0}
  in
    {P.major;
     P.minor;
     P.root;
     P.objects;
     P.trailerdict = !trailerdict;
     P.was_linearized = false;
     P.saved_encryption = None}

(* FIXME Proper streaming to output / from input, rather than making a big string first. *)
let to_output o parse_content no_stream_data decompress_streams pdf =
  let b = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer b in
    J.format formatter (json_of_pdf parse_content no_stream_data decompress_streams pdf);
    Format.pp_print_flush formatter ();
    o.Pdfio.output_string (Buffer.contents b)
*)

let to_output _ _ _ _ _ = ()

let example_pdf =
  let page =
    {(Pdfpage.blankpage Pdfpaper.a4) with
        Pdfpage.content = [Pdfops.stream_of_ops []];
        Pdfpage.resources = Pdf.Dictionary []}
  in
    let pdf, pageroot = Pdfpage.add_pagetree (many page 1) (Pdf.empty ()) in
      Pdfpage.add_root pageroot [] pdf

(* FIXME Proper streaming to output / from input, rather than making a big string first. *)
let of_input i =
  (*pdf_of_json*)
  (*ignore (J.parse (Pdfio.string_of_bytes (Pdfio.bytes_of_input i 0 (i.Pdfio.in_channel_length))));*)
  example_pdf
