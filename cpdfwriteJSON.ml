module J = Tjjson
module P = Pdf
module O = Pdfops

let sof = Printf.sprintf "%f" (* To prevent "0." *)
let soi = string_of_int
let string_of_float _ = failwith "use sof"
let string_of_int _ = failwith "use soi"

let rec json_of_object pdf fcs no_stream_data = function
  | P.Null -> J.String "null"
  | P.Boolean b -> J.Bool b
  | P.Integer i -> J.Number (soi i)
  | P.Real r -> J.Number (sof r)
  | P.String s -> J.String s
  | P.Name n -> J.String n
  | P.Array objs -> J.Array (List.map (json_of_object pdf fcs no_stream_data) objs)
  | P.Dictionary elts ->
      List.iter
        (function
            ("/Contents", P.Indirect i) -> fcs i
          | ("/Contents", P.Array elts) -> List.iter (function P.Indirect i -> fcs i | _ -> ()) elts
          | _ -> ())
        elts;
      J.Object (List.map (fun (k, v) -> (k, json_of_object pdf fcs no_stream_data v)) elts)
  | P.Stream ({contents = (Pdf.Dictionary dict as d, stream)} as mut) as thestream ->
      Pdf.getstream thestream;
      let str =
        begin match Pdf.lookup_direct pdf "/FunctionType" d with
        | Some _ ->
            Pdfcodec.decode_pdfstream_until_unknown pdf thestream;
            begin match !mut with (_, Pdf.Got b) -> Pdfio.string_of_bytes b | _ -> "failure: decomp" end
        | None ->
            if no_stream_data then "<<stream data elided>>" else
              match stream with Pdf.Got b -> Pdfio.string_of_bytes b | Pdf.ToGet _ -> "failure: toget"
       end
      in
        json_of_object pdf fcs no_stream_data (P.Array [P.Dictionary dict; P.String str])
  | P.Stream _ -> J.String "error: stream with not-a-dictioary"
  | P.Indirect i ->
      begin match Pdf.lookup_obj pdf i with
      | P.Stream {contents = (Pdf.Dictionary dict as d, _)} ->
          begin match Pdf.lookup_direct pdf "/Subtype" d with
          | Some (Pdf.Name "/Form") -> fcs i
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
  | O.Op_BDC (s, obj) -> J.Array [J.String s; J.String (Pdfwrite.string_of_pdf obj); J.String "BDC"]
  | O.Op_gs s -> J.Array [J.String s; J.String "gs"]
  | O.Op_Do s -> J.Array [J.String s; J.String "Do"]
  | O.Op_CS s -> J.Array [J.String s; J.String "CS"]
  | O.Op_SCN fs -> J.Array ((List.map (fun x -> J.Number (sof x)) fs) @ [J.String "SCN"])
  | O.Op_j j -> J.Array [J.Number (soi j); J.String "j"] 
  | O.Op_cm t ->
      J.Array
        [J.Number (sof t.Pdftransform.a);
         J.Number (sof t.Pdftransform.b);
         J.Number (sof t.Pdftransform.c);
         J.Number (sof t.Pdftransform.d);
         J.Number (sof t.Pdftransform.e);
         J.Number (sof t.Pdftransform.f);
         J.String "cm"]
  | O.Op_d (fl, y) ->
      J.Array [J.Array (List.map (fun x -> J.Number (sof x)) fl); J.Number (sof y); J.String "d"]
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
        [J.Number (sof t.Pdftransform.a);
         J.Number (sof t.Pdftransform.b);
         J.Number (sof t.Pdftransform.c);
         J.Number (sof t.Pdftransform.d);
         J.Number (sof t.Pdftransform.e);
         J.Number (sof t.Pdftransform.f);
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
  | O.Op_SC fs -> J.Array (List.map (fun x -> J.Number (sof x)) fs @ [J.String "SC"])
  | O.Op_sc fs -> J.Array (List.map (fun x -> J.Number (sof x)) fs @ [J.String "sc"])
  | O.Op_scn fs -> J.Array (List.map (fun x -> J.Number (sof x)) fs @ [J.String "scn"])
  | O.Op_G k -> J.Array [J.Number (sof k); J.String "G"]
  | O.Op_g k -> J.Array [J.Number (sof k); J.String "g"]
  | O.Op_RG (r, g, b) -> J.Array [J.Number (sof r); J.Number (sof g); J.Number (sof b); J.String "RG"]
  | O.Op_rg (r, g, b) -> J.Array [J.Number (sof r); J.Number (sof g); J.Number (sof b); J.String "rg"]
  | O.Op_K (c, m, y, k) -> J.Array [J.Number (sof c); J.Number (sof m); J.Number (sof y); J.Number (sof k); J.String "K"]
  | O.Op_sh s -> J.Array [J.String s; J.String "sh"]
  | O.Op_MP s -> J.Array [J.String s; J.String "MP"]
  | O.Op_BMC s -> J.Array [J.String s; J.String "BMC"]
  | O.Op_Unknown _ -> J.Array [J.String "Unknown"]
  | O.Op_SCNName (s, fs) ->
      J.Array (List.map (fun x -> J.Number (sof x)) fs @ [J.String s; J.String "SCNName"])
  | O.Op_scnName (s, fs) ->
      J.Array (List.map (fun x -> J.Number (sof x)) fs @ [J.String s; J.String "scnName"])
  | O.InlineImage (dict, data) -> J.Array [json_of_object pdf (fun _ -> ()) no_stream_data dict; J.String (Pdfio.string_of_bytes data)]
  | O.Op_DP (s, obj) -> J.Array [J.String s; json_of_object pdf (fun _ -> ()) no_stream_data obj; J.String "DP"]

(* parse_stream needs pdf and resources. These are for lexing of inline images,
 * looking up the colourspace. We do not need to worry about inherited
 * resources, though? For now, don't worry about inherited resources: check in
* PDF standard. *)
let parse_content_stream pdf resources bs =
  let ops = Pdfops.parse_stream pdf resources [bs] in
    J.Array (List.map (json_of_op pdf false) ops)

let json_of_pdf parse_content no_stream_data pdf =
  let trailerdict = (0, json_of_object pdf (fun x -> ()) no_stream_data pdf.Pdf.trailerdict) in
  let content_streams = ref [] in
  let fcs n = content_streams := n::!content_streams in
  let pairs =
    let ps = ref [] in
      Pdf.objiter
        (fun i pdfobj ->
          ps := (i, json_of_object pdf fcs no_stream_data pdfobj)::!ps)
        pdf;
      trailerdict::!ps
  in
    if parse_content then
      List.iter (fun n -> Pdfcodec.decode_pdfstream_until_unknown pdf (Pdf.lookup_obj pdf n)) !content_streams;
    let pairs_parsed =
      if not parse_content then pairs else
        List.map
          (fun (objnum, obj) ->
             if Pdfutil.mem objnum !content_streams then
               begin match obj with
               | J.Array [dict; J.String _] ->
                   (* FIXME Proper resources here for reasons explained above *)
                   let streamdata =
                     match Pdf.lookup_obj pdf objnum with
                     | Pdf.Stream {contents = (_, Pdf.Got b)} -> b
                     | _ -> failwith "JSON: stream not decoded"
                   in
                     (objnum, J.Array [dict; parse_content_stream pdf (Pdf.Dictionary []) streamdata])
               | _ -> failwith "json_of_pdf: stream parsing inconsistency"
               end
             else
               (objnum, obj))
          pairs
    in
      J.Array
        (List.map
          (fun (objnum, jsonobj) -> J.Array [J.Number (soi objnum); jsonobj])
          pairs_parsed)

let write fh parse_content no_stream_data pdf =
  let b = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer b in
    Tjjson.format formatter (json_of_pdf parse_content no_stream_data pdf);
    Format.pp_print_flush formatter ();
    output_string fh (Buffer.contents b)

