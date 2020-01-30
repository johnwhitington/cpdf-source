module J = Tjjson
module P = Pdf
module O = Pdfops

let rec json_of_object fcs = function
  | P.Null -> J.String "null"
  | P.Boolean b -> J.Bool b
  | P.Integer i -> J.Number (string_of_int i)
  | P.Real r -> J.Number (string_of_float r)
  | P.String s -> J.String s
  | P.Name n -> J.String n
  | P.Array objs -> J.Array (List.map (json_of_object fcs) objs)
  | P.Dictionary elts ->
      List.iter (function ("/Contents", P.Indirect i) -> fcs i | _ -> ()) elts;
      J.Object (List.map (fun (k, v) -> (k, json_of_object fcs v)) elts)
  | P.Stream {contents = (Pdf.Dictionary dict, stream)} as thestream ->
      Pdf.getstream thestream;
      let str = match stream with Got b -> Pdfio.string_of_bytes b | ToGet _ -> "failure: toget" in
        json_of_object fcs (P.Array [P.Dictionary dict; P.String str])
  | P.Stream _ -> J.String "error: stream with not-a-dictioary"
  | P.Indirect i -> J.Number (string_of_int i)

let sof = string_of_float
let soi = string_of_int

let json_of_op = function
  | O.Op_S -> J.Array [J.String "S"]
  | O.Op_s -> J.Array [J.String "s"]
  | O.Op_f -> J.Array [J.String "f"]
  | O.Op_F -> J.Array [J.String "F"]
  | O.Op_f' ->J.Array [J.String "f'"]
  | O.Op_B -> J.Array [J.String "B"]
  | O.Op_B' -> J.Array [J.String "B'"]
  | O.Op_b -> J.Array [J.String "b"]
  | O.Op_b' -> J.Array [J.String "b'"]
  | O.Op_n -> J.Array [J.String "n"]
  | O.Op_W -> J.Array [J.String "W"]
  | O.Op_W' -> J.Array [J.String "W'"]
  | O.Op_BT -> J.Array [J.String "BT"]
  | O.Op_ET -> J.Array [J.String "ET"]
  | O.Op_q -> J.Array [J.String "q"]
  | O.Op_Q -> J.Array [J.String "Q"]
  | O.Op_h -> J.Array [J.String "h"]
  | O.Op_T' -> J.Array [J.String "T'"]
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

  | _ -> J.Array [J.String "UNIMPLEMENTED"]


 (* | O.Op_J j -> J.Array [J.String "J"]
  | O.Op_M m -> J.Array [J.String "m"]
  | O.Op_ri s -> J.Array [J.String "ri"]
  | O.Op_i i -> J.Array [J.String "i"]
  | O.Op_c (a, b, c, d, e, k) ->J.Array [J.String "c"]
  | O.Op_v (a, b, c, d) ->J.Array [J.String "v"]
  | O.Op_y (a, b, c, d) -> J.Array [J.String "y"]
  | O.Op_Tc c -> J.Array [J.String "Tc"]
  | O.Op_Tw w -> J.Array [J.String "Tw"]
  | O.Op_Tz z -> J.Array [J.String "Tz"]
  | O.Op_TL l -> J.Array [J.String "TL"]
  | O.Op_Tf (k, s) ->J.Array [J.String "Tf"]
  | O.Op_Tr i -> J.Array [J.String "Tr"]
  | O.Op_Ts k -> J.Array [J.String "Ts"]
  | O.Op_Td (k, k') ->J.Array [J.String "Td"]
  | O.Op_TD (k, k') ->J.Array [J.String "TD"]
  | O.Op_Tm t ->J.Array [J.String "Tm"]
  | O.Op_Tj s -> J.Array [J.String "Tj"]
  | O.Op_TJ pdfobject -> J.Array [J.String "TJ"]
  | O.Op_' s -> J.Array [J.String "'"]
  | O.Op_'' (k, k', s) -> J.Array [J.String "''"]
  | O.Op_d0 (k, k') ->J.Array [J.String "d0"]
  | O.Op_d1 (a, b, c, d, e, k) ->J.Array [J.String "d1"]
  | O.Op_cs s -> J.Array [J.String "cs"]
  | O.Op_SC fs -> J.Array [J.String "SC"]
  | O.Op_sc fs -> J.Array [J.String "sc"]
  | O.Op_scn fs -> J.Array [J.String "scn"]
  | O.Op_SCNName (s, fs) ->J.Array [J.String "SCNName"]
  | O.Op_scnName (s, fs) ->J.Array [J.String "scnName"]
  | O.Op_G k -> J.Array [J.String "G"]
  | O.Op_g k -> J.Array [J.String "g"]
  | O.Op_RG (r, g, b) ->J.Array [J.String "RG"]
  | O.Op_rg (r, g, b) ->J.Array [J.String "rg"]
  | O.Op_K (c, m, y, k) ->J.Array [J.String "K"]
  | O.Op_sh s -> J.Array [J.String "sh"]
  | O.InlineImage (dict, data) -> J.Array [J.String "InlineImage"]
  | O.Op_MP s -> J.Array [J.String "MP"]
  | O.Op_DP (s, obj) ->J.Array [J.String "DP"]
  | O.Op_BMC s -> J.Array [J.String "BMC"]
  | O.Op_Unknown _ ->J.Array [J.String "Unknown"] *)

let parse_content_stream str =
  let ops = Pdfops.parse_stream (Pdf.empty ()) (Pdf.Dictionary []) [Pdfio.bytes_of_string str] in
    J.Array (List.map json_of_op ops)

let json_of_pdf parse_content pdf =
  let trailerdict = (0, json_of_object (fun x -> ()) pdf.Pdf.trailerdict) in
  let content_streams = ref [] in
  let pairs =
    let ps = ref [] in
      Pdf.objiter
        (fun i pdfobj ->
          ps := (i, json_of_object (fun n -> content_streams := n::!content_streams) pdfobj)::!ps)
        pdf;
      trailerdict::!ps
  in
    let pairs_parsed =
      if not parse_content then pairs else
        List.map
          (fun (objnum, obj) ->
             if Pdfutil.mem objnum !content_streams then
               begin match obj with
               | J.Array [dict; J.String streamdata] ->
                   (objnum, J.Array [dict; parse_content_stream streamdata])
               | _ -> failwith "json_of_pdf: stream parsing inconsistency"
               end
             else
               (objnum, obj))
          pairs
    in
      J.Array
        (List.map
          (fun (objnum, jsonobj) -> J.Array [J.String (string_of_int objnum); jsonobj])
          pairs_parsed)

let write fh parse_content pdf =
  let b = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer b in
    Tjjson.format formatter (json_of_pdf parse_content pdf);
    Format.pp_print_flush formatter ();
    output_string fh (Buffer.contents b)


