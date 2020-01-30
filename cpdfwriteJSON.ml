module J = Tjjson
module P = Pdf

let rec json_of_object fcs = function
  | P.Null -> J.String "null"
  | P.Boolean b -> J.Bool b
  | P.Integer i -> J.Number (string_of_int i)
  | P.Real r -> J.Number (string_of_float r)
  | P.String s -> J.String s
  | P.Name n -> J.String n
  | P.Array objs -> J.Array (List.map (json_of_object fcs) objs)
  | P.Dictionary elts ->
      (* Detect contents stream object numbers for second pass *)
      List.iter (function ("/Contents", P.Indirect i) -> fcs i | _ -> ()) elts;
      J.Object (List.map (fun (k, v) -> (k, json_of_object fcs v)) elts)
  | P.Stream {contents = (Pdf.Dictionary dict, stream)} as thestream ->
      Pdf.getstream thestream;
      let str = match stream with Got b -> Pdfio.string_of_bytes b | ToGet _ -> "failure: toget" in
        json_of_object fcs (P.Array [P.Dictionary dict; P.String str])
  | P.Stream _ -> J.String "error: stream with not-a-dictioary"
  | P.Indirect i -> J.Number (string_of_int i)

let parse_content_stream str = J.String ("PARSED: str")

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


