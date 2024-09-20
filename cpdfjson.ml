(* Read and write PDF files in JSON format.

Format version 3: adds UTF8 option for strings for easier editing
Format version 2: adds object -1 with format data, roundtripping
Format version 1: no format specifier, output only

The file is an array of arrays containing an object number followed by an
object, one for each object in the file and two special ones:

Object -1: CPDF's own data with the PDF version number, CPDF JSON format
number, and flags used when writing (which may be required when reading):

  o /CPDFJSONformatversion (CPDFJSON integer (see below), currently 3)
  o /CPDFJSONcontentparsed (boolean, true if content streams have been parsed)
  o /CPDFJSONstreamdataincluded (boolean, true if stream data included. Cannot
  round-trip if false).
  o /CPDFJSONmajorpdfversion (CPDFJSON integer)
  o /CPDFJSONminorpdfversion (CPDFJSON integer)

Object 0: The PDF's trailer dictionary

Objects 1..n: The PDF's objects.

  o PDF arrays, dictionaries, booleans, and strings are the same in JSON.
  o Integers are written as {"I": 0}
  o Floats are written as {"F": 0.0}
  o Names are written as {"N": "/Pages"}
  o Indirect references are integers
  o Streams are {"S": [dict, data]}
  o Strings are converted into JSON strings in a way which is fully reversible.
    In original (utf8=false) mode, the bytes of the string in PDF representation
    are converted into UTF8, rather than the string itself being converted. In
    UTF8 mode (utf8=true), instead:
      1 If a String contains only PDFDocEncoding characters, is is converted
        to UTF8, and stored as {"U" : "..."}.
      2 If a String has a BOM and successfully converts to UTF8, it is converted
        to UTF8, and stored as {"U" : "..."}
      3 If a String has a BOM but fails to convert, or has no BOM, it is stored
        in original mode, as an unmarked string.
    In all cases, this process is still reversible:
      1. We try to convert back from UTF8 to PDFDocEncoding - this will always work
         on an unchanged string. If the string has changed, and we cannot convert to
         PDFDocEncoding, we convert back to UTF16 with a BOM.
      2. Same as (1) - if unaltered, will be UTF16, if altered, could be PDFDocEncoding
         or UTF16
      3. As in non-UTF-mode, reversible as we know.
    We need to mark strings as {"U" : ...} or not to preseve the distinction between
    PDFDocEncoding / UTF16BE on the one hand, and byte strings on the other.

There are two subformats: parsing content streams or not.  Hello World in CPDF
JSON without parsing content streams:

[
  [
  -1, { "/CPDFJSONformatversion": { "I": 2 },
  "/CPDFJSONcontentparsed": false, "/CPDFJSONstreamdataincluded": true,
  "/CPDFJSONmajorpdfversion": { "I": 1 },
  "/CPDFJSONminorpdfversion": { "I": 1 } } ], [
  0, { "/Size": { "I": 4 }, "/Root": 4,
  "/ID": [ "èÎ25\u001e³/°q:OÊ°u", "èÎ25\u001e³/°q:OÊ°u" ] } ], [
  1, { "/Type": { "N": "/Pages" }, "/Kids": [ 3 ], "/Count": { "I": 1 } } ],
  [
  2, {
  "S": [
    { "/Length": { "I": 49 } },
    "1 0 0 1 50 770 cm BT/F0 36 Tf(Hello, World!)Tj ET"
  ] } ], [
  3, { "/Type": { "N": "/Page" }, "/Parent": 1,
  "/Resources": {
    "/Font": {
      "/F0": {
        "/Type": { "N": "/Font" },
        "/Subtype": { "N": "/Type1" },
        "/BaseFont": { "N": "/Times-Italic" }
      }
    }
  },
  "/MediaBox": [
    { "I": 0 }, { "I": 0 }, { "F": 595.2755905510001 }, { "F": 841.88976378 }
  ], "/Rotate": { "I": 0 }, "/Contents": [ 2 ] } ], [
  4, { "/Type": { "N": "/Catalog" }, "/Pages": 1 } ]
]

Alternative object number 2 when parsing of object streams in operation:

2, {
"S": [
  {}, [
  [
  { "F": 1.0 }, { "F": 0.0 }, { "F": 0.0 }, { "F": 1.0 }, { "F": 50.0 }, {
  "F": 770.0 }, "cm" ], [ "BT" ], [ "/F0", { "F": 36.0 }, "Tf" ], [
  "Hello, World!", "Tj" ], [ "ET" ] ]
] } ], [

When parsing content streams:

  o Each operation is an array
  o The 'operation' for inline images is "InlineImage"

CPDF currently never preserves object streams, and only outputs unencrypted files.

When reloading a JSON file, CPDF knows how to correct or add /Length entries in
streams, so you need not worry about them.  *)

open Pdfutil
open Cpdferror

module J = Cpdfyojson.Safe
module P = Pdf
module O = Pdfops

let opf = function
  | `Assoc ["F", `Float f] -> f
  | `Assoc ["F", `Int i] -> float_of_int i
  | _ -> error "num: not a float"

let opi = function
  | `Assoc ["I", `Int i] -> i
  | `Assoc ["I", `Float f] -> int_of_float f
  | _ -> error "num: not an integer"

let rec op_of_json = function
  | `List [`String "S"] -> O.Op_S
  | `List [`String "s"] -> O.Op_s
  | `List [`String "f"] -> O.Op_f
  | `List [`String "F"] -> O.Op_F
  | `List [`String "f*"] -> O.Op_f'
  | `List [`String "B"] -> O.Op_B
  | `List [`String "B*"] -> O.Op_B'
  | `List [`String "b"] -> O.Op_b
  | `List [`String "b*"] -> O.Op_b'
  | `List [`String "n"] -> O.Op_n
  | `List [`String "W"] -> O.Op_W
  | `List [`String "W*"] -> O.Op_W'
  | `List [`String "BT"] -> O.Op_BT
  | `List [`String "ET"] -> O.Op_ET
  | `List [`String "q"] -> O.Op_q
  | `List [`String "Q"] -> O.Op_Q
  | `List [`String "h"] -> O.Op_h
  | `List [`String "T*"] -> O.Op_T'
  | `List [`String "EMC"] -> O.Op_EMC
  | `List [`String "BX"] -> O.Op_BX
  | `List [`String "EX"] -> O.Op_EX
  | `List [a; b; c; d; `String "re"] -> O.Op_re (opf a, opf b, opf c, opf d)
  | `List [a; b; c; d; `String "k"] -> O.Op_k (opf a, opf b, opf c, opf d)
  | `List [a; b; `String "m"] -> O.Op_m (opf a, opf b)
  | `List [a; b; `String "l"] -> O.Op_l (opf a, opf b)
  | `List [`String s; obj; `String "BDC"] -> O.Op_BDC (s, object_of_json obj)
  | `List [`String s; `String "gs"] -> O.Op_gs s
  | `List [`String s; `String "Do"] -> O.Op_Do s
  | `List [`String s; `String "CS"] -> O.Op_CS s
  | `List [i; `String "j"] -> O.Op_j (opi i)
  | `List [a; b; c; d; e; f; `String "cm"] ->
      O.Op_cm
        {Pdftransform.a = opf a; Pdftransform.b = opf b; Pdftransform.c = opf c;
         Pdftransform.d = opf d; Pdftransform.e = opf e; Pdftransform.f = opf f}
  | `List [`List fls; y; `String "d"] -> O.Op_d (map opf fls, opf y)
  | `List [a; `String "w"] -> O.Op_w (opf a)
  | `List [a; `String "J"] -> O.Op_J (opi a)
  | `List [a; `String "M"] -> O.Op_M (opf a)
  | `List [`String s; `String "ri"] -> O.Op_ri s
  | `List [a; `String "i"] -> O.Op_i (opi a)
  | `List [a; b; c; d; e; f; `String "c"] -> O.Op_c (opf a, opf b, opf c, opf d, opf e, opf f)
  | `List [a; b; c; d; `String "v"] -> O.Op_v (opf a, opf b, opf c, opf d)
  | `List [a; b; c; d; `String "y"] -> O.Op_y (opf a, opf b, opf c, opf d)
  | `List [a; `String "Tc"] -> O.Op_Tc (opf a)
  | `List [a; `String "Tw"] -> O.Op_Tw (opf a)
  | `List [a; `String "Tz"] -> O.Op_Tz (opf a)
  | `List [a; `String "TL"] -> O.Op_TL (opf a)
  | `List [`String k; n; `String "Tf"] -> O.Op_Tf (k, opf n)
  | `List [a; `String "Tr"] -> O.Op_Tr (opi a)
  | `List [a; `String "Ts"] -> O.Op_Ts (opf a)
  | `List [a; b; `String "Td"] -> O.Op_Td (opf a, opf b)
  | `List [a; b; `String "TD"] -> O.Op_TD (opf a, opf b)
  | `List [a; b; c; d; e; f; `String "Tm"] ->
        O.Op_Tm
          {Pdftransform.a = opf a; Pdftransform.b = opf b; Pdftransform.c = opf c;
           Pdftransform.d = opf d; Pdftransform.e = opf e; Pdftransform.f = opf f}
  | `List [`String s; `String "Tj"] -> Op_Tj s
  | `List [obj; `String "TJ"] -> Op_TJ (object_of_json obj)
  | `List [`String s; `String "'"] -> Op_' s
  | `List [a; b; `String s; `String "''"] -> Op_'' (opf a, opf b, s)
  | `List [a; b; `String "d0"] -> Op_d0 (opf a, opf b)
  | `List [a; b; c; d; e; f; `String "d1"] -> Op_d1 (opf a, opf b, opf c, opf d, opf e, opf f)
  | `List [`String s; `String "cs"] -> Op_cs s 
  | `List [a; `String "G"] -> Op_G (opf a);
  | `List [a; `String "g"] -> Op_g (opf a);
  | `List [a; b; c; `String "RG"] -> Op_RG (opf a, opf b, opf c);
  | `List [a; b; c; `String "rg"] -> Op_rg (opf a, opf b, opf c);
  | `List [a; b; c; d; `String "K"] -> Op_K (opf a, opf b, opf c, opf d);
  | `List [`String s; `String "sh"] -> Op_sh s;
  | `List [`String s; `String "MP"] -> Op_MP s;
  | `List [`String s; `String "BMC"] -> Op_BMC s;
  | `List [`String s; `String "Unknown"] -> O.Op_Unknown s
  | `List [`String s; obj; `String "DP"] -> O.Op_DP (s, object_of_json obj)
  | `List [a; `String b; `String "InlineImage"] ->
      O.InlineImage (object_of_json a, None, Pdfio.bytes_of_string b)
  | `List torev ->
      begin match rev torev with
      | `String "SCN"::ns -> O.Op_SCN (map opf (rev ns))
      | `String "SC"::ns -> O.Op_SC (map opf (rev ns))
      | `String "sc"::ns -> O.Op_sc (map opf (rev ns))
      | `String "scn"::ns -> O.Op_scn (map opf (rev ns))
      | `String "SCNName"::`String s::ns -> O.Op_SCNName (s, map opf (rev ns))
      | `String "scnName"::`String s::ns -> O.Op_scnName (s, map opf (rev ns))
      | j ->
          Pdfe.log (Printf.sprintf "Unable to read reversed op from %s\n" (J.show (`List j)));
          error "op reading failed"
      end
  | j ->
      Pdfe.log (Printf.sprintf "Unable to read op from %s\n" (J.show j));
      error "op reading failed"

and object_of_json = function
  | `Null -> P.Null
  | `Bool b -> P.Boolean b
  | `Int n -> Pdf.Indirect n
  | `String s -> P.String s
  | `List objs -> P.Array (map object_of_json objs)
  | `Assoc ["U", `String u] ->
      begin try P.String (Pdftext.pdfdocstring_of_utf8 u) with
        _ -> Pdfe.log (Printf.sprintf "Could not read UTF8 string %S\n" u); P.String u
      end
  | `Assoc ["I", `Int i] -> P.Integer i
  | `Assoc ["F", `Float f] -> P.Real f
  | `Assoc ["N", `String n] -> P.Name n
  | `Assoc ["S", `List [dict; `String data]] ->
      let d' =
        P.add_dict_entry (object_of_json dict) "/Length" (P.Integer (String.length data))
      in
        P.Stream (ref (d', P.Got (Pdfio.bytes_of_string data)))
  | `Assoc ["S", `List [dict; `List parsed_ops]] ->
      begin match 
        Pdfops.stream_of_ops (List.map op_of_json parsed_ops)
      with
        | P.Stream {contents = (_, Pdf.Got data)} ->
            let d' =
              P.add_dict_entry (object_of_json dict) "/Length" (P.Integer (Pdfio.bytes_size data))
            in
              P.Stream (ref (d', Pdf.Got data))
        | _ -> assert false
      end
  | `Assoc elts -> P.Dictionary (map (fun (n, o) -> (n, object_of_json o)) elts)
  | _ -> error "not recognised in object_of_json"

let pdf_of_json json =
  let objs = match json with `List objs -> objs | _ -> error "bad json top level" in
  let params = ref Pdf.Null in
  let trailerdict = ref Pdf.Null in
    let objects =
      option_map
        (function
         | `List [`Int objnum; o] ->
             begin match objnum with
             | -1 -> params := object_of_json o; None 
             | 0 -> trailerdict := object_of_json o; None
             | n when n < 0 -> None
             | n ->  Some (n, object_of_json o)
             end
         | _ -> error "json bad obj")
        objs
    in
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

let mkfloat f = `Assoc [("F", `Float f)]
let mkint i = `Assoc [("I", `Int i)]
let mkname n = `Assoc [("N", `String n)]

let rec json_of_object ~utf8 ?(clean_strings=false) pdf fcs ~no_stream_data ~parse_content = function
  | P.Null -> `Null
  | P.Boolean b -> `Bool b
  | P.Integer i -> mkint i
  | P.Real r -> mkfloat r
  | P.String s -> 
      if utf8 then
        begin try `Assoc [("U", `String (Pdftext.utf8_of_pdfdocstring s))] with _ -> `String s end
      else if clean_strings then `String (Pdftext.simplify_utf16be s)
      else `String s
  | P.Name n -> mkname n
  | P.Array objs -> `List (map (json_of_object ~utf8 pdf fcs ~no_stream_data ~parse_content) objs)
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
      `Assoc (map (fun (k, v) -> (k, json_of_object ~utf8 pdf fcs ~no_stream_data ~parse_content v)) elts)
  | P.Stream ({contents = (P.Dictionary dict as d, stream)} as mut) as thestream ->
      P.getstream thestream;
      let str, dict' =
        match P.lookup_direct pdf "/FunctionType" d, parse_content with
        | Some _, true ->
            Pdfcodec.decode_pdfstream_until_unknown pdf thestream;
            let dict = P.remove_dict_entry d "/Filter" in
              begin match !mut with (_, P.Got b) -> (Pdfio.string_of_bytes b, dict) | _ -> error "/FunctionType: failure: decomp" end
        | _ ->
            if no_stream_data then ("<<stream data elided>>", d) else
              match !mut with (_, P.Got b) -> (Pdfio.string_of_bytes b, d) | _ -> error "failure: toget"
      in
        (* We don't want to allow UTF8 processing of the stream here, so generate JSON without recursion. *)
        let dictjson = json_of_object ~utf8 pdf fcs ~no_stream_data ~parse_content dict' in
          `Assoc [("S", `List [dictjson; `String str])]
  | P.Stream _ -> error "error: stream with not-a-dictionary"
  | P.Indirect i ->
      begin match P.lookup_obj pdf i with
      | P.Stream {contents = (P.Dictionary dict as d, _)} ->
          begin match P.lookup_direct pdf "/Subtype" d with
          | Some (P.Name "/Form") -> fcs i
          | _ ->
              begin match P.lookup_direct pdf "/Type" d with
              | Some (P.Name "/Pattern") -> fcs i
              | _ -> ()
              end
          end
      | _ -> ()
      end;
      `Int i

let json_of_op utf8 pdf no_stream_data = function
  | O.Op_S -> `List [`String "S"]
  | O.Op_s -> `List [`String "s"]
  | O.Op_f -> `List [`String "f"]
  | O.Op_F -> `List [`String "F"]
  | O.Op_f' ->`List [`String "f*"]
  | O.Op_B -> `List [`String "B"]
  | O.Op_B' -> `List [`String "B*"]
  | O.Op_b -> `List [`String "b"]
  | O.Op_b' -> `List [`String "b*"]
  | O.Op_n -> `List [`String "n"]
  | O.Op_W -> `List [`String "W"]
  | O.Op_W' -> `List [`String "W*"]
  | O.Op_BT -> `List [`String "BT"]
  | O.Op_ET -> `List [`String "ET"]
  | O.Op_q -> `List [`String "q"]
  | O.Op_Q -> `List [`String "Q"]
  | O.Op_h -> `List [`String "h"]
  | O.Op_T' -> `List [`String "T*"]
  | O.Op_EMC -> `List [`String "EMC"]
  | O.Op_BX -> `List [`String "BX"]
  | O.Op_EX -> `List [`String "EX"]
  | O.Op_re (a, b, c, d) ->
      `List [mkfloat a; mkfloat b; mkfloat c; mkfloat d; `String "re"]
  | O.Op_k (c, m, y, k) ->
      `List [mkfloat c; mkfloat m; mkfloat y; mkfloat k; `String "k"]
  | O.Op_m (a, b) -> `List [mkfloat a; mkfloat b; `String "m"]
  | O.Op_l (a, b) -> `List [mkfloat a; mkfloat b; `String "l"]
  | O.Op_BDC (s, obj) -> `List [`String s; json_of_object ~utf8 pdf (fun _ -> ()) ~no_stream_data ~parse_content:false obj; `String "BDC"]
  | O.Op_gs s -> `List [`String s; `String "gs"]
  | O.Op_Do s -> `List [`String s; `String "Do"]
  | O.Op_CS s -> `List [`String s; `String "CS"]
  | O.Op_SCN fs -> `List ((map (fun x -> mkfloat x) fs) @ [`String "SCN"])
  | O.Op_j j -> `List [mkint j; `String "j"] 
  | O.Op_cm t ->
      `List
        [mkfloat t.Pdftransform.a; mkfloat t.Pdftransform.b; mkfloat t.Pdftransform.c;
         mkfloat t.Pdftransform.d; mkfloat t.Pdftransform.e; mkfloat t.Pdftransform.f;
         `String "cm"]
  | O.Op_d (fl, y) ->
      `List [`List (map (fun x -> mkfloat x) fl); mkfloat y; `String "d"]
  | O.Op_w w -> `List [mkfloat w; `String "w"]
  | O.Op_J j -> `List [mkint j; `String "J"]
  | O.Op_M m -> `List [mkfloat m; `String "M"]
  | O.Op_ri s -> `List [`String s; `String "ri"]
  | O.Op_i i -> `List [mkint i; `String "i"]
  | O.Op_c (a, b, c, d, e, f) ->
      `List
        [mkfloat a; mkfloat b; mkfloat c;
         mkfloat d; mkfloat e; mkfloat f; `String "c"]
  | O.Op_v (a, b, c, d) ->
      `List
        [mkfloat a; mkfloat b; mkfloat c;
         mkfloat d; `String "v"]
  | O.Op_y (a, b, c, d) ->
      `List
        [mkfloat a; mkfloat b; mkfloat c;
         mkfloat d; `String "y"]
  | O.Op_Tc c -> `List [mkfloat c; `String "Tc"]
  | O.Op_Tw w -> `List [mkfloat w; `String "Tw"]
  | O.Op_Tz z -> `List [mkfloat z; `String "Tz"]
  | O.Op_TL l -> `List [mkfloat l; `String "TL"]
  | O.Op_Tf (k, s) -> `List [`String k; mkfloat s; `String "Tf"]
  | O.Op_Tr i -> `List [mkint i; `String "Tr"]
  | O.Op_Ts k -> `List [mkfloat k; `String "Ts"]
  | O.Op_Td (k, k') -> `List [mkfloat k; mkfloat k'; `String "Td"]
  | O.Op_TD (k, k') -> `List [mkfloat k; mkfloat k'; `String "TD"]
  | O.Op_Tm t ->
      `List
        [mkfloat t.Pdftransform.a; mkfloat t.Pdftransform.b; mkfloat t.Pdftransform.c;
         mkfloat t.Pdftransform.d; mkfloat t.Pdftransform.e; mkfloat t.Pdftransform.f;
         `String "Tm"]
  | O.Op_Tj s -> `List [`String s; `String "Tj"]
  | O.Op_TJ pdfobject -> `List [json_of_object ~utf8 pdf (fun _ -> ()) ~no_stream_data ~parse_content:false pdfobject; `String "TJ"]
  | O.Op_' s -> `List [`String s; `String "'"]
  | O.Op_'' (k, k', s) -> `List [mkfloat k; mkfloat k'; `String s; `String "''"]
  | O.Op_d0 (k, k') -> `List [mkfloat k; mkfloat k'; `String "d0"]
  | O.Op_d1 (a, b, c, d, e, f) ->
      `List
        [mkfloat a; mkfloat b; mkfloat c;
         mkfloat d; mkfloat e; mkfloat f; `String "d1"]
  | O.Op_cs s -> `List [`String s; `String "cs"]
  | O.Op_SC fs -> `List (map (fun x -> mkfloat x) fs @ [`String "SC"])
  | O.Op_sc fs -> `List (map (fun x -> mkfloat x) fs @ [`String "sc"])
  | O.Op_scn fs -> `List (map (fun x -> mkfloat x) fs @ [`String "scn"])
  | O.Op_G k -> `List [mkfloat k; `String "G"]
  | O.Op_g k -> `List [mkfloat k; `String "g"]
  | O.Op_RG (r, g, b) -> `List [mkfloat r; mkfloat g; mkfloat b; `String "RG"]
  | O.Op_rg (r, g, b) -> `List [mkfloat r; mkfloat g; mkfloat b; `String "rg"]
  | O.Op_K (c, m, y, k) -> `List [mkfloat c; mkfloat m; mkfloat y; mkfloat k; `String "K"]
  | O.Op_sh s -> `List [`String s; `String "sh"]
  | O.Op_MP s -> `List [`String s; `String "MP"]
  | O.Op_BMC s -> `List [`String s; `String "BMC"]
  | O.Op_Unknown s -> `List [`String s; `String "Unknown"]
  | O.Op_Comment s  -> `List [`String s; `String "Comment"]
  | O.Op_SCNName (s, fs) ->
      `List (map (fun x -> mkfloat x) fs @ [`String s; `String "SCNName"])
  | O.Op_scnName (s, fs) ->
      `List (map (fun x -> mkfloat x) fs @ [`String s; `String "scnName"])
  | O.InlineImage (dict, dp, data) ->
      `List [json_of_object ~utf8 pdf (fun _ -> ()) ~no_stream_data ~parse_content:false dict; `String (Pdfio.string_of_bytes data); `String "InlineImage"]
  | O.Op_DP (s, obj) ->
      `List [`String s; json_of_object ~utf8 pdf (fun _ -> ()) ~no_stream_data ~parse_content:false obj; `String "DP"]

(* parse_stream needs pdf and resources. These are for lexing of inline images,
 * looking up the colourspace. *)
let parse_content_stream utf8 pdf resources bs =
  let ops = O.parse_stream pdf resources [bs] in
    `List (map (json_of_op utf8 pdf false) ops)

(* Make sure each page only has one page content stream. Otherwise,
   if not split on op boundaries, each one would fail to parse on its own. *)
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

(* Convert any strings in UTF16BE which could actually be in PDFDocEncoding
   (due to having no high bytes) to make editing JSON easier. *)
let rec ppstring_single_object pdf = function
  | Pdf.Dictionary d -> Pdf.recurse_dict (ppstring_single_object pdf) d
  | (Pdf.Stream {contents = (Pdf.Dictionary dict, data)}) ->
      Pdf.Stream {contents = (Pdf.recurse_dict (ppstring_single_object pdf) dict, data)}
  | Pdf.Array a -> Pdf.recurse_array (ppstring_single_object pdf) a
  | Pdf.String s -> Pdf.String (Pdftext.simplify_utf16be s)
  | x -> x

(* Do all objects, but skip the trailer dictionary since may mess up /ID if it
   happens to begin with UTF16BE BOM *)
let preprocess_strings pdf =
  Pdf.objselfmap (ppstring_single_object pdf) pdf

let json_of_pdf
  ~utf8 ~parse_content ~no_stream_data ~decompress_streams ~clean_strings
  pdf
=
  if clean_strings then preprocess_strings pdf;
  let pdf = if parse_content then precombine_page_content pdf else pdf in
  if decompress_streams then
    Pdf.objiter
      (fun _ obj -> match obj with Pdf.Stream _ -> Pdfcodec.decode_pdfstream_until_unknown pdf obj | _ -> ())
      pdf;
  Pdf.remove_unreferenced pdf;
  let trailerdict = (0, json_of_object ~utf8 pdf (fun x -> ()) ~no_stream_data ~parse_content:false pdf.P.trailerdict) in
  let parameters =
    (-1, json_of_object ~utf8 pdf (fun x -> ()) ~no_stream_data:false ~parse_content:false
      (Pdf.Dictionary [("/CPDFJSONformatversion", Pdf.Integer 3);
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
          ps := (i, json_of_object ~utf8 pdf fcs ~no_stream_data ~parse_content pdfobj)::!ps)
        pdf;
      parameters::trailerdict::sort compare !ps
  in
    let pairs_parsed =
      if not parse_content then pairs else
        map
          (fun (objnum, obj) ->
             if mem objnum !content_streams then
               begin match obj with
               | `Assoc ["S", `List [dict; `String _]] ->
                   let streamdata =
                     match P.lookup_obj pdf objnum with
                     | P.Stream {contents = (_, P.Got b)} -> b
                     | _ -> error "JSON: stream not decoded"
                   in
                     let dict =
                       match dict with
                       | `Assoc d ->
                           `Assoc (option_map (function (("/Filter" | "/Length"), _) -> None | (a, b) -> Some (a, b)) d)
                       | _ -> assert false
                     in
                       (objnum,
                        `Assoc ["S", `List [dict; parse_content_stream utf8 pdf (P.Dictionary []) streamdata]])
               | _ -> error "json_of_pdf: stream parsing inconsistency"
               end
             else
               (objnum, obj))
          pairs
    in
      `List
        (map
          (fun (objnum, jsonobj) -> `List [`Int objnum; jsonobj])
          pairs_parsed)

let to_output o ~utf8 ~parse_content ~no_stream_data ~decompress_streams ?(clean_strings=false) pdf =
  let json = json_of_pdf ~utf8 ~parse_content ~no_stream_data ~decompress_streams ~clean_strings pdf in
    match o.Pdfio.out_caml_channel with
    | Some ch -> J.pretty_to_channel ch json
    | None -> o.Pdfio.output_string (J.pretty_to_string json)

let of_input i =
  try
    match i.Pdfio.caml_channel with
    | Some ch ->
        pdf_of_json (J.from_channel ch)
    | None -> 
        let content = Pdfio.string_of_input i in
          pdf_of_json (J.from_string content)
  with
    e -> error (Printexc.to_string e)
