open Pdfutil
open Pdfio
open Cpdferror

type encoding =
  | Raw
  | UTF8
  | Stripped

(* Just strip everything which isn't 7 bit ASCII *)
let crude_de_unicode s =
  implode (map char_of_int (lose (fun x -> x > 127) (Pdftext.codepoints_of_pdfdocstring s)))

let encode_output enc s =
  match enc with
  | Raw -> s
  | UTF8 -> Pdftext.utf8_of_pdfdocstring s
  | Stripped -> crude_de_unicode s

let xmp_template =
{|<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d'?>

<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'
 xmlns:iX='http://ns.adobe.com/iX/1.0/'>

 <rdf:Description about=''
  xmlns='http://ns.adobe.com/pdf/1.3/'
  xmlns:pdf='http://ns.adobe.com/pdf/1.3/'>
  <pdf:CreationDate>CREATEDATE</pdf:CreationDate>
  <pdf:ModDate>MODDATE</pdf:ModDate>
  <pdf:Producer>PRODUCER</pdf:Producer>
  <pdf:Creator>CREATOR</pdf:Creator>
  <pdf:Title>TITLE</pdf:Title>
  <pdf:Subject>SUBJECT</pdf:Subject>
  <pdf:Author>AUTHOR</pdf:Author>
  <pdf:Keywords>KEYWORDS</pdf:Keywords>
  <pdf:Trapped>TRAPPED</pdf:Trapped>
 </rdf:Description>

 <rdf:Description about=''
  xmlns='http://ns.adobe.com/xap/1.0/'
  xmlns:xap='http://ns.adobe.com/xap/1.0/'>
   <xap:CreateDate>CREATEDATE</xap:CreateDate>
   <xap:CreatorTool>CREATOR</xap:CreatorTool>
   <xap:ModifyDate>MODDATE</xap:ModifyDate>
   <xap:MetadataDate>METADATADATE</xap:MetadataDate>
 </rdf:Description>

 <rdf:Description about=''
  xmlns='http://purl.org/dc/elements/1.1/'
  xmlns:dc='http://purl.org/dc/elements/1.1/'>
   <dc:title>TITLE</dc:title>
 </rdf:Description>

</rdf:RDF>

<?xpacket end='r'?>|}

(* Set or replace metadata *)
let set_metadata_from_bytes keepversion data pdf =
  let metadata_stream =
    Pdf.Stream
      {contents =
        (Pdf.Dictionary
         ["/Length", Pdf.Integer (bytes_size data);
          "/Type", Pdf.Name "/Metadata";
          "/Subtype", Pdf.Name "/XML"],
         Pdf.Got data)}
  in
    let objnum = Pdf.addobj pdf metadata_stream in 
      let document_catalog =
        match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
        | Some s -> s
        | None -> error "Malformed PDF: No root."
      in
        let document_catalog' =
          Pdf.add_dict_entry document_catalog "/Metadata" (Pdf.Indirect objnum)
        in
          let rootnum = Pdf.addobj pdf document_catalog' in
            let trailerdict =
              Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect rootnum)
            in
            {pdf with
               Pdf.trailerdict = trailerdict;
               Pdf.root = rootnum;
               Pdf.minor =
                 if pdf.Pdf.major > 1 || keepversion then pdf.Pdf.minor else max 4 pdf.Pdf.minor}

let set_metadata keepversion filename pdf =
  let ch = open_in_bin filename in
    let data = mkbytes (in_channel_length ch) in
      for x = 0 to bytes_size data - 1 do
        bset data x (input_byte ch)
      done;
      set_metadata_from_bytes keepversion data pdf

(* Remove metadata *)
let remove_metadata pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | None -> error "malformed file" 
  | Some root ->
      let root' = Pdf.remove_dict_entry root "/Metadata" in
        let rootnum = Pdf.addobj pdf root' in
          {pdf with
             Pdf.trailerdict =
               Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect rootnum);
             Pdf.root =
               rootnum}

(* Print metadata *)
let get_metadata pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | None -> error "malformed file"
  | Some root ->
      match Pdf.lookup_direct pdf "/Metadata" root with
      | Some ((Pdf.Stream _) as s) ->
          Pdfcodec.decode_pdfstream pdf s;
          begin match s with
          | Pdf.Stream {contents = (_, Pdf.Got data)} -> Some data 
          | _ -> assert false
          end
      | _ -> None

let print_metadata pdf =
  match get_metadata pdf with
    None -> ()
  | Some data ->
      for x = 0 to bytes_size data - 1 do
        Printf.printf "%c" (char_of_int (bget data x))
      done


let get_info raw pdf =
  let infodict =
    match Pdf.lookup_direct pdf "/Info" pdf.Pdf.trailerdict with
    | Some infodict -> infodict
    | _ -> Pdf.Dictionary []
  in
    let getstring name =
      match Pdf.lookup_direct pdf name infodict with
      | Some (Pdf.String s) ->
          if raw then s else crude_de_unicode s
      | Some (Pdf.Boolean false) -> "False"
      | Some (Pdf.Boolean true) -> "True"
      | _ -> if name = "/Trapped" then "False" else ""
    in
      getstring
       
let get_info_utf8 pdf =
  let infodict =
    match Pdf.lookup_direct pdf "/Info" pdf.Pdf.trailerdict with
    | Some infodict -> infodict
    | _ -> Pdf.Dictionary []
  in
    (function name ->
      match Pdf.lookup_direct pdf name infodict with
      | Some (Pdf.String s) -> Pdftext.utf8_of_pdfdocstring s
      | Some (Pdf.Boolean false) -> "False"
      | Some (Pdf.Boolean true) -> "True"
      | _ -> if name = "/Trapped" then "False" else "")

let getstring encoding pdf =
  match encoding with
  | Raw -> get_info true pdf
  | Stripped -> get_info false pdf
  | UTF8 -> get_info_utf8 pdf

let get_catalog_item name pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | Some catalog ->
      begin match Pdf.lookup_direct pdf name catalog with
      | Some (Pdf.Name x) when x <> "" -> implode (tl (explode x))
      | _ -> ""
      end
  | _ -> ""

let get_viewer_pref_item name pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | Some catalog ->
      begin match Pdf.lookup_direct pdf "/ViewerPreferences" catalog with
      | Some d ->
          begin match Pdf.lookup_direct pdf name d with
          | Some (Pdf.Name x) when x <> "" -> implode (tl (explode x))
          | Some (Pdf.Boolean b) -> string_of_bool b
          | _ -> ""
          end
      | None -> ""
      end
  | _ -> ""

let output_info ?(json=ref [("none", `Null)]) encoding pdf =
  let notjson = !json = [("none", `Null)] in
  let getstring = getstring encoding pdf in
    if notjson then Printf.printf "Version: %i.%i\n" pdf.Pdf.major pdf.Pdf.minor;
    json =| ("Version", `List [`Int pdf.Pdf.major; `Int pdf.Pdf.minor]);
    if notjson then Printf.printf "Pages: %i\n" (Pdfpage.endpage pdf);
    json =| ("Pages", `Int (Pdfpage.endpage pdf));
    if notjson then Printf.printf "Title: %s\n" (getstring "/Title");
    json =| ("Title", if getstring "/Title" = "" then `Null else `String (getstring "/Title"));
    if notjson then Printf.printf "Author: %s\n" (getstring "/Author");
    json =| ("Author", if getstring "/Author" = "" then `Null else `String (getstring "/Author"));
    if notjson then Printf.printf "Subject: %s\n" (getstring "/Subject");
    json =| ("Subject", if getstring "/Subject" = "" then `Null else `String (getstring "/Subject"));
    if notjson then Printf.printf "Keywords: %s\n" (getstring "/Keywords");
    json =| ("Keywords", if getstring "/Keywords" = "" then `Null else `String (getstring "/Keywords"));
    if notjson then  Printf.printf "Creator: %s\n" (getstring "/Creator");
    json =| ("Creator", if getstring "/Creator" = "" then `Null else `String (getstring "/Creator"));
    if notjson then Printf.printf "Producer: %s\n" (getstring "/Producer");
    json =| ("Producer", if getstring "/Producer" = "" then `Null else `String (getstring "/Producer"));
    if notjson then Printf.printf "Created: %s\n" (getstring "/CreationDate");
    json =| ("Created", if getstring "/CreationDate" = "" then `Null else `String (getstring "/CreationDate"));
    if notjson then Printf.printf "Modified: %s\n" (getstring "/ModDate");
    json =| ("Modified", if getstring "/ModDate" = "" then `Null else `String (getstring "/ModDate"));
    if notjson then Printf.printf "Trapped: %s\n" (getstring "/Trapped");
    json =| ("Trapped", `Bool (bool_of_string (String.lowercase_ascii (getstring "/Trapped"))));
    if notjson then Printf.printf "PageMode: %s\n" (get_catalog_item "/PageMode" pdf);
    json =| ("PageMode", match (get_catalog_item "/PageMode" pdf) with "" -> `Null | x -> `String x);
    if notjson then Printf.printf "PageLayout: %s\n" (get_catalog_item "/PageLayout" pdf);
    json =| ("PageLayout", match (get_catalog_item "/PageLayout" pdf) with "" -> `Null | x -> `String x);
    if notjson then Printf.printf "HideToolbar: %s\n" (get_viewer_pref_item "/HideToolbar" pdf);
    json =| ("HideToolbar", match get_viewer_pref_item "/HideToolbar" pdf with "" -> `Null | s -> `Bool (bool_of_string s));
    if notjson then Printf.printf "HideMenubar: %s\n" (get_viewer_pref_item "/HideMenubar" pdf);
    json =| ("HideMenubar", match get_viewer_pref_item "/HideMenubar" pdf with "" -> `Null | s -> `Bool (bool_of_string s));
    if notjson then Printf.printf "HideWindowUI: %s\n" (get_viewer_pref_item "/HideWindowUI" pdf);
    json =| ("HideWindowUI", match get_viewer_pref_item "/HideWindowUI" pdf with "" -> `Null | s -> `Bool (bool_of_string s));
    if notjson then Printf.printf "FitWindow: %s\n" (get_viewer_pref_item "/FitWindow" pdf);
    json =| ("FitWindow", match get_viewer_pref_item "/FitWindow" pdf with "" -> `Null | s -> `Bool (bool_of_string s));
    if notjson then Printf.printf "CenterWindow: %s\n" (get_viewer_pref_item "/CenterWindow" pdf);
    json =| ("CenterWindow", match get_viewer_pref_item "/CenterWindow" pdf with "" -> `Null | s -> `Bool (bool_of_string s));
    if notjson then Printf.printf "DisplayDocTitle: %s\n" (get_viewer_pref_item "/DisplayDocTitle" pdf);
    json =| ("DisplayDocTitle", match get_viewer_pref_item "/DisplayDocTitle" pdf with "" -> `Null | s -> `Bool (bool_of_string s));
    if notjson then Printf.printf "NonFullScreenPageMode: %s\n" (get_viewer_pref_item "/NonFullScreenPageMode" pdf);
    json =| ("NonFullPageScreenMode", match (get_viewer_pref_item "/NonFullPageScreenMode" pdf) with "" -> `Null | x -> `String x);

type xmltree =
    E of Cpdfxmlm.tag * xmltree list
  | D of string

let xmltree_of_bytes b =
  let i = Cpdfxmlm.make_input (`String (0, string_of_bytes b)) in
    let el tag childs = E (tag, childs)
    and data d = D d in
      Cpdfxmlm.input_doc_tree ~el ~data i

let bytes_of_xmltree t =
  let buf = Buffer.create 1024 in
  let o = Cpdfxmlm.make_output (`Buffer buf) in
  let frag = function
      E (tag, childs) -> `El (tag, childs)
    | D d -> `Data d
  in
    Cpdfxmlm.output_doc_tree frag o t;
    bytes_of_string (Buffer.contents buf)

let rec string_of_xmltree = function
   D d ->
     Printf.sprintf "DATA {%s}" d
 | E (tag, trees) ->
     Printf.sprintf "ELT (%s, %s)"
       (string_of_tag tag)
       (string_of_xmltrees trees)

and string_of_tag ((n, n'), attributes) =
  Printf.sprintf
    "NAME |%s| |%s|, ATTRIBUTES {%s}" n n'
    (string_of_attributes attributes)

and string_of_attribute ((n, n'), str) =
  Printf.sprintf "ATTRNAME |%s| |%s|, STR {%s}" n n' str

and string_of_attributes attrs =
  fold_left
    (fun a b -> a ^ " " ^ b) "" (map string_of_attribute attrs)

and string_of_xmltrees trees =
  fold_left
    (fun a b -> a ^ " " ^ b) "" (map string_of_xmltree trees)

let adobe = "http://ns.adobe.com/pdf/1.3/"
let xmp = "http://ns.adobe.com/xap/1.0/"
let dc = "http://purl.org/dc/elements/1.1/"
let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
let pdfaid = "http://www.aiim.org/pdfa/ns/id/"
let pdfxid = "http://www.npes.org/pdfx/ns/id/"
let pdfe = "http://www.aiim.org/pdfe/ns/id/"
let pdfuaid = "http://www.aiim.org/pdfua/ns/id/"
let pdfvtid = "http://www.npes.org/pdfvt/ns/id/"

let combine_with_commas strs =
  String.trim
    (fold_left (fun x y -> x ^ (if x <> "" then ", " else "") ^ y) "" strs)

(* Collect all <li> elements inside a seq, bag, or alt. Combine with commas. If
none found, return empty string instead. *)
let collect_list_items = function
   E (((n, n'), _), elts) when
     n = rdf && (n' = "Alt" || n' = "Seq" || n' = "Bag")
   ->
     combine_with_commas
       (option_map
         (function
             E (((n, n'), _), [D d]) when n = rdf && n' = "li" ->
               Some d
           | _ -> None)
         elts)
 | _ -> ""

let collect_list_items_all all =
  match keep (function E _ -> true | _ -> false) all with
    h::_ -> Some (collect_list_items h)
  | [] -> None

let rec get_data_for namespace name = function
   D _ -> None
 | E (((n, n'), _), [D d]) when n = namespace && n' = name ->
     Some d
 | E (((n, n'), _), e) when n = namespace && n' = name ->
     collect_list_items_all e
 | E (_, l) ->
     match option_map (get_data_for namespace name) l with
       x :: _ -> Some x
     | _ -> None

(* PDF/A: <pdfaid:part>2</pdfaid:part> <pdfaid:conformance>B</pdfaid:conformance>
   PDF/E: <pdfe:ISO_PDFEVersion>PDF/E-1</pdfe:ISO_PDFEVersion>
   PDF/VT: <pdfvtid:GTS_PDFVTVersion>PDF/VT-1</pdfvtid:GTS_PDFVTVersion>
   PDF/UA: <pdfuaid:part>1</pdfuaid:part>
   PDF/X: <pdfxid:GTS_PDFXVersion>PDF/X-4</pdfxid:GTS_PDFXVersion> (Fallback DID /GTS_PDFXVersion) *)
let determine_subformats pdf =
  let formats = ref [] in
  let fallback_pdfx () =
    match Pdf.lookup_direct pdf "/Info" pdf.Pdf.trailerdict with
    | Some d ->
        begin match Pdf.lookup_direct pdf "/GTS_PDFXVersion" d with
        | Some (Pdf.String s) -> formats =| s
        | _ -> ()
        end
    | None -> ()
  in
    match get_metadata pdf with
    | None ->
        fallback_pdfx ();
        !formats
    | Some metadata ->
        let _, tree = xmltree_of_bytes metadata in
          (* PDF/E *)
          begin match get_data_for pdfe "ISO_PDFEVersion" tree with
          | Some s -> formats =| s
          | None -> ()
          end;
          (* PDF/UA *)
          begin match get_data_for pdfuaid "part" tree with
          | Some s -> formats =| "PDF/UA-" ^ s
          | None -> ()
          end;
          (* PDF/A *)
          begin match get_data_for pdfaid "part" tree with
          | Some part ->
              let conformance =
                match get_data_for pdfaid "conformance" tree with
                | Some s -> String.lowercase_ascii s
                | None -> ""
              in
                formats =| "PDF/A-" ^ part ^ conformance
          | None -> ()
          end;
          (* PDF/X *)
          begin match get_data_for pdfxid "GTS_PDFXVersion" tree with
          | Some s -> formats =| s
          | None -> fallback_pdfx ()
          end;
          (* PDF/VT *)
          begin match get_data_for pdfvtid "GTS_PDFVTVersion" tree with
          | Some s -> formats =| s
          | None -> ()
          end;
          !formats

let output_xmp_info ?(json=ref [("none", `Null)]) encoding pdf =
  let notjson = !json = [("none", `Null)] in
  let print_out tree title namespace name =
    match get_data_for namespace name tree with
      None -> ()
    | Some data ->
        if notjson then
          begin
            Printf.printf "%s: " title;
            print_endline data
          end
        else
          json =| (title, `String data)
  in
    if notjson
      then Printf.printf "Subformats: %s\n" (combine_with_commas (determine_subformats pdf))
      else json =| ("Subformats", `List (map (fun x -> `String x) (determine_subformats pdf)));
    match get_metadata pdf with
      None -> ()
    | Some metadata ->
        try
          let dtd, tree = xmltree_of_bytes metadata in
            print_out tree "XMP pdf:Keywords" adobe "Keywords";
            print_out tree "XMP pdf:Producer" adobe "Producer";
            print_out tree "XMP pdf:Trapped" adobe "Trapped";
            print_out tree "XMP pdf:Title" adobe "Title";
            print_out tree "XMP pdf:Creator" adobe "Creator";
            print_out tree "XMP pdf:Subject" adobe "Subject";
            print_out tree "XMP pdf:Author" adobe "Author";
            print_out tree "XMP pdf:CreationDate" adobe "CreationDate";
            print_out tree "XMP pdf:ModDate" adobe "ModDate";
            print_out tree "XMP xmp:CreateDate" xmp "CreateDate";
            print_out tree "XMP xmp:CreatorTool" xmp "CreatorTool";
            print_out tree "XMP xmp:MetadataDate" xmp "MetadataDate";
            print_out tree "XMP xmp:ModifyDate" xmp "ModifyDate";
            print_out tree "XMP dc:title" dc "title";
            print_out tree "XMP dc:creator" dc "creator";
            print_out tree "XMP dc:subject" dc "subject";
            print_out tree "XMP dc:description" dc "description"
        with
          _ -> ()

(* Get XMP info equivalent of an old metadata field *)
let check = function
  "/Title" -> [(adobe, "Title"); (dc, "title")]
| "/Author" -> [(adobe, "Author"); (dc, "creator")]
| "/Subject" -> [(adobe, "Subject"); (dc, "subject")]
| "/Keywords" -> [(adobe, "Keywords")]
| "/Creator" -> [(adobe, "Creator"); (xmp, "CreatorTool")]
| "/Producer" -> [(adobe, "Producer")]
| "/CreationDate" -> [(adobe, "CreationDate"); (xmp, "CreateDate")]
| "/ModDate" -> [(adobe, "ModificationDate"); (xmp, "ModifyDate")]
| _ -> failwith "Cpdf.check_name not known"

let get_xmp_info pdf name =
  let tocheck = check name in
  match get_metadata pdf with
    None -> ""
  | Some metadata ->
      try
        let _, tree = xmltree_of_bytes metadata in
          let results = map (fun (kind, key) -> match get_data_for kind key tree with Some x -> x | None -> "") tocheck in
            match lose (eq "") results with
             x::_ -> x
           | [] -> ""
      with
        _ -> ""

(* Set XMP info *)
let rec set_xml_field kind fieldname value = function
  D data -> D data
| E (((n, n'), m), _ (*[D _]*)) when n = kind && n' = fieldname -> (* Replace anything inside, including nothing i.e <tag/> *)
    E (((n, n'), m), [D value])
| E (x, ts) -> E (x, map (set_xml_field kind fieldname value) ts)

let set_pdf_info_xml kind fieldname value xmldata pdf =
  let dtd, tree = xmltree_of_bytes xmldata in
  let str =
    match value with
      Pdf.String s -> Pdftext.utf8_of_pdfdocstring s
    | Pdf.Boolean true -> "True"
    | Pdf.Boolean false -> "False"
    | _ -> failwith "set_pdf_info_xml: not a string"
  in
  let newtree = set_xml_field kind fieldname str tree in
    bytes_of_xmltree (dtd, newtree)

let set_pdf_info_xml_many changes value xmldata pdf =
  let xmldata = ref xmldata in
    iter
      (fun (kind, fieldname) ->
         xmldata := set_pdf_info_xml kind fieldname value !xmldata pdf)
      changes;
    !xmldata

(* Set an entry in the /Info dictionary *)

(* We must parse the date to get its components, then use strftime to build the
 * new string in XMP format *)

type date =
  {mutable year : int;
   mutable month : int; (* 1 - 12 *)
   mutable day : int; (* 1 - 31 *)
   mutable hour : int; (* 0 - 23 *)
   mutable minute : int; (* 0 - 59 *)
   mutable second : int; (* 0 - 59 *)
   mutable ut_relationship : int; (* -1, 0, +1 *)
   mutable offset_hours : int; (* 0 - 59 *)
   mutable offset_minutes : int (* 0 - 59 *)}

let default_date () =
  {year = 0;
   month = 1;
   day = 1;
   hour = 0;
   minute = 0;
   second = 0;
   ut_relationship = 0;
   offset_hours = 0;
   offset_minutes = 0}

(* XMP date format is YYYY-MM-DDThh:mm:ssTZD *)
let make_xmp_date_from_components d =
  let tzd =
    if d.ut_relationship = 0 && d.offset_hours = 0 && d.offset_minutes = 0 then "Z" else
    (if d.ut_relationship >=0 then "+" else "-") ^
    Printf.sprintf "%02i" d.offset_hours ^
    ":" ^
    Printf.sprintf "%02i" d.offset_minutes
  in 
    Cpdfstrftime.strftime
      ~time:{Cpdfstrftime._tm_sec = d.second;
             Cpdfstrftime._tm_min = d.minute;
             Cpdfstrftime._tm_hour = d.hour;
             Cpdfstrftime._tm_mday = d.day;
             Cpdfstrftime._tm_mon = d.month - 1;
             Cpdfstrftime._tm_year = d.year - 1900;
             Cpdfstrftime._tm_wday = 0;
             Cpdfstrftime._tm_yday = 0;
             Cpdfstrftime._tm_isdst = false}
      "%Y-%m-%dT%H:%M:%S"
  ^
    tzd

let xmp_date date =
  let d = default_date () in
  try
    match explode date with
      'D'::':'::r ->
        begin match r with
          y1::y2::y3::y4::r ->
            d.year <- int_of_string (implode [y1; y2; y3; y4]);
            begin match r with
              m1::m2::r ->
                d.month <- int_of_string (implode [m1; m2]);
                begin match r with
                  d1::d2::r ->
                  d.day <- int_of_string (implode [d1; d2]);
                  begin match r with
                    h1::h2::r ->
                    d.hour <- int_of_string (implode [h1; h2]);
                    begin match r with
                      m1::m2::r ->
                      d.minute <- int_of_string (implode [m1; m2]);
                      begin match r with
                       s1::s2::r ->
                       d.second <- int_of_string (implode [s1; s2]);
                         begin match r with
                           o::r ->
                           d.ut_relationship <-
                             if o = '+' then 1 else
                             if o = '-' then -1 else
                             0;
                           begin match r with
                             h1::h2::'\''::r ->
                             d.offset_hours <- int_of_string (implode [h1; h2]);
                             begin match r with
                               m1::m2::_ ->
                               d.offset_minutes <- int_of_string (implode [m1; m2]);
                               raise Exit
                             | _ -> raise Exit
                             end
                           | _ -> raise Exit
                           end
                         | _ -> raise Exit
                         end
                      | _ -> raise Exit
                      end
                    | _ -> raise Exit
                    end
                  | _ -> raise Exit
                  end
                | _ -> raise Exit
                end
            | _ -> raise Exit  
            end
        | _ ->
          Pdfe.log (Printf.sprintf "xmp_date: Malformed date string (no year): %s\n" date);
          make_xmp_date_from_components d
        end
    | _ ->
        Pdfe.log (Printf.sprintf "xmp_date: Malformed date string (no prefix): %s\n" date);
        make_xmp_date_from_components d
  with
    Exit -> make_xmp_date_from_components d

let set_pdf_info ?(xmp_also=false) ?(xmp_just_set=false) (key, value, version) pdf =
  let infodict =
    match Pdf.lookup_direct pdf "/Info" pdf.Pdf.trailerdict with
    | Some d -> d
    | None -> Pdf.Dictionary []
  in
    let infodict' = Pdf.add_dict_entry infodict key value in
      let objnum = Pdf.addobj pdf infodict' in
        if not xmp_just_set then
          begin
            pdf.Pdf.trailerdict <-
              Pdf.add_dict_entry pdf.Pdf.trailerdict "/Info" (Pdf.Indirect objnum);
            if pdf.Pdf.major = 1 then pdf.Pdf.minor <- max pdf.Pdf.minor version
          end;
        if xmp_also then
          begin match get_metadata pdf with
            None -> pdf
          | Some xmldata ->
              let xmp_date = function Pdf.String s -> Pdf.String (xmp_date s) | _ -> failwith "xmp_date not a string" in
              let changes, value =
                match key with
                | "/Producer" -> [(adobe, "Producer")], value
                | "/Creator" -> [(adobe, "Creator"); (xmp, "CreatorTool"); (dc, "creator")], value
                | "/Author" -> [(adobe, "Author")], value
                | "/Title" -> [(adobe, "Title"); (dc, "title")], value
                | "/Subject" -> [(adobe, "Subject"); (dc, "subject")], value
                | "/Keywords" -> [(adobe, "Keywords")], value
                | "/CreationDate" -> [(adobe, "CreationDate"); (xmp, "CreateDate")], xmp_date value
                | "/ModDate" -> [(adobe, "ModDate"); (xmp, "ModifyDate")], xmp_date value
                | "/Trapped" -> [(adobe, "Trapped")], value
                | _ -> failwith "Unknown call to set_pdf_info"
              in
                set_metadata_from_bytes
                  true
                  (set_pdf_info_xml_many changes value xmldata pdf)
                  pdf
          end
       else
         pdf

let expand_date = function
  | "now" ->
      begin match Sys.getenv_opt "CPDF_REPRODUCIBLE_DATES" with
      | Some "true" -> Cpdfstrftime.strftime ~time:Cpdfstrftime.dummy "D:%Y%m%d%H%M%S" 
      | _ -> Cpdfstrftime.strftime "D:%Y%m%d%H%M%S"
      end
  | x -> x

(* Set metadata date *)
let set_metadata_date pdf date =
  match get_metadata pdf with
    None -> pdf
  | Some xmldata ->
      let changes= [(xmp, "MetadataDate")] in
      let value = match date with "now" -> xmp_date (expand_date "now") | x -> x in
        set_metadata_from_bytes
          true
          (set_pdf_info_xml_many changes (Pdf.String value) xmldata pdf)
          pdf


(* Copy an /ID from one file to another *)
let copy_id keepversion copyfrom copyto =
  match Pdf.lookup_direct copyfrom "/ID" copyfrom.Pdf.trailerdict with
  | None -> copyto (* error "Source PDF file has no /ID entry to copy from" *)
  | Some id ->
      copyto.Pdf.trailerdict <-
        Pdf.add_dict_entry copyto.Pdf.trailerdict "/ID" id;
      copyto.Pdf.minor <-
        if copyto.Pdf.major > 1 || keepversion then copyto.Pdf.minor else max copyto.Pdf.minor 1;
      copyto

let replacements pdf =
  let info = get_info_utf8 pdf in
    [("CREATEDATE", xmp_date (let i = info "/CreationDate" in if i = "" then expand_date "now" else i));
     ("MODDATE", xmp_date (let i = info "/ModDate" in if i = "" then expand_date "now" else i));
     ("PRODUCER", info "/Producer");
     ("CREATOR", info "/Creator");
     ("TITLE", info "/Title");
     ("SUBJECT", info "/Subject");
     ("AUTHOR", info "/Author");
     ("KEYWORDS", info "/Keywords");
     ("TRAPPED", info "/Trapped");
     ("METADATADATE", xmp_date (expand_date "now"))]

let create_metadata pdf =
  let xmp = ref xmp_template in
  iter
    (fun (s, r) -> xmp := string_replace_all s r !xmp)
    (replacements pdf);
  set_metadata_from_bytes false (bytes_of_string !xmp) pdf

(* Set viewer preferences *)
let set_viewer_preference (key, value, version) pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | Some catalog ->
      let viewer_preferences =
        match Pdf.lookup_direct pdf "/ViewerPreferences" catalog with
        | Some d -> d
        | None -> Pdf.Dictionary []
      in
        let viewer_preferences' =
          Pdf.add_dict_entry viewer_preferences key value
        in
          let catalog' =
            Pdf.add_dict_entry catalog "/ViewerPreferences" viewer_preferences'
          in
            let catalognum = Pdf.addobj pdf catalog' in
              let trailerdict' =
                Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect catalognum)
              in
                {pdf with
                  Pdf.minor = max pdf.Pdf.minor version;
                  Pdf.root = catalognum;
                  Pdf.trailerdict = trailerdict'}
  | None -> error "bad root"

(* Set page layout *)
let set_page_layout pdf s =
  match s with
  | "SinglePage" | "OneColumn" | "TwoColumnLeft"
  | "TwoColumnRight" | "TwoPageLeft" | "TwoPageRight" ->
      begin match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
      | Some catalog ->
          let catalog' =
            Pdf.add_dict_entry catalog "/PageLayout" (Pdf.Name ("/" ^ s))
          in
            let catalognum = Pdf.addobj pdf catalog' in
              let trailerdict' =
                Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect catalognum)
              in
                {pdf with
                  Pdf.root = catalognum;
                  Pdf.trailerdict = trailerdict'}
      | None -> error "bad root"
      end
  | _ -> error "Unknown page layout"

(* Set page mode *)
let set_page_mode pdf s =
  match s with
  | "UseNone" | "UseOutlines" | "UseThumbs"
  | "FullScreen" | "UseOC" | "UseAttachments" ->
      begin match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
      | Some catalog ->
          let catalog' =
            Pdf.add_dict_entry catalog "/PageMode" (Pdf.Name ("/" ^ s))
          in
            let catalognum = Pdf.addobj pdf catalog' in
              let trailerdict' =
                Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect catalognum)
              in
                {pdf with
                  Pdf.root = catalognum;
                  Pdf.trailerdict = trailerdict'}
      | None -> error "bad root"
      end
  | _ -> error "Unknown page mode"

let set_non_full_screen_page_mode pdf s =
  match s with
  | "UseNone" | "UseOutlines" | "UseThumbs"
  | "UseOC" | "UseAttachments" ->
      begin match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
      | Some catalog ->
          let viewerprefs =
            match Pdf.lookup_direct pdf "/ViewerPreferences" catalog with
            | Some d -> d
            | None -> Pdf.Dictionary []
          in
            let viewerprefsnum =
              Pdf.addobj pdf (Pdf.add_dict_entry viewerprefs "/NonFullScreenPageMode" (Pdf.Name ("/" ^ s)))
            in
            let catalog' = Pdf.add_dict_entry catalog "/ViewerPreferences" (Pdf.Indirect viewerprefsnum)in
            let catalognum = Pdf.addobj pdf catalog' in
              let trailerdict' =
                Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect catalognum)
              in
                {pdf with
                  Pdf.root = catalognum;
                  Pdf.trailerdict = trailerdict'}
      | None -> error "bad root"
      end
  | _ -> error "Unknown non full screen page mode"

(* Set open action *)
let set_open_action pdf ?dest fit pagenumber =
  if pagenumber > Pdfpage.endpage pdf || pagenumber < 0 then
    raise (error "set_open_action: invalid page number")
  else
    let pageobjectnumber = select pagenumber (Pdf.page_reference_numbers pdf) in
      let destination =
        match dest with
        | Some s ->
            begin match Pdfread.parse_single_object s with
            | Pdf.Array (Pdf.Integer pagenum::more) ->
                begin try
                  let pageobjectnumber = select pagenum (Pdf.page_reference_numbers pdf) in
                    Pdf.Array (Pdf.Indirect pageobjectnumber::more)
                with
                  _ -> raise (Pdf.PDFError "bad page number in custom destination")
                end
            | _ | exception _ ->
              raise (Pdf.PDFError "Bad destination syntax")
            end
        | None ->
            if fit then
              Pdf.Array [Pdf.Indirect pageobjectnumber; Pdf.Name "/Fit"]
            else
              Pdf.Array [Pdf.Indirect pageobjectnumber; Pdf.Name "/XYZ"; Pdf.Null; Pdf.Null; Pdf.Null]
      in
        let open_action =
          Pdf.Dictionary [("/D", destination); ("/S", Pdf.Name "/GoTo")]
        in
          match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
          | Some catalog ->
              let catalog' =
                Pdf.add_dict_entry catalog "/OpenAction" open_action
              in
                let catalognum = Pdf.addobj pdf catalog' in
                  let trailerdict' =
                    Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect catalognum)
                  in
                    {pdf with Pdf.root = catalognum; Pdf.trailerdict = trailerdict'}
          | None -> error "bad root"

let set_version v pdf =
  pdf.Pdf.minor <- v
