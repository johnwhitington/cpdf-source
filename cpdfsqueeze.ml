open Pdfutil
open Pdfio

(*
let report_pdf_size pdf =
  Pdf.remove_unreferenced pdf;
  Pdfwrite.pdf_to_file_options ~preserve_objstm:false ~generate_objstm:false
  ~compress_objstm:false None false pdf "temp.pdf";
  let fh = open_in_bin "temp.pdf" in
    Printf.printf "Size %i bytes\n" (in_channel_length fh);
    flush stdout;
    close_in fh
*)

type dedup_stats =
  {mutable rounds : int;
   mutable hash_buckets : int;
   mutable candidate_objects : int;
   mutable equality_checks : int;
   mutable stream_header_rejects : int;
   mutable stream_materializations : int;
   mutable removed_objects : int}

let empty_dedup_stats () =
  {rounds = 0;
   hash_buckets = 0;
   candidate_objects = 0;
   equality_checks = 0;
   stream_header_rejects = 0;
   stream_materializations = 0;
   removed_objects = 0}

let add_dedup_stats total round =
  total.rounds <- total.rounds + round.rounds;
  total.hash_buckets <- total.hash_buckets + round.hash_buckets;
  total.candidate_objects <- total.candidate_objects + round.candidate_objects;
  total.equality_checks <- total.equality_checks + round.equality_checks;
  total.stream_header_rejects <- total.stream_header_rejects + round.stream_header_rejects;
  total.stream_materializations <- total.stream_materializations + round.stream_materializations;
  total.removed_objects <- total.removed_objects + round.removed_objects

let string_of_dedup_stats stats =
  Printf.sprintf
    "%i rounds, %i hash buckets, %i candidate objects, %i equality checks, %i stream header rejects, %i stream materializations, %i objects removed"
    stats.rounds
    stats.hash_buckets
    stats.candidate_objects
    stats.equality_checks
    stats.stream_header_rejects
    stats.stream_materializations
    stats.removed_objects

type content_stream_stats =
  {pages_rewritten : int;
   xobjects_rewritten : int}

let string_of_content_stream_stats stats =
  Printf.sprintf
    "%i pages rewritten, %i form xobjects rewritten"
    stats.pages_rewritten
    stats.xobjects_rewritten

let stream_length = function
  | Pdf.Got bytes -> bytes_size bytes
  | Pdf.ToGet toget -> Pdf.length_of_toget toget

let rec canonicalize_object_for_squeeze = function
  | Pdf.Array values ->
      Pdf.Array (map canonicalize_object_for_squeeze values)
  | Pdf.Dictionary dict ->
      Pdf.Dictionary
        (sort
           (fun (ka, _) (kb, _) -> compare ka kb)
           (map (fun (k, v) -> k, canonicalize_object_for_squeeze v) dict))
  | obj -> obj

let normalized_stream_dict_for_squeeze dict length =
  canonicalize_object_for_squeeze
    (Pdf.add_dict_entry
       (Pdf.remove_dict_entry dict "/Length")
       "/Length"
       (Pdf.Integer length))

let squeeze_hash_for_object = function
  | Pdf.Stream {contents = (dict, stream)} ->
      Hashtbl.hash_param 256 256
        (normalized_stream_dict_for_squeeze dict (stream_length stream))
  | obj ->
      Hashtbl.hash_param 256 256 (canonicalize_object_for_squeeze obj)

let bytes_equal left right =
  let left_length = bytes_size left in
    left_length = bytes_size right &&
    if left_length <= Sys.max_string_length then
      string_of_bytes left = string_of_bytes right
    else
    let rec loop pos =
      pos = left_length ||
      (bget_unsafe left pos = bget_unsafe right pos && loop (pos + 1))
    in
      loop 0

(* Sample across the body before paying for a full digest. Large streams with
   the same headers commonly differ near the end; reading every byte merely to
   separate those candidates makes ordinary-file deduplication slower. *)
let hash_bytes_for_squeeze data =
  let length = bytes_size data in
  let sample_length = min 256 length in
  let sample = Bytes.create sample_length in
    for pos = 0 to sample_length - 1 do
      let offset =
        if length <= 256 then pos
        else (pos / 32) * ((length - 32) / 7) + pos mod 32
      in
        Bytes.unsafe_set sample pos (Char.chr (bget_unsafe data offset))
    done;
    Hashtbl.hash sample

let stream_data_for_squeeze stats = function
  | Pdf.Stream {contents = (_, Pdf.Got data)} -> data
  | Pdf.Stream _ as stream ->
      stats.stream_materializations <- stats.stream_materializations + 1;
      Pdf.getstream stream;
      begin match stream with
      | Pdf.Stream {contents = (_, Pdf.Got data)} -> data
      | _ -> failwith "stream_data_for_squeeze"
      end
  | _ -> failwith "stream_data_for_squeeze"

let squeeze_hash_for_pair stats stream_hashes (objnum, obj) =
  match obj with
  | Pdf.Stream {contents = (dict, stream)} ->
      let header_hash =
        Hashtbl.hash_param 256 256
          (normalized_stream_dict_for_squeeze dict (stream_length stream))
      in
      let body_hash =
        try Hashtbl.find stream_hashes objnum with
        | Not_found ->
            let body_hash =
              hash_bytes_for_squeeze (stream_data_for_squeeze stats obj)
            in
              Hashtbl.add stream_hashes objnum body_hash;
              body_hash
      in
        Hashtbl.hash (header_hash, body_hash)
  | _ ->
      Hashtbl.hash_param 256 256 (canonicalize_object_for_squeeze obj)

let rec find_inherited_entry pdf entry obj =
  match Pdf.lookup_immediate entry obj with
  | Some value -> Some value
  | None ->
      match Pdf.lookup_direct pdf "/Parent" obj with
      | Some (Pdf.Dictionary parent) ->
          find_inherited_entry pdf entry (Pdf.Dictionary parent)
      | _ -> None

let rec merge_resource_values pdf preferred fallback =
  match Pdf.direct pdf preferred, Pdf.direct pdf fallback with
  | Pdf.Dictionary preferred_dict, Pdf.Dictionary fallback_dict ->
      fold_left
        (fun merged (key, fallback_value) ->
           match lookup key preferred_dict with
           | Some preferred_value ->
               begin match
                 Pdf.direct pdf preferred_value,
                 Pdf.direct pdf fallback_value
               with
               | Pdf.Dictionary _, Pdf.Dictionary _ ->
                   Pdf.add_dict_entry
                     merged
                     key
                     (merge_resource_values pdf preferred_value fallback_value)
               | _ -> merged
               end
           | None ->
               Pdf.add_dict_entry merged key fallback_value)
        (Pdf.Dictionary preferred_dict)
        fallback_dict
  | _ -> preferred

let effective_resources pdf obj inherited_resources =
  match Pdf.lookup_immediate "/Resources" obj, inherited_resources with
  | Some resources, Some inherited when pdf.Pdf.major = 1 && pdf.Pdf.minor < 2 ->
      merge_resource_values pdf resources inherited
  | Some resources, Some _ -> resources
  | Some resources, None -> resources
  | None, Some inherited -> inherited
  | None, None ->
      match find_inherited_entry pdf "/Resources" obj with
      | Some resources -> resources
      | None -> Pdf.Dictionary []

let time_operation ?(details = fun _ -> "") log label f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  let detail = details result in
    log
      (Printf.sprintf
         "%s took %.3fs%s\n"
         label
         elapsed
         (if detail = "" then "" else " (" ^ detail ^ ")"));
    result

let list_has_multiple_elements = function
  | _::_::_ -> true
  | _ -> false

let old_style_filter = function
  | Some (Pdf.Name ("/ASCIIHexDecode" | "/ASCII85Decode" | "/LZWDecode" | "/RunLengthDecode")) -> true
  | Some (Pdf.Array (Pdf.Name ("/ASCIIHexDecode" | "/ASCII85Decode" | "/LZWDecode" | "/RunLengthDecode")::_)) -> true
  | _ -> false

let should_recompress_stream pdf dict =
  match Pdf.lookup_direct pdf "/Filter" dict, Pdf.lookup_direct pdf "/Type" dict with
  | _, Some (Pdf.Name "/Metadata") -> false
  | None, _
  | Some (Pdf.Array []), _ -> true
  | filter, _ -> old_style_filter filter

let stream_recompression_changed pdf original_filter original_length stream =
  match stream with
  | Pdf.Stream {contents = (newdict, newstream)} ->
      let new_filter = Pdf.lookup_direct pdf "/Filter" newdict in
      let new_length = stream_length newstream in
        if old_style_filter original_filter
           || compare original_filter new_filter <> 0
           || original_length <> new_length
        then 1 else 0
  | _ -> assert false

let try_recompress_stream pdf dict stream =
  let original_filter = Pdf.lookup_direct pdf "/Filter" dict in
  let original_length =
    match stream with
    | Pdf.Stream {contents = (_, stream)} -> stream_length stream
    | _ -> assert false
  in
    begin
      try Pdfcodec.decode_pdfstream_until_unknown pdf stream with
      | _ -> Pdfe.log "Warning: Skipping re-encoding of a stream\n"
    end;
    Pdfcodec.encode_pdfstream ~only_if_smaller:true pdf Pdfcodec.Flate stream;
    stream_recompression_changed pdf original_filter original_length stream

(* Recompress anything which isn't compressed (or compressed with old-fashioned
   mechanisms), unless it's metadata. *)
let recompress_stream pdf = function
  (* If there is no compression, or bad compression with /FlateDecode *)
  | Pdf.Stream {contents = (dict, _)} as stream ->
      if should_recompress_stream pdf dict then try_recompress_stream pdf dict stream
      else 0
  | _ -> assert false

let recompress_pdf_count pdf =
  let rewritten = ref 0 in
    if not (Pdfcrypt.is_encrypted pdf) then
      Pdf.iter_stream (fun stream -> rewritten := !rewritten + recompress_stream pdf stream) pdf;
    !rewritten

let recompress_pdf pdf =
  ignore (recompress_pdf_count pdf);
  pdf

let decompress_pdf pdf =
  if not (Pdfcrypt.is_encrypted pdf) then
    (Pdf.iter_stream (Pdfcodec.decode_pdfstream_until_unknown pdf) pdf);
    pdf

(* Decoding replaces the stream reference. A fresh reference lets the parser
   decode without changing the original compressed stream, even on failure. *)
let copy_stream pdf stream =
  match Pdf.direct pdf stream with
  | Pdf.Stream contents -> Pdf.Stream (ref !contents)
  | _ -> raise Not_found

let parse_content_streams pdf resources streams =
  Pdfops.parse_operators pdf resources (map (copy_stream pdf) streams)

(* Include dictionary overhead, especially /Filter, when comparing tiny streams.
   The common object/stream delimiters cancel for a single-stream replacement;
   omitting them for multiple originals makes the check conservative. *)
let serialized_stream_size = function
  | Pdf.Stream {contents = (dict, stream)} ->
      let length = stream_length stream in
        length + String.length
          (Pdfwrite.string_of_pdf (normalized_stream_dict_for_squeeze dict length))
  | _ -> raise Not_found

let content_streams_size pdf streams =
  sum (map (fun stream -> serialized_stream_size (Pdf.direct pdf stream)) streams)

let objects_equal_for_squeeze pdf stats (_, x) (_, y) =
  match x, y with
  | Pdf.Stream {contents = (xdict, xstream)}, Pdf.Stream {contents = (ydict, ystream)} ->
      let xlength = stream_length xstream
      and ylength = stream_length ystream in
        if
          xlength <> ylength ||
          compare
            (normalized_stream_dict_for_squeeze xdict xlength)
            (normalized_stream_dict_for_squeeze ydict ylength) <> 0
        then
        (stats.stream_header_rejects <- stats.stream_header_rejects + 1; false)
      else
        begin
          stats.equality_checks <- stats.equality_checks + 1;
          bytes_equal
            (stream_data_for_squeeze stats x)
            (stream_data_for_squeeze stats y)
        end
  | _ ->
      stats.equality_checks <- stats.equality_checks + 1;
      compare
        (canonicalize_object_for_squeeze x)
        (canonicalize_object_for_squeeze y) = 0

let remove_unique_objects stats pairs =
  let buckets = Hashtbl.create 2048 in
  let stream_hashes = Hashtbl.create 2048 in
  let group_by_hash hash_for_pair bucket =
    let refined = Hashtbl.create 16 in
      iter
        (fun pair ->
           let hash = hash_for_pair pair in
           let existing =
             try Hashtbl.find refined hash with Not_found -> []
           in
             Hashtbl.replace refined hash (pair::existing))
        bucket;
      Hashtbl.fold
        (fun _ candidates acc ->
           if list_has_multiple_elements candidates then candidates::acc else acc)
        refined []
  in
  let refine_stream_bucket bucket =
    let sampled = group_by_hash (squeeze_hash_for_pair stats stream_hashes) bucket in
      flatten
        (map
           (fun candidates ->
              if length candidates < 8 then [candidates] else
                (* A full C digest bounds the equality work when samples collide.
                   Digest collisions still go through actual byte comparisons. *)
                group_by_hash
                  (fun (_, obj) ->
                     Digest.string (string_of_bytes (stream_data_for_squeeze stats obj)))
                  candidates)
           sampled)

  in
    iter
      (fun ((_, obj) as pair) ->
         let hash = squeeze_hash_for_object obj in
         let existing =
           try Hashtbl.find buckets hash with
           | Not_found -> []
         in
           Hashtbl.replace buckets hash (pair::existing))
      pairs;
    Hashtbl.fold
      (fun _ bucket acc ->
         if list_has_multiple_elements bucket then
           match bucket with
           (* Comparing a few candidates directly avoids reading and hashing
              every body just to reject one or two streams. *)
           | (_, Pdf.Stream _)::_ when length bucket >= 8 ->
               refine_stream_bucket bucket @ acc
           | _ -> bucket::acc
         else
           acc)
      buckets
      []

let duplicate_object_groups pdf stats pairs =
  let rec add_to_groups pair = function
    | [] -> [[pair]]
    | (leader::_ as group)::rest ->
        if objects_equal_for_squeeze pdf stats pair leader then
          (pair::group)::rest
        else
          group::add_to_groups pair rest
    | []::rest -> add_to_groups pair rest
  in
    fold_left (fun groups pair -> add_to_groups pair groups) [] pairs

let duplicate_groups_for_squeeze pdf stats pairs =
  let buckets = remove_unique_objects stats pairs in
    flatten
      (map
         (fun bucket ->
            stats.hash_buckets <- stats.hash_buckets + 1;
            stats.candidate_objects <- stats.candidate_objects + length bucket;
            keep list_has_multiple_elements (duplicate_object_groups pdf stats bucket))
         buckets)

let is_shareable_duplicate_group pdf = function
  | [] -> assert false
  | (_, h)::_ ->
      begin match Pdf.lookup_direct pdf "/Type" h with
      | Some (Pdf.Name "/Page") -> false
      | _ ->
          match Pdf.lookup_direct pdf "/Subtype" h with
          | Some (Pdf.Name (  "/Text" | "/Link" | "/FreeText"
                            | "/Line" | "/Square" | "/Circle"
                            | "/Polygon" | "/PolyLine" | "/Highlight"
                            | "/Underline" | "/Squiggly" | "/StrikeOut"
                            | "/Caret" | "/Stamp" | "/Ink"
                            | "/Popup" | "/FileAttachment" | "/Sound"
                            | "/Movie" | "/Screen" |  "/Widget"
                            | "/PrinterMark" | "/TrapNet" | "/3D"
                            | "/Redact" | "/Projection" | "/RichMedia")) -> false
          | _ -> true
      end

let removed_objects_in_groups groups =
  sum
    (map
       (function [] | [_] -> 0 | l -> length l - 1)
       groups)

let apply_squeeze_groups pdf groups =
  let pdfr = ref pdf in
  let object_stream_ids = Hashtbl.copy pdf.Pdf.objects.Pdf.object_stream_ids in
  let changetable = Hashtbl.create 512 in
    iter
      (function [] -> assert false | (h, _)::t ->
         iter (fun (e, _) -> Hashtbl.add changetable e h; Pdf.removeobj pdf e) t)
      groups;
    pdfr := Pdf.renumber ~preserve_order:true changetable !pdfr;
    pdf.Pdf.root <- !pdfr.Pdf.root;
    pdf.Pdf.objects <- !pdfr.Pdf.objects;
    (* Renumbering maps several deleted objects to one survivor. Their old
       object-stream hints must not give that survivor several memberships:
       the writer can then emit inconsistent xref entries and lose resources.
       Keep only the original memberships of objects which still exist. *)
    let surviving = hashset_of_list (Pdf.objnumbers pdf) in
      Hashtbl.filter_map_inplace
        (fun objnum stream_id ->
           if Hashtbl.mem surviving objnum then Some stream_id else None)
        object_stream_ids;
      pdf.Pdf.objects.Pdf.object_stream_ids <- object_stream_ids;
    pdf.Pdf.trailerdict <- !pdfr.Pdf.trailerdict

let squeeze_pairs pdf stats pairs =
  let groups =
    keep
      (is_shareable_duplicate_group pdf)
      (duplicate_groups_for_squeeze pdf stats pairs)
  in
    stats.rounds <- stats.rounds + 1;
    let removed_objects = removed_objects_in_groups groups in
      stats.removed_objects <- stats.removed_objects + removed_objects;
      if removed_objects > 0 then
        apply_squeeze_groups pdf groups

let really_squeeze pdf =
  let stats = empty_dedup_stats () in
  let objs = ref [] in
    Pdf.objiter (fun objnum _ -> objs := (objnum, Pdf.lookup_obj pdf objnum) :: !objs) pdf;
    squeeze_pairs pdf stats !objs;
    stats

(* Squeeze the form xobject at objnum.

Old PDFs (< v1.2) may need resources from the page or its ancestors in addition
to the form's own resources. Newer forms use their own resource dictionaries.
Parsing uses private stream references so a skipped rewrite keeps its encoding. *)
let xobjects_done = Hashtbl.create 256

let squeeze_form_xobject_children recurse pdf resources =
  match Pdf.lookup_direct pdf "/XObject" resources with
  | Some (Pdf.Dictionary d) ->
      fold_left
        (fun count -> function
           | _, Pdf.Indirect i ->
               count + recurse pdf (Some resources) i
           | _ -> count)
        0
        d
  | _ -> 0

let rewrite_form_xobject_if_smaller pdf obj data rewritten_children =
  match obj with
  | Pdf.Stream ({contents = (dict, _)} as original) ->
      let dict =
        Pdf.add_dict_entry
          (Pdf.remove_dict_entry (Pdf.remove_dict_entry dict "/Filter") "/DecodeParms")
          "/Length" (Pdf.Integer (bytes_size data))
      in
      let replacement = Pdf.Stream (ref (dict, Pdf.Got data)) in
        ignore (recompress_stream pdf replacement);
        if serialized_stream_size replacement <= serialized_stream_size obj then
          begin
            begin match replacement with
            | Pdf.Stream contents -> original := !contents
            | _ -> assert false
            end;
            rewritten_children + 1
          end
        else rewritten_children
  | _ -> failwith "squeeze_form_xobject"

let rec squeeze_form_xobject f pdf inherited_resources objnum =
  if Hashtbl.mem xobjects_done objnum then 0 else
    begin
      Hashtbl.replace xobjects_done objnum ();
      let obj = Pdf.lookup_obj pdf objnum in
      let resources = effective_resources pdf obj inherited_resources in
      let rewritten_children =
        squeeze_form_xobject_children (squeeze_form_xobject f) pdf resources
      in
        match Pdf.lookup_direct pdf "/Subtype" obj with
        | Some (Pdf.Name "/Form") ->
              let mediabox =
                match Pdf.lookup_direct pdf "/BBox" obj with
                | Some x -> x
                | None -> Pdf.Array [Pdf.Integer 0; Pdf.Integer 0; Pdf.Integer 612; Pdf.Integer 792]
              in
              begin match
                Pdfops.stream_of_ops
                  (f pdf mediabox resources (parse_content_streams pdf resources [Pdf.Indirect objnum]))
              with
              | Pdf.Stream {contents = (_, Pdf.Got data)} ->
                  rewrite_form_xobject_if_smaller pdf obj data rewritten_children
              | _ -> failwith "squeeze_form_xobject"
              end
        | _ -> rewritten_children
    end

(* For a list of indirects representing content streams, make sure that none of
them are duplicated in the PDF. This indicates sharing, which parsing and
rewriting the streams might destroy, thus making the file bigger. *)
let no_duplicates content_stream_numbers stream_numbers =
  List.for_all
    (fun n ->
       match tryfind content_stream_numbers n with
       | Some count -> count < 2
       | None -> true)
    stream_numbers

let page_content_streams pdf dict =
  match lookup "/Contents" dict with
  | Some contents ->
      begin match Pdf.direct pdf contents with
      | Pdf.Array streams -> streams
      | Pdf.Stream _ -> [contents]
      | _ -> raise Not_found
      end
  | None -> raise Not_found

(* Count the streams, rather than an indirect array containing the streams. *)
let content_streams_of_page pdf refnum =
  match Pdf.lookup_obj pdf refnum with
  | Pdf.Dictionary dict ->
      begin try
        option_map (function Pdf.Indirect i -> Some i | _ -> None)
          (page_content_streams pdf dict)
      with Not_found -> [] end
  | _ -> []

let content_stream_reference_counts numbers =
  let counts = Hashtbl.create 1024 in
    iter
      (fun n ->
         let count =
           match tryfind counts n with
           | Some count -> count + 1
           | None -> 1
         in
           Hashtbl.replace counts n count)
      numbers;
    counts

let squeeze_progress_reporter total_pages =
  let report_interval =
    if total_pages >= 1000 then 250
    else if total_pages >= 250 then 100
    else if total_pages >= 50 then 25
    else 10
  in
    fun pagenum ->
      if
        !Cpdfutil.progress &&
        (pagenum = 1 || pagenum = total_pages || pagenum mod report_interval = 0)
      then
        Printf.eprintf "%i/%i.%!" pagenum total_pages

let squeeze_page_xobjects f pdf xobjects_rewritten resources =
  match Pdf.lookup_direct pdf "/XObject" resources with
  | Some (Pdf.Dictionary xobjs) ->
      iter
        (function
           | _, Pdf.Indirect i ->
               xobjects_rewritten :=
                 !xobjects_rewritten + squeeze_form_xobject f pdf (Some resources) i
           | _ -> failwith "squeeze_xobject")
        xobjs
  | _ -> ()

let squeeze_page_content_streams
  f pdf content_stream_counts pages_rewritten xobjects_rewritten
  objnum
 =
  match Pdf.lookup_obj pdf objnum with
  | Pdf.Dictionary dict as d
      when Pdf.lookup_direct pdf "/Type" d = Some (Pdf.Name "/Page") ->
        let resources = effective_resources pdf d None in
        let mediabox =
          match find_inherited_entry pdf "/MediaBox" d with
          | Some x -> x
          | None -> Pdf.Array [Pdf.Integer 0; Pdf.Integer 0; Pdf.Integer 612; Pdf.Integer 792]
        in
          begin try
            let content_streams = page_content_streams pdf dict in
            let content_stream_numbers =
              map (function Pdf.Indirect i -> i | _ -> assert false) content_streams
            in
              if no_duplicates content_stream_counts content_stream_numbers then
                let original_size =
                  content_streams_size pdf content_streams
                in
                let newstream =
                  Pdfops.stream_of_ops
                    (f pdf mediabox resources (parse_content_streams pdf resources content_streams))
                in
                  ignore (recompress_stream pdf newstream);
                  if serialized_stream_size newstream <= original_size then
                    begin
                      incr pages_rewritten;
                      let newstream_objnum = Pdf.addobj pdf newstream in
                      let newdict =
                        Pdf.add_dict_entry
                          d "/Contents" (Pdf.Indirect newstream_objnum)
                      in
                        Pdf.addobj_given_num pdf (objnum, newdict)
                    end;
              squeeze_page_xobjects f pdf xobjects_rewritten resources
          with
          | Not_found -> ()
          end
  | _ -> ()

(* For each object in the PDF marked with /Type /Page, for each /Contents
indirect reference or array of such, decode and recode that content stream. *)
let process_all_content_streams f pdf =
  let page_reference_numbers = Pdf.page_reference_numbers pdf in
  let total_pages = length page_reference_numbers in
  let report_progress = squeeze_progress_reporter total_pages in
    let content_stream_counts =
      content_stream_reference_counts
        (flatten (map (content_streams_of_page pdf) page_reference_numbers))
    in
      let pages_rewritten = ref 0 in
      let xobjects_rewritten = ref 0 in
        Hashtbl.clear xobjects_done;
        Cpdfutil.progress_line_no_end
          (Printf.sprintf
             "Squeezing page data and xobjects (%i pages): "
             total_pages);
        iter2
          (fun pagenum objnum ->
             report_progress pagenum;
             squeeze_page_content_streams
               f pdf
               content_stream_counts
               pages_rewritten
               xobjects_rewritten
               objnum)
          (indx page_reference_numbers)
          page_reference_numbers;
        Cpdfutil.progress_done ();
        {pages_rewritten = !pages_rewritten;
         xobjects_rewritten = !xobjects_rewritten}

(* Run object deduplication enough times for the number of objects to stabilize. *)
let squeeze_to_fixed_point ?(log = fun _ -> ()) pdf =
  let stats = empty_dedup_stats () in
  let keep_going = ref true in
    while !keep_going do
      let before = Pdf.objcard pdf in
      let round = really_squeeze pdf in
      let after = Pdf.objcard pdf in
        add_dedup_stats stats round;
        if round.removed_objects > 0 then
          log
            (Printf.sprintf
               "Squeeze round %i removed %i objects (%i -> %i)\n"
               stats.rounds
               round.removed_objects
               before
               after);
        keep_going := after < before
    done;
    stats

let squeeze_initial_dedup log pdf =
  ignore
    (time_operation
       ~details:string_of_dedup_stats
       log
       "Initial deduplication"
       (fun () -> squeeze_to_fixed_point ~log pdf))

let squeeze_page_data_phase log f pdf =
  let pagedata_stats =
    time_operation
      ~details:string_of_content_stream_stats
      log
      "Squeezing page data and xobjects"
      (fun () -> process_all_content_streams f pdf)
  in
    pagedata_stats.pages_rewritten > 0 || pagedata_stats.xobjects_rewritten > 0

let squeeze ?logto ~reprocess ~pagedata pdf =
  let log x =
    match logto with
    | None -> Cpdfutil.progress_line (String.trim x)
    | Some "nolog" -> ()
    | Some s ->
        let fh = open_out_gen [Open_wronly; Open_creat] 0o666 s in
          seek_out fh (out_channel_length fh);
          output_string fh x;
          close_out fh
  in
    try
      log (Printf.sprintf "Beginning squeeze: %i objects\n" (Pdf.objcard pdf));
      squeeze_initial_dedup log pdf;
      (* Compress originals once before measuring rewrites; accepted replacements
         are already compressed, so neither needs a second encoding pass. *)
      let recompressed_streams =
        time_operation ~details:string_of_int log "Recompressing document"
          (fun () -> recompress_pdf_count pdf)
      in
      let reprocessed =
        reprocess && squeeze_page_data_phase log
          (fun pdf mediabox resources ops ->
             Cpdfcontent.compress ~pdf ~mediabox:(Pdf.parse_rectangle pdf mediabox) ~resources ~ops)
          pdf
      in
      let rewritten =
        pagedata && squeeze_page_data_phase log (fun _ _ _ ops -> ops) pdf
      in
        if reprocessed || rewritten then
          time_operation log "Removing unreferenced objects after page data rewrite"
            (fun () -> Pdf.remove_unreferenced pdf);
        if reprocessed || rewritten || recompressed_streams > 0 then
          ignore
            (time_operation ~details:string_of_dedup_stats log "Final deduplication"
               (fun () -> squeeze_to_fixed_point ~log pdf));
      log (Printf.sprintf "Finished squeeze\n")
    with
    | e ->
        raise
          (Pdf.PDFError
             (Printf.sprintf
                "Squeeze failed. No output written.\n Proximate error was:\n %s"
                (Printexc.to_string e)))
