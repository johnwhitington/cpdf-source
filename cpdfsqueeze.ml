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
   xobjects_rewritten : int;
   rewritten_page_streams : int list}

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
      Hashtbl.hash
        (Pdfwrite.string_of_pdf
           (normalized_stream_dict_for_squeeze dict (stream_length stream)))
  | obj ->
      Hashtbl.hash
        (Pdfwrite.string_of_pdf (canonicalize_object_for_squeeze obj))

let bytes_equal left right =
  let left_length = bytes_size left in
    left_length = bytes_size right &&
    let rec loop pos =
      pos = left_length ||
      (bget_unsafe left pos = bget_unsafe right pos && loop (pos + 1))
    in
      loop 0

let hash_bytes_for_squeeze data =
  let hash = ref 2166136261 in
    for pos = 0 to bytes_size data - 1 do
      hash := ((!hash lxor bget_unsafe data pos) * 16777619) land max_int
    done;
    !hash

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
        Hashtbl.hash
          (Pdfwrite.string_of_pdf
             (normalized_stream_dict_for_squeeze dict (stream_length stream)))
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
      Hashtbl.hash
        (Pdfwrite.string_of_pdf (canonicalize_object_for_squeeze obj))

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
  | Some resources, Some inherited ->
      merge_resource_values pdf resources inherited
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

let copy_stream_for_measurement = function
  | Pdf.Stream _ as stream ->
      Pdf.getstream stream;
      begin match stream with
      | Pdf.Stream {contents = (dict, Pdf.Got data)} ->
          Pdf.Stream {contents = (dict, Pdf.Got (copybytes data))}
      | _ -> assert false
      end
  | _ -> assert false

let recompressed_stream_size pdf stream =
  let copy = copy_stream_for_measurement stream in
    ignore (recompress_stream pdf copy);
    match copy with
    | Pdf.Stream {contents = (_, stream)} -> stream_length stream
    | _ -> assert false

let content_streams_size_after_recompression pdf objnums =
  sum
    (map
       (fun objnum -> recompressed_stream_size pdf (Pdf.lookup_obj pdf objnum))
       objnums)

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
  let refine_stream_bucket bucket =
    let refined = Hashtbl.create 16 in
      iter
        (fun ((_, obj) as pair) ->
           let hash = squeeze_hash_for_pair stats stream_hashes pair in
           let existing =
             try Hashtbl.find refined hash with
             | Not_found -> []
           in
             Hashtbl.replace refined hash (pair::existing))
        bucket;
      Hashtbl.fold
        (fun _ refined_bucket acc ->
           if list_has_multiple_elements refined_bucket then refined_bucket::acc else acc)
        refined
        []
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
           | (_, Pdf.Stream _)::_ -> refine_stream_bucket bucket @ acc
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
  let changetable = Hashtbl.create 512 in
    iter
      (function [] -> assert false | (h, _)::t ->
         iter (fun (e, _) -> Hashtbl.add changetable e h; Pdf.removeobj pdf e) t)
      groups;
    pdfr := Pdf.renumber ~preserve_order:true changetable !pdfr;
    pdf.Pdf.root <- !pdfr.Pdf.root;
    pdf.Pdf.objects <- !pdfr.Pdf.objects;
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

let unique_existing_object_numbers pdf objnums =
  let seen = Hashtbl.create 1024 in
    fold_left
      (fun existing objnum ->
         if Hashtbl.mem seen objnum then existing else
           begin
             Hashtbl.add seen objnum ();
             try
               ignore (Pdf.lookup_obj pdf objnum);
               objnum::existing
             with
             | Not_found -> existing
           end)
      []
      objnums

let squeeze_rewritten_page_data pdf objnums =
  let stats = empty_dedup_stats () in
  let objnums = unique_existing_object_numbers pdf objnums in
    if objnums <> [] then
      squeeze_pairs
        pdf
        stats
        (map (fun objnum -> objnum, Pdf.lookup_obj pdf objnum) objnums);
    stats

(* Squeeze the form xobject at objnum.

For old PDFs (< v1.2) any resources from the page (or its ancestors in
the page tree!) are also needed - we must merge them with the ones from the
xobject itself. However, it it safe for now -- in the unlikely event that the
resources actually need to be available, the parse will fail, the squeeze of
this object will fail, and we bail out. *)
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
  let replacement =
    Pdf.Stream
      {contents =
         (Pdf.Dictionary [("/Length", Pdf.Integer (bytes_size data))],
          Pdf.Got data)}
  in
    if recompressed_stream_size pdf replacement <= recompressed_stream_size pdf obj then
      begin
        begin match obj with
        | Pdf.Stream ({contents = (d, _)} as str) ->
            str :=
              (Pdf.add_dict_entry d "/Length" (Pdf.Integer (bytes_size data)),
               Pdf.Got data)
        | _ -> failwith "squeeze_form_xobject"
        end;
        rewritten_children + 1
      end
    else
      rewritten_children

let rec squeeze_form_xobject pdf inherited_resources objnum =
  if Hashtbl.mem xobjects_done objnum then 0 else
    begin
      Hashtbl.replace xobjects_done objnum ();
      let obj = Pdf.lookup_obj pdf objnum in
      let resources = effective_resources pdf obj inherited_resources in
      let rewritten_children =
        squeeze_form_xobject_children squeeze_form_xobject pdf resources
      in
        match Pdf.lookup_direct pdf "/Subtype" obj with
        | Some (Pdf.Name "/Form") ->
              begin match
                Pdfops.stream_of_ops
                  (Pdfops.parse_operators pdf resources [Pdf.Indirect objnum])
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

(* Give a list of content stream numbers, given a page reference number *)
let content_streams_of_page pdf refnum =
  match Pdf.direct pdf (Pdf.lookup_obj pdf refnum) with
  | Pdf.Dictionary dict ->
      begin match lookup "/Contents" dict with
      | Some (Pdf.Indirect i) -> [i]
      | Some (Pdf.Array x) ->
          option_map (function Pdf.Indirect i -> Some i | _ -> None) x
      | _ -> []
      end
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

let page_content_streams pdf dict =
  match lookup "/Contents" dict with
  | Some (Pdf.Indirect i) ->
      begin match Pdf.direct pdf (Pdf.Indirect i) with
      | Pdf.Array x -> x
      | _ -> [Pdf.Indirect i]
      end
  | Some (Pdf.Array x) -> x
  | _ -> raise Not_found

let squeeze_page_xobjects pdf xobjects_rewritten resources =
  match Pdf.lookup_direct pdf "/XObject" resources with
  | Some (Pdf.Dictionary xobjs) ->
      iter
        (function
           | _, Pdf.Indirect i ->
               xobjects_rewritten :=
                 !xobjects_rewritten + squeeze_form_xobject pdf (Some resources) i
           | _ -> failwith "squeeze_xobject")
        xobjs
  | _ -> ()

let squeeze_page_content_streams
  pdf content_stream_counts rewritten_page_streams pages_rewritten xobjects_rewritten
  objnum
 =
  match Pdf.lookup_obj pdf objnum with
  | Pdf.Dictionary dict as d
      when Pdf.lookup_direct pdf "/Type" d = Some (Pdf.Name "/Page") ->
        let resources = effective_resources pdf d None in
          begin try
            let content_streams = page_content_streams pdf dict in
            let content_stream_numbers =
              map (function Pdf.Indirect i -> i | _ -> assert false) content_streams
            in
              if no_duplicates content_stream_counts content_stream_numbers then
                let original_size =
                  content_streams_size_after_recompression
                    pdf
                    content_stream_numbers
                in
                let newstream =
                  Pdfops.stream_of_ops
                    (Pdfops.parse_operators pdf resources content_streams)
                in
                  if recompressed_stream_size pdf newstream <= original_size then
                    begin
                      incr pages_rewritten;
                      let newstream_objnum = Pdf.addobj pdf newstream in
                        rewritten_page_streams := newstream_objnum::!rewritten_page_streams;
                      let newdict =
                        Pdf.add_dict_entry
                          d "/Contents" (Pdf.Indirect newstream_objnum)
                      in
                        Pdf.addobj_given_num pdf (objnum, newdict)
                    end;
              squeeze_page_xobjects pdf xobjects_rewritten resources
          with
          | Not_found -> ()
          end
  | _ -> ()

(* For each object in the PDF marked with /Type /Page, for each /Contents
indirect reference or array of such, decode and recode that content stream. *)
let squeeze_all_content_streams pdf =
  let page_reference_numbers = Pdf.page_reference_numbers pdf in
  let total_pages = length page_reference_numbers in
  let report_progress = squeeze_progress_reporter total_pages in
    let content_stream_counts =
      content_stream_reference_counts
        (flatten (map (content_streams_of_page pdf) page_reference_numbers))
    in
      let pages_rewritten = ref 0 in
      let xobjects_rewritten = ref 0 in
      let rewritten_page_streams = ref [] in
        Hashtbl.clear xobjects_done;
        Cpdfutil.progress_line_no_end
          (Printf.sprintf
             "Squeezing page data and xobjects (%i pages): "
             total_pages);
        iter2
          (fun pagenum objnum ->
             report_progress pagenum;
             squeeze_page_content_streams
               pdf
               content_stream_counts
               rewritten_page_streams
               pages_rewritten
               xobjects_rewritten
               objnum)
          (indx page_reference_numbers)
          page_reference_numbers;
        Cpdfutil.progress_done ();
        {pages_rewritten = !pages_rewritten;
         xobjects_rewritten = !xobjects_rewritten;
         rewritten_page_streams = !rewritten_page_streams}

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

let squeeze_page_data_phase log pdf =
  let pagedata_stats =
    time_operation
      ~details:string_of_content_stream_stats
      log
      "Squeezing page data and xobjects"
      (fun () -> squeeze_all_content_streams pdf)
  in
    if pagedata_stats.pages_rewritten > 0 || pagedata_stats.xobjects_rewritten > 0 then
      begin
        time_operation
          log
          "Removing unreferenced objects after page data rewrite"
          (fun () -> Pdf.remove_unreferenced pdf);
        ignore
          (time_operation
             ~details:string_of_dedup_stats
             log
             "Deduplicating rewritten page data"
             (fun () ->
                if pagedata_stats.xobjects_rewritten > 0 then
                  squeeze_to_fixed_point ~log pdf
                else
                  squeeze_rewritten_page_data pdf pagedata_stats.rewritten_page_streams))
      end
    else
      log "Skipping page-data cleanup and deduplication; nothing was rewritten\n"

let squeeze_recompression_phase log pdf =
  let recompressed_streams =
    time_operation
      ~details:string_of_int
      log
      "Recompressing document"
      (fun () -> recompress_pdf_count pdf)
  in
    if recompressed_streams > 0 then
      ignore
        (time_operation
           ~details:string_of_dedup_stats
           log
           "Final squeeze pass"
           (fun () -> squeeze_to_fixed_point ~log pdf))
    else
      log "Skipping post-recompression cleanup and final deduplication; no streams changed\n"

let squeeze ?logto ?(pagedata=true) pdf =
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
      if pagedata then squeeze_page_data_phase log pdf;
      squeeze_recompression_phase log pdf;
      log (Printf.sprintf "Finished squeeze\n")
    with
    | e ->
        raise
          (Pdf.PDFError
             (Printf.sprintf
                "Squeeze failed. No output written.\n Proximate error was:\n %s"
                (Printexc.to_string e)))
