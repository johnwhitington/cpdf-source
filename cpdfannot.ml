open Pdfutil
open Cpdferror

(* List annotations *)
let get_annotation_string encoding pdf annot =
  match Pdf.lookup_direct pdf "/Contents" annot with
  | Some (Pdf.String s) -> Cpdfmetadata.encode_output encoding s
  | _ -> ""

let print_annotation encoding pdf num s =
  let s = get_annotation_string encoding pdf s in
  match s with
  | "" -> ()
  | s ->
    flprint (Printf.sprintf "Page %d: " num);
    flprint s;
    flprint "\n"

let list_page_annotations encoding pdf num page =
  match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
  | Some (Pdf.Array annots) ->
      iter (print_annotation encoding pdf num) (map (Pdf.direct pdf) annots)
  | _ -> ()

let rewrite_destination calculate_pagenumber d =
  match d with
  | Pdf.Array (Pdf.Indirect i::r) ->
      Pdf.Array (Pdf.Integer (calculate_pagenumber (Pdfdest.Fit (Pdfdest.PageObject i)))::r)
  | x -> x

let rewrite_destinations pdf annot =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
  let calculate_pagenumber =  Pdfpage.pagenumber_of_target ~fastrefnums pdf in
    (* Deal with /P in annotation *)
    let annot =
      match Pdf.indirect_number pdf "/P" annot with
      | Some i -> Pdf.add_dict_entry annot "/P" (Pdf.Integer (calculate_pagenumber (Pdfdest.Fit (Pdfdest.PageObject i))))
      | None -> annot
    in
    (* Deal with /Dest in annotation *)
    match Pdf.lookup_direct pdf "/Dest" annot with
    | Some d -> Pdf.add_dict_entry annot "/Dest" (rewrite_destination calculate_pagenumber d)
    | None ->
        (* Deal with /A --> /D dest when /A --> /S = /GoTo *)
        match Pdf.lookup_direct pdf "/A" annot with
        | Some action ->
            begin match Pdf.lookup_direct pdf "/D" action with
            | Some d ->
                Pdf.add_dict_entry
                  annot "/A" (Pdf.add_dict_entry action "/D" (rewrite_destination calculate_pagenumber d))
            | None -> annot
            end
       | None -> annot

let extra = ref []

let serial = ref ~-1

let getserial () =
  serial +=1; !serial

let objnum_to_serial_map = ref []

let annotations_json_page pdf page pagenum =
  match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
  | Some (Pdf.Array annots) ->
      map
        (fun annot ->
           let serial = getserial () in
             begin match annot with
             | Pdf.Indirect i -> objnum_to_serial_map := (i, serial)::!objnum_to_serial_map
             | _ -> Printf.eprintf "annotations must be indirect\n"
             end;
             let annot = Pdf.direct pdf annot in
             let annot = rewrite_destinations pdf annot in
               extra := annot::!extra;
               `List [`Int pagenum; `Int serial; Cpdfjson.json_of_object ~utf8:true ~clean_strings:true pdf (fun _ -> ()) ~no_stream_data:false ~parse_content:false annot])
        annots
  | _ -> []

(* Rewrite any /Parent entries in /Popup annotations to have annot serial number, not object number, and all /Popup entries in parent annotations similarly. *)
let postprocess_json_pdf objnum_to_serial_map pdf obj =
  let obj =
    match obj with
    | Pdf.Dictionary d ->
        (* These things seem to be to do with digital signatures, which aren't
           going to survive round-tripping of annotations anyway, and drag in
           all sorts of extra objects we don't want, so we remove them. *)
        let d = remove "/Lock" (remove "/V" d) in Pdf.Dictionary d
    | _ -> obj
  in
  match obj with
  | Pdf.Dictionary d ->
      let obj =
        begin match lookup "/Subtype" d, lookup "/Parent" d with
        | Some (Pdf.Name "/Popup"), Some (Pdf.Indirect i) ->
            begin match lookup i objnum_to_serial_map with
            | Some s -> Pdf.add_dict_entry obj "/Parent" (Pdf.Integer s)
            | None -> Printf.eprintf "Warning: Cpdfannot.process_extra_object: could not find serial number\n"; obj
            end
        | _ ->
            (* If not a popup annotation, remove /Parent. It drags in lots of
               extra objects (the whole page tree!) with a widget
               annotation, and we are unlikely to be able to round-trip them
               anyway. One day, if we can match FDF properly, it might be
               possible, but not now. *)
            Pdf.remove_dict_entry obj "/Parent"
        end
      in
        begin match obj with
        | Pdf.Dictionary d ->
            begin match lookup "/Popup" d with
            | Some (Pdf.Indirect i) ->
                begin match lookup i objnum_to_serial_map with
                | Some s -> Pdf.add_dict_entry obj "/Popup" (Pdf.Integer s)
                | None -> Printf.eprintf "Warning: Cpdfannot.process_extra_object: could not find serial number 2\n"; obj
                end
            | _ -> obj
            end
        | _ -> obj
        end
  | x -> x

let postprocess_json pdf objnum_to_serial_map json =
  map
   (function
    | `List [`Int pagenum; `Int serial; jo] ->
        let pdfobj = Cpdfjson.object_of_json jo in
        let fixed = postprocess_json_pdf objnum_to_serial_map pdf pdfobj in
        `List [`Int pagenum; `Int serial; Cpdfjson.json_of_object ~utf8:true ~clean_strings:true pdf (fun _ -> ()) ~no_stream_data:false ~parse_content:false fixed]
    | _ -> assert false)
   json

let list_annotations_json range pdf =
  extra := [];
  serial := ~-1;
  objnum_to_serial_map := [];
  let module J = Cpdfyojson.Safe in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pagenums = indx pages in
  let pairs = combine pages pagenums in
  let pairs = option_map (fun (p, n) -> if mem n range then Some (p, n) else None) pairs in
  let pages, pagenums = split pairs in
  let json = flatten (map2 (annotations_json_page pdf) pages pagenums) in
  let json = postprocess_json pdf !objnum_to_serial_map json in
  let extra = map (postprocess_json_pdf !objnum_to_serial_map pdf) !extra in
  (*Printf.printf "%i extra roots to explore\n" (length extra);
  iter (fun x -> Printf.eprintf "%s\n\n" (Pdfwrite.string_of_pdf x)) extra;*)
  let extra =
    map
      (fun n ->
         `List
           [`Int ~-n;
            Cpdfjson.json_of_object ~utf8:true ~clean_strings:true pdf (fun _ -> ())
              ~no_stream_data:false ~parse_content:false (Pdf.lookup_obj pdf n)])
      (setify
        (flatten
          (map 
            (fun x ->
               let r = Pdf.objects_referenced [] [] pdf x in
                 (*Printf.printf "%i extra for annot %s\n" (length r)
                 (Pdfwrite.string_of_pdf x);*) r)
          extra)))
  in
  let header =
    `List
     [`Int 0;
      Cpdfjson.json_of_object ~utf8:true ~clean_strings:true pdf (fun _ -> ())
        ~no_stream_data:false ~parse_content:false
        (Pdf.Dictionary ["/CPDFJSONannotformatversion", Pdf.Integer 1])]
  in
  let json = `List ([header] @ json @ extra) in
    J.pretty_to_channel stdout json

let list_annotations ~json range encoding pdf =
  if json
    then list_annotations_json range pdf
    else Cpdfpage.iter_pages (list_page_annotations encoding pdf) pdf range

(* Return annotations *)
let get_annotations encoding pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    flatten
      (map2
       (fun page pagenumber ->
         match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
         | Some (Pdf.Array annots) ->
             let strings =
               map (get_annotation_string encoding pdf) (map (Pdf.direct pdf) annots)
             in
               combine (many pagenumber (length strings)) strings
         | _ -> [])
        pages
        (ilist 1 (length pages))) 

let get_annotations_json pdf =
  let module J = Cpdfyojson.Safe in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pagenums = indx pages in
  let json = `List (flatten (map2 (annotations_json_page pdf) pages pagenums)) in
    Pdfio.bytes_of_string (J.to_string json)

(** Set annotations from JSON, keeping any existing ones. *)
let set_annotations_json pdf i =
  let module J = Cpdfyojson.Safe in
  let content = Pdfio.string_of_bytes (Pdfio.bytes_of_input i 0 i.Pdfio.in_channel_length) in
  let json = J.from_string content in
  (* Find largest negative objnumber. Then add number of annot objects. *)
  match json with
  | `List entries ->
      let maxobjnum =
        fold_left max min_int (map (fun e -> match e with `List (`Int i::_) -> abs i | _ -> error "Bad annots JSON entry") entries)
      in
        (* Renumber the PDF so everything has bigger object numbers than that. *)
        Printf.printf "maxobjnum = %i\n" maxobjnum;
        ()
  | _ -> error "Bad Annotations JSON file"

(* Equalise the page lengths of two PDFs by chopping or extending the first one.
*)
let equalise_lengths a b =
  let a' =
    if Pdfpage.endpage a < Pdfpage.endpage b then
      Pdfpage.change_pages false a
        (Pdfpage.pages_of_pagetree a @
           many (Pdfpage.blankpage Pdfpaper.a4) (Pdfpage.endpage b - Pdfpage.endpage a))
    else if Pdfpage.endpage a > Pdfpage.endpage b then
      Pdfpage.change_pages false a
        (take (Pdfpage.pages_of_pagetree a) (Pdfpage.endpage b))
    else a 
  in
    a', b

(* Copy annotations *)

(* FIXME: Why does this chop the files to the same length? Should be able to
apply annotations from a longer file to a shorter? *)

(* Rewrite any annotation destinations to point to pages in the
destination file. This prevents pages being copied, and ensures the links are
correct Any Indirect link inside a /Dest is rewritten if in the table. If not
inside a /Dest, nothing is rewritten. *)
let rec renumber_in_dest table indest = function
    Pdf.Indirect i -> 
      begin
        try Pdf.Indirect (Hashtbl.find table i) with _ -> Pdf.Indirect i
      end
  | Pdf.Array a ->
      Pdf.recurse_array (renumber_in_dest table indest) a
  | Pdf.Dictionary d ->
      Pdf.Dictionary
        (map
          (function
             ("/Dest", v) -> ("/Dest", renumber_in_dest table true v)
           | (k, v) -> (k, renumber_in_dest table indest v))
          d)
  | x -> x 

let renumber_in_object pdf objnum table =
  Pdf.addobj_given_num
    pdf (objnum, (renumber_in_dest table false (Pdf.lookup_obj pdf objnum)))

let copy_annotations_page topdf frompdf frompage topage =
  match Pdf.lookup_direct frompdf "/Annots" frompage.Pdfpage.rest with
    Some (Pdf.Array frompage_annots as annots) ->
      let table =
        hashtable_of_dictionary
          (combine
             (Pdf.page_reference_numbers frompdf)
             (Pdf.page_reference_numbers topdf))
      in
        iter
         (function
            (* FIXME: We assume they are indirects. Must also do direct, though rare.*)
            Pdf.Indirect x ->
              (*Printf.printf "Copying annotation %s which is\n%s\n"
                (Pdfwrite.string_of_pdf (Pdf.Indirect x))
                (Pdfwrite.string_of_pdf (Pdf.direct frompdf (Pdf.Indirect
                x)));*)
              renumber_in_object frompdf x table
          | _ -> ())
         frompage_annots;
        let objects_to_copy = Pdf.objects_referenced [] [] frompdf annots in
          iter
            (fun n ->
               ignore (Pdf.addobj_given_num topdf (n, Pdf.lookup_obj frompdf n)))
            objects_to_copy;
          let topage_annots =
            match Pdf.lookup_direct frompdf "/Annots" topage.Pdfpage.rest with
            | Some (Pdf.Array annots) -> annots
            | _ -> []
          in
            let merged_dict = Pdf.Array (frompage_annots @ topage_annots) in
              let topage' =
                {topage with Pdfpage.rest =
                   Pdf.add_dict_entry topage.Pdfpage.rest "/Annots" merged_dict}
              in
                topdf, topage'
  | Some x -> topdf, topage
  | None -> topdf, topage

let copy_annotations range frompdf topdf =
  let frompdf, topdf = equalise_lengths frompdf topdf in
    match Pdf.renumber_pdfs [frompdf; topdf] with 
    | [frompdf; topdf] ->
        let frompdf_pages = Pdfpage.pages_of_pagetree frompdf in
        let topdf_pages = Pdfpage.pages_of_pagetree topdf in
          let pdf = ref topdf
          and pages = ref []
          and pnum = ref 1
          and frompdf_pages = ref frompdf_pages
          and topdf_pages = ref topdf_pages in
            (* Go through, updating pdf and collecting new pages. *)
            while not (isnull !frompdf_pages) do
              let frompdf_page = hd !frompdf_pages
              and topdf_page = hd !topdf_pages in
                let pdf', page =
                  if mem !pnum range
                    then copy_annotations_page !pdf frompdf frompdf_page topdf_page
                    else !pdf, topdf_page
                in
                  pdf := pdf';
                  pages =| page;
                  incr pnum;
                  frompdf_pages := tl !frompdf_pages;
                  topdf_pages := tl !topdf_pages
            done;
            Pdfpage.change_pages true !pdf (rev !pages)
    | _ -> assert false

(* Remove annotations *)
let remove_annotations range pdf =
  let remove_annotations_page pagenum page =
    if mem pagenum range then
      let rest' =
        Pdf.remove_dict_entry page.Pdfpage.rest "/Annots"
      in
        {page with Pdfpage.rest = rest'}
    else
      page
  in
    Cpdfpage.process_pages (Pdfpage.ppstub remove_annotations_page) pdf range
