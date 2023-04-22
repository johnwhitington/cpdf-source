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

let rewrite_destination f d =
  match d with
  | Pdf.Array (Pdf.Indirect i::r) ->
      Pdf.Array (Pdf.Integer (f i)::r)
  | x -> x

let rewrite_destinations f pdf annot =
  (* Deal with /P in annotation *)
  let annot =
    match Pdf.indirect_number pdf "/P" annot with
    | Some i -> Pdf.add_dict_entry annot "/P" (Pdf.Integer (f i))
    | None -> annot
  in
  (* Deal with /Dest in annotation *)
  match Pdf.lookup_direct pdf "/Dest" annot with
  | Some d -> Pdf.add_dict_entry annot "/Dest" (rewrite_destination f d)
  | None ->
      (* Deal with /A --> /D dest when /A --> /S = /GoTo *)
      match Pdf.lookup_direct pdf "/A" annot with
      | Some action ->
          begin match Pdf.lookup_direct pdf "/D" action with
          | Some d ->
              Pdf.add_dict_entry
                annot "/A" (Pdf.add_dict_entry action "/D" (rewrite_destination f d))
          | None -> annot
          end
     | None -> annot

let extra = ref []

let annotations_json_page calculate_pagenumber pdf page pagenum =
  match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
  | Some (Pdf.Array annots) ->
      option_map
        (fun annot ->
           begin match annot with
           | Pdf.Indirect objnum ->
               let annot = Pdf.direct pdf annot in
               let annot =
                 rewrite_destinations
                   (fun i -> calculate_pagenumber (Pdfdest.Fit (Pdfdest.PageObject i)))
                   pdf annot
               in
                 extra := annot::!extra;
                 Some (`List
                   [`Int pagenum;
                    `Int objnum;
                     Cpdfjson.json_of_object ~utf8:true ~clean_strings:true pdf (fun _ -> ())
                       ~no_stream_data:false ~parse_content:false annot])
           | _ -> Printf.eprintf "Warning: annotations must be indirect\n"; None
           end)
        annots
  | _ -> []

let list_annotations_json range pdf =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
  let calculate_pagenumber =  Pdfpage.pagenumber_of_target ~fastrefnums pdf in
  extra := [];
  let module J = Cpdfyojson.Safe in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pagenums = indx pages in
  let pairs = combine pages pagenums in
  let pairs = option_map (fun (p, n) -> if mem n range then Some (p, n) else None) pairs in
  let pages, pagenums = split pairs in
  let json = flatten (map2 (annotations_json_page calculate_pagenumber pdf) pages pagenums) in
  (*Printf.printf "%i extra roots to explore\n" (length extra);
  iter (fun x -> Printf.eprintf "%s\n\n" (Pdfwrite.string_of_pdf x)) extra;*)
  let extra =
    map
      (fun n ->
         `List
           [`Int n;
            Cpdfjson.json_of_object ~utf8:true ~clean_strings:true pdf (fun _ -> ())
              ~no_stream_data:false ~parse_content:false (Pdf.lookup_obj pdf n)])
      (setify
        (flatten
          (map 
            (fun x ->
               let r = Pdf.objects_referenced [] [] pdf x in
                 (*Printf.printf "%i extra for annot %s\n" (length r)
                 (Pdfwrite.string_of_pdf x);*) r)
          !extra)))
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

let get_annotations_json pdf range =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
  let calculate_pagenumber =  Pdfpage.pagenumber_of_target ~fastrefnums pdf in
  let module J = Cpdfyojson.Safe in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pagenums = indx pages in
  let pairs = combine pagenums pages in
  let pairs_in_range = option_map (fun (pn, p) -> if mem pn range then Some (pn, p) else None) pairs in
  let pagenums, pages = split pairs_in_range in
  let json = `List (flatten (map2 (annotations_json_page calculate_pagenumber pdf) pages pagenums)) in
    Pdfio.bytes_of_string (J.to_string json)

(** Set annotations from JSON, keeping any existing ones. *)
let set_annotations_json pdf i =
  let module J = Cpdfyojson.Safe in
  let content = Pdfio.string_of_input i in
  let json = J.from_string content in
  (* Find largest negative objnumber. Then add number of annot objects. *)
  match json with
  | `List entries ->
      (* Renumber the PDF so everything has bigger object numbers than that. *)
      let maxobjnum =
        fold_left max min_int
          (map
            (function
             | `List [_; `Int i; _] -> i
             | `List [`Int i; _] -> abs i
             | _ -> error "Bad annots entry")
           entries)
      in
      let pdf_objnums = map fst (list_of_hashtbl pdf.Pdf.objects.Pdf.pdfobjects) in
      let change_table =
        hashtable_of_dictionary (map2 (fun f t -> (f, t)) pdf_objnums (ilist (maxobjnum + 1) (maxobjnum + length pdf_objnums)))
      in
      let pdf' = Pdf.renumber change_table pdf in
        pdf.root <- pdf'.root;
        pdf.objects <- pdf'.objects;
        pdf.trailerdict <- pdf'.trailerdict;
        (* 1. Extract the extra objects, and add them to the file *)
        let extras = 
          ()
        in
          (* 2. Extract the annotation objects, and rewrite their destinations from page numbers to pages, add them to file *)
          (* 3. Add the annots entries to each file *)
          ()
  | _ -> error "Bad Annotations JSON file"

let copy_annotations range frompdf topdf =
  set_annotations_json topdf (Pdfio.input_of_bytes (get_annotations_json frompdf range))

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
