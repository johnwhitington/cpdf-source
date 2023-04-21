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

let get_annotations_json pdf =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
  let calculate_pagenumber =  Pdfpage.pagenumber_of_target ~fastrefnums pdf in
  let module J = Cpdfyojson.Safe in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pagenums = indx pages in
  let json = `List (flatten (map2 (annotations_json_page calculate_pagenumber pdf) pages pagenums)) in
    Pdfio.bytes_of_string (J.to_string json)

(** Set annotations from JSON, keeping any existing ones. *)
let set_annotations_json pdf i =
  let module J = Cpdfyojson.Safe in
  let content = Pdfio.string_of_bytes (Pdfio.bytes_of_input i 0 i.Pdfio.in_channel_length) in
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
      let pdf = Pdf.renumber change_table pdf in
        (* 1. Rewrite all serial numbers in all places (annots and extra objects?) *)
        (* 2. Add all the negative numbered objects straight to the file *)
        (* 3. Add all the actual annotations to the /Annots array on each page *)
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

(* Copy annotations. FIXME: This code is deprecated in favour of extracting annotations to JSON
and then re-adding. *)

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
