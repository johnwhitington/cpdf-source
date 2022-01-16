open Pdfutil

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

(* In the future, we will allow round-tripping of JSON annotations, but this
   will be complicated. For now, we just turn some indirect things into direct
   things, so that the output contains all the pertinent information, not for
   round-tripping, but for mere extraction. *)
let make_direct pdf annot =
  match Pdf.lookup_direct pdf "/A" annot with
  | None -> annot
  | Some d -> Pdf.add_dict_entry annot "/A" d

let rewrite_destination calculate_pagenumber d =
  match d with
  | Pdf.Array (Pdf.Indirect i::r) ->
      Pdf.Array (Pdf.Indirect (calculate_pagenumber (Pdfdest.Fit (Pdfdest.PageObject i)))::r)
  | x -> x

let rewrite_destinations pdf annot =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
  let calculate_pagenumber =  Pdfpage.pagenumber_of_target ~fastrefnums pdf in
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

let annotations_json_page pdf page pagenum =
  match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
  | Some (Pdf.Array annots) ->
      map
        (fun annot ->
            let annot = make_direct pdf annot in
             `List [`Int pagenum; Cpdfjson.json_of_object ~clean_strings:true pdf (fun _ -> ()) false false annot])
        (map (Pdf.direct pdf) annots)
  | _ -> []

let list_annotations_json pdf =
  let module J = Cpdfyojson.Safe in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pagenums = indx pages in
  let json = `List (flatten (map2 (annotations_json_page pdf) pages pagenums)) in
    J.pretty_to_channel stdout json

let list_annotations ~json encoding pdf =
  let range = Cpdfpagespec.parse_pagespec pdf "all" in
  if json
    then list_annotations_json pdf
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
    Cpdfpage.process_pages (Cpdfutil.ppstub remove_annotations_page) pdf range
