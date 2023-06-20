(** A loose JSON equivalent of XFDF. *)
open Pdfutil
open Cpdferror

(* List annotations, simple old style. *)
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

let list_annotations range encoding pdf =
  Cpdfpage.iter_pages (list_page_annotations encoding pdf) pdf range

(* New, JSON style *)
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

(* We exclude the same annotations as the XFDF spec does. *)
let excluded pdf annot =
  match Pdf.lookup_direct pdf "/Subtype" annot with
  | Some (Pdf.Name ("/Movie" | "/Widget" | "/Screen" | "/PrinterMark" | "/TrapNet")) -> true
  | _ -> false

let extra = ref []

let annotations_json_page calculate_pagenumber pdf page pagenum =
  match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
  | Some (Pdf.Array annots) ->
      option_map
        (fun annot ->
           begin match annot with
           | Pdf.Indirect objnum ->
               let annot = Pdf.direct pdf annot in
               if excluded pdf annot then None else
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
           | _ -> Pdfe.log "Warning: annotations must be indirect\n"; None
           end)
        annots
  | _ -> []

let get_annotations_json pdf range =
  let refnums = Pdf.page_reference_numbers pdf in
  let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
  let calculate_pagenumber =  Pdfpage.pagenumber_of_target ~fastrefnums pdf in
  extra := [];
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pagenums = indx pages in
  let pairs = combine pages pagenums in
  let pairs = option_map (fun (p, n) -> if mem n range then Some (p, n) else None) pairs in
  let pages, pagenums = split pairs in
  let json = flatten (map2 (annotations_json_page calculate_pagenumber pdf) pages pagenums) in
  let jsonobjnums : int list = map (function `List [_; `Int n; _] -> n | _ -> assert false) json in
  (*Printf.eprintf "%i extra roots to explore\n" (length !extra);
  iter (fun x -> Pdfe.log (Printf.sprintf "%s\n\n" (Pdfwrite.string_of_pdf x))) !extra;*)
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
               let x = Pdf.remove_dict_entry x "/Popup" in
               let x = Pdf.remove_dict_entry x "/Parent" in
               let r = Pdf.objects_referenced [] [] pdf x in
                 (*Printf.eprintf "%i extra for annot %s\n" (length r) (Pdfwrite.string_of_pdf x);*)
                 r)
          !extra)))
  in
  let extra =
    option_map
      (function `List [`Int n; _] as json -> if mem n jsonobjnums then None else Some json | _ -> assert false)
      extra
  in
  let header =
    `List
     [`Int 0;
      Cpdfjson.json_of_object ~utf8:true ~clean_strings:true pdf (fun _ -> ())
        ~no_stream_data:false ~parse_content:false
        (Pdf.Dictionary ["/CPDFJSONannotformatversion", Pdf.Integer 1])]
  in
  let json = `List ([header] @ json @ extra) in
    Pdfio.bytes_of_string (Cpdfyojson.Safe.pretty_to_string json)

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

(** Set annotations from JSON, keeping any existing ones. *)
let set_annotations_json pdf i =
  match Cpdfyojson.Safe.from_string (Pdfio.string_of_input i) with
  | `List entries ->
      (* Renumber the PDF so everything has bigger object numbers than that. *)
      let maxobjnum =
        fold_left max min_int
          (map
            (function
             | `List [_; `Int i; _] | `List [`Int i; _] -> i
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
        (* Add the extra objects back in and build the annotations. *) 
        let extras = option_map (function `List [`Int i; o] -> Some (i, o) | _ -> None) entries in
        let annots = option_map (function `List [`Int pagenum; `Int i; o] -> Some (pagenum, i, o) | _ -> None) entries in
          iter (fun (i, o) -> Pdf.addobj_given_num pdf (i, Cpdfjson.object_of_json o)) extras;
          let pageobjnummap =
            let refnums = Pdf.page_reference_numbers pdf in
              combine (indx refnums) refnums
          in
          let pages = Pdfpage.pages_of_pagetree pdf in
          let annotsforeachpage = collate compare (sort compare annots) in
          let newpages =
            map2
              (fun pagenum page ->
                 let forthispage = flatten (keep (function (p, _, _)::t when p = pagenum -> true | _ -> false) annotsforeachpage) in
                   iter
                     (fun (pnum, i, o) ->
                        let pageobjnum = match lookup pnum pageobjnummap with Some x -> x | None -> 0 in
                        let f = fun pnum -> if pageobjnum = 0 then pnum else pageobjnum in
                          Pdf.addobj_given_num pdf (i, rewrite_destinations f pdf (Cpdfjson.object_of_json o)))
                     forthispage;
                   if forthispage = [] then page else
                     let annots =
                       match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with | Some (Pdf.Array annots) -> annots | _ -> []
                     in
                     let newannots = map (fun (_, i, _) -> Pdf.Indirect i) forthispage in
                       {page with Pdfpage.rest = Pdf.add_dict_entry page.Pdfpage.rest "/Annots" (Pdf.Array (annots @ newannots))})
              (indx pages)
              pages
          in
            let pdf' = Pdfpage.change_pages true pdf newpages in
              pdf.root <- pdf'.root;
              pdf.objects <- pdf'.objects;
              pdf.trailerdict <- pdf'.trailerdict
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
