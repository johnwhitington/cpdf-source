open Pdfutil

(* FIXME: All of this should probably be pushed down into CamlPDF *)

(* For uses of process_pages which don't need to deal with matrices, this
   function transforms into one which returns the identity matrix *)
let ppstub f n p = (f n p, n, Pdftransform.i_matrix)

(* These may move into CamlPDF at some point *)
let process_xobject f pdf resources i =
  let xobj = Pdf.lookup_obj pdf i in
    match Pdf.lookup_direct pdf "/Subtype" xobj with
    | None -> raise (Pdf.PDFError "No /Subtype in Xobject") 
    | Some (Pdf.Name "/Form") ->
        Pdf.getstream xobj;
        begin match xobj with
        | Pdf.Stream ({contents = Pdf.Dictionary dict, Pdf.Got bytes} as rf) ->
            begin match f pdf resources [Pdf.Stream rf] with
            | [Pdf.Stream {contents = (Pdf.Dictionary dict', data)}] ->
                let dict' =
                  Pdf.remove_dict_entry
                    (Pdf.Dictionary (mergedict dict dict'))
                    "/Filter"
                in
                  rf := (dict', data)
            | _ -> assert false
            end
        | _ -> assert false (* getstream would have complained already *)
        end
    | Some _ -> ()

let process_xobjects pdf page f =
  match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
  | Some (Pdf.Dictionary elts) ->
      iter
        (fun (k, v) ->
          match v with
          | Pdf.Indirect i -> process_xobject f pdf page.Pdfpage.resources i
          | _ -> raise (Pdf.PDFError "process_xobject"))
        elts
  | _ -> ()

(*(* The content transformed by altering any use of [Op_cm]. But we must also
alter any /Matrix entries in pattern dictionaries *)
let change_pattern_matrices_resources pdf tr resources =
  try
    begin match Pdf.lookup_direct pdf "/Pattern" resources with
    | Some (Pdf.Dictionary patterns) ->
        let entries =
          map
            (fun (name, p) ->
              (*Printf.printf "Changing matrices of pattern %s\n" name;*)
              let old_pattern = Pdf.direct pdf p in
                let new_pattern =
                  let existing_tr = Pdf.parse_matrix pdf "/Matrix" old_pattern in
                    let new_tr = Pdftransform.matrix_compose (Pdftransform.matrix_invert tr) existing_tr in
                      Pdf.add_dict_entry old_pattern "/Matrix" (Pdf.make_matrix new_tr)
                in
                  name, Pdf.Indirect (Pdf.addobj pdf new_pattern))
            patterns
         in
           Pdf.add_dict_entry resources "/Pattern" (Pdf.Dictionary entries)
    | _ -> resources
  end
    with
      Pdftransform.NonInvertable ->
        Printf.eprintf "Warning: noninvertible matrix\n%!";
        resources

let change_pattern_matrices_page pdf tr page =
  let page =
    {page with Pdfpage.resources = change_pattern_matrices_resources pdf tr page.Pdfpage.resources}
  in
    match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
    | Some (Pdf.Dictionary elts) ->
        iter
          (fun (k, v) -> 
             match v with
             | Pdf.Indirect i ->
                 (* Check if it's a form XObject. If so, rewrite its resources and add back as same number. *)
                 begin match Pdf.lookup_direct pdf "/Subtype" v with
                 | Some (Pdf.Name "/Form") ->
                     (*Printf.printf "Processing form xobject %s for patterns\n" k; *)
                     let form_xobject = Pdf.lookup_obj pdf i in
                       begin match Pdf.lookup_direct pdf "/Resources" form_xobject with
                       | Some resources ->
                           let form_xobject' =
                             Pdf.add_dict_entry form_xobject "/Resources" (change_pattern_matrices_resources pdf tr resources)  
                           in
                             Pdf.addobj_given_num pdf (i, form_xobject')
                       | _ -> ()
                       end
                 | _ -> ()
                 end;
             | _ -> raise (Pdf.PDFError "change_pattern_matrices_page"))
          elts;
        page
    | _ -> page*)

(* Union two resource dictionaries from the same PDF. *)
let combine_pdf_resources pdf a b =
  let a_entries =
    match a with
    | Pdf.Dictionary entries -> entries
    | _ -> []
  in let b_entries =
    match b with
    | Pdf.Dictionary entries -> entries
    | _ -> []
  in
    let resource_keys =
      ["/Font"; "/ExtGState"; "/ColorSpace"; "/Pattern";
       "/Shading"; "/XObject"; "/Properties"]
    in
      let combine_entries key =
        let a_entries =
          match Pdf.lookup_direct pdf key a with
          | Some (Pdf.Dictionary d) -> d
          | _ -> []
        in let b_entries =
          match Pdf.lookup_direct pdf key b with
          | Some (Pdf.Dictionary d) -> d
          | _ -> []
        in
          if a_entries = [] && b_entries = [] then
            None
          else
            Some (key, Pdf.Dictionary (a_entries @ b_entries))
      in
        let unknown_keys_a = lose (fun (k, _) -> mem k resource_keys) a_entries in
        let unknown_keys_b = lose (fun (k, _) -> mem k resource_keys) b_entries in
        let combined_known_entries = option_map combine_entries resource_keys in
          fold_left
            (fun dict (k, v) -> Pdf.add_dict_entry dict k v)
            (Pdf.Dictionary [])
            (unknown_keys_a @ unknown_keys_b @ combined_known_entries)

let transform_rect pdf transform rect =
  let minx, miny, maxx, maxy = Pdf.parse_rectangle pdf rect in
    let (x0, y0) = Pdftransform.transform_matrix transform (minx, miny) in
    let (x1, y1) = Pdftransform.transform_matrix transform (maxx, maxy) in
    let (x2, y2) = Pdftransform.transform_matrix transform (minx, maxy) in
    let (x3, y3) = Pdftransform.transform_matrix transform (maxx, miny) in
      let minx = fmin (fmin x0 x1) (fmin x2 x3) in
      let miny = fmin (fmin y0 y1) (fmin y2 y3) in
      let maxx = fmax (fmax x0 x1) (fmax x2 x3) in
      let maxy = fmax (fmax y0 y1) (fmax y2 y3) in
        Pdf.Array [Pdf.Real minx; Pdf.Real miny; Pdf.Real maxx; Pdf.Real maxy]

let transform_quadpoint_single pdf transform = function
  | [x1; y1; x2; y2; x3; y3; x4; y4] ->
      let x1, y1, x2, y2, x3, y3, x4, y4 =
        Pdf.getnum pdf x1, Pdf.getnum pdf y1,
        Pdf.getnum pdf x2, Pdf.getnum pdf y2,
        Pdf.getnum pdf x3, Pdf.getnum pdf y3,
        Pdf.getnum pdf x4, Pdf.getnum pdf y4
      in
        let (x1, y1) = Pdftransform.transform_matrix transform (x1, y1) in
        let (x2, y2) = Pdftransform.transform_matrix transform (x2, y2) in
        let (x3, y3) = Pdftransform.transform_matrix transform (x3, y3) in
        let (x4, y4) = Pdftransform.transform_matrix transform (x4, y4) in
          map (fun x -> Pdf.Real x) [x1; y1; x2; y2; x3; y3; x4; y4]
  | qp ->
     Printf.eprintf "Malformed /QuadPoints format: must be a multiple of 8 entries\n";
     qp

let transform_quadpoints pdf transform = function
| Pdf.Array qps ->
    Pdf.Array (flatten (map (transform_quadpoint_single pdf transform) (splitinto 8 qps)))
| qp ->
    Printf.eprintf "Unknown or malformed /QuadPoints format %s\n" (Pdfwrite.string_of_pdf qp);
    qp

(* Apply transformations to any annotations in /Annots (i.e their /Rect and
/QuadPoints entries). Also as a best-effort service, altering other
coordinates, like the endpoints /L in a line annotation. *)
let transform_annotations pdf transform rest =
  match Pdf.lookup_direct pdf "/Annots" rest with
  | Some (Pdf.Array annots) ->
      (* Always indirect references, so alter in place *)
      iter
        (function
         | Pdf.Indirect i ->
             let annot = Pdf.lookup_obj pdf i in
             let rect' =
               match Pdf.lookup_direct pdf "/Rect" annot with
               | Some rect -> transform_rect pdf transform rect
               | None -> raise (Pdf.PDFError "transform_annotations: no rect")
               in
             let quadpoints' =
               match Pdf.lookup_direct pdf "/QuadPoints" annot with
               | Some qp -> Some (transform_quadpoints pdf transform qp)
               | None -> None
               in
             let line' =
               match Pdf.lookup_direct pdf "/L" annot with
               | Some rect -> Some (transform_rect pdf transform rect)
               | _ -> None
             in
             let annot = Pdf.add_dict_entry annot "/Rect" rect' in
             let annot =
               match quadpoints' with
               | Some qp -> Pdf.add_dict_entry annot "/QuadPoints" qp 
               | None -> annot
             in
             let annot =
               match line' with
               | Some l -> Pdf.add_dict_entry annot "/L" l
               | None -> annot
             in
               Pdf.addobj_given_num pdf (i, annot)
         | _ -> Printf.eprintf "transform_annotations: not indirect\n%!")
        annots
   | _ -> ()
