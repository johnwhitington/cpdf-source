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
