open Pdfutil

(* 1. Get list of indirects of all OCGs from the /OCProperties, and their textual names
 * 2. Calculate a change list to coalesce them
 * 3. Remove any changed ones from the /OCGs and /Order and /ON and /OFF in /OCProperties
 * 4. Do the changes to all indirect references in the whole pdf  *)
(*FIXME Pre-existing nulls - what to do? *)
let ocg_coalesce pdf =
  match Pdf.lookup_direct pdf "/OCProperties" (Pdf.catalog_of_pdf pdf) with
    None -> ()
  | Some ocpdict ->
      let number_name_pairs =
        match Pdf.lookup_direct pdf "/OCGs" ocpdict with
          Some (Pdf.Array ocgs) ->
            begin let numbers =
              map (function Pdf.Indirect i -> i | _ -> failwith "Malformed /OCG entry") ocgs
            in
              let names =
                map
                  (fun i ->
                     try
                       begin match Pdf.lookup_obj pdf i with
                         Pdf.Dictionary d ->
                           begin match Pdf.lookup_direct pdf "/Name" (Pdf.Dictionary d) with
                             Some (Pdf.String s) -> s
                           | _ -> failwith "ocg: missing name"
                           end
                       | _ -> 
                           failwith "ocg: not a dictionary"
                       end
                      with _ -> failwith "OCG object missing")
                  numbers
              in
                combine numbers names
            end
        | _ -> failwith "Malformed or missing /OCGs"
      in
        (*iter (fun (num, name) -> Printf.printf "%i = %s\n" num name) number_name_pairs;*)
        let changes =
          let cf (_, name) (_, name') = compare name name' in
          let sets = collate cf (List.stable_sort cf number_name_pairs) in
          flatten (option_map (function [] -> None | (hnum, _)::t -> Some (map (function (tnum, _) -> (tnum, hnum)) t)) sets)
        in
        (*Printf.printf "\nChanges are:\n";
        List.iter (fun (f, t) -> Printf.printf "%i -> %i\n" f t) changes;*)
        let new_ocproperties =
          let remove_from_array key nums dict =
            match Pdf.lookup_direct pdf key dict with
            | Some (Pdf.Array elts) ->
                let elts' = option_map (function Pdf.Indirect i -> if mem i nums then None else Some (Pdf.Indirect i) | _ -> None) elts in
                  Pdf.add_dict_entry dict key (Pdf.Array elts')
            | _ -> dict
          in
          let remove_from_array_inside_d key nums dict =
            match Pdf.lookup_direct pdf "/D" dict with
            | Some (Pdf.Dictionary ddict) ->
                begin match Pdf.lookup_direct pdf key (Pdf.Dictionary ddict) with
                | Some (Pdf.Array elts) ->
                    let elts' = option_map (function Pdf.Indirect i -> if mem i nums then None else Some (Pdf.Indirect i) | _ -> None) elts in
                    Pdf.add_dict_entry dict "/D" (Pdf.add_dict_entry (Pdf.Dictionary ddict) key (Pdf.Array elts'))
                | _ -> dict
                end
            | _ -> failwith "No /D dict in OCGProperties"
          in
          let nums = map fst changes in
            (*Printf.printf "\nto remove:\n";
            List.iter (Printf.printf "%i ") nums;*)
            remove_from_array "/OCGs" nums
              (remove_from_array_inside_d "/ON" nums
                (remove_from_array_inside_d "/OFF" nums
                  (remove_from_array_inside_d "/Order" nums ocpdict)))
        in
        (*flprint (Pdfwrite.string_of_pdf new_ocproperties);*)
        let ocp_objnum = Pdf.addobj pdf new_ocproperties in
        let new_catalog = Pdf.addobj pdf (Pdf.add_dict_entry (Pdf.catalog_of_pdf pdf) "/OCProperties" (Pdf.Indirect ocp_objnum)) in
        pdf.Pdf.trailerdict <- Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect new_catalog);
        pdf.Pdf.root <- new_catalog;
        Pdf.objselfmap (Pdf.renumber_object_parsed ~preserve_order:false pdf (hashtable_of_dictionary changes)) pdf

let ocg_get_list pdf =
  let l = ref [] in
  begin match Pdf.lookup_direct pdf "/OCProperties" (Pdf.catalog_of_pdf pdf) with
    None -> ()
  | Some ocpdict ->
      match Pdf.lookup_direct pdf "/OCGs" ocpdict with
        Some (Pdf.Array elts) ->
          iter
            (function
               Pdf.Indirect i ->
                 (match Pdf.lookup_direct pdf "/Name" (Pdf.lookup_obj pdf i) with
                    Some (Pdf.String s) -> l := s::!l | _ -> ())
             | _ -> ())
            elts
      | _ -> ()
  end;
  rev !l

(* Output all OCGs in JSON such that simple changes can be made by
   roundtripping. Of course, we can't change the tags in the page content, so
   what can be done with this is, by it nature, limited. *)
let ocg_list_json pdf =
  (* 1. Find the top of the OCG set of objects. Is it always indirect? If so, easy. *)
  (* 2. Locate all reachable objects *)
  (* 3. Write them out in CPDFJSON format, with a little header /CPDFJSONocgformatversion *)
  (* 4. Format should match roughly annotations JSON format - or is it a little simpler? Follow also usage of utf8, clean_strings etc. *)
  `Null

let ocg_list json pdf =
  if json then flprint (Cpdfyojson.Safe.pretty_to_string (ocg_list_json pdf)) else
    List.iter (Printf.printf "%s\n") (map Pdftext.utf8_of_pdfdocstring (ocg_get_list pdf))

let ocg_replace json pdf =
  ()

let ocg_rename f t pdf =
  Pdf.objselfmap
    (function
      Pdf.Dictionary d ->
        begin match Pdf.lookup_direct pdf "/Type" (Pdf.Dictionary d) with
          Some (Pdf.Name "/OCG") ->
            begin match Pdf.lookup_direct pdf "/Name" (Pdf.Dictionary d) with
              Some (Pdf.String s) when s = f ->
                Pdf.add_dict_entry (Pdf.Dictionary d) "/Name" (Pdf.String t)
            | _ -> Pdf.Dictionary d
            end
        | _ -> Pdf.Dictionary d
        end
     | x -> x
    )
    pdf

let ocg_order_all pdf =
  match Pdf.lookup_direct pdf "/OCProperties" (Pdf.catalog_of_pdf pdf) with
    None -> ()
  | Some ocpdict ->
      match Pdf.lookup_direct pdf "/OCGs" ocpdict with
        Some (Pdf.Array elts) ->
          begin match Pdf.lookup_direct pdf "/D" ocpdict with
            Some (Pdf.Dictionary d) ->
              let newd = Pdf.add_dict_entry (Pdf.Dictionary d) "/Order" (Pdf.Array elts) in
              let new_ocproperties = Pdf.add_dict_entry ocpdict "/D" newd in
              let ocp_objnum = Pdf.addobj pdf new_ocproperties in
              let new_catalog = Pdf.addobj pdf (Pdf.add_dict_entry (Pdf.catalog_of_pdf pdf) "/OCProperties" (Pdf.Indirect ocp_objnum)) in
                pdf.Pdf.trailerdict <- Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect new_catalog);
                pdf.Pdf.root <- new_catalog
          | _ -> ()
          end
      | _ -> ()
