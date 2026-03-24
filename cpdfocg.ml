open Pdfutil
open Cpdferror

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
  let string s = `String (Pdftext.utf8_of_pdfdocstring s) in
  let int i = `Int i in
  let float f = `Float f in
  let opt f x = match x with None -> `Null | Some s -> f s in
  let list f l = `List (map f l) in
  match Pdfocg.read_ocg pdf with
  | None -> `Null
  | Some ocg ->
      let json_of_state = function
        | Pdfocg.OCG_ON -> `String "/ON"
        | Pdfocg.OCG_OFF -> `String "/OFF"
        | Pdfocg.OCG_Unchanged -> `String "/Unchanged"
      in
      let json_of_listmode = function
        | Pdfocg.OCG_AllPages -> `String "/AllPages"
        | Pdfocg.OCG_VisiblePages -> `String "/VisiblePages"
      in
      let json_of_event = function
        | Pdfocg.OCG_View -> `String "/View"
        | Pdfocg.OCG_Print -> `String "/Print"
        | Pdfocg.OCG_Export -> `String "/Export"
      in
      let json_of_order order =
        `List (map (fun (n, ocgs) -> `Assoc [("Name", opt string n); ("OCGs", list int ocgs)]) order)
      in
      let json_of_usage_application_dictionary d =
        `Assoc
           [("Event", json_of_event d.Pdfocg.ocg_event);
            ("OCGs", list int d.Pdfocg.ocg_ocgs);
            ("Category", list string d.Pdfocg.ocg_category)]
      in
      let json_of_config c =
        `Assoc
           [("Name", opt string c.Pdfocg.ocgconfig_name);
            ("Creator", opt string c.Pdfocg.ocgconfig_creator);
            ("BaseState", json_of_state c.Pdfocg.ocgconfig_basestate);
            ("ON", list int c.Pdfocg.ocgconfig_on);
            ("OFF", list int c.Pdfocg.ocgconfig_off);
            ("Intent", list string c.Pdfocg.ocgconfig_intent);
            ("AS", list json_of_usage_application_dictionary c.Pdfocg.ocgconfig_usage_application_dictionaries);
            ("Order", opt json_of_order c.Pdfocg.ocgconfig_order);
            ("ListMode", json_of_listmode c.Pdfocg.ocgconfig_listmode);
            ("RBGroups", opt (list (list int)) c.Pdfocg.ocgconfig_rbgroups);
            ("Locked", list int c.Pdfocg.ocgconfig_locked)]
      in
      let json_of_usage u =
        `Assoc
           [("CreatorInfo-Creator", opt string (match u.Pdfocg.ocg_creatorinfo with None -> None | Some (a, _) -> Some a));
            ("CreatorInfo-Subtype", opt string (match u.Pdfocg.ocg_creatorinfo with None -> None | Some (_, b) -> Some b));
            ("Language-Language", opt string (match u.Pdfocg.ocg_language with None -> None | Some (l, _) -> Some l));
            ("Language-Preferred", opt string (match u.Pdfocg.ocg_language with None -> None | Some (_, o) -> o));
            ("Export", opt string u.Pdfocg.ocg_export);
            ("Zoom-Min", opt float u.Pdfocg.ocg_zoom_min);
            ("Zoom-Max", opt float u.Pdfocg.ocg_zoom_max);
            ("Print-Subtype", opt string u.Pdfocg.ocg_print_subtype);
            ("Print-PrintState", opt string u.Pdfocg.ocg_print_printstate);
            ("ViewState", opt string u.Pdfocg.ocg_viewstate);
            ("User-Type", opt string (match u.Pdfocg.ocg_user with None -> None | Some (a, _) -> Some a));
            ("User-Name", opt (list string) (match u.Pdfocg.ocg_user with None -> None | Some (_, b) -> Some b));
            ("PageElement-Subtype", opt string u.Pdfocg.ocg_page_element_subtype)]
      in
      let json_of_ocg o =
        `Assoc
           [("Name", string o.Pdfocg.ocg_name);
            ("Intent", list string o.Pdfocg.ocg_intent);
            ("Usage", opt json_of_usage o.Pdfocg.ocg_usage)]
      in
        `Assoc
           [("OCGs", `Assoc (map (fun (i, o) -> (string_of_int i, json_of_ocg o)) ocg.ocgs));
            ("Default", json_of_config ocg.ocg_default_config);
            ("Configs", list json_of_config ocg.ocg_configs)]

let ocg_list json pdf =
  if json then flprint (Cpdfyojson.Safe.pretty_to_string (ocg_list_json pdf)) else
    List.iter (Printf.printf "%s\n") (map Pdftext.utf8_of_pdfdocstring (ocg_get_list pdf))

let ocg_read_usage json =
  match json with
  | `Null -> None
  | `Assoc l ->
      begin match sort compare l with 
      | [("CreatorInfo-Creator", cc);
         ("CreatorInfo-Subtype", cs);
         ("Export", e);
         ("Language-Language", ll);
         ("Language-Preferred", lp);
         ("PageElement-Subtype", pes);
         ("Print-PrintState", pp);
         ("Print-Subtype", ps);
         ("User-Name", un);
         ("User-Type", ut);
         ("ViewState", vs);
         ("Zoom-Max", zmax);
         ("Zoom-Min", zmin)] ->
           Some
             {Pdfocg.ocg_creatorinfo =
                begin match cc, cs with
                | `Null, `Null -> None
                | `String a, `String b -> Some (a, b)
                | _ -> error "ocg_read_usage: malformed creator info"
                end;
              Pdfocg.ocg_language =
                begin match ll, lp with
                | `Null, `Null -> None
                | `String a, `String b -> Some (a, Some b)
                | `String a, `Null -> Some (a, None)
                | _ -> error "ocg_read_usage: malformed language"
                end;
              Pdfocg.ocg_export =
                begin match e with `Null -> None | `String s -> Some s | _ -> error "ocg_read_usage: malformed export" end;
              Pdfocg.ocg_zoom_min =
                begin match zmin with `Null -> None | `Float s -> Some s | _ -> error "ocg_read_usage: malformed zoom min" end;
              Pdfocg.ocg_zoom_max =
                begin match zmax with `Null -> None | `Float s -> Some s | _ -> error "ocg_read_usage: malformed zoom max" end;
              Pdfocg.ocg_print_subtype =
                begin match ps with `Null -> None | `String s -> Some s | _ -> error "ocg_read_usage: malformed print subtype" end;
              Pdfocg.ocg_print_printstate =
                begin match pp with `Null -> None | `String s -> Some s | _ -> error "ocg_read_usage: malformed print printstate" end;
              Pdfocg.ocg_viewstate =
                begin match vs with `Null -> None | `String s -> Some s | _ -> error "ocg_read_usage: malformed print viewstate" end;
              Pdfocg.ocg_user =
                begin match ut, un with
                | `Null, `Null -> None
                | `String a, `List b -> Some (a, map (function `String s -> s | _ -> error "ocg_read_usage: malformed user") b)
                | _ -> error "ocg_read_usage: malformed user"
                end;
              Pdfocg.ocg_page_element_subtype =
                begin match vs with `Null -> None | `String s -> Some s | _ -> error "ocg_read_usage: malformed subtype" end}
      | _ -> error "ocg_read_usage: malformed"
      end
  | _ -> error "ocg_read_usage: malformed"

let rec ocg_read_config json =
  match json with
  | `Assoc l ->
      begin match sort compare l with
      | [("AS", a);
         ("BaseState", bs);
         ("Creator", c);
         ("Intent", i);
         ("ListMode", lm);
         ("Locked", l);
         ("Name", n);
         ("OFF", off);
         ("ON", on);
         ("Order", o);
         ("RBGroups", rb)] ->
           {Pdfocg.ocgconfig_name =
              begin match n with `Null -> None | `String s -> Some s | _ -> error "ocg_read_config: malformed name" end;
            Pdfocg.ocgconfig_creator =
              begin match c with `Null -> None | `String s -> Some s | _ -> error "ocg_read_config: malformed creator" end;
            Pdfocg.ocgconfig_basestate =
              begin match bs with
              | `String "/ON" -> Pdfocg.OCG_ON
              | `String "/OFF" -> Pdfocg.OCG_OFF
              | `String "/Unchanged" -> Pdfocg.OCG_Unchanged
              | _ -> error "ocg_read_config: malformed base state"
              end;
            Pdfocg.ocgconfig_on =
              begin match on with
              | `List l -> map (function `Int i -> i | _ -> error "ocg_read_config: malformed on") l
              | _ -> error "ocg_read_config: malformed on"
              end;
            Pdfocg.ocgconfig_off =
              begin match off with
              | `List l -> map (function `Int i -> i | _ -> error "ocg_read_config: malformed off") l
              | _ -> error "ocg_read_config: malformed off"
              end;
            Pdfocg.ocgconfig_intent =
              begin match off with
              | `List l -> map (function `String i -> i | _ -> error "ocg_read_config: malformed intent") l
              | _ -> error "ocg_read_config: malformed intent"
              end;
            Pdfocg.ocgconfig_usage_application_dictionaries =
              begin match a with
              | `List l -> option_map ocg_read_appdict l
              | _ -> error "ocg_read_config: malformed usage application dictionary list"
              end;
            Pdfocg.ocgconfig_order =
              begin match o with
              | `Null -> None
              | `List pairs ->
                  let extracted_pairs =
                    map
                      (function
                       | `Assoc pair ->
                            begin match sort compare pair with
                            | [("Name", name); ("OCGs", ocgs)] ->
                                  let stringopt =
                                    match name with
                                    | `Null -> None
                                    | `String s -> Some s
                                    | _ -> error "ocg_read_config: malformed order name"
                                  in
                                  let intlist =
                                    match ocgs with
                                    | `List l -> map (function `Int i -> i | _ -> error "ocg_read_config: malformed order ocg") l
                                    | _ -> error "ocg_read_config: malformed order ocg"
                                  in
                                    (stringopt, intlist)
                            | _ -> error "ocg_read_config: malformed order pair"
                            end
                       | _ -> error "ocg_read_config: malformed order pair")
                      pairs
                  in
                    Some extracted_pairs
              | _ -> error "ocg_read_config: malformed order"
              end;
            Pdfocg.ocgconfig_listmode =
              begin match lm with
              | `String "/AllPages" -> Pdfocg.OCG_AllPages
              | `String "/VisiblePages" -> Pdfocg.OCG_VisiblePages
              | _ -> error "ocg_read_config: malformed list mode"
              end;
            Pdfocg.ocgconfig_rbgroups =
              begin match rb with
              | `Null -> None
              | `List ls ->
                  Some
                    (map
                       (function
                        | `List l -> map (function `Int i -> i | _ -> error "ocg_read_config: bad rbgroups") l
                        | _ -> error "ocg_read_config: bad rbgroups")
                       ls)
              | _ -> error "ocg_read_config: bad rbgroups"
              end;
            Pdfocg.ocgconfig_locked =
              begin match l with
              | `List l -> map (function `Int i -> i | _ -> error "ocg_read_config: malformed locked") l
              | _ -> error "ocg_read_config: malformed locked"
              end}
      | _ -> error "ocg_read_config: malformed"
      end
  | _ -> error "ocg_read_config: malformed"

and ocg_read_appdict json =
  match json with
  | `Null -> None
  | `Assoc a ->
      begin match sort compare a with
      | [("Category", `List categories);
         ("Event", `String event);
         ("OCGs", `List ocgs)] ->
          Some
            {Pdfocg.ocg_event =
              begin match event with
              | "/View" -> Pdfocg.OCG_View
              | "/Print" -> Pdfocg.OCG_Print
              | "/Export" -> Pdfocg.OCG_Export
              | _ -> error "ocg_read_appdict: bad event"
              end;
             Pdfocg.ocg_ocgs = []; (*map fst (map ocg_read_ocg_with_num ocgs);*) (* FIXME FIXME FIXME *)
             Pdfocg.ocg_category = map (function `String s -> s | _ -> error "ocg_read_ocg: bad category") categories}
      | _ ->
         error "ocg_read_appdict: malformed"
      end
  | _ -> error "ocg_read_appdict: malformed"

and ocg_read_ocg = function
  | `Assoc l ->
      begin match sort compare l with
      | [("Intent", `List intents);
         ("Name", `String name);
         ("Usage", usage)] ->
            {Pdfocg.ocg_name = name;
             Pdfocg.ocg_intent = (map (function `String s -> s | _ -> error "ocg_read_ocg: malformed intent") intents);
             Pdfocg.ocg_usage = ocg_read_usage usage}
      | _ -> error "ocg_read_ocg: malformed"
      end
  | _ -> error "ocg_read_ocg: malformed" 

and ocg_read_ocg_with_num = function
  | (ocg_number_as_string, ocg) ->
         (int_of_string ocg_number_as_string, ocg_read_ocg ocg)
  | _ -> error "ocg_read_ocg_with_num: malformed"
   
let ocg_read_json json =
  match json with
  | `Null -> None
  | `Assoc a ->
      begin match sort compare a with
      | [("Configs", `List configs);
         ("Default", default);
         ("OCGs", `Assoc ocgs)] ->
             Some
               {Pdfocg.ocgs = map ocg_read_ocg_with_num ocgs;
                Pdfocg.ocg_default_config = ocg_read_config default;
                Pdfocg.ocg_configs = map ocg_read_config configs}
      | _ -> error "ocg_read_json: malformed JSON top-level dictionary"
      end
  | _ -> error "ocg_read_json: malformed JSON top-level"

let ocg_replace filename pdf =
  let json = Cpdfyojson.Safe.from_string (contents_of_file filename) in
    match ocg_read_json json with
    | None -> ()
    | Some ocg -> Pdfocg.write_ocg pdf ocg

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
