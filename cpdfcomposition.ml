open Pdfutil

let size pdf i =
  String.length (Pdfwrite.string_of_pdf_including_data (Pdf.lookup_obj pdf i))

(* FIXME Add soft masks *)
let find_composition_images pdf i obj marked =
  match Hashtbl.find marked i with () -> 0 | exception Not_found -> 
  match Pdf.lookup_direct pdf "/Subtype" obj with
  | Some (Pdf.Name "/Image") ->
      Hashtbl.add marked i ();
      String.length (Pdfwrite.string_of_pdf_including_data obj)
  | _ -> 0

(* If it has /Font, find all objects referenced from it, and add
any not already marked to the count *)
let find_composition_fonts pdf i obj marked =
  match Hashtbl.find marked i with () -> 0 | exception Not_found -> 
  let l = ref 0 in
  match Pdf.lookup_direct pdf "/Type" obj with
  | Some (Pdf.Name "/Font") ->
      iter
        (fun i ->
           (*Printf.printf "Object %i\n%s\n" i (Pdfwrite.string_of_pdf (Pdf.lookup_obj pdf i));*)
           match Hashtbl.find marked i with
           | () -> ()
           | exception Not_found -> l += size pdf i; Hashtbl.add marked i ())
        (Pdf.objects_referenced [] [] pdf (Pdf.Indirect i));
     !l
  | _ -> 0

(* FIXME: Add xobjects *)
let find_composition_content_streams pdf i obj marked =
  match Hashtbl.find marked i with () -> 0 | exception Not_found -> 
  match Pdf.lookup_direct pdf "/Type" obj with
  | Some (Pdf.Name "/Page") ->
      (*Printf.printf "Found a page...%s\n" (Pdfwrite.string_of_pdf (Pdf.direct pdf obj));*)
      let cs =
        match obj with Pdf.Dictionary d -> 
          begin match lookup "/Contents" d with
          | Some (Pdf.Indirect i) -> [i]
          | Some (Pdf.Array is) -> option_map (function Pdf.Indirect i -> Some i | _ -> None) is
          | _ -> []
          end
        | _ -> []
      in
        (*Printf.printf "Found %i content streams\n" (length cs);*)
        let l = ref 0 in
          iter
            (fun i ->
              (*Printf.printf "Considering content stream %i\n" i;*)
              match Hashtbl.find marked i with
              | () -> ()
              | exception Not_found -> Hashtbl.add marked i (); l += size pdf i)
            cs;
          !l
  | _ -> 0

let find_composition_structure_info pdf i obj marked = 0

let find_composition_link_annotations pdf i obj marked = 0

let find_composition_embedded_files pdf i obj marked = 0

let find_composition pdf =
  let marked = null_hash () in
  let images = ref 0 in
  let fonts = ref 0 in
  let content_streams = ref 0 in
  let structure_info = ref 0 in
  let link_annotations = ref 0 in
  let embedded_files = ref 0 in
    Pdf.objiter
      (fun i obj ->
        (*Printf.printf "Marked objects at beginning: ";
        Hashtbl.iter (fun k () -> Printf.printf "%i " k) marked;
        Printf.printf "\n";*)
         match Hashtbl.find marked i with _ -> () | exception Not_found ->
           (*embedded_files += find_composition_embedded_files pdf i obj marked;
           images += find_composition_images pdf i obj marked;*)
           content_streams += find_composition_content_streams pdf i obj marked;
           (*structure_info += find_composition_structure_info pdf i obj marked;
           link_annotations += find_composition_link_annotations pdf i obj marked;*)
           fonts += find_composition_fonts pdf i obj marked)

      pdf;
    (!images, !fonts, !content_streams, !structure_info, !link_annotations, !embedded_files)

(* First go: images, fonts, content streams, structure info, link annotations, embedded files *)
let show_composition_json filesize pdf =
  let perc x = float_of_int x /. float_of_int filesize *. 100. in
  let images, fonts, content_streams, structure_info, link_annotations, embedded_files =
    find_composition pdf
  in
  let r = images + fonts + content_streams + structure_info + link_annotations + embedded_files in
    `List [`Tuple [`String "Images"; `Int images; `Float (perc images)];
           `Tuple [`String "Fonts"; `Int fonts; `Float (perc fonts)];
           `Tuple [`String "Content streams"; `Int content_streams; `Float (perc content_streams)];
           `Tuple [`String "Structure Info"; `Int structure_info; `Float (perc structure_info)];
           `Tuple [`String "Link Annotations"; `Int link_annotations; `Float (perc link_annotations)];
           `Tuple [`String "Embedded Files"; `Int embedded_files; `Float (perc embedded_files)];
           `Tuple [`String "Unclassified"; `Int (filesize - r); `Float (perc (filesize - r))]]

let show_composition filesize json pdf =
  let module J = Cpdfyojson.Safe in
  let j = show_composition_json filesize pdf in
  if json then (flprint (J.pretty_to_string j); flprint "\n") else
    match j with
    | `List js ->
        iter
          (function
           | `Tuple [`String a; `Int b; `Float c] -> Printf.printf "%s: %i bytes (%.2f%%)\n" a b c
           | _ -> ())
        js
    | _ -> ()
