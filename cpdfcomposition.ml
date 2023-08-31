open Pdfutil

let find_composition_structure_info pdf marked =
  match Pdf.lookup_obj pdf pdf.Pdf.root with
  | Pdf.Dictionary d ->
      begin match lookup "/StructTreeRoot" d with
      | Some x ->
          let l = ref [] in
          let objs = Pdf.objects_referenced ["/Pg"] [] pdf x in
            iter
              (fun i ->
                 match Hashtbl.find marked i with
                 | () -> ()
                 | exception Not_found -> l := i::!l; Hashtbl.add marked i ())
              objs;
            !l
      | _ -> []
      end
  | _ -> []

let find_composition_images pdf i obj marked =
  match Hashtbl.find marked i with () -> [] | exception Not_found -> 
  match Pdf.lookup_direct pdf "/Subtype" obj with
  | Some (Pdf.Name "/Image") ->
      Hashtbl.add marked i (); [i]
  | _ -> []

(* If it has /Font, find all objects referenced from it, and add
any not already marked to the count *)
let find_composition_fonts pdf i obj marked =
  match Hashtbl.find marked i with () -> [] | exception Not_found -> 
  let l = ref [] in
  match Pdf.lookup_direct pdf "/Type" obj with
  | Some (Pdf.Name "/Font") ->
      iter
        (fun i ->
           (*Printf.printf "Object %i\n%s\n" i (Pdfwrite.string_of_pdf (Pdf.lookup_obj pdf i));*)
           match Hashtbl.find marked i with
           | () -> ()
           | exception Not_found -> l := i::!l; Hashtbl.add marked i ())
        (Pdf.objects_referenced [] [] pdf (Pdf.Indirect i));
     !l
  | _ -> []

let find_composition_content_streams pdf i obj marked =
  match Hashtbl.find marked i with () -> [] | exception Not_found -> 
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
        let l = ref [] in
          iter
            (fun i ->
              (*Printf.printf "Considering content stream %i\n" i;*)
              match Hashtbl.find marked i with
              | () -> ()
              | exception Not_found -> Hashtbl.add marked i (); l := i::!l)
            cs;
          !l
  | _ ->
      match Pdf.lookup_direct pdf "/Subtype" obj with
      | Some (Pdf.Name "/Form") ->
          Hashtbl.add marked i ();
          [i]
      | _ -> []

let find_composition pdf =
  let marked = null_hash () in
  let images = ref [] in
  let fonts = ref [] in
  let content_streams = ref [] in
    Pdf.objiter
      (fun i obj ->
        (*Printf.printf "Looking at object %i\n" i;
        Printf.printf "Which is %s\n" (Pdfwrite.string_of_pdf (Pdf.lookup_obj pdf i));
        Printf.printf "Marked objects at beginning: ";
        Hashtbl.iter (fun k () -> Printf.printf "%i " k) marked;
        Printf.printf "\n";*)
         match Hashtbl.find marked i with _ -> () | exception Not_found ->
           images := find_composition_images pdf i obj marked @ !images;
           content_streams := find_composition_content_streams pdf i obj marked @ !content_streams;
           fonts := find_composition_fonts pdf i obj marked @ !fonts)
      pdf;
    let structure_info = find_composition_structure_info pdf marked in
    (!images, !fonts, !content_streams, structure_info)

let size pdf i =
  String.length (Pdfwrite.string_of_pdf_including_data (Pdf.lookup_obj pdf i))

let compressed_size pdf objnums =
  if objnums = [] then 0 else
  (* If there were object streams, assume objects were in them, and compressed with FlateDecode *)
  if Hashtbl.length pdf.Pdf.objects.Pdf.object_stream_ids = 0 then
    sum (map (size pdf) (setify objnums))
  else
    let b = Buffer.create 262144 in
    let streams = ref 0 in
      iter
        (fun i ->
           match Pdf.lookup_obj pdf i with
           | Pdf.Stream _ -> streams += size pdf i
           | obj -> Buffer.add_string b (Pdfwrite.string_of_pdf_including_data obj))
         objnums;
      !streams + Pdfio.bytes_size (Pdfcodec.encode_flate (Pdfio.bytes_of_string (Buffer.contents b)))

(* If no object streams, calculate the size of the xref table. If streams, the xref stream total *)
let compressed_xref_table_size pdf = 
  if Hashtbl.length pdf.Pdf.objects.Pdf.object_stream_ids = 0 then 20 * Pdf.objcard pdf else
    compressed_size pdf (map fst (list_of_hashtbl pdf.Pdf.objects.Pdf.object_stream_ids))

let show_composition_json filesize pdf =
  let perc x = float_of_int x /. float_of_int filesize *. 100. in
  let o_images, o_fonts, o_content_streams, o_structure_info = find_composition pdf in
  let images, fonts, content_streams, structure_info, attached_files, xref_table =
      compressed_size pdf o_images,
      compressed_size pdf o_fonts,
      compressed_size pdf o_content_streams,
      compressed_size pdf o_structure_info,
      Cpdfattach.size_attached_files pdf,
      compressed_xref_table_size pdf
  in
  let r = images + fonts + content_streams + structure_info + attached_files + xref_table in
    `List [`Tuple [`String "Images"; `Int images; `Float (perc images)];
           `Tuple [`String "Fonts"; `Int fonts; `Float (perc fonts)];
           `Tuple [`String "Content streams"; `Int content_streams; `Float (perc content_streams)];
           `Tuple [`String "Structure Info"; `Int structure_info; `Float (perc structure_info)];
           `Tuple [`String "Attached Files"; `Int attached_files; `Float (perc attached_files)];
           `Tuple [`String "XRef Table"; `Int xref_table; `Float (perc xref_table)];
           `Tuple [`String "Unclassified"; `Int (filesize - r); `Float (perc (filesize - r))]]

let show_composition_json_blob filesize pdf =
  Pdfio.bytes_of_string (Cpdfyojson.Safe.pretty_to_string (show_composition_json filesize pdf))

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
