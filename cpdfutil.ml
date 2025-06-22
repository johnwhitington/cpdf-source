open Pdfutil

let progress = ref false

let progress_line s =
  if !progress then Printf.eprintf "%s\n%!" s

let progress_line_no_end s =
  if !progress then Printf.eprintf "%s%!" s

let progress_page n =
  if !progress then Printf.eprintf "%i%!" n

let progress_endpage () =
  if !progress then Printf.eprintf ".%!"

let progress_done () =
  if !progress then Printf.eprintf "\n%!"

let rec dict_entry_single_object f pdf = function
  | (Pdf.Dictionary d) -> f (Pdf.recurse_dict (dict_entry_single_object f pdf) d)
  | (Pdf.Stream {contents = (Pdf.Dictionary dict, data)}) ->
      f (Pdf.Stream {contents = (Pdf.recurse_dict (dict_entry_single_object f pdf) dict, data)})
  | Pdf.Array a -> Pdf.recurse_array (dict_entry_single_object f pdf) a
  | x -> x

(* FIXME are we sure that functional values can never appear in the equality here? *)
let remove_dict_entry pdf key search =
  let f d =
    match search with
    | None -> Pdf.remove_dict_entry d key
    | Some s ->
        match Pdf.lookup_direct pdf key d with
        | Some v when v = s -> Pdf.remove_dict_entry d key
        | _ -> d
  in
    Pdf.objselfmap (dict_entry_single_object f pdf) pdf;
    pdf.Pdf.trailerdict <- dict_entry_single_object f pdf pdf.Pdf.trailerdict

let replace_dict_entry pdf key value search =
  let f d =
    match search with
    | None -> begin try Pdf.replace_dict_entry d key value with _ -> d end
    | Some s ->
        match Pdf.lookup_direct pdf key d with
        | Some v when v = s -> Pdf.replace_dict_entry d key value
        | _ -> d
  in
    Pdf.objselfmap (dict_entry_single_object f pdf) pdf;
    pdf.Pdf.trailerdict <- dict_entry_single_object f pdf pdf.Pdf.trailerdict

let injectible = function '&' | '|' | '\n' | '`' | '$' -> true | _ -> false

let check_injectible s =
  if List.exists injectible (explode s) then
    begin
      Pdfe.log "Insecure character in path name. Exiting\n";
      exit 2
    end
