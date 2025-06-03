open Pdfutil

(* Empty any /JS string, Empty any /URI (javascript:). *)
let remove_javascript pdf = 
  let rec remove_javascript_single_object f pdf = function
    | (Pdf.Dictionary d) -> f (Pdf.recurse_dict (remove_javascript_single_object f pdf) d)
    | (Pdf.Stream {contents = (Pdf.Dictionary dict, data)}) ->
        f (Pdf.Stream {contents = (Pdf.recurse_dict (remove_javascript_single_object f pdf) dict, data)})
    | Pdf.Array a -> Pdf.recurse_array (remove_javascript_single_object f pdf) a
    | x -> x
  in
  let f d =
    let d =
      match Pdf.lookup_direct pdf "/JS" d with
      | Some _ -> Pdf.add_dict_entry d "/JS" (Pdf.String "")
      | None -> d
    in
      match Pdf.lookup_direct pdf "/URI" d with
      | Some (Pdf.String s) when String.length s >= 11 && String.sub s 0 11 = "javascript:" -> Pdf.add_dict_entry d "/URI" (Pdf.String "")
      | _ -> d
  in
    Pdf.objselfmap (remove_javascript_single_object f pdf) pdf;
    ignore (Pdf.remove_chain pdf ["/Root"; "/Names"; "/JavaScript"])

(* Any dictionary with /S /JavaScript or any /URI (javascript:...) *) 
let contains_javascript pdf =
  let found = ref false in
  let rec contains_javascript_single_object f pdf = function
    | (Pdf.Dictionary d) -> f (Pdf.recurse_dict (contains_javascript_single_object f pdf) d)
    | (Pdf.Stream {contents = (Pdf.Dictionary dict, data)}) ->
        f (Pdf.Stream {contents = (Pdf.recurse_dict (contains_javascript_single_object f pdf) dict, data)})
    | Pdf.Array a -> Pdf.recurse_array (contains_javascript_single_object f pdf) a
    | x -> x
  in
  let f d =
    begin match Pdf.lookup_direct pdf "/S" d with
    | Some (Pdf.String "/JavaScript") -> set found
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/URI" d with
    | Some (Pdf.String s) when String.length s >= 11 && String.sub s 0 11 = "javascript:" -> set found; d
    | _ -> d
    end
  in
    Pdf.objiter (fun _ obj -> ignore (contains_javascript_single_object f pdf obj)) pdf;
  (* Any /Root -> /Names -> /JavaScript *)
  begin match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/Names"; "/JavaScript"] with
  | Some _ -> set found
  | None -> ()
  end;
  !found
