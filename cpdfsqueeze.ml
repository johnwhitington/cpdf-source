open Pdfutil
open Pdfio

(* For debugging *)
(*let report_pdf_size pdf =
  Pdf.remove_unreferenced pdf;
  Pdfwrite.pdf_to_file_options ~preserve_objstm:false ~generate_objstm:false
  ~compress_objstm:false None false pdf "temp.pdf";
  let fh = open_in_bin "temp.pdf" in
    Printf.printf "Size %i bytes\n" (in_channel_length fh);
    flush stdout;
    close_in fh*)

(* Recompress anything which isn't compressed (or compressed with old-fashioned
   mechanisms), unless it's metadata. *)

(* TODO The use of this function in cpdfcommand.ml actually takes some power
   away from the user - maybe they don't want old-fashioned stuff
   re-compressed, but only uncompressed data compressed. Consider adding a flag
   -recompress-only-uncompressed and an argument to this function. *)
let recompress_stream pdf = function
  (* If there is no compression, or bad compression with /FlateDecode *)
  | Pdf.Stream {contents = (dict, _)} as stream ->
      begin match
        Pdf.lookup_direct pdf "/Filter" dict, 
        Pdf.lookup_direct pdf "/Type" dict
      with
      | _, Some (Pdf.Name "/Metadata") -> ()
      | (  None
         | Some (Pdf.Name ("/ASCIIHexDecode" | "/ASCII85Decode" | "/LZWDecode" | "/RunLengthDecode"))
         | Some (Pdf.Array []
         | Pdf.Array (Pdf.Name ("/ASCIIHexDecode" | "/ASCII85Decode" | "/LZWDecode" | "/RunLengthDecode")::_)
        )), _ ->
           begin try Pdfcodec.decode_pdfstream_until_unknown pdf stream with _ -> Pdfe.log "Warning: Skipping re-encoding of a stream\n" end;
           Pdfcodec.encode_pdfstream ~only_if_smaller:true pdf Pdfcodec.Flate stream
      | _ -> ()
      end
  | _ -> assert false

let recompress_pdf pdf =
  if not (Pdfcrypt.is_encrypted pdf) then
    Pdf.iter_stream (recompress_stream pdf) pdf;
    pdf

let decompress_pdf pdf =
  if not (Pdfcrypt.is_encrypted pdf) then
    (Pdf.iter_stream (Pdfcodec.decode_pdfstream_until_unknown pdf) pdf);
    pdf

(* Equality on PDF objects *)
let pdfobjeq pdf x y =
  let x = Pdf.lookup_obj pdf x 
  and y = Pdf.lookup_obj pdf y in
    begin match x with Pdf.Stream _ -> Pdf.getstream x | _ -> () end;
    begin match y with Pdf.Stream _ -> Pdf.getstream y | _ -> () end;
    compare x y

let really_squeeze pdf =
  let objs = ref [] in
    Pdf.objiter (fun objnum _ -> objs := objnum :: !objs) pdf;
    let toprocess =
      keep
        (fun x -> length x > 1)
        (collate (pdfobjeq pdf) (sort (pdfobjeq pdf) !objs))
    in
      (* Remove any pools of objects which are page objects, since Adobe Reader
       * gets confused when there are duplicate page objects. *)
      let toprocess =
        option_map
          (function
             [] -> assert false
           | h::_ as l ->
               match Pdf.lookup_direct pdf "/Type" (Pdf.lookup_obj pdf h) with
                 Some (Pdf.Name "/Page") -> None
               | _ -> Some l)
          toprocess
      in
        let pdfr = ref pdf in
        let changetable = Hashtbl.create 100 in
          iter
            (function [] -> assert false | h::t ->
               iter (fun e -> Hashtbl.add changetable e h) t)
            toprocess;
          pdfr := Pdf.renumber changetable !pdfr;
          Pdf.remove_unreferenced !pdfr;
          pdf.Pdf.root <- !pdfr.Pdf.root;
          pdf.Pdf.objects <- !pdfr.Pdf.objects;
          pdf.Pdf.trailerdict <- !pdfr.Pdf.trailerdict

(* Squeeze the form xobject at objnum.

For old PDFs (< v1.2) any resources from the page (or its ancestors in
the page tree!) are also needed - we must merge them with the ones from the
xobject itself. However, it it safe for now -- in the unlikely event that the
resources actually need to be available, the parse will fail, the squeeze of
this object will fail, and we bail out. *)
let xobjects_done = ref []

let rec squeeze_form_xobject pdf objnum =
  if mem objnum !xobjects_done then () else
    begin
      xobjects_done := objnum :: !xobjects_done;
      let obj = Pdf.lookup_obj pdf objnum in
        begin match Pdf.lookup_chain pdf obj ["/Resources"; "/XObject"] with
        | Some (Pdf.Dictionary d) ->
            iter (function (k, Pdf.Indirect i) -> squeeze_form_xobject pdf i | _ -> ()) d
        | _ -> ()
        end;
        match Pdf.lookup_direct pdf "/Subtype" obj with
          Some (Pdf.Name "/Form") ->
            let resources =
              match Pdf.lookup_direct pdf "/Resources" obj with
                Some d -> d
              | None -> Pdf.Dictionary []
            in
              begin match
                Pdfops.stream_of_ops
                  (Pdfops.parse_operators pdf resources [Pdf.Indirect objnum])
              with
                Pdf.Stream {contents = (_, Pdf.Got data)} ->
                  (* Put replacement data in original stream, and overwrite /Length *)
                  begin match obj with
                    Pdf.Stream ({contents = (d, _)} as str) ->
                      str :=
                        (Pdf.add_dict_entry d "/Length" (Pdf.Integer (bytes_size data)),
                         Pdf.Got data)
                  | _ -> failwith "squeeze_form_xobject"
                  end
              | _ -> failwith "squeeze_form_xobject"
              end
        | _ -> ()
    end

(* For a list of indirects representing content streams, make sure that none of
them are duplicated in the PDF. This indicates sharing, which parsing and
rewriting the streams might destroy, thus making the file bigger. *)
let no_duplicates content_stream_numbers stream_numbers =
  not
    (mem false
       (map
         (fun n -> length (keep (eq n) content_stream_numbers) < 2)
         stream_numbers))

(* Give a list of content stream numbers, given a page reference number *)
let content_streams_of_page pdf refnum =
  match Pdf.direct pdf (Pdf.lookup_obj pdf refnum) with
    Pdf.Dictionary dict ->
      begin match lookup "/Contents" dict with
        Some (Pdf.Indirect i) -> [i]
      | Some (Pdf.Array x) ->
          option_map (function Pdf.Indirect i -> Some i | _ -> None) x
      | _ -> []
      end
  | _ -> []

(* For each object in the PDF marked with /Type /Page, for each /Contents
indirect reference or array of such, decode and recode that content stream. *)
let squeeze_all_content_streams pdf =
  let page_reference_numbers = Pdf.page_reference_numbers pdf in
    let all_content_streams_in_doc =
      flatten (map (content_streams_of_page pdf) page_reference_numbers)
    in
      xobjects_done := [];
      Pdf.objiter
        (fun objnum _ ->
          match Pdf.lookup_obj pdf objnum with
            Pdf.Dictionary dict as d
              when
                Pdf.lookup_direct pdf "/Type" d = Some (Pdf.Name "/Page")
              ->
                let resources =
                  match Pdf.lookup_direct pdf "/Resources" d with
                    Some d -> d
                  | None -> Pdf.Dictionary []
                in
                  begin try
                    let content_streams =
                      match lookup "/Contents" dict with
                        Some (Pdf.Indirect i) ->
                          begin match Pdf.direct pdf (Pdf.Indirect i) with
                            Pdf.Array x -> x
                          | _ -> [Pdf.Indirect i]
                          end
                      | Some (Pdf.Array x) -> x
                      | _ -> raise Not_found
                    in
                      if
                        no_duplicates
                          all_content_streams_in_doc
                          (map (function Pdf.Indirect i -> i | _ -> assert false) content_streams)
                      then
                        let newstream =
                          Pdfops.stream_of_ops
                            (Pdfops.parse_operators pdf resources content_streams)
                        in
                          let newdict =
                            Pdf.add_dict_entry
                              d "/Contents" (Pdf.Indirect (Pdf.addobj pdf newstream))
                          in
                            Pdf.addobj_given_num pdf (objnum, newdict);
                            (* Now process all xobjects related to this page *)
                            begin match Pdf.lookup_direct pdf "/XObject" resources with
                              Some (Pdf.Dictionary xobjs) ->
                                iter
                                  (function
                                     (_, Pdf.Indirect i) -> squeeze_form_xobject pdf i
                                    | _ -> failwith "squeeze_xobject")
                                  xobjs
                            | _ -> ()
                            end
                  with
                    (* No /Contents, which is ok. Or a parsing failure due to
                     uninherited resources. FIXME: Add support for inherited
                     resources. NB 24th March 2023 we tried this, and sizes went up
                     on many files and down on none! So reverted. *)
                    Not_found -> ()
                  end
            | _ -> ())
        pdf

(* We run squeeze enough times for the number of objects to not change *)
let squeeze ?logto ?(pagedata=true) pdf =
  let log x =
    match logto with
      None -> print_string x; flush stdout
    | Some "nolog" -> ()
    | Some s ->
        let fh = open_out_gen [Open_wronly; Open_creat] 0o666 s in
          seek_out fh (out_channel_length fh);
          output_string fh x;
          close_out fh
  in
    try
      let n = ref (Pdf.objcard pdf) in
      log (Printf.sprintf "Beginning squeeze: %i objects\n" (Pdf.objcard pdf));
      while !n > (ignore (really_squeeze pdf); Pdf.objcard pdf) do
        n := Pdf.objcard pdf;
        log (Printf.sprintf "Squeezing... Down to %i objects\n" (Pdf.objcard pdf));
      done;
      if pagedata then
        begin
          log (Printf.sprintf "Squeezing page data and xobjects\n");
          squeeze_all_content_streams pdf;
        end;
        log (Printf.sprintf "Recompressing document\n");
        ignore (recompress_pdf pdf);
    with
      e ->
        raise
          (Pdf.PDFError
             (Printf.sprintf
                "Squeeze failed. No output written.\n Proximate error was:\n %s"
                (Printexc.to_string e)))
