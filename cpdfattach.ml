open Pdfutil
open Pdfio
open Cpdferror

(* Attaching files *)
let attach_file ?memory keepversion topage pdf file =
  let data =
    match memory with
      Some data -> data
    | None ->
        let ch = open_in_bin file in
        let len = in_channel_length ch in
        let stream = mkbytes len in
        let i = input_of_channel ch in
          setinit i stream 0 len;
          close_in ch;
          stream
  in
    let filestream =
      Pdf.Stream
        (ref (Pdf.Dictionary
               [("/Length", Pdf.Integer (bytes_size data));
                ("/Type", Pdf.Name "/EmbeddedFile");
                ("/Params",
                   Pdf.Dictionary
                     [("/Size", Pdf.Integer (bytes_size data));
                      ("/CheckSum", Pdf.String (Digest.string (string_of_bytes data)))
                     ])],
              Pdf.Got data))
    in
      let filestream_num = Pdf.addobj pdf filestream in
      let basename = Pdftext.pdfdocstring_of_utf8 (Filename.basename file) in
        let filespec =
          Pdf.Dictionary
            [("/EF", Pdf.Dictionary ["/F", Pdf.Indirect filestream_num]);
             ("/F", Pdf.String basename);
             ("/Type", Pdf.Name "/Filespec");
             ("/Desc", Pdf.String "");
             ("/UF", Pdf.String basename)]
        in
          match topage with
          | None ->
              (* Look up /Names and /EmbeddedFiles and /Names. *)
              let rootdict = Pdf.lookup_obj pdf pdf.Pdf.root in
                let namedict =
                  match Pdf.lookup_direct pdf "/Names" rootdict with
                  | None -> Pdf.Dictionary []
                  | Some namedict -> namedict
                in
                  let embeddednamedict =
                    match Pdf.lookup_direct pdf "/EmbeddedFiles" namedict with
                    | None -> Pdf.Dictionary []
                    | Some embeddednamedict -> embeddednamedict
                  in
                    let elts =
                      match Pdf.lookup_direct pdf "/Names" embeddednamedict with
                      | Some (Pdf.Array elts) -> elts
                      | _ -> []
                    in
                      let filespecobj = Pdf.addobj pdf filespec in
                      let names' = Pdf.Array (elts @ [Pdf.String basename; Pdf.Indirect filespecobj]) in
                      let embeddednamedict' = Pdf.add_dict_entry embeddednamedict "/Names" names' in
                      let namedict' = Pdf.add_dict_entry namedict "/EmbeddedFiles" embeddednamedict' in
                      let rootdict' = Pdf.add_dict_entry rootdict "/Names" namedict' in
                      let rootnum = Pdf.addobj pdf rootdict' in
                        {pdf with
                           Pdf.minor = if keepversion then pdf.Pdf.minor else max pdf.Pdf.minor 4;
                           Pdf.root = rootnum;
                           Pdf.trailerdict =
                             Pdf.add_dict_entry
                               pdf.Pdf.trailerdict "/Root" (Pdf.Indirect rootnum)}
          | Some pagenumber ->
              let pages = Pdfpage.pages_of_pagetree pdf in
                if pagenumber < 0 || pagenumber > length pages then error "attach_file: Page not found" else
                  let page = select pagenumber pages in
                    let annots =
                      match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
                      | Some (Pdf.Array annots) -> annots
                      | _ -> []
                    in
                      let rect =
                        let minx, miny, maxx, maxy = Pdf.parse_rectangle page.Pdfpage.mediabox in
                          Pdf.Array [Pdf.Real 18.; Pdf.Real (maxy -. 45.); Pdf.Real 45.; Pdf.Real (maxy -. 18.)]
                      in
                        let filespecobj = Pdf.addobj pdf filespec in
                        let annot =
                          Pdf.Dictionary
                            [("/FS", Pdf.Indirect filespecobj);
                             ("/Subtype", Pdf.Name "/FileAttachment");
                             ("/Contents", Pdf.String basename);
                             ("/Rect", rect)]
                        in
                          let annots' = Pdf.Array (annot::annots) in
                            let page' =
                              {page with Pdfpage.rest = Pdf.add_dict_entry page.Pdfpage.rest "/Annots" annots'}
                            in
                              let pages' = replace_number pagenumber page' pages in
                                let pdf = Pdfpage.change_pages true pdf pages' in
                                  {pdf with
                                     Pdf.minor = if keepversion then pdf.Pdf.minor else max pdf.Pdf.minor 4}

type attachment =
  {name : string;
   pagenumber : int;
   data : unit -> Pdfio.bytes}

let list_attached_files pdf =
  let toplevel =
    match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
    | None -> []
    | Some rootdict ->
        match Pdf.lookup_direct pdf "/Names" rootdict with
        | None -> []
        | Some namedict ->
            match Pdf.lookup_direct pdf "/EmbeddedFiles" namedict with
            | Some nametree ->
                 map
                   (function (x, ef) ->
                      match Pdf.lookup_direct pdf "/EF" ef with
                      | Some ((Pdf.Dictionary _) as d) ->
                          begin match Pdf.lookup_direct pdf "/F" d with
                          | Some stream ->
                              {name = x;
                               pagenumber = 0;
                               data =
                                 (fun () ->
                                   try
                                     Pdf.getstream stream;
                                     Pdfcodec.decode_pdfstream pdf stream;
                                     match stream with
                                       Pdf.Stream {contents = (_, Pdf.Got data)} -> data
                                     | _ -> raise Not_found
                                   with
                                     _ -> raise (Pdf.PDFError "could not retreive attachment data"))}
                          | None -> raise (Pdf.PDFError "/F not found")
                          end
                      | _ -> raise (Pdf.PDFError "/EF not found"))
                   (option_map
                     (function (Pdf.String s, ef) -> Some (s, ef) | _ -> None)
                     (Pdf.contents_of_nametree pdf nametree))
            | _ -> []
  in let pagelevel =
    let pages = Pdfpage.pages_of_pagetree pdf in
      flatten
        (map2
          (fun page pagenumber ->
             option_map
               (function annot ->
                  match Pdf.lookup_direct pdf "/Subtype" annot with
                  | Some (Pdf.Name "/FileAttachment") ->
                      (match Pdf.lookup_direct pdf "/Contents" annot with
                      | Some (Pdf.String s) ->
                          begin match Pdf.lookup_direct pdf "/FS" annot with
                          | Some ((Pdf.Dictionary _) as d) ->
                              (*Printf.eprintf "%s\n%!" (Pdfwrite.string_of_pdf d);*)
                              begin match Pdf.lookup_direct pdf "/EF" d with
                              |  Some ((Pdf.Dictionary _) as d) ->
                                   begin match Pdf.lookup_direct pdf "/F" d with
                                   | Some stream ->
                                       Some
                                        {name = s;
                                         pagenumber = pagenumber;
                                         data =
                                           (fun () ->
                                             try
                                               Pdf.getstream stream;
                                               Pdfcodec.decode_pdfstream pdf stream;
                                               match stream with
                                                 Pdf.Stream {contents = (_, Pdf.Got data)} -> data
                                               | _ -> raise Not_found
                                             with
                                               _ -> raise (Pdf.PDFError "could not retreive attachment data"))}
                                   | _ -> raise (Pdf.PDFError "no /F found in attachment")
                                   end
                              | _ ->
                                  Some
                                    {name = s;
                                     pagenumber = pagenumber;
                                     data = (fun () -> raise (Pdf.PDFError "no attachment data"))}
                              end
                          | _ -> None
                          end
                      | _ -> None)
                  | _ -> None)
               (match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
               | Some (Pdf.Array annots) -> annots
               | _ -> []))
          pages
          (indx pages))
  in
    toplevel @ pagelevel

(* \section{Remove Attached files} *)
let remove_attached_files_on_pages pdf =
  let remove_from_page page =
    {page with Pdfpage.rest =
       Pdf.add_dict_entry page.Pdfpage.rest "/Annots"
         (Pdf.Array
           (option_map
             (function annot ->
                match Pdf.lookup_direct pdf "/Subtype" annot with
                | Some (Pdf.Name "/FileAttachment") -> None
                | _ -> Some annot)
             (match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
             | Some (Pdf.Array annots) -> annots
             | _ -> [])))}
  in
    Pdfpage.change_pages true pdf (map remove_from_page (Pdfpage.pages_of_pagetree pdf))

let remove_attached_files pdf =
  let pdf = remove_attached_files_on_pages pdf in
    match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
    | None -> pdf
    | Some rootdict ->
        match Pdf.lookup_direct pdf "/Names" rootdict with
        | None -> pdf
        | Some namedict ->
            let namedict' = Pdf.remove_dict_entry namedict "/EmbeddedFiles" in
              let rootdict' = Pdf.add_dict_entry rootdict "/Names" namedict' in
                let rootdict'num = Pdf.addobj pdf rootdict' in
                  {pdf with
                     Pdf.root =
                       rootdict'num;
                     Pdf.trailerdict =
                       Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect rootdict'num)}

