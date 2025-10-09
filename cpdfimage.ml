open Pdfutil
open Pdfio
open Cpdferror

let debug_image_processing = ref false

let image_command x =
  begin match Sys.getenv_opt "CPDF_SHOW_EXT" with
  | Some "true" -> flprint ("\n" ^ x ^ "\n")
  | _ -> ()
  end;
  Sys.command x

let complain_jbig2enc path =
  if path = "" then error "Specify jbig2enc location with -jbig2enc"

let complain_convert path =
  if path = "" then error "Specify magick location with -im"

let remove x =
  try (*Printf.printf "%s\n" x;*) Sys.remove x with _ -> ()

let pnm_white ch = output_char ch ' '
let pnm_newline ch = output_char ch '\n'
let pnm_output_string = Stdlib.output_string
 
let pnm_header ch w h =
  pnm_white ch;
  pnm_output_string ch (string_of_int w);
  pnm_white ch;
  pnm_output_string ch (string_of_int h);
  pnm_white ch

let pnm_to_channel_24 ch w h s =
  pnm_output_string ch "P6";
  pnm_header ch w h;
  pnm_output_string ch "255";
  pnm_newline ch;
  bytes_to_output_channel ch s

(* The image in PDF format will 4 bits for each, but padded to a whole byte at
   end of image line. We need to convert to 8 bits per sample with no padding at
   the end of image line. *)
let pnm_to_channel_4bpp ch w h s =
  pnm_output_string ch "P5";
  pnm_header ch w h;
  pnm_output_string ch "15";
  pnm_newline ch;
  let o, odata = input_output_of_bytes (w * h) in
  let bs = bitbytes_of_input (input_of_bytes s) in
    for y = 1 to h do
      for x = 1 to w do
        o.output_byte (getval_31 bs 4)
      done;
      align bs
    done;
    bytes_to_output_channel ch (extract_bytes_from_input_output o odata)

let pnm_to_channel_8 ch w h s =
  pnm_output_string ch "P5";
  pnm_header ch w h;
  pnm_output_string ch "255";
  pnm_newline ch;
  bytes_to_output_channel ch s

let pnm_to_channel_1_inverted ch w h s =
  pnm_output_string ch "P4";
  pnm_header ch w h;
  pnm_newline ch;
  let inverted = Pdfio.copybytes s in
    Pdfio.bytes_selfmap lnot inverted;
    bytes_to_output_channel ch inverted

let cmyk_to_channel_32 ch w h s =
  bytes_to_output_channel ch s

let cmyk_to_channel_32_invert ch w h s =
  let inverted = Pdfio.copybytes s in
    Pdfio.bytes_selfmap (fun x -> 255 - x) inverted;
    bytes_to_output_channel ch inverted

let jbig2_serial = ref 0

let jbig2_globals = null_hash ()

let write_stream name stream =
  let fh = open_out_bin name in
    Pdfio.bytes_to_output_channel fh stream;
    close_out fh

let write_image ~raw ?path_to_p2p ?path_to_im pdf resources name image =
  Cpdfutil.check_injectible name;
  match Pdfimage.get_image_24bpp pdf resources image with
  | Pdfimage.JPEG (stream, _) -> write_stream (name ^ ".jpg") stream
  | Pdfimage.JPEG2000 (stream, _) -> write_stream (name ^ ".jpx") stream
  | Pdfimage.JBIG2 (stream, _, global) ->
      begin match global with
      | None ->
          (*Printf.printf "JBIG2: No global, writing plain\n";*)
          write_stream (name ^ ".jbig2") stream
      | Some g ->
          (*Printf.printf "JBIG2: there is a global\n";*)
          let go () =
            let serial, _ = Hashtbl.find jbig2_globals g in
              write_stream (name ^ ".jbig2__" ^ string_of_int serial) stream
          in
            try go () with Not_found ->
              jbig2_serial += 1;
              let globaldata =
                let obj = Pdf.lookup_obj pdf g in
                  Pdfcodec.decode_pdfstream_until_unknown pdf obj;
                  match obj with | Pdf.Stream {contents = (_, Got b)} -> Some b | _ -> None
              in
                match globaldata with
                | Some d ->
                    Hashtbl.add jbig2_globals g (!jbig2_serial, d);
                    let filename = Filename.concat (Filename.dirname name) (string_of_int !jbig2_serial ^ ".jbig2global") in
                      write_stream filename d;
                      go ()
                | None ->
                    Pdfe.log "Could not extract JBIG2Globals. Skipping this image."
      end
  | Pdfimage.Raw (w, h, Pdfimage.BPP24, stream) ->
      let pnm = name ^ ".pnm" in
      let png = name ^ ".png" in
      let fh = open_out_bin pnm in
        pnm_to_channel_24 fh w h stream;
        close_out fh;
        begin match path_to_p2p with
        | None ->
          begin match path_to_im with
            None ->
              if not raw then Pdfe.log "Neither pnm2png nor imagemagick found. Specify with -p2p or -im\n"
          | Some path_to_im ->
            begin match
              image_command (Filename.quote_command path_to_im [pnm; png])
            with
              0 -> remove pnm
            | _ -> 
              Pdfe.log "Call to imagemagick failed: did you specify -p2p or -im correctly?\n";
              remove pnm
            end
          end
        | Some path_to_p2p ->
          begin match
            image_command (Filename.quote_command path_to_p2p ~stdout:png ["-gamma"; "0.45"; "-quiet"; pnm])
          with
          | 0 -> remove pnm
          | _ ->
              Pdfe.log "Call to pnmtopng failed: did you specify -p2p correctly?\n";
              remove pnm
          end
        end
  | _ ->
      Pdfe.log (Printf.sprintf "Unsupported image type when extracting image %s " name)

let written = ref []

let combine_image_and_mask ?path_to_im name maskname stem =
  (* Look for name.png, maskname.png. Make name-maskname-combined.png *)
  match path_to_im with
  | None -> error "combine_image_and_mask: imagemagick not found"
  | Some path_to_im ->
      let out =
        image_command
          (Filename.quote_command path_to_im
            [name ^ ".png"; maskname ^ ".png"; "-compose"; "CopyOpacity"; "-composite";
             Filename.dirname stem  ^ Filename.dir_sep ^ Filename.basename name ^ "-" ^ Filename.basename maskname ^ "-combined" ^ ".png"])
      in
        if out > 0 then Pdfe.log "could not combine image and mask"

let extract_images_inner ~readname ?name ~raw ?path_to_p2p ?path_to_im encoding serial pdf resources stem pnum images =
  let names =
    map
      (fun image ->
         match name with Some s -> s | None ->
           (* Abuse @S *)
           let stem = string_replace_all "%objnum" "@S" stem in
             Cpdfbookmarks.name_of_spec
               encoding [] pdf 0 (stem ^ "-p" ^ string_of_int pnum)
               (let r = !serial in serial := !serial + 1; r) "" (match image with Pdf.Indirect i -> i | _ -> 0) 0)
      images
  in
    readname := names @ !readname;
    iter2 (write_image ~raw ?path_to_p2p ?path_to_im pdf resources) names images

let rec extract_images_form_xobject ~raw ?path_to_p2p ?path_to_im encoding dedup dedup_per_page pdf serial stem pnum form =
  let resources =
    match Pdf.lookup_direct pdf "/Resources" form with
      Some (Pdf.Dictionary d) -> Pdf.Dictionary d
    | _ -> Pdf.Dictionary []
  in
    let images, forms =
      let xobjects =
        match Pdf.lookup_direct pdf "/XObject" resources with
        | Some (Pdf.Dictionary elts) -> map snd elts
        | _ -> []
      in
        (* Remove any already in !written. Add any remaining to !written, if !args.dedup or !args.dedup_page *)
        let images, forms = List.partition (fun o -> Pdf.lookup_direct pdf "/Subtype" o = Some (Pdf.Name "/Image")) xobjects in
        let already_written, images = List.partition (function Pdf.Indirect n -> mem n !written | _ -> false) images in
          if dedup || dedup_per_page then
            written := (option_map (function Pdf.Indirect n -> Some n | _ -> None) images) @ !written;
          images, forms
    in
      iter (extract_images_form_xobject ~raw ?path_to_p2p ?path_to_im encoding dedup dedup_per_page pdf serial stem pnum) forms;
      let readname = ref [] in
        extract_images_inner ~readname ~raw ?path_to_p2p ?path_to_im encoding serial pdf resources stem pnum images

let extract_inline_images ~raw ?path_to_p2p ?path_to_im encoding pdf page pnum serial stem =
  iter
    (function
     | Pdfops.InlineImage (dict, _, data) ->
         let newdict = Pdf.add_dict_entry dict "/Length" (Pdf.Integer (bytes_size data)) in
         let fakeobj = Pdf.Stream {contents = newdict, Pdf.Got data} in
         (* Abuse @S as above. *)
         let stem = string_replace_all "%objnum" "@S" stem in
         let name =
           Cpdfbookmarks.name_of_spec
             encoding [] pdf 0 (stem ^ "-p" ^ string_of_int pnum ^ "-inline")
             (let r = !serial in serial := !serial + 1; r) "" 0 0
         in
           write_image ~raw ?path_to_p2p ?path_to_im pdf page.Pdfpage.resources name fakeobj
     | _ -> ())
    (Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content)

let extract_inline_images_form ~raw ?path_to_p2p ?path_to_im encoding pdf pnum serial stem form =
  let fakepage =
    {Pdfpage.content = [form];
     Pdfpage.mediabox = Pdf.Array [Pdf.Integer 0; Pdf.Integer 0; Pdf.Integer 0; Pdf.Integer 0];
     Pdfpage.resources = begin match Pdf.lookup_direct pdf "/Resources" form with Some d -> d | None -> Pdf.Dictionary [] end;
     Pdfpage.rotate = Pdfpage.Rotate0;
     Pdfpage.rest = Pdf.Dictionary []}
  in
    extract_inline_images ~raw ?path_to_p2p ?path_to_im encoding pdf fakepage pnum serial stem

let extract_images ~merge_masks ~inline ?(raw=false) ?path_to_p2p ?path_to_im encoding dedup dedup_per_page pdf range stem =
  Hashtbl.clear jbig2_globals;
  jbig2_serial := 0;
  if dedup || dedup_per_page then written := [];
  let pdf_pages = Pdfpage.pages_of_pagetree pdf in
    let pages =
      option_map
        (function (i, pdf_pages) -> if mem i range then Some pdf_pages else None)
        (combine (indx pdf_pages) pdf_pages)
    in
      let serial = ref 0 in
        iter2
          (fun page pnum ->
             Cpdfutil.progress_page pnum;
             if dedup_per_page then written := [];
             let xobjects =
               match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
               | Some (Pdf.Dictionary elts) -> map snd elts
               | _ -> []
             in
               let images = keep (fun o -> Pdf.lookup_direct pdf "/Subtype" o = Some (Pdf.Name "/Image")) xobjects in
               let already_written, images = List.partition (function Pdf.Indirect n -> mem n !written | _ -> false) images in
               if dedup || dedup_per_page then
                 written := (option_map (function Pdf.Indirect n -> Some n | _ -> None) images) @ !written;
               let forms = keep (fun o -> Pdf.lookup_direct pdf "/Subtype" o = Some (Pdf.Name "/Form")) xobjects in
               let name = ref [] in
               let maskname = ref [] in
                 extract_images_inner ~readname:name ~raw ?path_to_p2p ?path_to_im encoding serial pdf page.Pdfpage.resources stem pnum images;
                 iter
                   (fun d ->
                      match Pdf.direct pdf d with
                      | Pdf.Stream {contents = (Pdf.Dictionary dict, _)} ->
                          begin match lookup "/SMask" dict with
                          | Some (Pdf.Indirect i) ->
                              extract_images_inner ~readname:maskname ~raw ?path_to_p2p ?path_to_im encoding serial pdf (Pdf.Dictionary []) (stem ^ "-smask") pnum [Pdf.Indirect i]
                          | _ ->
                              maskname := "NONE"::!maskname
                          end
                      | _ -> maskname := "NONE"::!maskname)
                   (rev images);
                 iter2
                   (fun name maskname -> if maskname <> "NONE" then combine_image_and_mask ?path_to_im name maskname stem)
                   !name !maskname;
                 iter (extract_images_form_xobject ~raw ?path_to_p2p ?path_to_im encoding dedup dedup_per_page pdf serial stem pnum) forms;
                 if inline then
                   begin
                     extract_inline_images ~raw ?path_to_p2p ?path_to_im encoding pdf page pnum serial stem;
                     iter (extract_inline_images_form ~raw ?path_to_p2p ?path_to_im encoding pdf pnum serial stem) forms;
                   end;
                 Cpdfutil.progress_endpage ())
          pages
          (indx pages);
          Cpdfutil.progress_done ()

let extract_single_image ~merge_masks ?(raw=false) ?path_to_p2p ?path_to_im encoding pdf objnum stem =
  let name = ref [] in
  let maskname = ref [] in
  extract_images_inner ~readname:name ~name:stem ~raw ?path_to_p2p ?path_to_im encoding (ref 0) pdf (Pdf.Dictionary []) stem 0 [Pdf.Indirect objnum];
  begin match Pdf.direct pdf (Pdf.Indirect objnum) with
  | Pdf.Stream {contents = (Pdf.Dictionary dict, _)}->
      begin match lookup "/SMask" dict with
      | Some (Pdf.Indirect i) ->
          extract_images_inner ~readname:maskname ~name:(stem ^ "-smask") ~raw ?path_to_p2p ?path_to_im encoding (ref 0) pdf (Pdf.Dictionary []) stem 0 [Pdf.Indirect i]
      | _ -> ()
      end
  | _ -> ()
  end;
  if merge_masks then
    begin match !name, !maskname with
    | [name], [maskname] ->
        combine_image_and_mask ?path_to_im name maskname stem
    | _ -> ()
    end

(* Image resolution *)
type xobj =
  | Image of int * int (* width, height *)
  | Form of Pdftransform.transform_matrix * Pdf.pdfobject * Pdf.pdfobject

let image_results = ref []

let rec image_resolution_page ~inline pdf page pagenum images =
  (*Printf.printf "image_resolution_page, inline = %b, pagenum = %i\n" inline pagenum;*)
  try
    let pageops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content
    and transform = ref [ref Pdftransform.i_matrix] in
      iter
        (function
         | Pdfops.Op_cm matrix ->
             begin match !transform with
             | [] -> raise (Failure "no transform")
             | _ -> (hd !transform) := Pdftransform.matrix_compose !(hd !transform) matrix
             end
         | Pdfops.InlineImage (dict, _, _) ->
             (*flprint "Found an inline image\n";*)
             if inline then
               begin
                 match Pdf.lookup_direct_orelse pdf "/Width" "/W" dict, Pdf.lookup_direct_orelse pdf "/Height" "/H" dict with
                 | Some (Pdf.Integer w), Some (Pdf.Integer h) ->
                     let trans (x, y) =
                       match !transform with
                       | [] -> raise (Failure "no transform")
                       | _ -> Pdftransform.transform_matrix !(hd !transform) (x, y)
                     in
                       let o = trans (0., 0.)
                       and x = trans (1., 0.)
                       and y = trans (0., 1.) in
                       let lx = Pdfunits.inches (distance_between o x) Pdfunits.PdfPoint in
                       let ly = Pdfunits.inches (distance_between o y) Pdfunits.PdfPoint in
                         let wdpi = float w /. lx
                         and hdpi = float h /. ly in
                           image_results := (pagenum, "/InlineImage", w, h, wdpi, hdpi, 0)::!image_results
                 | _ -> ()
               end
         | Pdfops.Op_Do xobject ->
             let trans (x, y) =
               match !transform with
               | [] -> raise (Failure "no transform")
               | _ -> Pdftransform.transform_matrix !(hd !transform) (x, y)
             in
               let o = trans (0., 0.)
               and x = trans (1., 0.)
               and y = trans (0., 1.)
               in
                 (*i Printf.printf "o = %f, %f, x = %f, %f, y = %f, %f\n" (fst o) (snd o) (fst x) (snd x) (fst y) (snd y); i*)
                 let rec lookup_image k = function
                   | [] -> assert false
                   | (_, a, _, _) as h::_ when a = k -> h
                   | _::t -> lookup_image k t 
                 in
                   begin match lookup_image xobject images with
                   | (pagenum, name, Form (xobj_matrix, content, resources), objnum) ->
                        let content =
                          (* Add in matrix etc. *)
                          let total_matrix = Pdftransform.matrix_compose xobj_matrix !(hd !transform) in
                            let ops =
                              Pdfops.Op_cm total_matrix::
                              Pdfops.parse_operators pdf resources [content]
                            in
                              Pdfops.stream_of_ops ops
                        in
                          let page =
                            {Pdfpage.content = [content];
                             Pdfpage.mediabox = Pdfpage.rectangle_of_paper Pdfpaper.a4;
                             Pdfpage.resources = resources;
                             Pdfpage.rotate = Pdfpage.Rotate0;
                             Pdfpage.rest = Pdf.Dictionary []}
                          in
                            let newpdf = Pdfpage.change_pages false pdf [page] in
                              image_resolution ~inline newpdf [1] pagenum
                   | (pagenum, name, Image (w, h), objnum) ->
                       let lx = Pdfunits.inches (distance_between o x) Pdfunits.PdfPoint in
                       let ly = Pdfunits.inches (distance_between o y) Pdfunits.PdfPoint in
                         let wdpi = float w /. lx
                         and hdpi = float h /. ly in
                         image_results := (pagenum, xobject, w, h, wdpi, hdpi, objnum)::!image_results;
                           (*Printf.printf "%i, %s, %i, %i, %f, %f\n" pagenum xobject w h wdpi hdpi;*)
                   end
         | Pdfops.Op_q ->
             begin match !transform with
             | [] -> raise (Failure "Unbalanced q/Q ops")
             | h::t ->
                 let h' = ref Pdftransform.i_matrix in
                   h' := !h;
                   transform := h'::h::t
             end
         | Pdfops.Op_Q ->
             begin match !transform with
             | [] -> raise (Failure "Unbalanced q/Q ops")
             | _ -> transform := tl !transform
             end
         | _ -> ())
        pageops
    with
      e -> Printf.printf "Error %s\n" (Printexc.to_string e); flprint "\n"

and image_resolution ~inline pdf range real_pagenum =
  let images = ref [] in
    Cpdfpage.iter_pages
      (fun pagenum page ->
         let pagenum = if real_pagenum > 0 then real_pagenum else pagenum in
         (* 1. Get all image names and their native resolutions from resources as string * int * int *)
         match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
          | Some (Pdf.Dictionary xobjects) ->
              iter
                (function (name, xobject) ->
                   let objnum = match xobject with Pdf.Indirect i -> i | _ -> 0 in
                   match Pdf.lookup_direct pdf "/Subtype" xobject with
                   | Some (Pdf.Name "/Image") ->
                       let width =
                         match Pdf.lookup_direct pdf "/Width" xobject with
                         | Some x -> Pdf.getnum pdf x
                         | None -> 1.
                       and height =
                         match Pdf.lookup_direct pdf "/Height" xobject with
                         | Some x -> Pdf.getnum pdf x
                         | None -> 1.
                       in
                         images := (pagenum, name, Image (int_of_float width, int_of_float height), objnum)::!images
                   | Some (Pdf.Name "/Form") ->
                       let resources =
                         match Pdf.lookup_direct pdf "/Resources" xobject with
                         | None -> page.Pdfpage.resources (* Inherit from page or form above. *)
                         | Some r -> r
                       and contents =
                         xobject 
                       and matrix =
                         match Pdf.lookup_direct pdf "/Matrix" xobject with
                         | Some (Pdf.Array [a; b; c; d; e; f]) ->
                             {Pdftransform.a = Pdf.getnum pdf a; Pdftransform.b = Pdf.getnum pdf b; Pdftransform.c = Pdf.getnum pdf c;
                              Pdftransform.d = Pdf.getnum pdf d; Pdftransform.e = Pdf.getnum pdf e; Pdftransform.f = Pdf.getnum pdf f}
                         | _ -> Pdftransform.i_matrix
                       in
                         images := (pagenum, name, Form (matrix, contents, resources), objnum)::!images
                   | _ -> ()
                )
                xobjects
          | _ -> ())
      pdf
      (if real_pagenum = 0 then range else [1]);
      (* Now, split into differing pages, and call [image_resolution_page] on each one *)
      let pagesplits =
        map
          (function (a, _, _, _)::_ as ls -> (a, ls) | _ -> assert false)
          (collate (fun (a, _, _, _) (b, _, _, _) -> compare a b) (rev !images))
      and pages =
        Pdfpage.pages_of_pagetree pdf
      in
      (* If inline is set we in fact need to process all pages in the range
         even if no image xobjects are linked to that page. So, add
         fake entries with empty image lists. *)
      let pagesplits =
        let fsts = map fst pagesplits in
          if inline then
            option_map (fun p -> if mem p fsts then None else Some (p, [])) range @ pagesplits
          else
            pagesplits 
      in
        iter
          (function (pagenum, images) ->
             let pagenum = if real_pagenum > 0 then 1 else pagenum in
             let page = select pagenum pages in
               image_resolution_page ~inline pdf page pagenum images)
          pagesplits

let is_below_dpi dpi (_, _, _, _, wdpi, hdpi, _) =
  wdpi < dpi || hdpi < dpi

let image_resolution ~inline pdf range dpi =
  image_results := [];
  image_resolution ~inline pdf range 0;
  sort compare (rev (keep (is_below_dpi dpi) !image_results))

let image_resolution_json ~inline pdf range dpi =
  let images = image_resolution ~inline pdf range dpi in
    Pdfio.bytes_of_string
      (Cpdfyojson.Safe.pretty_to_string
        (`List (map (fun (pagenum, xobject, w, h, wdpi, hdpi, objnum) ->
           `Assoc [("Object", `Int objnum); ("Page", `Int pagenum); ("XObject", `String xobject);
                   ("W", `Int w); ("H", `Int h); ("Xdpi", `Float wdpi); ("Ydpi", `Float hdpi)]) images)))

(* All the images in file referenced at least once from the given range of pages. *)
let images ~inline pdf range =
  let images = null_hash () in
  let formnums = null_hash () in
  let iifakeobjs = null_hash () in
  let rec collect_inline_images pagenum stream =
    if inline then
      begin
        let resources =
          match Pdf.lookup_direct pdf "/Resources" stream with
          | Some d -> d
          | None -> Pdf.Dictionary []
        in
        let contents =
          match Pdf.lookup_direct pdf "/Contents" stream with
          | Some (Pdf.Indirect i) -> [Pdf.Indirect i]
          | Some (Pdf.Array a) -> a
          | _ -> []
        in
          let ops = Pdfops.parse_operators pdf resources contents in
            iter
              (function
               | Pdfops.InlineImage (dict, _, data) ->
                   let xobject =
                     let dict = Pdf.add_dict_entry dict "/Subtype" (Pdf.Name "/Image") in
                     let dict = Pdf.add_dict_entry dict "/Length" (Pdf.Integer (bytes_size data)) in
                     let i = Pdf.addobj pdf (Pdf.Stream {contents = dict, Pdf.Got data}) in
                       Hashtbl.add iifakeobjs i ();
                       Pdf.Indirect i
                   in
                     process_xobject (Pdf.Dictionary []) pagenum (Pdfpage.blankpage Pdfpaper.a4) ("/InlineImage", xobject)
               | _ -> ())
              ops
     end
  and process_xobject resources pagenum page (name, xobject) =
    match Pdf.lookup_direct pdf "/Subtype" xobject with
    | Some (Pdf.Name "/Image") ->
        begin match xobject with
        | Pdf.Indirect i ->
            begin match Hashtbl.find images i with
            | (pagenums, n, w, h, s, bpc, cs, f, mt) ->
                Hashtbl.replace images i (pagenum::pagenums, n, w, h, s, bpc, cs, f, mt)
            | exception Not_found ->
                let width =
                  match Pdf.lookup_direct_orelse pdf "/Width" "/W" xobject with
                  | Some x -> Pdf.getnum pdf x
                  | None -> 1.
                and height =
                  match Pdf.lookup_direct_orelse pdf "/Height" "/H" xobject with
                  | Some x -> Pdf.getnum pdf x
                  | None -> 1.
                and size =
                  match Pdf.lookup_direct pdf "/Length" xobject with
                  | Some (Pdf.Integer x) -> x
                  | _ -> 0
                and bpc =
                  match Pdf.lookup_direct_orelse pdf "/BitsPerComponent" "/BPC" xobject with
                  | Some (Pdf.Integer x) -> Some x
                  | _ -> None
                and colourspace =
                  match Pdf.lookup_direct_orelse pdf "/ColorSpace" "/CS" xobject with
                  | Some x -> Some (Pdfspace.string_of_colourspace (Pdfspace.read_colourspace pdf resources x))
                  | None -> None
                and filter =
                  match Pdf.lookup_direct_orelse pdf "/Filter" "/F" xobject with
                  | Some (Pdf.Array [x]) | Some x -> Some (Pdfwrite.string_of_pdf x)
                  | None -> None
                and masktype =
                  match Pdf.lookup_direct_orelse pdf "/ImageMask" "/IM" xobject with
                  | Some (Pdf.Boolean true) -> ("ImageMask", 0)
                  | _ ->
                      match Pdf.lookup_direct pdf "/Mask" xobject with
                      | Some (Pdf.Stream _) ->
                          let objnum =
                            match Pdf.direct pdf xobject with
                            | Pdf.Stream {contents = (Pdf.Dictionary d, _)} ->
                                begin match lookup "/Mask" d with
                                | Some (Pdf.Indirect i) ->
                                    process_xobject resources pagenum page ("/Mask", Pdf.Indirect i);
                                    i
                                | _ -> 0
                                end
                            | _ -> 0
                          in
                            ("ExplicitMask", objnum)
                      | Some _ -> ("ColourKeyMask", 0)
                      | None ->
                          match Pdf.lookup_direct pdf "/SMask" xobject with
                          | Some _ ->
                              let objnum =
                                match Pdf.direct pdf xobject with
                                | Pdf.Stream {contents = (Pdf.Dictionary d, _)} ->
                                    begin match lookup "/SMask" d with
                                    | Some (Pdf.Indirect i) ->
                                        process_xobject resources pagenum page ("/SMask", Pdf.Indirect i);
                                        i
                                    | _ -> 0
                                    end
                                | _ -> 0
                              in
                                ("SMask", objnum)
                          | _ ->
                              match Pdf.lookup_direct pdf "/SMaskInData" xobject with
                              | Some _ -> ("SMaskInData", 0)
                              | _ -> ("NoMask", 0)
                in
                  Hashtbl.replace images i ([pagenum], name, int_of_float width, int_of_float height, size, bpc, colourspace, filter, masktype)
            end
        | _ -> ()
        end
    | Some (Pdf.Name "/Form") ->
        begin match xobject with
        | Pdf.Indirect i ->
            begin match Hashtbl.find formnums i with
            | () -> ()
            | exception Not_found ->
                let fakeobj =
                  Pdf.Dictionary
                    [("/Resources", match Pdf.lookup_direct pdf "/Resources" xobject with Some d -> d | None -> Pdf.Dictionary []);
                     ("/Contents", Pdf.Indirect i)]
                in
                collect_inline_images pagenum fakeobj;
                Hashtbl.add formnums i ();
                begin match Pdf.lookup_direct pdf "/Resources" xobject with
                | Some r ->
                    begin match Pdf.lookup_direct pdf "/XObject" r with
                    | Some (Pdf.Dictionary xobjects) -> iter (process_xobject r pagenum page) xobjects
                    | _ -> ()
                    end
                | None -> ()
                end
            end
        | _ -> ()
        end
    | _ -> ()
    in
      Cpdfpage.iter_pages
        (fun pagenum page ->
          let fakeobj = Pdf.Dictionary [("/Resources", page.resources); ("/Contents", Pdf.Array page.content)] in
           collect_inline_images pagenum fakeobj;
           match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
            | Some (Pdf.Dictionary xobjects) ->
                iter (process_xobject page.Pdfpage.resources pagenum page) xobjects
            | _ -> ())
        pdf
        range;
        Hashtbl.iter (fun k _ -> Pdf.removeobj pdf k) iifakeobjs;
        let images = list_of_hashtbl images in
        let images = map (fun (i, (pnums, n, w, h, s, bpc, c, filter, mt)) -> (i, (setify (sort compare pnums), n, w, h, s, bpc, c, filter, mt))) images in
        let images = sort (fun (_, (pnums, _, _, _, _, _, _, _, _)) (_, (pnums', _, _, _, _, _, _, _, _)) -> compare (hd pnums) (hd pnums')) images in
         `List
           (map
             (fun (i, (pnums, n, w, h, size, bpc, cs, filter, (masktype, maskobjnum))) ->
               `Assoc [("Object", `Int (if Hashtbl.mem iifakeobjs i then 0 else i));
                       ("Pages", `List (map (fun x -> `Int x) pnums));
                       ("Name", `String n);
                       ("Width", `Int w);
                       ("Height", `Int h);
                       ("Bytes", `Int size);
                       ("BitsPerComponent", match bpc with None -> `Null | Some bpc -> `Int bpc);
                       ("Colourspace", match cs with None -> `Null | Some s -> `String s);
                       ("Filter", match filter with None -> `Null | Some s -> `String s);
                       ("Mask", `String masktype);
                       ("MaskObjNum", match maskobjnum with 0 -> `Null | n -> `Int n)])
             images)

let obj_of_jpeg_data ~path_to_im data =
  let w, h = 
    try Cpdfjpeg.jpeg_dimensions data with _ ->
      let temp = Filename.temp_file "cpdf" ".jpeg" in
        contents_to_file ~filename:temp (string_of_bytes data);
        let w, h = 
          Cpdfjpeg.backup_jpeg_dimensions ~path_to_im temp
        in
          begin try Sys.remove temp with _ -> () end;
          (w, h)
  in
  let d = 
    ["/Length", Pdf.Integer (Pdfio.bytes_size data);
     "/Filter", Pdf.Name "/DCTDecode";
     "/BitsPerComponent", Pdf.Integer 8;
     "/ColorSpace", Pdf.Name "/DeviceRGB";
     "/Subtype", Pdf.Name "/Image";
     "/Width", Pdf.Integer w;
     "/Height", Pdf.Integer h]
  in
    Pdf.Stream {contents = (Pdf.Dictionary d, Pdf.Got data)}, []

(* Given all the IDAT data, decompress and depredicte, split the alpha channel
   off, and recombine all the data into two new compressed data streams. *)
let split_mask png =
  if png.Cpdfpng.bitdepth > 8 then error "Cannot use alpha PNGs with >8bpp" else
  let data = Pdfcodec.decode_flate png.Cpdfpng.idat in
  let channels = match png.colortype with 4 -> 1 | 6 -> 3 | _ -> error "obj_of_png_data/split_mask: bad colortype" in
  let data = Pdfcodec.decode_predictor 15 (channels + 1) png.bitdepth png.width data in
  let i = Pdfio.input_of_bytes data in
  let predictor = false in
  let (colourso, coloursr), (masko, maskr) =
    Pdfio.input_output_of_bytes (png.width * png.height * channels),
    Pdfio.input_output_of_bytes (png.width * png.height)
  in
    try
      while true do
        do_many (fun _ -> colourso.output_byte (int_of_char (unopt (i.input_char ())))) channels;
        masko.output_byte (int_of_char (unopt (i.input_char ())))
      done;
      (Pdfio.mkbytes 0, Some (Pdfio.mkbytes 0), predictor)
    with
      _ -> 
        (Pdfcodec.encode_flate (Pdfio.extract_bytes_from_input_output colourso coloursr),
         Some (Pdfcodec.encode_flate (Pdfio.extract_bytes_from_input_output masko maskr)),
         predictor)

let obj_of_png_data pdf data =
  let png = Cpdfpng.read_png (Pdfio.input_of_bytes data) in
  let imagedata, mask, predictor =
    match png.colortype with
    | 4 | 6 -> split_mask png
    | _ -> png.idat, None, true
  in
  let smask =
    match mask with
    | None -> []
    | Some data ->
        let smask =
          Pdf.Stream
           {contents = (Pdf.Dictionary
             (["/Length", Pdf.Integer (Pdfio.bytes_size data);
              "/Subtype", Pdf.Name "/Image";
              "/BitsPerComponent", Pdf.Integer png.bitdepth;
              "/ColorSpace", Pdf.Name "/DeviceGray";
              "/Width", Pdf.Integer png.width;
              "/Height", Pdf.Integer png.height;
              "/Filter", Pdf.Name "/FlateDecode"]
              @
               (if predictor then
                  ["/DecodeParms", Pdf.Dictionary
                    ["/BitsPerComponent", Pdf.Integer png.bitdepth;
                     "/Colors", Pdf.Integer 1;
                     "/Columns", Pdf.Integer png.width;
                     "/Predictor", Pdf.Integer 15]]
                else [])),
             Pdf.Got data)}
        in
          ["/SMask", Pdf.Indirect (Pdf.addobj pdf smask)]
  in
  let d =
    ["/Length", Pdf.Integer (Pdfio.bytes_size png.idat);
     "/Subtype", Pdf.Name "/Image";
     "/BitsPerComponent", Pdf.Integer png.bitdepth;
     "/ColorSpace", Pdf.Name (match png.colortype with 0 | 4 -> "/DeviceGray" | 2 | 6 -> "/DeviceRGB" | _ -> error "obj_of_png_data unknown colortype");
     "/Width", Pdf.Integer png.width;
     "/Height", Pdf.Integer png.height;
     "/Filter", Pdf.Name "/FlateDecode"]
    @
     (if predictor then
       ["/DecodeParms", Pdf.Dictionary
          ["/BitsPerComponent", Pdf.Integer png.bitdepth;
           "/Colors", Pdf.Integer (match png.colortype with 0 | 4 -> 1 | 2 | 6 -> 3 | _ -> error "obj_of_png_data unknown colortype ");
           "/Columns", Pdf.Integer png.width;
           "/Predictor", Pdf.Integer 15]]
      else
        [])
     @ smask
  in
    Pdf.Stream {contents = (Pdf.Dictionary d, Pdf.Got imagedata)}, []

let obj_of_jpeg2000_data data =
  let w, h = Cpdfjpeg2000.jpeg2000_dimensions data in
  let d =
    ["/Length", Pdf.Integer (Pdfio.bytes_size data);
     "/Filter", Pdf.Name "/JPXDecode";
     "/Subtype", Pdf.Name "/Image";
     "/Width", Pdf.Integer w;
     "/Height", Pdf.Integer h]
  in
    Pdf.Stream {contents = (Pdf.Dictionary d, Pdf.Got data)}, [] 

let jbig2_dimensions data =
  (bget data 11 * 256 * 256 * 256 + bget data 12 * 256 * 256 + bget data 13 * 256 + bget data 14,
   bget data 15 * 256 * 256 * 256 + bget data 16 * 256 * 256 + bget data 17 * 256 + bget data 18)

let obj_of_jbig2_data ?global data =
  let d, extra =
    let decodeparms, extra =
      match global with
      | Some data ->
          [("/DecodeParms", Pdf.Dictionary [("/JBIG2Globals", Pdf.Indirect 10000)])],
          [(10000, Pdf.Stream {contents = (Pdf.Dictionary [("/Length", Pdf.Integer (bytes_size data))], Pdf.Got data)})]
      | None ->
          [], []
    in
    let w, h = jbig2_dimensions data in
      [("/Length", Pdf.Integer (Pdfio.bytes_size data));
       ("/Filter", Pdf.Name "/JBIG2Decode");
       ("/Subtype", Pdf.Name "/Image");
       ("/BitsPerComponent", Pdf.Integer 1);
       ("/ColorSpace", Pdf.Name "/DeviceGray");
       ("/Width", Pdf.Integer w);
       ("/Height", Pdf.Integer h)]
      @ decodeparms, extra
  in
    Pdf.Stream {contents = (Pdf.Dictionary d, Pdf.Got data)}, extra

let image_of_input ?subformat ?title ~process_struct_tree fobj i =
  let pdf, title =
    match subformat with
    | None -> Pdf.empty (), begin match title with Some x -> x | None -> "" end
    | Some Cpdfua.PDFUA1 ->
        begin match title with
        | None -> error "no -title given" 
        | Some title -> Cpdfua.create_pdfua1 title Pdfpaper.a4 1, title
        end
    | Some Cpdfua.PDFUA2 ->
        begin match title with
        | None -> error "no -title given"
        | Some title -> Cpdfua.create_pdfua2 title Pdfpaper.a4 1, title
        end
  in
  let data = Pdfio.bytes_of_input i 0 i.Pdfio.in_channel_length in
  let obj, extras = fobj pdf data in
  iter (Pdf.addobj_given_num pdf) extras;
  let w = match Pdf.lookup_direct pdf "/Width" obj with Some x -> Pdf.getnum pdf x | _ -> assert false in
  let h = match Pdf.lookup_direct pdf "/Height" obj with Some x -> Pdf.getnum pdf x | _ -> assert false in
  let structinfo =
    match process_struct_tree, subformat with
    | _, (Some Cpdfua.PDFUA1 | Some Cpdfua.PDFUA2) | true, _ -> true
    | _ -> false
  in
    if subformat = Some Cpdfua.PDFUA2 then
      begin
        let str = Pdf.addobj pdf Pdf.Null in
        let figure = Pdf.addobj pdf Pdf.Null in
        let parent_tree = Pdf.addobj pdf Pdf.Null in
        let namespace = Pdf.addobj pdf (Pdf.Dictionary [("/NS", Pdf.String "http://iso.org/pdf2/ssn")]) in
        let document = Pdf.addobj pdf Pdf.Null in
        Pdf.addobj_given_num pdf (document, Pdf.Dictionary [("/K", Pdf.Array [Pdf.Indirect figure]); ("/P", Pdf.Indirect str); ("/S", Pdf.Name "/Document"); ("/NS", Pdf.Indirect namespace)]);
        Pdf.addobj_given_num pdf (parent_tree, Pdf.Dictionary [("/Nums", Pdf.Array [Pdf.Integer 1; Pdf.Array [Pdf.Indirect figure]])]);
        Pdf.addobj_given_num pdf (figure, Pdf.Dictionary [("/K", Pdf.Array [Pdf.Integer 0]); ("/P", Pdf.Indirect document); ("/S", Pdf.Name "/Figure"); ("/Alt", Pdf.String title)]);
        Pdf.addobj_given_num pdf (str, Pdf.Dictionary [("/Namespaces", Pdf.Array [Pdf.Indirect namespace]); ("/Type", Pdf.Name "/StructTreeRoot");
                                                       ("/K", Pdf.Array [Pdf.Indirect document]); ("/ParentTree", Pdf.Indirect parent_tree)]);
        Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"] (Pdf.Indirect str)
      end
    else if process_struct_tree || subformat = Some Cpdfua.PDFUA1 then
      begin
        let str = Pdf.addobj pdf Pdf.Null in
        let figure = Pdf.addobj pdf Pdf.Null in
        let parent_tree = Pdf.addobj pdf Pdf.Null in
        Pdf.addobj_given_num pdf (parent_tree, Pdf.Dictionary [("/Nums", Pdf.Array [Pdf.Integer 1; Pdf.Array [Pdf.Indirect figure]])]);
        Pdf.addobj_given_num pdf (figure, Pdf.Dictionary [("/K", Pdf.Array [Pdf.Integer 0]); ("/P", Pdf.Indirect str); ("/S", Pdf.Name "/Figure"); ("/Alt", Pdf.String title)]);
        Pdf.addobj_given_num pdf (str, Pdf.Dictionary [("/Type", Pdf.Name "/StructTreeRoot"); ("/K", Pdf.Array [Pdf.Indirect figure]); ("/ParentTree", Pdf.Indirect parent_tree)]);
        Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"] (Pdf.Indirect str)
      end;
  let ops =
      (if structinfo then [Pdfops.Op_BDC ("/Figure", Pdf.Dictionary [("/MCID", Pdf.Integer 0)])] else [])
    @ [Pdfops.Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (0., 0.);
                                                         Pdftransform.Scale ((0., 0.), w, h)]);
       Pdfops.Op_Do "/I0"]
    @ (if structinfo then [Pdfops.Op_EMC] else [])
  in
  let page =
    {Pdfpage.content = [Pdfops.stream_of_ops ops];
     Pdfpage.mediabox = Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real w; Pdf.Real h];
     Pdfpage.resources = Pdf.Dictionary ["/XObject", Pdf.Dictionary ["/I0", Pdf.Indirect (Pdf.addobj pdf obj)]];
     Pdfpage.rotate = Pdfpage.Rotate0;
     Pdfpage.rest = if structinfo then Pdf.Dictionary [("/StructParents", Pdf.Integer 1)] else Pdf.Dictionary []}
  in
  let pdf, pageroot = Pdfpage.add_pagetree [page] pdf in
    Pdfpage.add_root pageroot [] pdf

let jpeg_to_jpeg pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~jpeg_to_jpeg_scale ~interpolate ~q ~path_to_convert s dict reference =
  if q < 0. || q > 100. then error "Out of range quality";
  complain_convert path_to_convert;
  let w = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer i) -> i | _ -> error "bad width" in
  let h = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer i) -> i | _ -> error "bad height" in
  if w * h < pixel_threshold then (if !debug_image_processing then Printf.printf "pixel threshold not met\n%!") else
  Pdf.getstream s;
  let size = match Pdf.lookup_direct pdf "/Length" dict with Some (Pdf.Integer i) -> i | _ -> 0 in
  if size < length_threshold then (if !debug_image_processing then Printf.printf "length threshold not met\n%!") else
  let out = Filename.temp_file "cpdf" "convertin.jpg" in
  let out2 = Filename.temp_file "cpdf" "convertout.jpg" in
  let fh = open_out_bin out in
    begin match s with Pdf.Stream {contents = _, Pdf.Got d} -> Pdfio.bytes_to_output_channel fh d | _ -> () end;
    close_out fh;
    let retcode =
    let scaling =
      if jpeg_to_jpeg_scale <> 100. then
        [(if interpolate then "-sample" else "-resize"); string_of_float jpeg_to_jpeg_scale ^ "%"]
      else
        []
    in
      let command = 
        Filename.quote_command path_to_convert ([out] @ scaling @ ["-quality"; string_of_float q ^ "%"; out2])
      in
        image_command command
    in
    if retcode = 0 then
      begin
        try
          let result = open_in_bin out2 in
          let newsize = in_channel_length result in
          let perc_ok = float newsize /. float size < percentage_threshold /. 100. in
          if newsize < size && perc_ok then
            begin
              let data = Pdfio.bytes_of_input_channel result in
              let w, h = try Cpdfjpeg.jpeg_dimensions data with e -> try close_in result; Cpdfjpeg.backup_jpeg_dimensions ~path_to_im:path_to_convert out2 with e -> (-1, -1) in
              if (w, h) = (-1, -1) then
                Printf.printf "Could not determine JPEG dimensions. Skipping.\n%!"
              else
                begin
                  if !debug_image_processing then Printf.printf "JPEG to JPEG %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
                  reference :=
                    Pdf.add_dict_entry (Pdf.add_dict_entry (Pdf.add_dict_entry dict "/Length" (Pdf.Integer newsize)) "/Width" (Pdf.Integer w)) "/Height" (Pdf.Integer h),
                    Pdf.Got data
                end
            end
          else
           begin
             if !debug_image_processing then Printf.printf "no size reduction\n%!"
           end;
          close_in result
       with e ->
         if !debug_image_processing then Printf.printf "Error %S\n%!" (Printexc.to_string e);
         remove out;
         remove out2
      end
    else
      if !debug_image_processing then Printf.printf "external process failed\n%!";
    remove out;
    remove out2

let jpeg2000_to_jpeg2000 pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~jpeg_to_jpeg_scale ~interpolate ~q ~path_to_convert s dict reference =
  if q < 0. || q > 100. then error "Out of range quality";
  complain_convert path_to_convert;
  let w = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer i) -> i | _ -> error "bad width" in
  let h = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer i) -> i | _ -> error "bad height" in
  if w * h < pixel_threshold then (if !debug_image_processing then Printf.printf "pixel threshold not met\n%!") else
  Pdf.getstream s;
  let size = match Pdf.lookup_direct pdf "/Length" dict with Some (Pdf.Integer i) -> i | _ -> 0 in
  if size < length_threshold then (if !debug_image_processing then Printf.printf "length threshold not met\n%!") else
  let out = Filename.temp_file "cpdf" "convertin.jp2" in
  let out2 = Filename.temp_file "cpdf" "convertout.jp2" in
  let fh = open_out_bin out in
    begin match s with Pdf.Stream {contents = _, Pdf.Got d} -> Pdfio.bytes_to_output_channel fh d | _ -> () end;
    close_out fh;
    let retcode =
    let scaling =
      if jpeg_to_jpeg_scale <> 100. then
        [(if interpolate then "-sample" else "-resize"); string_of_float jpeg_to_jpeg_scale ^ "%"]
      else
        []
    in
      let command = 
        Filename.quote_command path_to_convert ([out] @ scaling @ ["-quality"; string_of_float q ^ "%"; out2])
      in
        image_command command
    in
    if retcode = 0 then
      begin
        try
          let result = open_in_bin out2 in
          let newsize = in_channel_length result in
          let perc_ok = float newsize /. float size < percentage_threshold /. 100. in
          if newsize < size && perc_ok then
            begin
              let data = Pdfio.bytes_of_input_channel result in
              let w, h = try Cpdfjpeg2000.jpeg2000_dimensions data with e -> (-1, -1) in
              if (w, h) = (-1, -1) then
                Printf.printf "Could not determine JPEG2000 dimensions. Skipping.\n%!"
              else
                begin
                  if !debug_image_processing then Printf.printf "JPEG2000 to JPEG2000 %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
                  reference :=
                    Pdf.add_dict_entry (Pdf.add_dict_entry (Pdf.add_dict_entry dict "/Length" (Pdf.Integer newsize)) "/Width" (Pdf.Integer w)) "/Height" (Pdf.Integer h),
                    Pdf.Got data
                end
            end
          else
           begin
             if !debug_image_processing then Printf.printf "no size reduction\n%!"
           end;
          close_in result
       with e ->
         if !debug_image_processing then Printf.printf "Error %S\n%!" (Printexc.to_string e);
         remove out;
         remove out2
      end
    else
      if !debug_image_processing then Printf.printf "external process failed\n%!";
    remove out;
    remove out2

let suitable_num pdf dict =
  match Pdf.lookup_direct pdf "/ColorSpace" dict with
  | Some (Pdf.Name ("/DeviceRGB" | "/CalRGB")) -> 3
  | Some (Pdf.Name ("/DeviceGray" | "/CalGray")) -> 1
  | Some (Pdf.Name "/DeviceCMYK") -> 4
  | Some (Pdf.Array [Pdf.Name "/Lab"; _]) -> 3
  | Some (Pdf.Array [Pdf.Name "/ICCBased"; stream]) ->
      begin match Pdf.lookup_direct pdf "/N" stream with
      | Some (Pdf.Integer 3) -> 3
      | Some (Pdf.Integer 1) -> 1
      | Some (Pdf.Integer 4) -> 4
      | _ -> 0
      end
  | Some (Pdf.Array (Pdf.Name ("/Separation")::_)) -> ~-1
  | Some (Pdf.Array (Pdf.Name ("/Indexed")::_)) -> ~-2
  | _ -> 0

let lossless_out pdf ~invert_cmyk ~pixel_threshold ~length_threshold extension s dict reference =
  let old = !reference in
  let restore () = reference := old in
  let bpc = Pdf.lookup_direct pdf "/BitsPerComponent" dict in
  let components = suitable_num pdf dict in
  match components, bpc with
  | (1 | 3 | 4 | -1 | -2), Some (Pdf.Integer (8 as bpc))
  | -2, Some (Pdf.Integer (4 as bpc)) ->
      let w = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer i) -> i | _ -> error "bad width" in
      let h = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer i) -> i | _ -> error "bad height" in
      if w * h < pixel_threshold then (if !debug_image_processing then Printf.printf "pixel threshold not met\n%!"; None) else
      let size = match Pdf.lookup_direct pdf "/Length" dict with Some (Pdf.Integer i) -> i | _ -> 0 in
      if size < length_threshold then (if !debug_image_processing then Printf.printf "length threshold not met\n%!"; None) else
      begin
        Pdfcodec.decode_pdfstream_until_unknown pdf s;
        match Pdf.lookup_direct pdf "/Filter" (fst !reference) with Some x -> restore (); if !debug_image_processing then Printf.printf "%S Unable to decompress\n%!" (Pdfwrite.string_of_pdf x); None | None ->
        let out = Filename.temp_file "cpdf" ("convertin" ^ (if suitable_num pdf dict < 4 then ".pnm" else ".cmyk")) in
        let out2 = Filename.temp_file "cpdf" ("convertout" ^ (if extension <> ".png" then extension else if suitable_num pdf dict < 4 then extension else ".cmyk")) in
        let fh = open_out_bin out in
        let data = match s with Pdf.Stream {contents = _, Pdf.Got d} -> d | _ -> assert false in
        (if bpc = 4 && components = -2 then pnm_to_channel_4bpp else
         if components = 3 then pnm_to_channel_24 else
         if components = 4 then (if invert_cmyk then cmyk_to_channel_32_invert else cmyk_to_channel_32) else pnm_to_channel_8) fh w h data;
        close_out fh;
        Some (out, out2, size, components, w, h)
      end
  | colspace, bpc ->
    let colspace = Pdf.lookup_direct pdf "/ColorSpace" dict in
    let colspace, bpc, filter = 
      (match colspace with None -> "none" | Some x -> Pdfwrite.string_of_pdf x),
      (match bpc with None -> "none" | Some x -> Pdfwrite.string_of_pdf x),
      (match Pdf.lookup_direct pdf "/Filter" dict with None -> "none" | Some x -> Pdfwrite.string_of_pdf x)
    in
      print_string (Pdfwrite.string_of_pdf dict);
      print_string (Printf.sprintf "%s (%s) [%s]\n" colspace bpc filter);
      if !debug_image_processing then Printf.printf "colourspace not suitable\n%!";
      restore ();
      None (* an image we cannot or do not handle *)

let lossless_to_jpeg pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~qlossless ~path_to_convert s dict reference =
  complain_convert path_to_convert;
  match lossless_out pdf ~invert_cmyk:true ~pixel_threshold ~length_threshold ".jpg" s dict reference with
  | None -> ()
  | Some (_, _, _, -2, _, _) ->
      if !debug_image_processing then Printf.printf "skipping indexed colorspace\n%!"
  | Some (out, out2, size, components, w, h) ->
  let retcode =
    let command = 
      (Filename.quote_command path_to_convert
        ((if components = 4 then ["-depth"; "8"; "-size"; string_of_int w ^ "x" ^ string_of_int h] else []) @
        [out; "-quality"; string_of_float qlossless ^ "%"] @
        (if components = 1 then ["-colorspace"; "Gray"] else if components = 4 then ["-colorspace"; "CMYK"] else ["-type"; "truecolor"]) @
        [out2]))
    in
      image_command command
  in
  if retcode = 0 then
    begin
      try
        let result = open_in_bin out2 in
        let newsize = in_channel_length result in
        let perc_ok = float newsize /. float size < percentage_threshold /. 100. in
        if newsize < size && perc_ok then
          begin
            if !debug_image_processing then Printf.printf "lossless to JPEG %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
            reference :=
              (Pdf.add_dict_entry
                (Pdf.add_dict_entry dict "/Length" (Pdf.Integer newsize))
                 "/Filter"
                 (Pdf.Name "/DCTDecode")),
              Pdf.Got (Pdfio.bytes_of_input_channel result)
          end
        else
          begin
            if !debug_image_processing then Printf.printf "no size reduction\n%!"
          end;
          close_in result
      with
        e ->
          if !debug_image_processing then Printf.printf "Failed with %S\n%!" (Printexc.to_string e);
          remove out;
          remove out2
    end
  else
    if !debug_image_processing then Printf.printf "Return code not zero\n%!";
  remove out;
  remove out2

let test_components pdf dict =
  match suitable_num pdf dict with -1 | -2 -> 1 | x -> x 

let test_bpc pdf dict =
  match Pdf.lookup_direct pdf "/BitsPerComponent" dict with
  | Some (Pdf.Integer i) -> i
  | _ -> 0

(* Read dimensions output from imagemagick's info: "%w %h"*)
let read_info_dimensions filename =
  match Pdfgenlex.lex (Pdfio.input_of_string (contents_of_file filename)) with
  | [LexInt w; LexInt h] -> (w, h)
  | _ -> error "read_info_dimensions"

let lossless_resample pdf ~pixel_threshold ~length_threshold ~factor ~interpolate ~path_to_convert s dict reference =
  complain_convert path_to_convert;
  let in_components = test_components pdf dict in
  let in_bpc = test_bpc pdf dict in
  (*Printf.printf "***lossless_resample IN dictionary: %S\n" (Pdfwrite.string_of_pdf dict); *)
  (*Printf.printf "\n***IN components = %i, bpc = %i\n" in_components in_bpc;*)
  let out3 = Filename.temp_file "cpdf" "dimens" in 
  match lossless_out pdf ~invert_cmyk:false ~pixel_threshold ~length_threshold ".png" s dict reference with
  | None -> ()
  | Some (out, out2, size, components, w, h) ->
  let retcode =
    let command = 
      (Filename.quote_command path_to_convert
        ((if components = 4 then ["-depth"; "8"; "-size"; string_of_int w ^ "x" ^ string_of_int h] else []) @ [out] @
        (if components = 1 then ["-define"; "png:color-type=0"; "-colorspace"; "Gray"]
         else if components = 3 then ["-define"; "png:color-type=2"; "-colorspace"; "RGB"]
         else if components = 4 then ["-colorspace"; "CMYK"] else []) @
        [if interpolate && components > -2 then "-resize" else "-sample"; string_of_float factor ^ "%"] @ (if components = 4 then ["-write"] else []) @ [out2] @
        (if components = 4 then ["-format"; "%w %h"; "info:"] else [])))
      ^
        (if components = 4 then " >" ^ out3 else "") (* Quoting would mangle redirection. *)
    in
      image_command command
  in
  try
  if retcode = 0 then
    begin
      let result = open_in_bin out2 in
      let newsize = in_channel_length result in
      if newsize < size then
        begin
          match rev (explode out2) with
          | 'k'::'y'::'m'::'c'::_ ->
            (* We have '.cmyk' not '.png' returned. *)
            let new_w, new_h = read_info_dimensions out3 in
            let dict =
              Pdf.remove_dict_entry
                (Pdf.remove_dict_entry
                  (Pdf.add_dict_entry
                    (Pdf.add_dict_entry dict "/Height" (Pdf.Integer new_h))
                    "/Width" (Pdf.Integer new_w))
                  "/DecodeParms")
                "/Filter"
            in
            if !debug_image_processing then Printf.printf "lossless resample %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
            reference := (dict, Pdf.Got (Pdfio.bytes_of_input_channel result))
          | _ -> 
            reference :=
              (match fst (obj_of_png_data pdf (Pdfio.bytes_of_input_channel result)) with
              | Pdf.Stream {contents = Pdf.Dictionary d, data} as s ->
                  let out_components = test_components pdf s in
                  let out_bpc = test_bpc pdf s in
                  (*Printf.printf "***OUT components = %i, bpc = %i\n" out_components out_bpc;*)
                  let rgb_to_grey_special =
                    let was_rgb =
                      match Pdf.lookup_direct pdf "/ColorSpace" dict with
                      | Some (Pdf.Name ("/DeviceRGB" | "/CalRGB")) -> true
                      | _ -> false
                    in
                      in_bpc = out_bpc && in_components = 3 && out_components = 1 && was_rgb
                  in
                  (*Printf.printf "***rgb_to_grey_special = %b\n" rgb_to_grey_special;*)
                  if (out_components <> in_components || in_bpc <> out_bpc) && not rgb_to_grey_special then
                    begin
                      if !debug_image_processing then Printf.printf "wrong bpc / components returned. Skipping.\n%!";
                      !reference
                    end
                  else
                  begin
                    if !debug_image_processing then Printf.printf "lossless resample %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
                    let d' = fold_right (fun (k, v) d -> if k <> "/ColorSpace" || rgb_to_grey_special then add k v d else d) d (match dict with Pdf.Dictionary x -> x | _ -> []) in
                      (*Printf.printf "***lossless_resample OUT dictionary: %S\n" (Pdfwrite.string_of_pdf (Pdf.Dictionary d')); *)
                      (Pdf.Dictionary d', data)
                  end
              | _ -> assert false)
        end
      else
        begin
          if !debug_image_processing then Printf.printf "no size reduction\n%!"
        end;
        close_in result
    end;
    remove out;
    remove out2;
    remove out3
  with e ->
    if !debug_image_processing then Printf.printf "Unable: %S\n" (Printexc.to_string e);
    remove out;
    remove out2;
    remove out3

let lossless_resample_target_dpi objnum pdf ~pixel_threshold ~length_threshold ~factor ~target_dpi_info ~interpolate ~path_to_convert s dict reference =
  try
    let real_factor = factor /. Hashtbl.find target_dpi_info objnum *. 100.  in
      if real_factor < 100. then
        lossless_resample pdf ~pixel_threshold ~length_threshold ~factor:real_factor ~interpolate ~path_to_convert s dict reference
      else
        if !debug_image_processing then Printf.printf "failed to meet dpi target\n%!"
  with
    Not_found -> if !debug_image_processing then Printf.printf "Warning: orphaned image, skipping\n" (* Could not find DPI data - an orphan image. *)

let jpeg_to_jpeg_wrapper objnum pdf ~target_dpi_info ~pixel_threshold ~length_threshold ~percentage_threshold ~jpeg_to_jpeg_scale ~jpeg_to_jpeg_dpi ~interpolate ~q ~path_to_convert s dict reference =
  if jpeg_to_jpeg_dpi = 0. then
    jpeg_to_jpeg pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~jpeg_to_jpeg_scale ~interpolate ~q ~path_to_convert s dict reference
  else
    try
      let factor = jpeg_to_jpeg_dpi in
      let real_factor = factor /. Hashtbl.find target_dpi_info objnum *. 100. in
        if real_factor < 100. then
          jpeg_to_jpeg pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~jpeg_to_jpeg_scale:real_factor ~interpolate ~q ~path_to_convert s dict reference
        else
          if !debug_image_processing then Printf.printf "failed to meet dpi target\n%!"
    with
      Not_found -> if !debug_image_processing then Printf.printf "Warning: orphaned image, skipping\n" (* Could not find DPI data - an orphan image. *)

let jpeg2000_to_jpeg2000_wrapper objnum pdf ~target_dpi_info ~pixel_threshold ~length_threshold ~percentage_threshold ~jpeg_to_jpeg_scale ~jpeg_to_jpeg_dpi ~interpolate ~q ~path_to_convert s dict reference =
  if jpeg_to_jpeg_dpi = 0. then
    jpeg2000_to_jpeg2000 pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~jpeg_to_jpeg_scale ~interpolate ~q ~path_to_convert s dict reference
  else
    try
      let factor = jpeg_to_jpeg_dpi in
      let real_factor = factor /. Hashtbl.find target_dpi_info objnum *. 100. in
        if real_factor < 100. then
          jpeg2000_to_jpeg2000 pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~jpeg_to_jpeg_scale:real_factor ~interpolate ~q ~path_to_convert s dict reference
        else
          if !debug_image_processing then Printf.printf "failed to meet dpi target\n%!"
    with
      Not_found -> if !debug_image_processing then Printf.printf "Warning: orphaned image, skipping\n" (* Could not find DPI data - an orphan image. *)

let recompress_1bpp_ccitt_lossless ~pixel_threshold ~length_threshold pdf s dict reference =
  let old = !reference in
  let restore () = reference := old in
  let w = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer i) -> i | _ -> error "bad width" in
  let h = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer i) -> i | _ -> error "bad height" in
  (*if w * h < pixel_threshold then (if !debug_image_processing then Printf.printf "pixel threshold not met\n%!") else*)
  let size = match Pdf.lookup_direct pdf "/Length" dict with Some (Pdf.Integer i) -> i | _ -> 0 in
  (*if size < length_threshold then (if !debug_image_processing then Printf.printf "length threshold not met\n%!") else*)
    begin
      Pdfcodec.decode_pdfstream_until_unknown pdf s;
      match Pdf.lookup_direct pdf "/Filter" (fst !reference) with
      | Some x ->
          if !debug_image_processing then Printf.printf "could not decode - skipping %s length %i\n%!" (Pdfwrite.string_of_pdf x) size;
          restore ()
      | None ->
        let data = match s with Pdf.Stream {contents = _, Pdf.Got d} -> d | _ -> assert false in
        let compressed = Pdfcodec.encode_ccitt w h data in
        let newsize = bytes_size compressed in
          if true (*newsize < size*) then
            begin
              if !debug_image_processing then Printf.printf "1bpp to CCITT %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
              reference :=
                (Pdf.add_dict_entry
                (Pdf.add_dict_entry
                  (Pdf.add_dict_entry dict "/Length" (Pdf.Integer newsize))
                   "/Filter"
                   (Pdf.Array [Pdf.Name "/CCITTFaxDecode"])) "/DecodeParms" (Pdf.Array [Pdf.Dictionary [("/K", Pdf.Integer 0); ("/Columns", Pdf.Integer w)]])),
                Pdf.Got (compressed)
            end
           else
             if !debug_image_processing then Printf.printf "no size reduction\n%!"
    end

let recompress_1bpp_ccittg4_lossless ~pixel_threshold ~length_threshold pdf s dict reference =
  let old = !reference in
  let restore () = reference := old in
  let w = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer i) -> i | _ -> error "bad width" in
  let h = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer i) -> i | _ -> error "bad height" in
  (*if w <> 25 || h <> 6 then (if !debug_image_processing then Printf.printf "Debug skipping...\n%!") else*)
  (*if w * h < pixel_threshold then (if !debug_image_processing then Printf.printf "pixel threshold not met\n%!") else*)
  let size = match Pdf.lookup_direct pdf "/Length" dict with Some (Pdf.Integer i) -> i | _ -> 0 in
  (*if size < length_threshold then (if !debug_image_processing then Printf.printf "length threshold not met\n%!") else*)
    begin
      Pdfcodec.decode_pdfstream_until_unknown pdf s;
      match Pdf.lookup_direct pdf "/Filter" (fst !reference) with
      | Some x ->
          if !debug_image_processing then Printf.printf "could not decode - skipping %s length %i\n%!" (Pdfwrite.string_of_pdf x) size;
          restore ()
      | None ->
        let data = match s with Pdf.Stream {contents = _, Pdf.Got d} -> d | _ -> assert false in
        let compressed = Pdfcodec.encode_ccittg4 w h data in
        let newsize = bytes_size compressed in
          if true (* newsize < size *) then
            begin
              if !debug_image_processing then Printf.printf "1bpp to CCITT G4 %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
              reference :=
                (Pdf.add_dict_entry
                (Pdf.add_dict_entry
                  (Pdf.add_dict_entry dict "/Length" (Pdf.Integer newsize))
                   "/Filter"
                   (Pdf.Array [Pdf.Name "/CCITTFaxDecode"])) "/DecodeParms" (Pdf.Array [Pdf.Dictionary [("/K", Pdf.Integer ~-1); ("/Columns", Pdf.Integer w)]])),
                Pdf.Got (compressed)
            end
           else
             if !debug_image_processing then Printf.printf "no size reduction\n%!"
    end

let recompress_1bpp_jbig2_lossless ~pixel_threshold ~length_threshold ~path_to_jbig2enc pdf s dict reference =
  complain_jbig2enc path_to_jbig2enc;
  let old = !reference in
  let restore () = reference := old in
  let w = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer i) -> i | _ -> error "bad width" in
  let h = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer i) -> i | _ -> error "bad height" in
  if w * h < pixel_threshold then (if !debug_image_processing then Printf.printf "pixel threshold not met\n%!") else (* (but also, jbig2enc fails on tiny images) *)
  let size = match Pdf.lookup_direct pdf "/Length" dict with Some (Pdf.Integer i) -> i | _ -> 0 in
  if size < length_threshold then (if !debug_image_processing then Printf.printf "length threshold not met\n%!") else
  begin
    Pdfcodec.decode_pdfstream_until_unknown pdf s;
    match Pdf.lookup_direct pdf "/Filter" (fst !reference) with
    | Some x ->
        if !debug_image_processing then Printf.printf "could not decode - skipping %s length %i\n%!" (Pdfwrite.string_of_pdf x) size;
        restore ()
    | None ->
      let out = Filename.temp_file "cpdf" "convertin.pnm" in
      let out2 = Filename.temp_file "cpdf" "convertout.jbig2" in
      let fh = open_out_bin out in
      let data = match s with Pdf.Stream {contents = _, Pdf.Got d} -> d | _ -> assert false in
        pnm_to_channel_1_inverted fh w h data;
        close_out fh;
        let retcode =
          let command = Filename.quote_command ~stdout:out2 path_to_jbig2enc ["-d"; "-p"; out] in
            image_command command
        in
          if retcode <> 0 then
            restore ()
          else
            begin
              let result = open_in_bin out2 in
              let newsize = in_channel_length result in
              if newsize < size then
                begin
                  if !debug_image_processing then Printf.printf "1bpp to JBIG2 %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
                  reference :=
                    (Pdf.remove_dict_entry
                    (Pdf.add_dict_entry
                      (Pdf.add_dict_entry dict "/Length" (Pdf.Integer newsize))
                       "/Filter"
                       (Pdf.Name "/JBIG2Decode")) "/DecodeParms"),
                    Pdf.Got (Pdfio.bytes_of_input_channel result)
                end
               else
                begin
                  if !debug_image_processing then Printf.printf "no size reduction\n%!"
                end;
                close_in result
            end;
          remove out;
          remove out2
  end

(* Recompress 1bpp images (except existing JBIG2 compressed ones) to lossy jbig2 *)
let preprocess_jbig2_lossy ~path_to_jbig2enc ~jbig2_lossy_threshold ~length_threshold ~pixel_threshold ~dpi_threshold inrange highdpi pdf =
 complain_jbig2enc path_to_jbig2enc;
 let objnum_name_pairs = ref [] in
 let process_obj objnum s =
   match s with
   | Pdf.Stream ({contents = dict, _} as reference) ->
       let old = !reference in
       let restore () = reference := old in
       if Hashtbl.mem inrange objnum && (dpi_threshold = 0. || Hashtbl.mem highdpi objnum) then begin match
         Pdf.lookup_direct pdf "/Subtype" dict,
         Pdf.lookup_direct pdf "/BitsPerComponent" dict,
         Pdf.lookup_direct pdf "/ImageMask" dict
       with
       | Some (Pdf.Name "/Image"), Some (Pdf.Integer 1), _
       | Some (Pdf.Name "/Image"), _, Some (Pdf.Boolean true) ->
           let w = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer i) -> i | _ -> error "bad width" in
           let h = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer i) -> i | _ -> error "bad height" in
           if w * h < pixel_threshold then (if !debug_image_processing then Printf.printf "pixel threshold not met\n%!") else (* (but also, jbig2enc fails on tiny images) *)
           let size = match Pdf.lookup_direct pdf "/Length" dict with Some (Pdf.Integer i) -> i | _ -> 0 in
           if size < length_threshold then (if !debug_image_processing then Printf.printf "length threshold not met\n%!") else
             begin
               Pdfcodec.decode_pdfstream_until_unknown pdf s;
               match Pdf.lookup_direct pdf "/Filter" (fst !reference) with
               | Some x ->
                   if !debug_image_processing then Printf.printf "could not decode - skipping %s length %i\n%!" (Pdfwrite.string_of_pdf x) size;
                   restore ()
               | None ->
                   let out = Filename.temp_file "cpdf" "convertin.pnm" in
                   let fh = open_out_bin out in
                   let data = match s with Pdf.Stream {contents = _, Pdf.Got d} -> d | _ -> assert false in
                     pnm_to_channel_1_inverted fh w h data;
                     close_out fh;
                     if !debug_image_processing then Printf.printf "JBIG2Lossy: obj %i is suitable\n%!" objnum;
                     objnum_name_pairs := (objnum, out)::!objnum_name_pairs
             end
       | _ -> () (* not a 1bpp image *)
       end
   | _ -> () (* not a stream *)
 in
   Pdf.objiter process_obj pdf;
   if length !objnum_name_pairs > 10000 then Pdfe.log "Too many jbig2 streams" else
   if length !objnum_name_pairs = 0 then () else
   let jbig2out = Filename.temp_file "cpdf" "jbig2" in
   let retcode =
     let command =
       Filename.quote_command
         path_to_jbig2enc
         ?stderr:(if !debug_image_processing then None else Some Filename.null)
         (["-p"; "-s"; "-d"; "-t"; string_of_float jbig2_lossy_threshold; "-b"; jbig2out] @ map snd !objnum_name_pairs)
     in
       image_command command
   in
   iter remove (map snd !objnum_name_pairs);
   if retcode = 0 then
     begin
       let globals = bytes_of_string (contents_of_file (jbig2out ^ ".sym")) in
       let globalobj =
         Pdf.addobj pdf (Pdf.Stream {contents = Pdf.Dictionary [("/Length", Pdf.Integer (bytes_size globals))], Pdf.Got globals})
       in
         iter2
           (fun (objnum, _) i ->
              let data = bytes_of_string (contents_of_file (jbig2out ^ Printf.sprintf ".%04i" i)) in
              let basic_obj =
                Pdf.Stream
                  {contents =
                     Pdf.Dictionary [("/Length", Pdf.Integer (bytes_size data));
                                     ("/Filter", Pdf.Name "/JBIG2Decode");
                                     ("/DecodeParms", Pdf.Dictionary [("/JBIG2Globals", Pdf.Indirect globalobj)])],
                     Pdf.Got data}
              in
              let dict = match Pdf.lookup_obj pdf objnum with Pdf.Stream {contents = d, _} -> d | _ -> Pdf.Dictionary [] in
                Pdf.addobj_given_num pdf
                 (objnum,
                  (match basic_obj with
                   | Pdf.Stream {contents = Pdf.Dictionary d, data} ->
                       let d' = fold_right (fun (k, v) d -> add k v d) d (match dict with Pdf.Dictionary x -> x | _ -> []) in
                         Pdf.Stream {contents = Pdf.Dictionary d', data}
                   | _ -> assert false)))
           !objnum_name_pairs
           (indx0 !objnum_name_pairs)
     end
   else
     begin
       Pdfe.log "Call to jbig2enc failed"
     end;
   iter (fun i -> remove (jbig2out ^ Printf.sprintf ".%04i" i)) (indx0 !objnum_name_pairs);
   remove (jbig2out ^ ".sym")

let lossless_to_jpeg2000 objnum pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~qlossless2000 ~path_to_convert s dict reference =
  complain_convert path_to_convert;
  match lossless_out pdf ~invert_cmyk:false ~pixel_threshold ~length_threshold ".jp2" s dict reference with
  | None -> ()
  | Some (_, _, _, -2, _, _) ->
      if !debug_image_processing then Printf.printf "skipping indexed colorspace\n%!"
  | Some (out, out2, size, components, w, h) ->
  let retcode =
    let command = 
      (Filename.quote_command path_to_convert
        ((if components = 4 then ["-depth"; "8"; "-size"; string_of_int w ^ "x" ^ string_of_int h] else []) @
        [out; "-quality"; string_of_float qlossless2000 ^ "%"] @
        [out2]))
    in
      image_command command
  in
  if retcode = 0 then
    begin
      try
        let result = open_in_bin out2 in
        let newsize = in_channel_length result in
        let perc_ok = float newsize /. float size < percentage_threshold /. 100. in
        if newsize < size && perc_ok then
          begin
            if !debug_image_processing then Printf.printf "lossless to JPEG2000 %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
            reference :=
              (Pdf.remove_dict_entry
                (Pdf.add_dict_entry
                (Pdf.add_dict_entry dict "/Length" (Pdf.Integer newsize))
                 "/Filter"
                 (Pdf.Name "/JPXDecode")) "/ColorSpace"),
              Pdf.Got (Pdfio.bytes_of_input_channel result)
          end
        else
          begin
            if !debug_image_processing then Printf.printf "no size reduction\n%!"
          end;
          close_in result
      with
        e ->
          if !debug_image_processing then Printf.printf "Failed with %S\n%!" (Printexc.to_string e);
          remove out;
          remove out2
    end
  else
    if !debug_image_processing then Printf.printf "Return code not zero\n%!";
  remove out;
  remove out2

let process
  ~q ~qlossless ~qlossless2000 ~onebppmethod ~onebppmethodforce ~jbig2_lossy_threshold ~length_threshold ~percentage_threshold ~pixel_threshold ~dpi_threshold
  ~factor ~interpolate ~jpeg_to_jpeg_scale ~jpeg_to_jpeg_dpi ~path_to_jbig2enc ~path_to_convert range pdf
=
  let inrange =
    match images ~inline:false pdf range with
    | `List l -> hashset_of_list (map (function `Assoc (("Object", `Int i)::_) -> i | _ -> assert false) l)
    | _ -> assert false
  in
  let highdpi, target_dpi_info =
    let objnums, dpi =
      if dpi_threshold = 0. && factor > 0. && jpeg_to_jpeg_dpi = 0. then ([], []) else
        let results = image_resolution ~inline:false pdf range max_float in
          (*iter (fun (_, _, _, _, wdpi, hdpi, objnum) -> Printf.printf "From image_resolution %f %f %i\n" wdpi hdpi objnum) results;*)
          let cmp (_, _, _, _, _, _, a) (_, _, _, _, _, _, b) = compare a b in
          let sets = collate cmp (sort cmp results) in
          let heads = map hd (map (sort (fun (_, _, _, _, a, b, _) (_, _, _, _, c, d, _) -> compare (fmin a b) (fmin c d))) sets) in
          (*iter (fun (_, _, _, _, wdpi, hdpi, objnum) -> Printf.printf "Lowest resolution exemplar %f %f %i\n" wdpi hdpi objnum) heads;*)
          let needed = keep (fun (_, _, _, _, wdpi, hdpi, objnum) -> fmin wdpi hdpi > dpi_threshold) heads in
          (*iter (fun (_, _, _, _, wdpi, hdpi, objnum) -> Printf.printf "keep %f %f %i\n" wdpi hdpi objnum) needed;*)
            map (fun (_, _, _, _, _, _, objnum) -> objnum) needed,
            map (fun (_, _, _, _, wdpi, hdpi, objnum) -> (objnum, fmin wdpi hdpi)) heads
            (*iter (fun (x, d) -> Printf.printf "obj %i at %f dpi\n" x d) r; r*)
    in
      hashset_of_list objnums, hashtable_of_dictionary dpi
  in
  begin match onebppmethod with "JBIG2Lossy" -> preprocess_jbig2_lossy ~path_to_jbig2enc ~jbig2_lossy_threshold ~dpi_threshold ~length_threshold ~pixel_threshold inrange highdpi pdf | _ -> () end;
  let nobjects = Pdf.objcard pdf in
  let ndone = ref 0 in
  let process_obj objnum s =
    match s with
    | Pdf.Stream ({contents = dict, _} as reference) ->
        ndone += 1;
        if Hashtbl.mem inrange objnum && (dpi_threshold = 0. || Hashtbl.mem highdpi objnum) then begin match
          Pdf.lookup_direct pdf "/Subtype" dict,
          Pdf.lookup_direct pdf "/Filter" dict,
          Pdf.lookup_direct pdf "/BitsPerComponent" dict,
          Pdf.lookup_direct pdf "/ImageMask" dict
        with
        | Some (Pdf.Name "/Image"), Some (Pdf.Name "/DCTDecode" | Pdf.Array [Pdf.Name "/DCTDecode"]), _, _ ->
            if q < 100. || jpeg_to_jpeg_scale <> 100. || jpeg_to_jpeg_dpi <> 0.  then
              begin
                if !debug_image_processing then Printf.printf "(%i/%i) Object %i (JPEG)... %!" !ndone nobjects objnum;
                jpeg_to_jpeg_wrapper objnum pdf ~target_dpi_info ~pixel_threshold ~length_threshold ~percentage_threshold ~jpeg_to_jpeg_scale ~jpeg_to_jpeg_dpi ~interpolate ~q ~path_to_convert s dict reference
              end
        | Some (Pdf.Name "/Image"), Some (Pdf.Name "/JPXDecode" | Pdf.Array [Pdf.Name "/JPXDecode"]), _, _ ->
            if qlossless2000 < 0. then
              begin
                if !debug_image_processing then Printf.printf "(%i/%i) Object %i (JPEG2000)... %!" !ndone nobjects objnum;
                jpeg2000_to_jpeg2000_wrapper objnum pdf ~target_dpi_info ~pixel_threshold ~length_threshold ~percentage_threshold ~jpeg_to_jpeg_scale ~jpeg_to_jpeg_dpi ~interpolate ~q:~-.qlossless2000 ~path_to_convert s dict reference
              end
        | Some (Pdf.Name "/Image"), _, Some (Pdf.Integer 1), _
        | Some (Pdf.Name "/Image"), _, _, Some (Pdf.Boolean true) ->
            begin match onebppmethod with
            | "JBIG2" ->
                begin
                  if !debug_image_processing then Printf.printf "(%i/%i) Object %i (1bpp)... %!" !ndone nobjects objnum;
                  recompress_1bpp_jbig2_lossless ~pixel_threshold ~length_threshold ~path_to_jbig2enc pdf s dict reference
                end
            | "CCITT" ->
                begin
                  if !debug_image_processing then Printf.printf "(%i/%i) Object %i (1bpp)... %!" !ndone nobjects objnum;
                  recompress_1bpp_ccitt_lossless ~pixel_threshold ~length_threshold pdf s dict reference
                end
            | "CCITTG4" ->
                begin
                  if !debug_image_processing then Printf.printf "(%i/%i) Object %i (1bpp)... %!" !ndone nobjects objnum;
                  recompress_1bpp_ccittg4_lossless ~pixel_threshold ~length_threshold pdf s dict reference
                end
            | "JBIG2Lossy" -> ()
            | "None" -> ()
            | _ -> error "unknown 1bpp method"
            end
        | Some (Pdf.Name "/Image"), _, _, _ ->
            (* FIXME Define a proper data type to replace the mess of numbers for classification of lossless transforms. Also above for JPEG. *)
            if qlossless < 101. then
              begin
                if !debug_image_processing then Printf.printf "(%i/%i) Object %i (lossless)... %!" !ndone nobjects objnum;
                lossless_to_jpeg pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~qlossless ~path_to_convert s dict reference
              end
            else if qlossless2000 < 101. then
              begin
                if !debug_image_processing then Printf.printf "(%i/%i) Object %i (lossless)... %!" !ndone nobjects objnum;
                lossless_to_jpeg2000 objnum pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~qlossless2000 ~path_to_convert s dict reference;
              end
            else
              begin
                if factor < 101. then
                  begin
                    if !debug_image_processing then Printf.printf "(%i/%i) Object %i (lossless)... %!" !ndone nobjects objnum;
                    if factor < 0. then
                      lossless_resample_target_dpi objnum pdf ~pixel_threshold ~length_threshold ~factor:~-.factor ~target_dpi_info ~interpolate ~path_to_convert s dict reference
                    else
                      lossless_resample pdf ~pixel_threshold ~length_threshold ~factor ~interpolate ~path_to_convert s dict reference
                  end
              end
        | _ -> () (* not an image *)
        end
    | _ -> ndone += 1 (* not a stream *)
  in
    Pdf.objiter process_obj pdf
