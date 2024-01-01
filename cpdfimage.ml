open Pdfutil
open Pdfio
open Cpdferror

let debug_image_processing = ref false

let remove x =
  try Sys.remove x with _ -> ()

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
              Sys.command (Filename.quote_command path_to_im [pnm; png])
            with
              0 -> remove pnm
            | _ -> 
              Pdfe.log "Call to imagemagick failed: did you specify -p2p or -im correctly?\n";
              remove pnm
            end
          end
        | Some path_to_p2p ->
          begin match
            Sys.command (Filename.quote_command path_to_p2p ~stdout:png ["-gamma"; "0.45"; "-quiet"; pnm])
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

let extract_images_inner ~raw ?path_to_p2p ?path_to_im encoding serial pdf resources stem pnum images =
  let names = map
    (fun _ ->
       Cpdfbookmarks.name_of_spec
         encoding [] pdf 0 (stem ^ "-p" ^ string_of_int pnum)
         (let r = !serial in serial := !serial + 1; r) "" 0 0) (indx images)
  in
    iter2 (write_image ~raw ?path_to_p2p ?path_to_im pdf resources) names images

let rec extract_images_form_xobject ~raw ?path_to_p2p ?path_to_im encoding dedup dedup_per_page pdf serial stem pnum form =
  let resources =
    match Pdf.lookup_direct pdf "/Resources" form with
      Some (Pdf.Dictionary d) -> Pdf.Dictionary d
    | _ -> Pdf.Dictionary []
  in
    let images =
      let xobjects =
        match Pdf.lookup_direct pdf "/XObject" resources with
        | Some (Pdf.Dictionary elts) -> map snd elts
        | _ -> []
      in
        (* Remove any already in !written. Add any remaining to !written, if !args.dedup or !args.dedup_page *)
        let images = keep (fun o -> Pdf.lookup_direct pdf "/Subtype" o = Some (Pdf.Name "/Image")) xobjects in
        let already_written, images = List.partition (function Pdf.Indirect n -> mem n !written | _ -> false) images in
          if dedup || dedup_per_page then
            written := (option_map (function Pdf.Indirect n -> Some n | _ -> None) images) @ !written;
          images
    in
      extract_images_inner ~raw ?path_to_p2p ?path_to_im encoding serial pdf resources stem pnum images

let extract_images ?(raw=false) ?path_to_p2p ?path_to_im encoding dedup dedup_per_page pdf range stem =
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
                 extract_images_inner ~raw ?path_to_p2p ?path_to_im encoding serial pdf page.Pdfpage.resources stem pnum images;
                 iter (extract_images_form_xobject ~raw ?path_to_p2p ?path_to_im encoding dedup dedup_per_page pdf serial stem pnum) forms)
          pages
          (indx pages)

(* Image resolution *)
type xobj =
  | Image of int * int (* width, height *)
  | Form of Pdftransform.transform_matrix * Pdf.pdfobject * Pdf.pdfobject (* Will add actual data later. *)

let image_results = ref []

let add_image_result i =
  image_results := i::!image_results

(* Given a page and a list of (pagenum, name, thing) *)
let rec image_resolution_page pdf page pagenum dpi (images : (int * string * xobj) list) =
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
                   | (_, a, _) as h::_ when a = k -> h
                   | _::t -> lookup_image k t 
                 in
                   begin match lookup_image xobject images with
                   | (pagenum, name, Form (xobj_matrix, content, resources)) ->
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
                              image_resolution newpdf [pagenum] dpi
                   | (pagenum, name, Image (w, h)) ->
                       let lx = Pdfunits.points (distance_between o x) Pdfunits.Inch in
                       let ly = Pdfunits.points (distance_between o y) Pdfunits.Inch in
                         let wdpi = float w /. lx
                         and hdpi = float h /. ly in
                           add_image_result (pagenum, xobject, w, h, wdpi, hdpi)
                           (*Printf.printf "%i, %s, %i, %i, %f, %f\n" pagenum xobject w h wdpi hdpi*)
                         (*i else
                           Printf.printf "S %i, %s, %i, %i, %f, %f\n" pagenum xobject (int_of_float w) (int_of_float h) wdpi hdpi i*)
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

and image_resolution pdf range dpi =
  let images = ref [] in
    Cpdfpage.iter_pages
      (fun pagenum page ->
         (* 1. Get all image names and their native resolutions from resources as string * int * int *)
         match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
          | Some (Pdf.Dictionary xobjects) ->
              iter
                (function (name, xobject) ->
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
                         images := (pagenum, name, Image (int_of_float width, int_of_float height))::!images
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
                         images := (pagenum, name, Form (matrix, contents, resources))::!images
                   | _ -> ()
                )
                xobjects
          | _ -> ())
      pdf
      range;
      (* Now, split into differing pages, and call [image_resolution_page] on each one *)
      let pagesplits =
        map
          (function (a, _, _)::_ as ls -> (a, ls) | _ -> assert false)
          (collate (fun (a, _, _) (b, _, _) -> compare a b) (rev !images))
      and pages =
        Pdfpage.pages_of_pagetree pdf
      in
        iter
          (function (pagenum, images) ->
             let page = select pagenum pages in
               image_resolution_page pdf page pagenum dpi images)
          pagesplits

let image_resolution pdf range dpi =
  image_results := [];
  image_resolution pdf range dpi;
  rev !image_results

(* All the images in file referenced at least once from the given range of pages. *)
let images pdf range =
  let images = null_hash () in
  let formnums = null_hash () in
  let rec process_xobject resources pagenum page (name, xobject) =
    match Pdf.lookup_direct pdf "/Subtype" xobject with
    | Some (Pdf.Name "/Image") ->
        begin match xobject with
        | Pdf.Indirect i ->
            begin match Hashtbl.find images i with
            | (pagenums, n, w, h, s, bpc, cs, f) ->
                Hashtbl.replace images i (pagenum::pagenums, n, w, h, s, bpc, cs, f)
            | exception Not_found ->
                let width =
                  match Pdf.lookup_direct pdf "/Width" xobject with
                  | Some x -> Pdf.getnum pdf x
                  | None -> 1.
                and height =
                  match Pdf.lookup_direct pdf "/Height" xobject with
                  | Some x -> Pdf.getnum pdf x
                  | None -> 1.
                and size =
                  match Pdf.lookup_direct pdf "/Length" xobject with
                  | Some (Pdf.Integer x) -> x
                  | _ -> 0
                and bpc =
                  match Pdf.lookup_direct pdf "/BitsPerComponent" xobject with
                  | Some (Pdf.Integer x) -> x
                  | _ -> 0
                and colourspace =
                  match Pdf.lookup_direct pdf "/ColorSpace" xobject with
                  | Some x -> Some (Pdfspace.string_of_colourspace (Pdfspace.read_colourspace pdf resources x))
                  | None -> None
                and filter =
                  match Pdf.lookup_direct pdf "/Filter" xobject with
                  | Some (Pdf.Array [x]) | Some x -> Some (Pdfwrite.string_of_pdf x)
                  | None -> None
                in
                  Hashtbl.replace images i ([pagenum], name, int_of_float width, int_of_float height, size, bpc, colourspace, filter)
            end
        | _ -> ()
        end
    | Some (Pdf.Name "/Form") ->
        begin match xobject with
        | Pdf.Indirect i ->
            begin match Hashtbl.find formnums i with
            | () -> ()
            | exception Not_found ->
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
           match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
            | Some (Pdf.Dictionary xobjects) ->
                iter (process_xobject page.Pdfpage.resources pagenum page) xobjects
            | _ -> ())
        pdf
        range;
        let images = list_of_hashtbl images in
        let images = map (fun (i, (pnums, n, w, h, s, bpc, c, filter)) -> (i, (setify (sort compare pnums), n, w, h, s, bpc, c, filter))) images in
        let images = sort (fun (_, (pnums, _, _, _, _, _, _, _)) (_, (pnums', _, _, _, _, _, _, _)) -> compare (hd pnums) (hd pnums')) images in
         `List
           (map
             (fun (i, (pnums, n, w, h, size, bpc, cs, filter)) ->
               `Assoc [("Object", `Int i);
                       ("Pages", `List (map (fun x -> `Int x) pnums));
                       ("Name", `String n);
                       ("Width", `Int w);
                       ("Height", `Int h);
                       ("Bytes", `Int size);
                       ("BitsPerComponent", `Int bpc);
                       ("Colourspace", match cs with None -> `Null | Some s -> `String s);
                       ("Filter", match filter with None -> `Null | Some s -> `String s)])
             images)

let obj_of_jpeg_data data =
  let w, h = Cpdfjpeg.jpeg_dimensions data in
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

let obj_of_png_data data =
  let png = Cpdfpng.read_png (Pdfio.input_of_bytes data) in
    let d =
      ["/Length", Pdf.Integer (Pdfio.bytes_size png.idat);
       "/Filter", Pdf.Name "/FlateDecode";
       "/Subtype", Pdf.Name "/Image";
       "/BitsPerComponent", Pdf.Integer png.bitdepth;
       "/ColorSpace", Pdf.Name (match png.colortype with 0 -> "/DeviceGray" | 2 -> "/DeviceRGB" | _ -> error "obj_of_png_data 1");
       "/DecodeParms", Pdf.Dictionary
                        ["/BitsPerComponent", Pdf.Integer png.bitdepth;
                         "/Colors", Pdf.Integer (match png.colortype with 0 -> 1 | 2 -> 3 | _ -> error "obj_of_png_data 2");
                         "/Columns", Pdf.Integer png.width;
                         "/Predictor", Pdf.Integer 15];
       "/Width", Pdf.Integer png.width;
       "/Height", Pdf.Integer png.height]
    in
      Pdf.Stream {contents = (Pdf.Dictionary d, Pdf.Got png.idat)}, []

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

let image_of_input fobj i =
  let pdf = Pdf.empty () in
  let data = Pdfio.bytes_of_input i 0 i.Pdfio.in_channel_length in
  let obj, extras = fobj () data in
  iter (Pdf.addobj_given_num pdf) extras;
  let w = match Pdf.lookup_direct pdf "/Width" obj with Some x -> Pdf.getnum pdf x | _ -> assert false in
  let h = match Pdf.lookup_direct pdf "/Height" obj with Some x -> Pdf.getnum pdf x | _ -> assert false in
  let page =
    {Pdfpage.content =
      [Pdfops.stream_of_ops
      [Pdfops.Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (0., 0.);
                                                       Pdftransform.Scale ((0., 0.), w, h)]);
       Pdfops.Op_Do "/I0"]];
     Pdfpage.mediabox = Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real w; Pdf.Real h];
     Pdfpage.resources =
       Pdf.Dictionary
         ["/XObject", Pdf.Dictionary ["/I0", Pdf.Indirect (Pdf.addobj pdf obj)]];
     Pdfpage.rotate = Pdfpage.Rotate0;
     Pdfpage.rest = Pdf.Dictionary []}
  in
  let pdf, pageroot = Pdfpage.add_pagetree [page] pdf in
    Pdfpage.add_root pageroot [] pdf

let jpeg_to_jpeg pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~q ~path_to_convert s dict reference =
  let w = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer i) -> i | _ -> error "bad width" in
  let h = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer i) -> i | _ -> error "bad height" in
  if w * h < pixel_threshold then (if !debug_image_processing then Printf.printf "pixel threshold not met\n%!") else
  Pdf.getstream s;
  let size = match Pdf.lookup_direct pdf "/Length" dict with Some (Pdf.Integer i) -> i | _ -> 0 in
  if size < length_threshold then (if !debug_image_processing then Printf.printf "length threshold not met\n%!") else
  let out = Filename.temp_file "cpdf" "convertin" ^ ".jpg" in
  let out2 = Filename.temp_file "cpdf" "convertout" ^ ".jpg" in
  let fh = open_out_bin out in
    begin match s with Pdf.Stream {contents = _, Pdf.Got d} -> Pdfio.bytes_to_output_channel fh d | _ -> () end;
    close_out fh;
    let retcode =
      let command = 
        (Filename.quote_command path_to_convert [out; "-quality"; string_of_int q ^ "%"; out2])
      in
        (*0Printf.printf "%S\n" command;*) Sys.command command
    in
    if retcode = 0 then
      begin
        let result = open_in_bin out2 in
        let newsize = in_channel_length result in
        if newsize < size then
          begin
            if !debug_image_processing then Printf.printf "JPEG to JPEG %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
            reference := Pdf.add_dict_entry dict "/Length" (Pdf.Integer newsize), Pdf.Got (Pdfio.bytes_of_input_channel result)
          end
        else
         begin
           if !debug_image_processing then Printf.printf "no size reduction\n%!"
         end
      end
    else
      begin
        Printf.printf "external process failed\n%!"
      end;
    remove out;
    remove out2

let suitable_num pdf dict =
  match Pdf.lookup_direct pdf "/ColorSpace" dict with
  | Some (Pdf.Name "/DeviceRGB") -> 3
  | Some (Pdf.Name "/DeviceGray") -> 1
  | Some (Pdf.Name "/DeviceCMYK") -> 4
  | Some (Pdf.Array [Pdf.Name "/ICCBased"; stream]) ->
      begin match Pdf.lookup_direct pdf "/N" stream with
      | Some (Pdf.Integer 3) -> 3
      | Some (Pdf.Integer 1) -> 1
      | Some (Pdf.Integer 4) -> 4
      | _ -> 0
      end
  | Some (Pdf.Array (Pdf.Name "/Separation"::_)) -> ~-1
  | _ -> 0

let lossless_out pdf ~pixel_threshold ~length_threshold extension s dict reference =
  let bpc = Pdf.lookup_direct pdf "/BitsPerComponent" dict in
  let components = suitable_num pdf dict in
  match components, bpc with
  | (1 | 3 | 4 | -1), Some (Pdf.Integer 8) ->
      let w = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer i) -> i | _ -> error "bad width" in
      let h = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer i) -> i | _ -> error "bad height" in
      if w * h < pixel_threshold then (if !debug_image_processing then Printf.printf "pixel threshold not met\n%!"; None) else
      let size = match Pdf.lookup_direct pdf "/Length" dict with Some (Pdf.Integer i) -> i | _ -> 0 in
      if size < length_threshold then (if !debug_image_processing then Printf.printf "length threshold not met\n%!"; None) else
      begin
        Pdfcodec.decode_pdfstream_until_unknown pdf s;
        match Pdf.lookup_direct pdf "/Filter" (fst !reference) with Some _ -> None | None ->
        let out = Filename.temp_file "cpdf" "convertin" ^ (if suitable_num pdf dict < 4 then ".pnm" else ".cmyk") in
        let out2 = Filename.temp_file "cpdf" "convertout" ^ extension in
        let fh = open_out_bin out in
        let data = match s with Pdf.Stream {contents = _, Pdf.Got d} -> d | _ -> assert false in
        (if components = 3 then pnm_to_channel_24 else
         if components = 4 then cmyk_to_channel_32 else pnm_to_channel_8) fh w h data;
        close_out fh;
        Some (out, out2, size, components, w, h)
      end
  | colspace, bpc ->
    (*let colspace = Pdf.lookup_direct pdf "/ColorSpace" dict in
    let colspace, bpc, filter = 
      (match colspace with None -> "none" | Some x -> Pdfwrite.string_of_pdf x),
      (match bpc with None -> "none" | Some x -> Pdfwrite.string_of_pdf x),
      (match Pdf.lookup_direct pdf "/Filter" dict with None -> "none" | Some x -> Pdfwrite.string_of_pdf x)
    in
      print_string (Pdfwrite.string_of_pdf dict);
      print_string (Printf.sprintf "%s (%s) [%s]\n" colspace bpc filter);*)
      if !debug_image_processing then Printf.printf "colourspace not suitable\n%!";
      None (* an image we cannot or do not handle *)

let lossless_to_jpeg pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~qlossless ~path_to_convert s dict reference =
  match lossless_out pdf ~pixel_threshold ~length_threshold ".jpg" s dict reference with None -> () | Some (out, out2, size, components, w, h) ->
  let retcode =
    let command = 
      (Filename.quote_command path_to_convert
        ((if components = 4 then ["-depth"; "8"; "-size"; string_of_int w ^ "x" ^ string_of_int h] else []) @
        [out; "-quality"; string_of_int qlossless ^ "%"] @
        (if components = 1 then ["-colorspace"; "Gray"] else if components = 4 then ["-colorspace"; "CMYK"] else []) @
        [out2]))
    in
      (*Printf.printf "%S\n" command;*) Sys.command command
  in
  if retcode = 0 then
    begin
      let result = open_in_bin out2 in
      let newsize = in_channel_length result in
      if newsize < size then
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
    end;
  remove out;
  remove out2

let combine_dicts o n =
  let x = 
  fold_right (fun (k, v) d -> add k v d) o n
  in
    Printf.printf "%s\n" (Pdfwrite.string_of_pdf (Pdf.Dictionary x));
    x

(* FIXME Need to specify exactly where this works, how to process with convert for each etc. *)
let lossless_resample pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~factor ~interpolate ~path_to_convert s dict reference =
  match lossless_out pdf ~pixel_threshold ~length_threshold ".png" s dict reference with None -> () | Some (out, out2, size, components, w, h) ->
  let retcode =
    (* FIXME upscale required at all? *)
    let command = 
      (Filename.quote_command path_to_convert
        ((if components = 4 then ["-depth"; "8"; "-size"; string_of_int w ^ "x" ^ string_of_int h] else []) @
        (if components = 1 then ["-colorspace"; "Gray"] else if components = 3 then ["-colorspace"; "RGB"] else if components = 4 then ["-colorspace"; "CMYK"] else []) @
        [if interpolate then "-resize" else "-sample"; string_of_int factor ^ "%"] @
        [out] @
        ["PNG24:" ^ out2])) (*FIXME do we need this anymore? *)
    in
      (*Printf.printf "%S\n" command;*) Sys.command command
  in
  if retcode = 0 then
    begin
      let result = open_in_bin out2 in
      let newsize = in_channel_length result in
      if newsize < size then
        begin
          if !debug_image_processing then Printf.printf "lossless resample %i -> %i (%i%%)\n%!" size newsize (int_of_float (float newsize /. float size *. 100.));
          (* FIXME Check that we got back in what we expected? *)
          reference :=
            (match fst (obj_of_png_data (Pdfio.bytes_of_input_channel result)) with
            | Pdf.Stream {contents = Pdf.Dictionary d, data} ->
                let d' = fold_right (fun (k, v) d -> add k v d) d (match dict with Pdf.Dictionary x -> x | _ -> []) in
                  (Pdf.Dictionary d', data)
            | _ -> assert false)
        end
      else
        begin
          if !debug_image_processing then Printf.printf "no size reduction\n%!"
        end;
        close_in result
    end(*;
  remove out;
  remove out2*)

let recompress_1bpp_jbig2_lossless ~pixel_threshold ~length_threshold ~path_to_jbig2enc pdf s dict reference =
  let w = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer i) -> i | _ -> error "bad width" in
  let h = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer i) -> i | _ -> error "bad height" in
  if w * h < pixel_threshold then (if !debug_image_processing then Printf.printf "pixel threshold not met\n%!") else (* (but also, jbig2enc fails on tiny images) *)
  let size = match Pdf.lookup_direct pdf "/Length" dict with Some (Pdf.Integer i) -> i | _ -> 0 in
  if size < length_threshold then (if !debug_image_processing then Printf.printf "length threshold not met\n%!") else
  begin
    Pdfcodec.decode_pdfstream_until_unknown pdf s;
    match Pdf.lookup_direct pdf "/Filter" (fst !reference) with
    | Some x -> if !debug_image_processing then Printf.printf "could not decode - skipping %s length %i\n%!" (Pdfwrite.string_of_pdf x) size
    | None ->
      let out = Filename.temp_file "cpdf" "convertin" ^ ".pnm" in
      let out2 = Filename.temp_file "cpdf" "convertout" ^ ".jbig2" in
      let fh = open_out_bin out in
      let data = match s with Pdf.Stream {contents = _, Pdf.Got d} -> d | _ -> assert false in
        pnm_to_channel_1_inverted fh w h data;
        close_out fh;
        let retcode =
          let command = Filename.quote_command ~stdout:out2 path_to_jbig2enc ["-p"; out] in
            (*Printf.printf "%S\n" command;*) Sys.command command
        in
          if retcode = 0 then
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

(* JPEG to JPEG: RGB and CMYK JPEGS *)
(* Lossless to JPEG: 8bpp Grey, 8bpp RGB, 8bpp CMYK including separation and ICCBased colourspaces *)
(* 1 bit: anything to JBIG2 lossless (no globals) *)
let process
  ?q ?qlossless ?onebppmethod ~length_threshold ~percentage_threshold ~pixel_threshold ~factor ~interpolate
  ~path_to_jbig2enc ~path_to_convert pdf
=
  let nobjects = Pdf.objcard pdf in
  let ndone = ref 0 in
  let process_obj objnum s =
    match s with
    | Pdf.Stream ({contents = dict, _} as reference) ->
        ndone += 1;
        begin match
          Pdf.lookup_direct pdf "/Subtype" dict,
          Pdf.lookup_direct pdf "/Filter" dict,
          Pdf.lookup_direct pdf "/BitsPerComponent" dict,
          Pdf.lookup_direct pdf "/ImageMask" dict
        with
        | Some (Pdf.Name "/Image"), Some (Pdf.Name "/DCTDecode" | Pdf.Array [Pdf.Name "/DCTDecode"]), _, _ ->
            begin match q with
            | Some q ->
                if q < 100 then
                  begin
                    if !debug_image_processing then Printf.printf "(%i/%i) Object %i (JPEG)... %!" !ndone nobjects objnum;
                    jpeg_to_jpeg pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~q ~path_to_convert s dict reference
                  end
            | None -> ()
            end
        | Some (Pdf.Name "/Image"), _, Some (Pdf.Integer 1), _
        | Some (Pdf.Name "/Image"), _, _, Some (Pdf.Boolean true) ->
            begin match onebppmethod with
            | Some "JBIG2" ->
                begin
                  if !debug_image_processing then Printf.printf "(%i/%i) object %i (1bpp)... %!" !ndone nobjects objnum;
                  recompress_1bpp_jbig2_lossless ~pixel_threshold ~length_threshold ~path_to_jbig2enc pdf s dict reference
                end
            | _ -> ()
            end
        | Some (Pdf.Name "/Image"), _, _, _ ->
            begin match qlossless with
            | Some qlossless ->
                if qlossless < 101 then
                  begin
                    if !debug_image_processing then Printf.printf "(%i/%i) object %i (lossless)... %!" !ndone nobjects objnum;
                    lossless_to_jpeg pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~qlossless ~path_to_convert s dict reference
                  end
                else
                  begin
                    if factor < 100 then
                      begin
                        if !debug_image_processing then Printf.printf "(%i/%i) object %i (lossless)... %!" !ndone nobjects objnum;
                        lossless_resample pdf ~pixel_threshold ~length_threshold ~percentage_threshold ~factor ~interpolate ~path_to_convert s dict reference
                      end
                  end
            | None -> ()
            end
        | _ -> () (* not an image *)
        end
    | _ -> ndone += 1 (* not a stream *)
  in
    Pdf.objiter process_obj pdf
