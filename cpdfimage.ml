open Pdfutil
open Pdfio

(* Extract Images. *)
let pnm_to_channel_24 channel w h s =
  let white () = output_char channel ' ' 
  and newline () = output_char channel '\n'
  and output_string = Stdlib.output_string channel in
    output_string "P6";
    white ();
    output_string (string_of_int w);
    white ();
    output_string (string_of_int h);
    white ();
    output_string "255";
    newline ();
    let pos = ref 0 in
      for y = 1 to h do
        for x = 1 to w * 3 do
          output_byte channel (bget s !pos);
          incr pos
        done
      done


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
          Printf.printf "JBIG2: No global, writing plain\n";
          write_stream (name ^ ".jbig2") stream
      | Some g ->
          Printf.printf "JBIG2: there is a global\n";
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
              0 -> Sys.remove pnm
            | _ -> 
              Pdfe.log "Call to imagemagick failed: did you specify -p2p or -im correctly?\n";
              Sys.remove pnm
            end
          end
        | Some path_to_p2p ->
          begin match
            Sys.command (Filename.quote_command path_to_p2p ~stdout:png ["-gamma"; "0.45"; "-quiet"; pnm])
          with
          | 0 -> Sys.remove pnm
          | _ ->
              Pdfe.log "Call to pnmtopng failed: did you specify -p2p correctly?\n";
              Sys.remove pnm
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
            | (pagenums, n, w, h, cs) ->
                Hashtbl.replace images i (pagenum::pagenums, n, w, h, cs)
            | exception Not_found ->
                let width =
                  match Pdf.lookup_direct pdf "/Width" xobject with
                  | Some x -> Pdf.getnum pdf x
                  | None -> 1.
                and height =
                  match Pdf.lookup_direct pdf "/Height" xobject with
                  | Some x -> Pdf.getnum pdf x
                  | None -> 1.
                and colourspace =
                  match Pdf.lookup_direct pdf "/ColorSpace" xobject with
                  | Some x -> Some (Pdfspace.string_of_colourspace (Pdfspace.read_colourspace pdf resources x))
                  | None -> None
                in
                  Hashtbl.replace images i ([pagenum], name, int_of_float width, int_of_float height, colourspace)
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
        let images = map (fun (i, (pnums, n, w, h, c)) -> (i, (setify (sort compare pnums), n, w, h, c))) images in
        let images = sort (fun (_, (pnums, _, _, _, _)) (_, (pnums', _, _, _, _)) -> compare (hd pnums) (hd pnums')) images in
         `List
           (map
             (fun (i, (pnums, n, w, h, cs)) ->
               `Assoc [("Object", `Int i);
                       ("Pages", `List (map (fun x -> `Int x) pnums));
                       ("Name", `String n);
                       ("Width", `Int w);
                       ("Height", `Int h);
                       ("Colourspace", match cs with None -> `Null | Some s -> `String s)])
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
     "/BitsPerComponent", Pdf.Integer 8;
     "/ColorSpace", Pdf.Name "/DeviceRGB";
     "/DecodeParms", Pdf.Dictionary
                      ["/BitsPerComponent", Pdf.Integer 8;
                       "/Colors", Pdf.Integer 3;
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

(* For each image xobject, process it through convert to reduce JPEG quality if we can. *)
let process pdf ~q ~qlossless ~path_to_convert =
  let process_obj _ s =
  match s with
  | Pdf.Stream ({contents = dict, _} as reference) ->
      begin match Pdf.lookup_direct pdf "/Subtype" dict, Pdf.lookup_direct pdf "/Filter" dict with
      | Some (Pdf.Name "/Image"), Some (Pdf.Name "/DCTDecode" | Pdf.Array [Pdf.Name "/DCTDecode"]) ->
          Pdf.getstream s;
          let out = Filename.temp_file "cpdf" "convertin" ^ ".jpg" in
          let out2 = Filename.temp_file "cpdf" "convertout" ^ ".jpg" in
          let fh = open_out_bin out in
            let size =
              begin match s with Pdf.Stream {contents = _, Pdf.Got d} -> Pdfio.bytes_to_output_channel fh d; bytes_size d | _ -> 0 end
            in
            close_out fh;
            let retcode =
              let command = 
                (Filename.quote_command path_to_convert
                  [out; "-quality"; string_of_int q ^ "%"; out2])
              in
                (*Printf.printf "%S\n" command;*)
                Sys.command command
            in
            if retcode = 0 then
              begin
                let result = open_in_bin out2 in
                let newsize = in_channel_length result in
                if newsize < size then
                  (*Printf.printf "%i -> %i\n" size newsize;*)
                  reference := Pdf.add_dict_entry dict "/Length" (Pdf.Integer newsize), Pdf.Got (Pdfio.bytes_of_input_channel result)
              end;
            Sys.remove out;
            Sys.remove out2
      | _ -> ()
      end
  | _ -> ()
  in
    Pdf.objiter process_obj pdf
