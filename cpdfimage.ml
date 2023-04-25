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

(* FIXME do this all at once *)
let write_stream name stream =
  let fh = open_out_bin name in
    for x = 0 to bytes_size stream - 1 do
      output_byte fh (bget stream x)
    done;
    close_out fh

(* Detect images we can write directly as PNGs, to avoid going through pnm2png or imagemagick.
This is when BPC = 8, colourspace = DeviceRGB or CalRGB, compression is /FlateDecode. *)
let write_image_png pdf resources name dict =
  (*Printf.printf "%s\n" (Pdfwrite.string_of_pdf (Pdf.direct pdf dict));*)
  match
    Pdfimage.colspace pdf dict resources,
    Pdfimage.bpc pdf dict,
    Pdf.lookup_direct pdf "/Filter" dict
  with
  | (Pdfspace.DeviceRGB | Pdfspace.CalRGB _),
    Some (Pdf.Integer 8),
    Some (Pdf.Name "/FlateDecode" | Pdf.Array [Pdf.Name "/FlateDecode"]) ->
      (*Printf.printf "Direct to png...\n";*)
      Pdf.getstream (Pdf.direct pdf dict);
      let ch = open_out_bin (name ^ ".png") in
      let o = Pdfio.output_of_channel ch in
      let width = match Pdf.lookup_direct pdf "/Width" dict with Some (Pdf.Integer x) -> x | _ -> raise Exit in
      let height = match Pdf.lookup_direct pdf "/Height" dict with Some (Pdf.Integer x) -> x | _ -> raise Exit in
      let idat = match Pdf.direct pdf dict with Pdf.Stream {contents = (_, Got bytes)} -> bytes | _ -> raise Exit in
        (*Printf.printf "all ok...\n";*)
        Cpdfpng.write_png {width; height; idat} o;
        close_out ch;
        true
  | _ -> false

let write_image path_to_p2p path_to_im pdf resources name image =
  let main () =
    begin match Pdfimage.get_image_24bpp pdf resources image with
    | Pdfimage.JPEG (stream, _) -> write_stream (name ^ ".jpg") stream
    | Pdfimage.JPEG2000 (stream, _) -> write_stream (name ^ ".jpx") stream
    | Pdfimage.JBIG2 (stream, _) -> write_stream (name ^ ".jbig2") stream
    | Pdfimage.Raw (w, h, Pdfimage.BPP24, stream) ->
        let pnm = name ^ ".pnm" in
        let png = name ^ ".png" in
        let fh = open_out_bin pnm in
          pnm_to_channel_24 fh w h stream;
          close_out fh;
          begin match path_to_p2p with
          | "" ->
            begin match path_to_im with
              "" -> Pdfe.log "Neither pnm2png nor imagemagick found. Specify with -p2p or -im\n"
            | _ ->
              begin match
                Sys.command (Filename.quote_command path_to_im [pnm; png])
              with
                0 -> Sys.remove pnm
              | _ -> 
                Pdfe.log "Call to imagemagick failed: did you specify -p2p or -im correctly?\n";
                Sys.remove pnm
              end
            end
          | _ ->
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
    end
  in
    match write_image_png pdf resources name image with
    | true -> ()
    | exception x -> Pdfe.log (Printf.sprintf "Failed to write PNG directly (%s)\n" (Printexc.to_string x)); main ()
    | _ -> main ()

let written = ref []

let extract_images_inner path_to_p2p path_to_im encoding serial pdf resources stem pnum images =
  let names = map
    (fun _ ->
       Cpdfbookmarks.name_of_spec
         encoding [] pdf 0 (stem ^ "-p" ^ string_of_int pnum)
         (let r = !serial in serial := !serial + 1; r) "" 0 0) (indx images)
  in
    iter2 (write_image path_to_p2p path_to_im pdf resources) names images

let rec extract_images_form_xobject path_to_p2p path_to_im encoding dedup dedup_per_page pdf serial stem pnum form =
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
      extract_images_inner path_to_p2p path_to_im encoding serial pdf resources stem pnum images

let extract_images path_to_p2p path_to_im encoding dedup dedup_per_page pdf range stem =
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
                 extract_images_inner path_to_p2p path_to_im encoding serial pdf page.Pdfpage.resources stem pnum images;
                 iter (extract_images_form_xobject path_to_p2p path_to_im encoding dedup dedup_per_page pdf serial stem pnum) forms)
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

