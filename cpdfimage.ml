open Pdfutil
open Pdfio

(* Extract Images. *)
let pnm_to_channel_24 channel w h s =
  let white () = output_char channel ' ' 
  and newline () = output_char channel '\n'
  and output_string = Pervasives.output_string channel in
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

let write_stream name stream =
  let fh = open_out_bin name in
    for x = 0 to bytes_size stream - 1 do
      output_byte fh (bget stream x)
    done;
    close_out fh

let write_image path_to_p2p path_to_im pdf resources name image =
  match Pdfimage.get_image_24bpp pdf resources image with
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
            "" -> Printf.eprintf "Neither pnm2png nor imagemagick found. Specify with -p2p or -im\n%!"
          | _ ->
            begin match
              Sys.command (Filename.quote_command path_to_im [pnm; png])
            with
              0 -> Sys.remove pnm
            | _ -> 
              Printf.eprintf "Call to imagemagick failed: did you specify -p2p correctly?\n%!";
              Sys.remove pnm
            end
          end
        | _ ->
          begin match
            Sys.command (Filename.quote_command path_to_p2p ~stdout:png ["-gamma"; "0.45"; "-quiet"; pnm])
          with
          | 0 -> Sys.remove pnm
          | _ ->
              Printf.eprintf "Call to pnmtopng failed: did you specify -p2p correctly?\n%!";
              Sys.remove pnm
          end
        end
  | _ ->
      Printf.eprintf "Unsupported image type when extracting image %s %!" name

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
