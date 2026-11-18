open Pdfutil

(* Call out to GhostScript to rasterize. Read back in and replace the page contents with the resultant PNG. *)
let rasterize tempfiles path_to_ghostscript gs_quiet path_to_im remake ?subformat ?title antialias downsample device res annots quality pdf range =
  if path_to_ghostscript = "" then begin
    Pdfe.log "Please supply path to gs with -gs\n";
    exit 2
  end;
  let tmppdf = Filename.temp_file "cpdf" ".pdf" in
  tempfiles := tmppdf::!tempfiles;
  Pdfwrite.pdf_to_file (Pdf.deep_copy pdf) tmppdf;
  let pdf = Pdfpage.change_pages true pdf
    (map2
      (fun page pnum ->
        if not (mem pnum range) then page else
        let tmpout = Filename.temp_file "cpdf" ".png" in
        tempfiles := tmpout::!tempfiles;
        let antialias, res =
          if downsample then ["-dDownScaleFactor=4"], res *. 4.  else
            let bits = if antialias then "4" else "1" in
              ["-dTextAlphaBits=" ^ bits; "-dGraphicsAlphaBits=" ^ bits], res
        in
        let gscall =
          Filename.quote_command path_to_ghostscript
            ((if gs_quiet then ["-dQUIET"] else []) @
            antialias @
            ["-dBATCH"; "-dNOPAUSE"; "-sDEVICE=" ^ device; "-dUseCropBox"; "-dShowAnnots=" ^ string_of_bool annots;
             "-dJPEGQ=" ^ string_of_int quality; "-sOutputFile=" ^ tmpout; "-sPageList=" ^ string_of_int pnum;
             "-r" ^ string_of_float res; tmppdf])
        in
          (*Printf.printf "CALL: %S\n" gscall;*)
          begin match Sys.command gscall with
          | 0 -> ()
          | _ -> Pdfe.log "Rasterization failed\n"; exit 2
          end;
        let data = Pdfio.bytes_of_string (Pdfutil.contents_of_file tmpout) in
        Sys.remove tmpout;
        let image, _ = (if device = "jpeg" || device = "jpeggray" then Cpdfimage.obj_of_jpeg_data ~path_to_im else Cpdfimage.obj_of_png_data pdf) data in
        let imageobj = Pdf.addobj pdf image in
        let w, h =
          match device with
          | "jpeg" | "jpeggray" -> Cpdfjpeg.jpeg_dimensions data
          | _ -> let png = Cpdfpng.read_png (Pdfio.input_of_bytes data) in (png.Cpdfpng.width, png.Cpdfpng.height)
        in
        let w, h =
          match page.Pdfpage.rotate with
          | Pdfpage.Rotate90 | Pdfpage.Rotate270 -> h, w
          | _ -> w, h
        in
        let w, h = if downsample then w * 4, h * 4 else w, h in
        let (minx, miny, maxx, maxy) =
          Pdf.parse_rectangle
            pdf
            (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
             | Some r -> r
             | None -> page.Pdfpage.mediabox)
        in
        let rotation = rad_of_deg (float_of_int (Pdfpage.int_of_rotation page.Pdfpage.rotate)) in
        let tx, ty =
          match page.Pdfpage.rotate with
          | Pdfpage.Rotate0 -> (minx, miny)
          | Pdfpage.Rotate270 -> (minx, miny +. (maxy -. miny))
          | Pdfpage.Rotate90 -> (minx +. (maxx -. minx), miny)
          | Pdfpage.Rotate180 -> (minx +. (maxx -. minx), miny +. (maxy -. miny))
        in
        let ops =
          [Pdfops.Op_cm
             (Pdftransform.matrix_of_transform
               [Pdftransform.Translate (tx, ty);
                Pdftransform.Scale ((0., 0.), float_of_int w *. 72. /. res, float_of_int h *. 72. /. res);
                Pdftransform.Rotate ((0., 0.), rotation)]);
           Pdfops.Op_BMC "/Artifact";
           Pdfops.Op_Do "/I0";
           Pdfops.Op_EMC]
        in
        {page with Pdfpage.content = [Pdfops.stream_of_ops ops];
                   Pdfpage.resources = Pdf.Dictionary [("/XObject", Pdf.Dictionary [("/I0", Pdf.Indirect imageobj)])]})
      (Pdfpage.pages_of_pagetree pdf)
      (ilist 1 (Pdfpage.endpage pdf)))
  in
    let pdf = if annots then Cpdfannot.remove_annotations range pdf else pdf in
    Sys.remove tmppdf;
    pdf

let write_images tempfiles path_to_ghostscript gs_quiet device res quality boxname annots antialias downsample spec pdf range =
  if path_to_ghostscript = "" then begin
    Pdfe.log "Please supply path to gs with -gs\n";
    exit 2
  end;
  let tmppdf = Filename.temp_file "cpdf" ".pdf" in
  tempfiles := tmppdf::!tempfiles;
  Pdfwrite.pdf_to_file (Pdf.deep_copy pdf) tmppdf;
  let endpage = Pdfpage.endpage pdf in
  iter2
    (fun page pnum ->
       if not (mem pnum range) then () else
       let out = Cpdfbookmarks.name_of_spec Cpdfmetadata.UTF8 [] pdf 0 spec pnum "" 0 0 in
       let antialias, res =
         if downsample then ["-dDownScaleFactor=4"], res *. 4.  else
           let bits = if antialias then "4" else "1" in
             ["-dTextAlphaBits=" ^ bits; "-dGraphicsAlphaBits=" ^ bits], res
       in
       let gscall =
         Filename.quote_command path_to_ghostscript
           ((if gs_quiet then ["-dQUIET"] else []) @
            (if boxname = None then [] else (Cpdfutil.check_injectible (unopt boxname); ["-dUse" ^ (implode (tl (explode (unopt boxname))))])) @ 
           antialias @
           ["-dBATCH"; "-dNOPAUSE"; "-sDEVICE=" ^ device; "-dShowAnnots=" ^ string_of_bool annots;
            "-dJPEGQ=" ^ string_of_int quality; "-sOutputFile=" ^ out; "-sPageList=" ^ string_of_int pnum;
            "-r" ^ string_of_float res; tmppdf])
       in
         begin match Sys.getenv_opt "CPDF_SHOW_EXT" with
         | Some "true" -> flprint (gscall ^ "\n")
         | _ -> ()
         end;
         begin match Sys.command gscall with
         | 0 -> ()
         | _ -> Pdfe.log "Rasterization failed\n"; exit 2
         end)
    (Pdfpage.pages_of_pagetree pdf)
    (ilist 1 endpage);
  Sys.remove tmppdf
