open Pdfutil

(* See if the given box is to be treated. For now:

  a) Glyphs - any intersection
  b) Image - any intersection
  c) InlineImage - any intersection
  d) Path - path must be wholly contained in box
  e) Shading - must be wholly contained in box *)
let box_matches (minx, miny, maxx, maxy) {Cpdfcontent.content; bounding_box = Quad (x0, y0, x1, y1, x2, y2, x3, y3)} =
  let bminx, bmaxx, bminy, bmaxy =
    fmin (fmin x0 x1) (fmin x2 x3), fmax (fmax x0 x1) (fmax x2 x3),
    fmin (fmin y0 y1) (fmin y2 y3), fmax (fmax y0 y1) (fmax y2 y3)
  in
  let any_intersection (minx, miny, maxx, maxy) (bminx, bminy, bmaxx, bmaxy) =
    box_overlap_float minx miny maxx maxy bminx bminy bmaxx bmaxy
  in
  let wholly_contained (minx, miny, maxx, maxy) (bminx, bminy, bmaxx, bmaxy) =
    bminx > minx && bmaxx < maxx && bminy > miny && bmaxy < maxy
  in
    if wholly_contained (minx, miny, maxx, maxy) (bminx, bminy, bmaxx, bmaxy) then Cpdfcontent.Encloses else
     match any_intersection (minx, miny, maxx, maxy) (bminx, bminy, bmaxx, bmaxy) with
     | Some (minx, miny, maxx, maxy) -> Intersects (Cpdfcontent.Quad (minx, miny, minx, maxy, maxx, maxy, maxx, miny))
     | None -> Nonintersecting

let select_boxes shape boxes =
  match shape with 
  | None -> boxes
  | Some (minx, miny, maxx, maxy) ->
      keep (fun box -> box_matches (minx, miny, maxx, maxy) box <> Cpdfcontent.Nonintersecting) boxes

let invert_overlap = function
  | Cpdfcontent.Nonintersecting -> Cpdfcontent.Encloses
  | _ -> Nonintersecting

(* Redact a path on a page *)
let redact_page pdf ~text_spec ~image_spec ~inline_image_spec ~vector_spec ~annotation_spec ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~color ~path ~invert page =
  let to_remove = ref [] in
  let ops =
    Cpdfcontent.filter
      ~pdf
      ~helpers:
        {path_to_jbig2dec;
         path_to_convert;
         path_to_jbig2enc;
         color;
         remove = (fun s -> to_remove := s::!to_remove);
         text_spec;
         image_spec;
         inline_image_spec;
         vector_spec;
         annotation_spec}
      ~f:(function content -> if invert then invert_overlap (box_matches path content) else box_matches path content)
      ~mediabox:(Pdf.parse_rectangle pdf page.Pdfpage.mediabox)
      ~resources:page.Pdfpage.resources
      ~ops:(Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content)
  in
    let ops = lose (function Pdfops.Op_Do n when mem n !to_remove -> true | _ -> false) ops in
    let ops = Cpdfcontent.postprocess_remove_empty_path_ops ops in
    let resources' =
      let xobjects =
        match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
        | Some (Pdf.Dictionary d) -> d
        | _ -> []
      in
        Pdf.add_dict_entry page.Pdfpage.resources "/XObject" (Pdf.Dictionary (lose (fun (k, _) -> mem k !to_remove) xobjects))
    in
      {page with
         Pdfpage.content = [Pdfops.stream_of_ops ops];
         Pdfpage.resources = resources'}

(* Redaction cannot copy with lossy JBIG2, because the round-tripping would
   could introduce new losses, and we don't know the settings that were used
   upon the first compression.  So it's too dangerous. Therefore, we convert
   all JBIG2Lossy to lossless JBIG2 (if possible) or CCITTG4 (if not) before
   embarking upon redaction. *)
let preprocess_jbig2lossy_to_jbig2lossless ?jbig2dec ~path_to_jbig2enc pdf =
  Pdf.objiter
    (fun objnum s ->
       match s with
       | Pdf.Stream ({contents = dict, _} as reference) ->
         begin match Pdf.lookup_chain pdf dict ["/DecodeParms"; "/JBIG2Globals"] with
         | Some _ ->
             Cpdfimage.recompress_1bpp_jbig2_lossless ?jbig2dec ~force:true ~pixel_threshold:0 ~length_threshold:0 ~path_to_jbig2enc pdf s dict reference
         | _ -> ()
         end
       | _ -> ())
     pdf

(* Detect if something intersects, overlaps etc. *)
let detected ~detection ~invert path_minx path_miny path_maxx path_maxy test_minx test_miny test_maxx test_maxy =
  let fi x = if invert then not x else x in
    match detection with
    | Cpdfcontent.Touching ->
        begin match box_overlap_float test_minx test_miny test_maxx test_maxy path_minx path_miny path_maxx path_maxy with
        | Some _ -> fi true
        | None -> fi false
        end
    | Cpdfcontent.Enclosing ->
        fi (box_union_float (path_minx, path_miny, path_maxx, path_maxy) (test_minx, test_miny, test_maxx, test_maxy) = (path_minx, path_miny, path_maxx, path_maxy))

(* Remove annotations as specified *)
let redact_annotations pdf range ~detection ~invert ~paths =
  Cpdfutil.progress_line "Redacting annotations...";
  let to_delete = ref [] in
  let redact_annotations_page pnum page =
    match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
    | Some (Pdf.Array annots) ->
        let annotation_objnums = option_map (function Pdf.Indirect i -> Some i | _ -> None) annots in
        let page =
          fold_left
            (fun page i ->
              match Pdf.lookup_direct pdf "/Rect" (Pdf.Indirect i) with
              | Some rect ->
                  let minx, miny, maxx, maxy = List.nth paths (pnum - 1) in
                  let aminx, aminy, amaxx, amaxy = Pdf.parse_rectangle pdf rect in
                    if detected ~detection ~invert minx miny maxx maxy aminx aminy amaxx amaxy then to_delete =| i;
                      page
              | None ->
                  page)
            page
            annotation_objnums
        in
        let popups_to_delete = ref [] in
        iter
          (fun i ->
            match Pdf.indirect_number pdf "/Parent" (Pdf.Indirect i) with
            | Some i' when mem i' !to_delete -> popups_to_delete =| i
            | _ -> ())
          annotation_objnums;
        {page with
          Pdfpage.rest =
            Pdf.add_dict_entry
              page.Pdfpage.rest
              "/Annots"
              (Pdf.Array (lose (function Pdf.Indirect i -> mem i !to_delete || mem i !popups_to_delete | _ -> false) annots))}
    | _ -> page
  in
    Cpdfpage.process_pages
      (Pdfpage.ppstub
        (fun pnum page -> if mem pnum range then redact_annotations_page pnum page else page))
      pdf
      range

let redact
  pdf ~text_spec ~image_spec ~inline_image_spec ~vector_spec ~annotation_spec ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc
  ~paths ~invert ~show ~color ~outline ~opacity ~linewidth ~underneath range
=
  preprocess_jbig2lossy_to_jbig2lossless ~jbig2dec:path_to_jbig2dec ~path_to_jbig2enc pdf;
  Cpdfutil.progress_line "Redacting content...";
  let pdf =
    Cpdfpage.process_pages
      (Pdfpage.ppstub
        (fun pnum page ->
           if mem pnum range then
             let path = List.nth paths (pnum - 1) in
               redact_page
                 pdf ~text_spec ~image_spec ~inline_image_spec ~vector_spec ~annotation_spec ~path_to_jbig2dec
                 ~path_to_convert ~path_to_jbig2enc ~color ~path ~invert page else page))
      pdf
      range
  in
    let pdf =
      match annotation_spec with
      | Remove, Some detection -> redact_annotations pdf range ~detection ~invert ~paths
      | _ -> pdf
    in
    if show then
      begin
        Cpdfutil.progress_line "Adding redaction appearance...";
        if length (collate compare (sort compare paths)) = 1 then
          let minx, miny, maxx, maxy = hd paths in
            Cpdfaddtext.addrectangle
              false (Printf.sprintf "%s %s" (string_of_float (maxx -. minx)) (string_of_float (maxy -. miny)))
              color outline linewidth opacity (Cpdfposition.PosLeft(minx, miny)) "/Absolute" underneath range pdf
        else
          let pdf = ref pdf in
            iter
              (fun pnum ->
                let minx, miny, maxx, maxy = List.nth paths (pnum - 1) in
                  pdf :=
                    Cpdfaddtext.addrectangle
                      false (Printf.sprintf "%s %s" (string_of_float (maxx -. minx)) (string_of_float (maxy -. miny)))
                      color outline linewidth opacity (Cpdfposition.PosLeft(minx, miny)) "/Absolute" underneath [pnum] !pdf)
              range;
            !pdf
      end
    else
      pdf

let redact_add_rectangle_pnum pdf ~path:(minx, miny, maxx, maxy) ~color ~outline ~opacity ~linewidth ~underneath pnum =
  Cpdfaddtext.addrectangle
    false (Printf.sprintf "%s %s" (string_of_float (maxx -. minx)) (string_of_float (maxy -. miny)))
    color outline linewidth opacity (Cpdfposition.PosLeft(minx, miny)) "/Absolute" underneath [pnum] pdf

(* Apply redaction annotations. *)
let apply
  pdf ~text_spec ~image_spec ~inline_image_spec ~vector_spec ~annotation_spec ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc
  ?(typ="/Redact") ~invert ~show ~color ~outline ~opacity ~linewidth ~underneath range
=
  preprocess_jbig2lossy_to_jbig2lossless ~jbig2dec:path_to_jbig2dec ~path_to_jbig2enc pdf;
  let rectangles = ref [] in
  let apply_page pnum page =
    match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
    | Some (Pdf.Array annots) ->
        let redact_annotation_objnums =
          option_map
            (function Pdf.Indirect i when Pdf.lookup_direct pdf "/Subtype" (Pdf.Indirect i) = Some (Pdf.Name typ) -> Some i | _ -> None)
            annots
        in
        let popups_of_redactions =
          option_map
            (function Pdf.Indirect i when Pdf.lookup_direct pdf "/Subtype" (Pdf.Indirect i) = Some (Pdf.Name "/Popup") ->
               let parent =
                 match Pdf.indirect_number pdf "/Parent" (Pdf.Indirect i) with
                 | Some i -> i
                 | _ -> 0
               in
                 if mem parent redact_annotation_objnums then Some i else None
             | _ -> None)
            annots
        in
        let page =
          fold_left
            (fun page i ->
              match Pdf.lookup_direct pdf "/Rect" (Pdf.Indirect i) with
              | Some rect ->
                  let path = Pdf.parse_rectangle pdf rect in
                    rectangles =| (pnum, path);
                    redact_page
                      pdf ~text_spec ~image_spec ~inline_image_spec ~vector_spec ~annotation_spec
                      ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~color ~path ~invert page
              | None ->
                  page)
            page
            redact_annotation_objnums
        in
        {page with
          Pdfpage.rest =
            Pdf.add_dict_entry
              page.Pdfpage.rest
              "/Annots"
              (Pdf.Array (lose (function Pdf.Indirect i -> mem i redact_annotation_objnums || mem i popups_of_redactions | _ -> false) annots))}
    | _ -> page
  in
    let pdf =
      Cpdfpage.process_pages
        (Pdfpage.ppstub
          (fun pnum page -> if mem pnum range then apply_page pnum page else page))
        pdf
        range
    in
      fold_left
        (fun pdf (pnum, path) ->
           let pdf = 
             match annotation_spec with
             | (Remove, Some detection) -> redact_annotations pdf [pnum] ~detection ~invert ~paths:(many path (Pdfpage.endpage pdf))
             | _ -> pdf
           in
             if show then redact_add_rectangle_pnum pdf ~path ~color ~outline ~opacity ~linewidth ~underneath pnum else pdf)
        pdf
        !rectangles

let colour ~light = function
  | Cpdfcontent.Glyph _ -> [Pdfops.Op_G (if light then 1. else 0.)] (* Black *)
  | Cpdfcontent.InlineImage _ -> [if light then Pdfops.Op_RG (1., 0.5, 0.5) else Pdfops.Op_RG (1., 0., 0.)] (* Red *)
  | Cpdfcontent.Image _ -> [if light then Pdfops.Op_RG (0.5, 1., 0.5) else Pdfops.Op_RG (0., 1., 0.)] (* Green *)
  | Cpdfcontent.Path _ -> [if light then Pdfops.Op_RG (0.5, 0.5, 1.) else Pdfops.Op_RG (0., 0., 1.)] (* Blue *)
  | Cpdfcontent.Shading _ -> [if light then Pdfops.Op_RG (0.5, 1., 1.) else Pdfops.Op_RG (0., 1., 1.)] (* Cyan *)
  | Cpdfcontent.Clip -> [Pdfops.Op_d ([4.; 4.], 0.); Pdfops.Op_G (if light then 1. else 0.)] (* Black, dashed. *)

let mkbox ~light {Cpdfcontent.content; bounding_box = Quad (x0, y0, x1, y1, x2, y2, x3, y3)} =
  [Pdfops.Op_q] @
  colour ~light content @ [Pdfops.Op_w 0.5] @
  [Pdfops.Op_m (x0, y0); Op_l (x1, y1); Op_l (x2, y2); Op_l (x3, y3); Op_h; Op_S] @
  [Pdfops.Op_Q]

let mkannotbox ~light (minx, miny, maxx, maxy) =
  [if light then Pdfops.Op_RG (1.0, 1.0, 0.5) else Pdfops.Op_RG (1.0, 1.0, 0.)] @ [Pdfops.Op_w 1.0] @
  [Pdfops.Op_m (minx, miny); Op_l (minx, maxy); Op_l (maxx, maxy); Op_l (maxx, miny); Op_l (minx, miny); Op_h; Op_S]

let show_annotation_bounding_boxes ~fast ~light pdf range =
  Cpdfutil.progress_line "Finding annotations...";
  let show_annotation_bounding_boxes_page pdf page =
    map
      (fun annot -> annot.Pdfannot.rectangle)
      (Pdfannot.annotations_of_page pdf page)
  in
  let bboxes = ref [] in
  let pdf =
    Cpdfpage.process_pages
      (Pdfpage.ppstub
        (fun pnum page -> if mem pnum range then (bboxes =| (pnum, show_annotation_bounding_boxes_page pdf page); page) else page))
           pdf
           range
  in
    let pdf = ref pdf in
    Cpdfutil.progress_line "Showing annotations...";
    let content_of_boxes boxes = flatten (map (mkannotbox ~light) boxes) in
    let opss =
      map
        (fun n -> match lookup n !bboxes with | Some boxes -> content_of_boxes boxes | None -> [])
        (ilist 1 (Pdfpage.endpage !pdf))
    in
      Cpdftweak.append_page_content_multiple_ops opss false fast !pdf

let show_bounding_boxes ~fast ~paths ~light pdf range =
  let pdf = show_annotation_bounding_boxes ~fast ~light pdf range in
  Cpdfutil.progress_line "Finding page content bounding boxes...";
  let show_bounding_boxes_page page =
    let ops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content in
    let page_boxes = ref [] in
      ignore
        (Cpdfcontent.filter
           ~pdf
           ~helpers:Cpdfcontent.empty_helpers
           ~f:(fun page_obj -> page_boxes =| page_obj; Nonintersecting)
           ~mediabox:(Pdf.parse_rectangle pdf page.Pdfpage.mediabox)
           ~resources:page.Pdfpage.resources
           ~ops);
      !page_boxes
  in
  let bboxes = ref [] in
  let pdf =
    Cpdfpage.process_pages
      (Pdfpage.ppstub
        (fun pnum page -> if mem pnum range then (bboxes =| (pnum, show_bounding_boxes_page page); page) else page))
           pdf
           range
  in
    let pdf = ref pdf in
    Cpdfutil.progress_line "Showing page content bounding boxes...";
    let content_of_boxes pnum boxes =
      let path = List.nth paths (pnum - 1) in
        flatten (map (mkbox ~light) (select_boxes path boxes))
    in
    let opss =
      map
        (fun pnum -> match lookup pnum !bboxes with Some boxes -> content_of_boxes pnum boxes | None -> [])
        (ilist 1 (Pdfpage.endpage !pdf))
    in
      Cpdftweak.append_page_content_multiple_ops opss false fast !pdf
