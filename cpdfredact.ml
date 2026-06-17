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

(* Redact a path on a page *)
let redact_page pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~path page =
  let to_remove = ref [] in
  let ops =
    Cpdfcontent.filter
      ~pdf
      ~path_to_jbig2dec
      ~path_to_convert
      ~path_to_jbig2enc
      ~f:(box_matches path)
      ~remove:(fun s -> to_remove := s::!to_remove)
      ~mediabox:(Pdf.parse_rectangle pdf page.Pdfpage.mediabox)
      ~resources:page.Pdfpage.resources
      ~ops:(Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content)
  in
    (*if !to_remove <> [] then
      begin
        Printf.printf "To remove at page level... ";
        iter (Printf.printf "%s ") !to_remove;
        flprint "\n";
      end;*)
    let ops =
      lose (function Pdfops.Op_Do n when mem n !to_remove -> true | _ -> false) ops
    in
    let ops =
      Cpdfcontent.postprocess_remove_empty_path_ops ops
    in
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

let redact pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~path:((minx, miny, maxx, maxy) as path) ~color ~outline ~opacity ~linewidth ~underneath range =
  let pdf =
    Cpdfpage.process_pages
      (Pdfpage.ppstub
        (fun pnum page -> if mem pnum range then redact_page pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~path page else page))
      pdf
      range
  in
    let pdf =
      Cpdfaddtext.addrectangle
        false (Printf.sprintf "%s %s" (string_of_float (maxx -. minx)) (string_of_float (maxy -. miny)))
        color outline linewidth opacity (Cpdfposition.PosLeft(minx, miny)) "/Absolute" underneath range pdf
    in
      pdf

let redact_add_rectangle_pnum pdf ~path:(minx, miny, maxx, maxy) ~color ~outline ~opacity ~linewidth ~underneath pnum =
  Cpdfaddtext.addrectangle
    false (Printf.sprintf "%s %s" (string_of_float (maxx -. minx)) (string_of_float (maxy -. miny)))
    color outline linewidth opacity (Cpdfposition.PosLeft(minx, miny)) "/Absolute" underneath [pnum] pdf

(* Apply redaction annotations. *)
let apply pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ?(typ="/Redact") ~color ~outline ~opacity ~linewidth ~underneath range =
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
                    redact_page pdf ~path_to_jbig2dec ~path_to_convert ~path_to_jbig2enc ~path page
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
           redact_add_rectangle_pnum pdf ~path ~color ~outline ~opacity ~linewidth ~underneath pnum)
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

let show_bounding_boxes ~fast ~shape ~light pdf range =
  let pdf = show_annotation_bounding_boxes ~fast ~light pdf range in
  Cpdfutil.progress_line "Finding page content bounding boxes...";
  let show_bounding_boxes_page page =
    let ops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content in
    let page_boxes = ref [] in
      ignore (Cpdfcontent.filter ~pdf ~path_to_jbig2dec:"" ~path_to_convert:"" ~path_to_jbig2enc:"" ~f:(fun page_obj -> page_boxes =| page_obj; Nonintersecting) ~remove:(fun _ -> ()) ~mediabox:(Pdf.parse_rectangle pdf page.Pdfpage.mediabox) ~resources:page.Pdfpage.resources ~ops);
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
    let content_of_boxes boxes = flatten (map (mkbox ~light) (select_boxes shape boxes)) in
    let opss =
      map
        (fun n -> match lookup n !bboxes with | Some boxes -> content_of_boxes boxes | None -> [])
        (ilist 1 (Pdfpage.endpage !pdf))
    in
      Cpdftweak.append_page_content_multiple_ops opss false fast !pdf
