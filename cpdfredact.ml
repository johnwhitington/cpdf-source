open Pdfutil

(* Redact a path on a page *)
let redact pdf ~path range =
  let redact_page page =
    let resources = Pdf.Null in
    let ops = [] in
    let ops' = Cpdfcontent.filter_ops ~f:(fun _ -> false) ~mediabox:(Pdf.parse_rectangle pdf page.Pdfpage.mediabox) ~resources ~ops in
      ignore ops'
  in
    Cpdfpage.process_pages
      (Pdfpage.ppstub
        (fun pnum page -> if mem pnum range then (redact_page page; page) else page))
           pdf
           range

(* Apply redaction annotations. *)
let apply pdf range = ()

(* Apply other annotations (Rect, Polyline) etc. as if they were redaction annotations. *)
let apply_type pdf typ range = ()

let colour ~light = function
  | Cpdfcontent.Glyph -> [Pdfops.Op_G (if light then 1. else 0.)] (* Black *)
  | Cpdfcontent.InlineImage -> [if light then Pdfops.Op_RG (1., 0.5, 0.5) else Pdfops.Op_RG (1., 0., 0.)] (* Red *)
  | Cpdfcontent.Image -> [if light then Pdfops.Op_RG (0.5, 1., 0.5) else Pdfops.Op_RG (0., 1., 0.)] (* Green *)
  | Cpdfcontent.Path -> [if light then Pdfops.Op_RG (0.5, 0.5, 1.) else Pdfops.Op_RG (0., 0., 1.)] (* Blue *)
  | Cpdfcontent.Shading -> [if light then Pdfops.Op_RG (0.5, 1., 1.) else Pdfops.Op_RG (0., 1., 1.)] (* Cyan *)
  | Cpdfcontent.Clip -> [Pdfops.Op_d ([4.; 4.], 0.); Pdfops.Op_G (if light then 1. else 0.)] (* Black, dashed. *)

let mkbox ~light (boxtype, (x0, y0, x1, y1, x2, y2, x3, y3)) =
  [Pdfops.Op_q] @
  colour ~light boxtype @ [Pdfops.Op_w 0.5] @
  [Pdfops.Op_m (x0, y0); Op_l (x1, y1); Op_l (x2, y2); Op_l (x3, y3); Op_h; Op_S] @
  [Pdfops.Op_Q]

let mkannotbox ~light (minx, miny, maxx, maxy) =
  [if light then Pdfops.Op_RG (1.0, 1.0, 0.5) else Pdfops.Op_RG (1.0, 1.0, 0.)] @ [Pdfops.Op_w 1.0] @
  [Pdfops.Op_m (minx, miny); Op_l (minx, maxy); Op_l (maxx, maxy); Op_l (maxx, miny); Op_l (minx, miny); Op_h; Op_S]

let show_annotation_bounding_boxes ~light pdf range =
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
      iter
        (fun (pnum, boxes) ->
           if boxes <> [] then
             pdf := Cpdftweak.append_page_content (Pdfops.string_of_ops (content_of_boxes boxes)) false false [pnum] !pdf)
        (rev !bboxes);
      !pdf

(* See if the given box is to be treated. For now:

  a) Glyphs - any intersection
  b) Image - any intersection
  c) InlineImage - any intersection
  d) Path - path must be wholly contained in box
  e) Shading - must be wholly contained in box *)
let box_matches (minx, miny, maxx, maxy) (boxtype, (x0, y0, x1, y1, x2, y2, x3, y3)) =
  let bminx, bmaxx, bminy, bmaxy =
    fmin (fmin x0 x1) (fmin x2 x3), fmax (fmax x0 x1) (fmax x2 x3),
    fmin (fmin y0 y1) (fmin y2 y3), fmax (fmax y0 y1) (fmax y2 y3)
  in
  let any_intersection (minx, miny, maxx, maxy) (bminx, bminy, bmaxx, bmaxy) =
    box_overlap_float minx miny maxx maxy bminx bminy bmaxx bmaxy <> None
  in
  let wholly_contained (minx, miny, maxx, maxy) (bminx, bminy, bmaxx, bmaxy) =
    bminx > minx && bmaxx < maxx && bminy > miny && bmaxx < maxy
  in
    match boxtype with
    | Cpdfcontent.Glyph | Image | InlineImage -> any_intersection (minx, miny, maxx, maxy) (bminx, bminy, bmaxx, bmaxy)
    | Path | Shading | Clip -> wholly_contained (minx, miny, maxx, maxy) (bminx, bminy, bmaxx, bmaxy)

let select_boxes shape boxes =
  match shape with 
  | None -> boxes
  | Some (minx, miny, maxx, maxy) ->
      keep (box_matches (minx, miny, maxx, maxy)) boxes

let show_bounding_boxes ~shape ~light pdf range =
  let pdf = show_annotation_bounding_boxes ~light pdf range in
  Cpdfutil.progress_line "Finding page content bounding boxes...";
  let show_bounding_boxes_page page =
    let ops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content in
    let page_boxes = ref [] in
      ignore (Cpdfcontent.filter_ops ~pdf ~f:(fun box -> page_boxes =| box; false) ~mediabox:(Pdf.parse_rectangle pdf page.Pdfpage.mediabox) ~resources:page.Pdfpage.resources ~ops);
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
      iter
        (fun (pnum, boxes) ->
           pdf := Cpdftweak.append_page_content (Pdfops.string_of_ops (content_of_boxes boxes)) false false [pnum] !pdf)
        (rev !bboxes);
      !pdf
