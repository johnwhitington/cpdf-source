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
  | Cpdfcontent.Glyph -> [Pdfops.Op_G (if light then 1.0 else 0.0)]
  | Cpdfcontent.InlineImage -> [if light then Pdfops.Op_RG (1.0, 0.5, 0.5) else Pdfops.Op_RG (1.0, 0., 0.)]
  | Cpdfcontent.Image -> [if light then Pdfops.Op_RG (0.5, 1.0, 0.5) else Pdfops.Op_RG (0., 1.0, 0.)]
  | Cpdfcontent.Path -> [if light then Pdfops.Op_RG (0.5, 0.5, 1.0) else Pdfops.Op_RG (0., 0., 1.0)]
  | Cpdfcontent.Shading -> [if light then Pdfops.Op_RG (0.5, 1.0, 1.0) else Pdfops.Op_RG (0., 1., 1.)]

let mkbox ~light (boxtype, (x0, y0, x1, y1, x2, y2, x3, y3)) =
  colour ~light boxtype @ [Pdfops.Op_w 0.5] @
  [Pdfops.Op_m (x0, y0); Op_l (x1, y1); Op_l (x2, y2); Op_l (x3, y3); Op_h; Op_S]

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

let show_bounding_boxes ~light pdf range =
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
    let content_of_boxes boxes = flatten (map (mkbox ~light) boxes) in
      iter
        (fun (pnum, boxes) ->
           pdf := Cpdftweak.append_page_content (Pdfops.string_of_ops (content_of_boxes boxes)) false false [pnum] !pdf)
        (rev !bboxes);
      !pdf
