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

let show_bounding_boxes pdf range =
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
    let content_of_boxes boxes =
      [Pdfops.Op_w 0.5] @
      flatten (map (fun (minx, miny, maxx, maxy) -> [Pdfops.Op_re (minx, miny, maxx -. minx, maxy -. miny); Op_S]) boxes)
    in
      iter
        (fun (pnum, boxes) ->
           pdf := Cpdftweak.append_page_content (Pdfops.string_of_ops (content_of_boxes boxes)) false false [pnum] !pdf)
        !bboxes;
      !pdf
