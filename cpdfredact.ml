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
