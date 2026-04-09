
let redact pdf ~path ~page =
  let resources = Pdf.Null in
  let ops = [] in
  let ops' = Cpdfcontent.filter_ops ~f:(fun () -> ()) ~mediabox:(Pdf.parse_rectangle pdf page.Pdfpage.mediabox) ~resources ~ops in
    ignore ops'

let x = Cpdfclip.Difference

let apply pdf range = ()

let apply_type typ pdf range = ()
