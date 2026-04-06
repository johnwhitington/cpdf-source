
let redact ~path ~page =
  let resources = Pdf.Null in
  let ops = [] in
  let ops' =Cpdfcontent.filter_ops ~f:(fun () -> ()) ~resources ~ops in
    ()
