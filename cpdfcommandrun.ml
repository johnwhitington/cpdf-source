let _ =
  match Filename.basename Sys.argv.(0) with
    "cpdf.top" -> ()
  | _ -> Cpdfcommand.go ()
