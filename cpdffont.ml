(* Embed missing fonts with Ghostscript. *)
let embed_missing_fonts path_to_ghostscript gs_quiet fi fo =
  if path_to_ghostscript = "" then begin
    Printf.eprintf "Please supply path to gs with -gs\n%!";
    exit 2
  end;
    let gscall =
      path_to_ghostscript ^
      " -dNOPAUSE " ^ (if gs_quiet then "-dQUIET" else "") ^ " -sDEVICE=pdfwrite -sOUTPUTFILE=" ^ Filename.quote fo ^
      " -dBATCH " ^ Filename.quote fi
    in
      match Sys.command gscall with
      | 0 -> exit 0
      | _ -> Printf.eprintf "Font embedding failed.\n%!"; exit 2
