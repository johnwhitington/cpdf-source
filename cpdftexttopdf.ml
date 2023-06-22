open Pdfutil

let rec of_utf8_with_newlines fontpack fontsize t =
  let items = ref [] in
  let currfont = ref ~-1 in
  let codepoints = Pdftext.codepoints_of_utf8 t in
  let currtext = ref [] in
  let process_codepoints cs =
    iter
      (fun u ->
         match Cpdfembed.get_char fontpack u with
         | Some (c, n, f) ->
             (*Printf.printf "Charcode %i, font number %i\n" c n;*)
             begin if n <> !currfont then
               begin
                 if !currtext <> [] then items := Cpdftype.Text (rev !currtext)::!items;
                 currtext := [];
                 currfont := n;
                 items := Cpdftype.Font (f, fontsize)::!items;
                 currtext := char_of_int c::!currtext;
               end
             else
               currtext := char_of_int c::!currtext
             end
         | None -> Printf.printf "No glyph for unicode U+%04X in this font\n" u)
      cs;
      items := Cpdftype.Text (rev !currtext)::!items
  in
  let buf = ref [] in
    List.iter
      (function
       | 10 (*'\n'*) ->
           let c = rev !buf in
             if c <> [] then process_codepoints c;
             items := Cpdftype.NewLine::!items;
             buf := []
       | 13 (*'\r'*) -> ()
       | x ->
           buf := x::!buf)
      codepoints;
    (* Do last one *)
    let c = rev !buf in
      if c <> [] then process_codepoints c;
      rev !items

let typeset ~papersize ~font ~fontsize text =
  let pdf = Pdf.empty () in
  let codepoints = setify (Pdftext.codepoints_of_utf8 (Pdfio.string_of_bytes text)) in
  let fontpack =
    match font with
    | Cpdfembed.PreMadeFontPack t -> t
    | Cpdfembed.EmbedInfo {fontfile; fontname; encoding} ->
        Cpdfembed.embed_truetype pdf ~fontfile ~fontname ~codepoints ~encoding
    | Cpdfembed.ExistingNamedFont ->
        raise (Pdf.PDFError "Can't use existing named font for text-to-PDF")
  in
  let instrs = of_utf8_with_newlines fontpack fontsize (Pdfio.string_of_bytes text) in
  let margin = Pdfunits.points (Pdfpaper.width papersize) (Pdfpaper.unit papersize) /. 15.  in
  let firstfont = hd (keep (function Cpdftype.Font _ -> true | _ -> false) instrs) in 
  let instrs = [firstfont; Cpdftype.BeginDocument] @ instrs in
  let pages = Cpdftype.typeset margin margin margin margin papersize pdf instrs in
    let pdf, pageroot = Pdfpage.add_pagetree pages pdf in
      Pdfpage.add_root pageroot [] pdf
