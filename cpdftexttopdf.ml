open Pdfutil
open Cpdferror

let of_utf8_with_newlines fontpack fontsize t =
  let items = ref [] in
  let currfont = ref ~-1 in
  let codepoints = Pdftext.codepoints_of_utf8 t in
  let currtext = ref [] in
  let process_codepoints cs =
    iter
      (fun u ->
         match Cpdfembed.get_char fontpack u with
         | Some (c, n, f) ->
             begin if n <> !currfont then
               begin
                 if !currtext <> [] then items := Cpdftype.Text (rev !currtext)::!items;
                 currtext := [];
                 currfont := n;
                 items := Cpdftype.Font (string_of_int n, f, fontsize)::!items;
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
             currtext := [];
             buf := []
       | 13 (*'\r'*) -> ()
       | x ->
           buf := x::!buf)
      codepoints;
    (* Do last one *)
    let c = rev !buf in
      if c <> [] then process_codepoints c;
      rev !items

(* Post process, adding Tag / EndTag around paragraphs *)
let rec tag_paragraphs = function
  | Cpdftype.NewLine::Cpdftype.NewLine::t ->
      Cpdftype.EndTag::Cpdftype.NewLine::Cpdftype.NewLine::Cpdftype.Tag "P"::tag_paragraphs t
  | x::t -> x::tag_paragraphs t
  | [] -> [Cpdftype.EndTag]

let tag_paragraphs l =
  Cpdftype.Tag "P"::tag_paragraphs l

let typeset ~process_struct_tree ?subformat ?title ~papersize ~font ~fontsize text =
  let process_struct_tree =
    process_struct_tree || subformat = Some Cpdfua.PDFUA1 || subformat = Some Cpdfua.PDFUA2
  in
  let pdf, title =
    match subformat with
    | None -> Pdf.empty (), begin match title with Some x -> x | None -> "" end
    | Some Cpdfua.PDFUA1 ->
        begin match title with
        | None -> error "no -title given" 
        | Some title -> Cpdfua.create_pdfua1 title papersize 1, title
        end
    | Some Cpdfua.PDFUA2 ->
        begin match title with
        | None -> error "no -title given"
        | Some title -> Cpdfua.create_pdfua2 title papersize 1, title
        end
  in
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
  (*flprint (Cpdftype.to_string instrs);
  flprint "------------------------------";*)
  let tagged = tag_paragraphs instrs in
  (*flprint (Cpdftype.to_string tagged);*)
  let margin = Pdfunits.points (Pdfpaper.width papersize) (Pdfpaper.unit papersize) /. 15.  in
  let instrs =
    if tagged = [] then [] else
      let firstfont = hd (keep (function Cpdftype.Font _ -> true | _ -> false) tagged) in 
        [firstfont; Cpdftype.BeginDocument] @ tagged
  in
  let pages = Cpdftype.typeset ~process_struct_tree margin margin margin margin papersize pdf instrs in
    let pdf, pageroot = Pdfpage.add_pagetree pages pdf in
      Pdfpage.add_root pageroot [] pdf
