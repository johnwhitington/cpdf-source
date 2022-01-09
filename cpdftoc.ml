open Pdfutil

(* We allow \n in titles. Split for typesetter. *)
let rec split_toc_title_inner a = function
  | '\\'::'n'::r -> rev a :: split_toc_title_inner [] r
  | x::xs -> split_toc_title_inner (x::a) xs
  | [] -> [rev a]

let split_toc_title = split_toc_title_inner []

(* And for new bookmark for TOC, change \\n to \n *)
let rec real_newline = function
  | '\\'::'n'::r -> '\n'::real_newline r
  | x::r -> x::real_newline r
  | [] -> []

(* Cpdftype codepoints from a font and UTF8 *)
let of_utf8 f t =
     Pdftext.codepoints_of_utf8 t
  |> option_map (Pdftext.charcode_extractor_of_font_real f)
  |> map char_of_int

(* Cpdftype codepoints from a font and PDFDocEndoding string *)
let of_pdfdocencoding f t =
  of_utf8 f (Pdftext.utf8_of_pdfdocstring t)

(* Typeset a table of contents with given font, font size and title. Mediabox
   (and CropBox) copied from first page of existing PDF. Margin of 10% inside
   CropBox. Font size of title twice body font size. Null page labels added for
   TOC, others bumped up and so preserved. *)
let typeset_table_of_contents ~font ~fontsize ~title ~bookmark pdf =
  let marks = Pdfmarks.read_bookmarks pdf in
  if marks = [] then (Printf.eprintf "No bookmarks, not making table of contents\n%!"; pdf) else
  let f, fs = (Pdftext.StandardFont (font, Pdftext.WinAnsiEncoding), fontsize) in
  let big = (Pdftext.StandardFont (font, Pdftext.WinAnsiEncoding), fontsize *. 2.) in
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let width, firstpage_papersize, pmaxx, pmaxy, margin =
    let width, height, xmax, ymax =
      match Pdf.parse_rectangle firstpage.Pdfpage.mediabox with
        xmin, ymin, xmax, ymax -> xmax -. xmin, ymax -. ymin, xmax, ymax
    in
      width, Pdfpaper.make Pdfunits.PdfPoint width height, xmax, ymax, fmin width height *. 0.1
  in
  let firstpage_cropbox =
    match Pdf.lookup_direct pdf "/CropBox" firstpage.Pdfpage.rest with
    | Some r -> Some (Pdf.parse_rectangle r)
    | None -> None
  in
  let width =
    match firstpage_cropbox with
    | Some (xmin, _, xmax, _) -> xmax -. xmin
    | None -> width 
  in
  let labels = Pdfpagelabels.read pdf in
  let lines =
    let refnums = Pdf.page_reference_numbers pdf in
    let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
    map
      (fun mark ->
         let indent = float mark.Pdfmarks.level *. fontsize *. 2. in 
         let text = of_pdfdocencoding f mark.Pdfmarks.text in
         let label =
           if mark.Pdfmarks.target = NullDestination then [' '] else 
           let pde =
             let pnum = Pdfpage.pagenumber_of_target ~fastrefnums pdf mark.Pdfmarks.target in
               try Pdfpagelabels.pagelabeltext_of_pagenumber pnum labels with Not_found -> string_of_int pnum
           in
             of_pdfdocencoding f pde
         in
         let widths = Cpdftype.font_widths f fontsize in
         let space =
              width
           -. margin *. 2.
           -. Cpdftype.width_of_string widths text
           -. Cpdftype.width_of_string widths label
           -. indent
         in
           [Cpdftype.BeginDest mark.Pdfmarks.target;
            Cpdftype.HGlue {Cpdftype.glen = indent; Cpdftype.gstretch = 0.};
            Cpdftype.Text text;
            Cpdftype.HGlue {Cpdftype.glen = space; Cpdftype.gstretch = 0.};
            Cpdftype.Text label;
            Cpdftype.EndDest;
            Cpdftype.NewLine])
      (Pdfmarks.read_bookmarks pdf)
  in
  let toc_pages =
    let title =
      let glue = Cpdftype.VGlue {glen = fontsize *. 2.; gstretch = 0.} in
        if title = "" then [] else
          flatten
            (map
              (fun l -> [Cpdftype.Text l; Cpdftype.NewLine])
              (split_toc_title (of_utf8 f title)))
          @ [glue]
    in
    let lm, rm, tm, bm =
      match firstpage_cropbox with
      | None -> (margin, margin, margin, margin)
      | Some (cminx, cminy, cmaxx, cmaxy) ->
          (cminx +. margin, (pmaxx -. cmaxx) +. margin, cminy +. margin, (pmaxy -. cmaxy) +. margin)
    in
      Cpdftype.typeset lm rm tm bm firstpage_papersize pdf
        ([Cpdftype.Font big; Cpdftype.BeginDocument] @ title @
          [Cpdftype.Font (f, fs)] @ flatten lines)
  in
  let toc_pages =
    match firstpage_cropbox with
    | Some (a, b, c, d) ->
        let rect =
          Pdf.Array [Pdf.Real a; Pdf.Real b; Pdf.Real c; Pdf.Real d]
        in
          map
            (fun p -> {p with Pdfpage.rest = Pdf.add_dict_entry p.Pdfpage.rest "/CropBox" rect})
            toc_pages
    | None -> toc_pages
  in
  let original_pages = Pdfpage.pages_of_pagetree pdf in
  let toc_pages_len = length toc_pages in
  let changes = map (fun n -> (n, n + toc_pages_len)) (indx original_pages) in
  let pdf = Pdfpage.change_pages ~changes true pdf (toc_pages @ original_pages) in
  let label =
    {Pdfpagelabels.labelstyle = NoLabelPrefixOnly;
     Pdfpagelabels.labelprefix = None;
     Pdfpagelabels.startpage = 1;
     Pdfpagelabels.startvalue = 1}
  in
  let labels' = label::map (fun l -> {l with Pdfpagelabels.startpage = l.Pdfpagelabels.startpage + toc_pages_len}) labels in
    Pdfpagelabels.write pdf labels';
    if bookmark then
      let marks = Pdfmarks.read_bookmarks pdf in
      let refnums = Pdf.page_reference_numbers pdf in
      let newmark =
        {Pdfmarks.level = 0;
         Pdfmarks.text = Pdftext.pdfdocstring_of_utf8 (implode (real_newline (explode title)));
         Pdfmarks.target = Pdfdest.XYZ (Pdfdest.PageObject (hd refnums), None, None, None);
         Pdfmarks.isopen = false}
      in
        Pdfmarks.add_bookmarks (newmark::marks) pdf
    else
      pdf
