open Pdfutil

let rec split_toc_title a = function
  | '\\'::'n'::r -> rev a :: split_toc_title [] r
  | x::xs -> split_toc_title (x::a) xs
  | [] -> [rev a]

let of_utf8 (f, fontsize) t =
     Pdftext.codepoints_of_utf8 t
  |> option_map (Pdftext.charcode_extractor_of_font_real f)
  |> map char_of_int
  |> implode

let of_pdfdocencoding (f, fontsize) t =
  of_utf8 (f, fontsize) (Pdftext.utf8_of_pdfdocstring t)

let typeset_table_of_contents ~font ~fontsize ~title pdf =
  let marks = Pdfmarks.read_bookmarks pdf in
  if marks = [] then (Printf.eprintf "No bookmarks, not making table of contents\n%!"; pdf) else
  let f = (Pdftext.StandardFont (font, Pdftext.WinAnsiEncoding), fontsize) in
  let big = (Pdftext.StandardFont (font, Pdftext.WinAnsiEncoding), fontsize *. 2.) in
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let firstpage_papersize, pmaxx, pmaxy =
    let width, height, xmax, ymax =
      match Pdf.parse_rectangle firstpage.Pdfpage.mediabox with
        xmin, ymin, xmax, ymax -> xmax -. xmin, ymax -. ymin, xmax, ymax
    in
      Pdfpaper.make Pdfunits.PdfPoint width height, xmax, ymax
  in
  let firstpage_cropbox =
    match Pdf.lookup_direct pdf "/CropBox" firstpage.Pdfpage.rest with
    | Some r -> Some (Pdf.parse_rectangle r)
    | None -> None
  in
  let labels = Pdfpagelabels.read pdf in
  let lines =
    let refnums = Pdf.page_reference_numbers pdf in
    let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
    map
      (fun mark ->
         let label =
           let pnum = Pdfpage.pagenumber_of_target ~fastrefnums pdf mark.Pdfmarks.target in
             try Pdfpagelabels.pagelabeltext_of_pagenumber pnum labels with Not_found -> string_of_int pnum
         in
           [Cpdftype.BeginDest mark.Pdfmarks.target;
            Cpdftype.HGlue {Cpdftype.glen = float mark.Pdfmarks.level *. fontsize *. 2.; Cpdftype.gstretch = 0.};
            Cpdftype.Text (explode (of_pdfdocencoding f mark.Pdfmarks.text));
            Cpdftype.HGlue {Cpdftype.glen = 100.; Cpdftype.gstretch = 0.};
            Cpdftype.Text (explode (of_pdfdocencoding f label));
            Cpdftype.EndDest;
            Cpdftype.NewLine])
      (Pdfmarks.read_bookmarks pdf)
  in
  let toc_pages =
    let title =
      flatten
        (map
          (fun l -> [Cpdftype.Text l; Cpdftype.NewLine])
          (split_toc_title [] (explode title)))
    in
    let lm, rm, tm, bm =
      match firstpage_cropbox with
      | None -> (50., 50., 50., 50.)
      | Some (cminx, cminy, cmaxx, cmaxy) ->
          (cminx +. 50., (pmaxx -. cmaxx) +. 50., cminy +. 50., (pmaxy -. cmaxy) +. 50.)
    in
      Cpdftype.typeset lm rm tm bm firstpage_papersize pdf
        ([Cpdftype.Font big] @ title @
          [Cpdftype.VGlue {glen = fontsize *. 2.; gstretch = 0.};
           Cpdftype.Font f] @ flatten lines)
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
    pdf