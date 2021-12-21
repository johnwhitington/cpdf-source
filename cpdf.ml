(* CPDF Core routines *)
open Pdfutil
open Pdfio
open Cpdferror

(* For uses of process_pages which don't need to deal with matrices, this
   function transforms into one which returns the identity matrix *)
let ppstub f n p = (f n p, n, Pdftransform.i_matrix)

(* Add stack operators to a content stream to ensure it is composeable. On
-fast, we don't check for Q deficit, assuming PDF is ISO. *)
let protect fast pdf resources content =
  let deficit =
    if fast then 0 else
      let ops = Pdfops.parse_operators pdf resources content in
      let qs = length (keep (eq Pdfops.Op_q) ops) in
      let bigqs = length (keep (eq Pdfops.Op_Q) ops) in
      let deficit = if qs > bigqs then qs - bigqs else 0 in
        if deficit <> 0 then Printf.eprintf "Q Deficit was nonzero. Fixing. %i\n%!" deficit;
        deficit
  in
    let addstream ops = Pdf.addobj pdf (Pdfops.stream_of_ops ops) in
    let q = addstream [Pdfops.Op_q] in
    let qs = addstream (many Pdfops.Op_Q deficit @ [Pdfops.Op_Q]) in
      [Pdf.Indirect q] @ content @ [Pdf.Indirect qs]

(* If a cropbox exists, make it the mediabox. If not, change nothing. *)
let copy_cropbox_to_mediabox pdf range =
  Cpdfpage.process_pages
    (ppstub (fun _ page ->
       match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
       | Some pdfobject -> {page with Pdfpage.mediabox = Pdf.direct pdf pdfobject}
       | None -> page))
    pdf
    range

(* Union two resource dictionaries from the same PDF. *)
let combine_pdf_resources pdf a b =
  let a_entries =
    match a with
    | Pdf.Dictionary entries -> entries
    | _ -> []
  in let b_entries =
    match b with
    | Pdf.Dictionary entries -> entries
    | _ -> []
  in
    let resource_keys =
      ["/Font"; "/ExtGState"; "/ColorSpace"; "/Pattern";
       "/Shading"; "/XObject"; "/Properties"]
    in
      let combine_entries key =
        let a_entries =
          match Pdf.lookup_direct pdf key a with
          | Some (Pdf.Dictionary d) -> d
          | _ -> []
        in let b_entries =
          match Pdf.lookup_direct pdf key b with
          | Some (Pdf.Dictionary d) -> d
          | _ -> []
        in
          if a_entries = [] && b_entries = [] then
            None
          else
            Some (key, Pdf.Dictionary (a_entries @ b_entries))
      in
        let unknown_keys_a = lose (fun (k, _) -> mem k resource_keys) a_entries in
        let unknown_keys_b = lose (fun (k, _) -> mem k resource_keys) b_entries in
        let combined_known_entries = option_map combine_entries resource_keys in
          fold_left
            (fun dict (k, v) -> Pdf.add_dict_entry dict k v)
            (Pdf.Dictionary [])
            (unknown_keys_a @ unknown_keys_b @ combined_known_entries)


(* Does the page have a defined box e.g "/CropBox" *)
let hasbox pdf page boxname =
  let pages = Pdfpage.pages_of_pagetree pdf in
    if page > length pages || page < 1 then raise (Failure "hasbox: bad page") else
      let p = select page pages in
        match Pdf.lookup_direct pdf boxname p.Pdfpage.rest with
        | Some _ -> true
        | _ -> false


(* \section{Shift page data} *)
let make_mediabox (xmin, ymin, xmax, ymax) =
  Pdf.Array
    [Pdf.Real xmin; Pdf.Real ymin; Pdf.Real xmax; Pdf.Real ymax]

(* Change the media box and other known boxes by the function [f] which takes
xmin, xmax, ymin, ymax as input. *)
let change_boxes f pdf page =
  let names = ["/TrimBox"; "/ArtBox"; "/CropBox"; "/BleedBox"]
  in let getbox n =
    Pdf.lookup_direct pdf n page.Pdfpage.rest
  in
    let boxes = combine names (map getbox names) in
      let toreplace = lose (function (_, None) -> true | _ -> false) boxes in
        let toreplace =
          map
            (function (name, Some value) -> (name, value) | _ -> assert false)
            toreplace
        in
          let rest' =
            fold_left
              (fun e (k, v) ->
                 let v =
                   make_mediabox (f (Pdf.parse_rectangle v))
                 in
                   Pdf.replace_dict_entry e k v)
              page.Pdfpage.rest
              toreplace
          in
            {page with
               Pdfpage.mediabox =
                 make_mediabox (f (Pdf.parse_rectangle page.Pdfpage.mediabox));
               Pdfpage.rest = rest'}

let process_xobject f pdf resources i =
  let xobj = Pdf.lookup_obj pdf i in
    match Pdf.lookup_direct pdf "/Subtype" xobj with
    | None -> raise (Pdf.PDFError "No /Subtype in Xobject") 
    | Some (Pdf.Name "/Form") ->
        Pdf.getstream xobj;
        begin match xobj with
        | Pdf.Stream ({contents = Pdf.Dictionary dict, Pdf.Got bytes} as rf) ->
            begin match f pdf resources [Pdf.Stream rf] with
            | [Pdf.Stream {contents = (Pdf.Dictionary dict', data)}] ->
                let dict' =
                  Pdf.remove_dict_entry
                    (Pdf.Dictionary (mergedict dict dict'))
                    "/Filter"
                in
                  rf := (dict', data)
            | _ -> assert false
            end
        | _ -> assert false (* getstream would have complained already *)
        end
    | Some _ -> ()

let process_xobjects pdf page f =
  match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
  | Some (Pdf.Dictionary elts) ->
      iter
        (fun (k, v) ->
          match v with
          | Pdf.Indirect i -> process_xobject f pdf page.Pdfpage.resources i
          | _ -> raise (Pdf.PDFError "process_xobject"))
        elts
  | _ -> ()

(* The content transformed by altering any use of [Op_cm]. But we must also
alter any /Matrix entries in pattern dictionaries *)
let change_pattern_matrices_resources pdf tr resources =
  try
    begin match Pdf.lookup_direct pdf "/Pattern" resources with
    | Some (Pdf.Dictionary patterns) ->
        let entries =
          map
            (fun (name, p) ->
              (*Printf.printf "Changing matrices of pattern %s\n" name;*)
              let old_pattern = Pdf.direct pdf p in
                let new_pattern =
                  let existing_tr = Pdf.parse_matrix pdf "/Matrix" old_pattern in
                    let new_tr = Pdftransform.matrix_compose (Pdftransform.matrix_invert tr) existing_tr in
                      Pdf.add_dict_entry old_pattern "/Matrix" (Pdf.make_matrix new_tr)
                in
                  name, Pdf.Indirect (Pdf.addobj pdf new_pattern))
            patterns
         in
           Pdf.add_dict_entry resources "/Pattern" (Pdf.Dictionary entries)
    | _ -> resources
  end
    with
      Pdftransform.NonInvertable ->
        Printf.eprintf "Warning: noninvertible matrix\n%!";
        resources

let change_pattern_matrices_page pdf tr page =
  let page =
    {page with Pdfpage.resources = change_pattern_matrices_resources pdf tr page.Pdfpage.resources}
  in
    match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
    | Some (Pdf.Dictionary elts) ->
        iter
          (fun (k, v) -> 
             match v with
             | Pdf.Indirect i ->
                 (* Check if it's a form XObject. If so, rewrite its resources and add back as same number. *)
                 begin match Pdf.lookup_direct pdf "/Subtype" v with
                 | Some (Pdf.Name "/Form") ->
                     (*Printf.printf "Processing form xobject %s for patterns\n" k; *)
                     let form_xobject = Pdf.lookup_obj pdf i in
                       begin match Pdf.lookup_direct pdf "/Resources" form_xobject with
                       | Some resources ->
                           let form_xobject' =
                             Pdf.add_dict_entry form_xobject "/Resources" (change_pattern_matrices_resources pdf tr resources)  
                           in
                             Pdf.addobj_given_num pdf (i, form_xobject')
                       | _ -> ()
                       end
                 | _ -> ()
                 end;
             | _ -> raise (Pdf.PDFError "change_pattern_matrices_page"))
          elts;
        page
    | _ -> page

let transform_rect transform rect =
  let minx, miny, maxx, maxy = Pdf.parse_rectangle rect in
    let (x0, y0) = Pdftransform.transform_matrix transform (minx, miny) in
    let (x1, y1) = Pdftransform.transform_matrix transform (maxx, maxy) in
    let (x2, y2) = Pdftransform.transform_matrix transform (minx, maxy) in
    let (x3, y3) = Pdftransform.transform_matrix transform (maxx, miny) in
      let minx = fmin (fmin x0 x1) (fmin x2 x3) in
      let miny = fmin (fmin y0 y1) (fmin y2 y3) in
      let maxx = fmax (fmax x0 x1) (fmax x2 x3) in
      let maxy = fmax (fmax y0 y1) (fmax y2 y3) in
        Pdf.Array [Pdf.Real minx; Pdf.Real miny; Pdf.Real maxx; Pdf.Real maxy]

let transform_quadpoint_single transform = function
  | [x1; y1; x2; y2; x3; y3; x4; y4] ->
      let x1, y1, x2, y2, x3, y3, x4, y4 =
        Pdf.getnum x1, Pdf.getnum y1,
        Pdf.getnum x2, Pdf.getnum y2,
        Pdf.getnum x3, Pdf.getnum y3,
        Pdf.getnum x4, Pdf.getnum y4
      in
        let (x1, y1) = Pdftransform.transform_matrix transform (x1, y1) in
        let (x2, y2) = Pdftransform.transform_matrix transform (x2, y2) in
        let (x3, y3) = Pdftransform.transform_matrix transform (x3, y3) in
        let (x4, y4) = Pdftransform.transform_matrix transform (x4, y4) in
          map (fun x -> Pdf.Real x) [x1; y1; x2; y2; x3; y3; x4; y4]
  | qp ->
     Printf.eprintf "Malformed /QuadPoints format: must be a multiple of 8 entries\n";
     qp

let transform_quadpoints transform = function
| Pdf.Array qps ->
    Pdf.Array (flatten (map (transform_quadpoint_single transform) (splitinto 8 qps)))
| qp ->
    Printf.eprintf "Unknown or malformed /QuadPoints format %s\n" (Pdfwrite.string_of_pdf qp);
    qp

(* Apply transformations to any annotations in /Annots (i.e their /Rect and /QuadPoints entries) *)
let transform_annotations pdf transform rest =
  match Pdf.lookup_direct pdf "/Annots" rest with
  | Some (Pdf.Array annots) ->
      (* Always indirect references, so alter in place *)
      iter
        (function
         | Pdf.Indirect i ->
             let annot = Pdf.lookup_obj pdf i in
             let rect' =
               match Pdf.lookup_direct pdf "/Rect" annot with
               | Some rect -> transform_rect transform rect
               | None -> raise (Pdf.PDFError "transform_annotations: no rect")
               in
             let quadpoints' =
               match Pdf.lookup_direct pdf "/QuadPoints" annot with
               | Some qp -> Some (transform_quadpoints transform qp)
               | None -> None
               in
             let annot = Pdf.add_dict_entry annot "/Rect" rect' in
             let annot =
               match quadpoints' with
               | Some qp -> Pdf.add_dict_entry annot "/QuadPoints" qp 
               | None -> annot
             in
               Pdf.addobj_given_num pdf (i, annot)
         | _ -> Printf.eprintf "transform_annotations: not indirect\n%!")
        annots
   | _ -> ()

let shift_page ?(fast=false) dxdylist pdf pnum page =
  let dx, dy = List.nth dxdylist (pnum - 1) in
    let transform_op =
      Pdfops.Op_cm (Pdftransform.matrix_of_op (Pdftransform.Translate (dx, dy)))
    in
      let page =
        change_pattern_matrices_page pdf (Pdftransform.mktranslate ~-.dx ~-.dy) page
      in
        transform_annotations pdf (Pdftransform.mktranslate dx dy) page.Pdfpage.rest;
        (Pdfpage.prepend_operators pdf [transform_op] ~fast page, pnum, Pdftransform.mktranslate dx dy)

let shift_pdf ?(fast=false) dxdylist pdf range =
  Cpdfpage.process_pages (shift_page ~fast dxdylist pdf) pdf range

(* Change a page's media box so its minimum x and y are 0, making other
operations simpler to think about. Any shift that is done is reflected in
other boxes (clip etc.) *)
let rectify_boxes ?(fast=false) pdf page =
  let minx, miny, _, _ =
    Pdf.parse_rectangle page.Pdfpage.mediabox
  in
    let f (iminx, iminy, imaxx, imaxy) =
      iminx -. minx, iminy -. miny, imaxx -. minx, imaxy -. miny
    in
      let page = change_boxes f pdf page in
        if minx <> 0. || miny <> 0.
          then
            begin let p, _, _ = shift_page ~fast [(-.minx),(-.miny)] pdf 1 page in p end
          else page

(* \section{Flip pages} *)
let flip_page ?(fast=false) transform_op pdf pnum page =
  let minx, miny, maxx, maxy =
    Pdf.parse_rectangle page.Pdfpage.mediabox
  in
    let tr = transform_op minx miny maxx maxy in
      let page = change_pattern_matrices_page pdf tr page in
        transform_annotations pdf tr page.Pdfpage.rest;
        (Pdfpage.prepend_operators pdf [Pdfops.Op_cm tr] ~fast page, pnum, tr)

let vflip_pdf ?(fast=false) pdf range =
  let transform_op _ miny _ maxy =
    Pdftransform.matrix_of_op
      (Pdftransform.Scale ((0., ((miny +. maxy) /. 2.)), 1., -.1.))
  in
    Cpdfpage.process_pages (flip_page ~fast transform_op pdf) pdf range

let hflip_pdf ?(fast=false) pdf range =
  let transform_op minx _ maxx _ =
    Pdftransform.matrix_of_op
      (Pdftransform.Scale (((minx +. maxx) /. 2., 0.), -.1., 1.))
  in
    Cpdfpage.process_pages (flip_page ~fast transform_op pdf) pdf range

let stamp_shift_of_position topline midline sw sh w h p =
  let half x = x /. 2.
  and dy =
    if midline then sh /. 2.
    else if topline then sh
    else 0.
  in
    let open Cpdfposition in
      match p with
      | PosCentre (ox, oy) -> ox -. half sw, oy -. dy
      | PosLeft (ox, oy) -> ox, oy -. dy
      | PosRight (ox, oy) -> ox -. sw, oy -. dy
      | Top o -> half w -. half sw, h -. o -. sh -. dy
      | TopLeft o -> o, h -. sh -. o -. dy
      | TopRight o -> w -. sw -. o, h -. sh -. o -. dy
      | Left o -> o, half h -. half sh -. dy
      | BottomLeft o -> o, o -. dy
      | Bottom o -> half w -. half sw, o -. dy
      | BottomRight o -> w -. sw -. o, o -. dy
      | Right o -> w -. sw -. o, half h -. half sh -. dy
      | Diagonal | ReverseDiagonal | Centre ->
          half w -. half sw, half h -. half sh -. dy

(* Combine Pdfpage.rest items for two PDFs. For now, we combine /Annots, and
 * copy everything else from adict. What else should we combine? *)
let combine_page_items pdf adict bdict =
  let getannots dict =
    begin match dict with
      Pdf.Dictionary d ->
        begin match lookup "/Annots" d with
          Some (Pdf.Array items) -> items
        | _ -> []
        end
    | _ -> []
    end
  in
    let a_annots = getannots adict in
    let b_annots = getannots bdict in
      match a_annots @ b_annots with
        [] -> adict
      | annots -> Pdf.add_dict_entry adict "/Annots" (Pdf.Array annots)

let do_stamp relative_to_cropbox fast position topline midline scale_to_fit isover pdf o u opdf =
  (* Scale page stamp o to fit page u *)
  let sxmin, symin, sxmax, symax =
    Pdf.parse_rectangle
      (match Pdf.lookup_direct pdf "/CropBox" o.Pdfpage.rest with | Some r -> r | None -> o.Pdfpage.mediabox)
  in let txmin, tymin, txmax, tymax =
    Pdf.parse_rectangle
      (match Pdf.lookup_direct pdf "/CropBox" u.Pdfpage.rest with | Some r -> r | None -> u.Pdfpage.mediabox)
  in
    let o =
      if scale_to_fit then
        let xmag = (txmax -. txmin) /. (sxmax -. sxmin) in
          let ymag = (tymax -. tymin) /. (symax -. symin) in
            let scale =
              if xmag < 0.999 && ymag < 0.999 then
                if xmag > ymag then xmag else ymag
              else if xmag >= 1.001 && ymag >= 1.001 then
                if xmag > ymag then ymag else xmag
              else if xmag >= 1.001 then ymag
              else xmag
            in
              let dx = txmin +. ((txmax -. txmin) -. (sxmax -. sxmin) *. scale) /. 2. in
                let dy = tymin +. ((tymax -. tymin) -. (symax -. symin) *. scale) /. 2. in
                  let matrix = 
                    (Pdftransform.matrix_of_transform
                         ([Pdftransform.Translate (dx, dy)] @
                          (if relative_to_cropbox then [Pdftransform.Translate (txmin, tymin)] else []) @
                          [Pdftransform.Scale ((sxmin, symin), scale, scale)]))
                  in
                    transform_annotations pdf matrix o.Pdfpage.rest;
                    let r = Pdfpage.prepend_operators pdf [Pdfops.Op_cm matrix] ~fast o in
                      change_pattern_matrices_page pdf matrix r
      else
        let sw = sxmax -. sxmin and sh = symax -. symin
        and w = txmax -. txmin and h = tymax -. tymin in
          let dx, dy = stamp_shift_of_position topline midline sw sh w h position in
            let matrix =
              (Pdftransform.matrix_of_transform
                ((if relative_to_cropbox then [Pdftransform.Translate (txmin, tymin)] else []) @
                [Pdftransform.Translate (dx, dy)]))
            in
              transform_annotations pdf matrix o.Pdfpage.rest;
              let r = Pdfpage.prepend_operators pdf [Pdfops.Op_cm matrix] ~fast o in
                change_pattern_matrices_page pdf matrix r
    in
      {u with
         Pdfpage.content =
           (if isover then ( @ ) else ( @@ ))
           (protect fast pdf u.Pdfpage.resources u.Pdfpage.content)
           (protect fast pdf o.Pdfpage.resources o.Pdfpage.content);
         Pdfpage.rest =
           combine_page_items pdf u.Pdfpage.rest o.Pdfpage.rest; 
         Pdfpage.resources =
           combine_pdf_resources pdf u.Pdfpage.resources o.Pdfpage.resources}

let stamp relative_to_cropbox position topline midline fast scale_to_fit isover range over pdf =
  let prefix = Pdfpage.shortest_unused_prefix pdf in
  Pdfpage.add_prefix over prefix;
  let marks = Pdfmarks.read_bookmarks pdf in
  let marks_refnumbers = Pdf.page_reference_numbers pdf in
  let pdf = Pdfmarks.remove_bookmarks pdf in
  let over = Pdfmarks.remove_bookmarks over in
  let pageseqs = ilist 1 (Pdfpage.endpage pdf) in
    let over_firstpage_pdf =
      match Pdfpage.pages_of_pagetree over with
      | [] -> error "empty PDF"
      | h::_ -> Pdfpage.change_pages ~changes:[(1, 1)] true over [h]
    in
      let merged =
        Pdfmerge.merge_pdfs
          false false ["a"; "b"] [pdf; over_firstpage_pdf] [pageseqs; [1]]
      in
        let merged =
          {merged with Pdf.saved_encryption = pdf.Pdf.saved_encryption}
        in
          let merged = Cpdfmetadata.copy_id true pdf merged in
            let merged_pages = Pdfpage.pages_of_pagetree merged in
              let under_pages, over_page =
                all_but_last merged_pages, last merged_pages
              in
                let new_pages =
                  map2
                    (fun pageseq under_page ->
                      do_stamp relative_to_cropbox fast position topline midline scale_to_fit isover merged
                      (if mem pageseq range then over_page else
                        Pdfpage.blankpage Pdfpaper.a4)
                      under_page over)
                    pageseqs
                    under_pages 
                in
                  let changed =
                    let changes =
                      map (fun x -> (x, x)) (ilist 1 (length new_pages))
                    in
                      Pdfpage.change_pages ~changes true merged new_pages
                  in
                    let new_refnumbers = Pdf.page_reference_numbers changed in
                    let changetable = hashtable_of_dictionary (combine marks_refnumbers new_refnumbers) in
                    let new_marks = map (Cpdfbookmarks.change_bookmark changetable) marks in
                      Pdfmarks.add_bookmarks new_marks changed

(* Combine pages from two PDFs. For now, assume equal length. *)

(* If [over] has more pages than [under], chop the excess. If the converse, pad
[over] to the same length *)
let equalize_pages under over =
  let length_under = Pdfpage.endpage under in
  let length_over = Pdfpage.endpage over in
    if length_over > length_under then
      let changes =
        map (fun x -> (x, x)) (ilist 1 length_under)
      in
        (under,
         (Pdfpage.change_pages
            ~changes true over (take (Pdfpage.pages_of_pagetree over) length_under)))
    else if length_under > length_over then
      let changes =
        map (fun x -> (x, x)) (ilist 1 length_over)
      in
        (under,
         Pdfpage.change_pages
           ~changes true over
           (Pdfpage.pages_of_pagetree over @
              (many (Pdfpage.blankpage Pdfpaper.a4) (length_under - length_over))))
    else
      under, over

let combine_pages fast under over scaletofit swap equalize =
  let debug_combine_pages = false in
  let debug_pdf pdf n =
    if debug_combine_pages then
      begin Pdf.remove_unreferenced pdf; Pdfwrite.pdf_to_file pdf n end
  in
  Pdfpage.add_prefix over (Pdfpage.shortest_unused_prefix under);
  let marks_under, marks_over = Pdfmarks.read_bookmarks under, Pdfmarks.read_bookmarks over in
  let under, over = if equalize then equalize_pages under over else under, over in
  let under_length, over_length = Pdfpage.endpage under, Pdfpage.endpage over in
    if under_length <> over_length then
      raise (Pdf.PDFError "combine_pages: not of equal length")
    else
      let pageseqs_under = ilist 1 (Pdfpage.endpage under) in
      let pageseqs_over = ilist 1 (Pdfpage.endpage over) in
      let merged =
        Pdfmerge.merge_pdfs
          false false ["a"; "b"] [under; over] [pageseqs_under; pageseqs_over]
      in
        debug_pdf merged "merged.pdf";
        let under_pages, over_pages =
          cleave (Pdfpage.pages_of_pagetree merged) under_length
        in
          let new_pages =
            map2
              (fun o u ->
                 do_stamp
                   false fast (BottomLeft 0.) false false scaletofit (not swap) merged o u over)
              over_pages under_pages
          in
            (* Build the changes. 123456 -> 123123 *)
            let changes =
              let len = length new_pages in
                combine (ilist 1 (len * 2)) (let x = ilist 1 len in x @ x)
            in
              let changed = Pdfpage.change_pages ~changes true merged new_pages in
                let r = Pdfmarks.add_bookmarks (marks_under @ marks_over) changed in
                   debug_pdf r "final.pdf";
                   r

(* \section{Set media box} *)
let set_mediabox xywhlist pdf range =
  let crop_page pnum page =
    let x, y, w, h = List.nth xywhlist (pnum - 1) in
    {page with
       Pdfpage.mediabox =
        (Pdf.Array
           [Pdf.Real x; Pdf.Real y;
            Pdf.Real (x +.  w); Pdf.Real (y +. h)])}
  in
    Cpdfpage.process_pages (ppstub crop_page) pdf range

(* Just used by cpdflib for historical reasons *)
let setBox box minx maxx miny maxy pdf range =
  let set_box_page _ page =
    {page with
       Pdfpage.rest =
         Pdf.add_dict_entry
           page.Pdfpage.rest box
           (Pdf.Array [Pdf.Real minx; Pdf.Real miny; Pdf.Real maxx; Pdf.Real maxy])}
  in
    Cpdfpage.process_pages (ppstub set_box_page) pdf range

(* \section{Cropping} *)
let crop_pdf ?(box="/CropBox") xywhlist pdf range =
  let crop_page pagenum page =
    {page with
       Pdfpage.rest =
         (Pdf.add_dict_entry
            page.Pdfpage.rest
            box
            (let x, y, w, h = List.nth xywhlist (pagenum - 1) in
              (Pdf.Array
                 [Pdf.Real x; Pdf.Real y;
                  Pdf.Real (x +.  w); Pdf.Real (y +. h)])))}
  in
    Cpdfpage.process_pages (ppstub crop_page) pdf range

(* Clip a page to one of its boxes, or the media box if that box is not
 * present. This is a hard clip, done by using a clipping rectangle, so that
 * the page may then be used as a stamp without extraneous material reapearing.
 * *)
let hard_box pdf range boxname mediabox_if_missing fast =
  Cpdfpage.process_pages
    (ppstub (fun pagenum page ->
       let minx, miny, maxx, maxy =
         if boxname = "/MediaBox" then
           Pdf.parse_rectangle page.Pdfpage.mediabox
         else
           match Pdf.lookup_direct pdf boxname page.Pdfpage.rest with
           | Some a -> Pdf.parse_rectangle a
           | _ ->
               if mediabox_if_missing
                 then Pdf.parse_rectangle page.Pdfpage.mediabox
                 else error (Printf.sprintf "hard_box: box %s not found" boxname)
       in
         let ops = [Pdfops.Op_re (minx, miny, maxx -. minx, maxy -. miny); Pdfops.Op_W; Pdfops.Op_n] in
           Pdfpage.prepend_operators pdf ops ~fast page))
    pdf
    range

let remove_cropping_pdf pdf range =
  let remove_cropping_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/CropBox")}
  in
    Cpdfpage.process_pages (ppstub remove_cropping_page) pdf range

let remove_trim_pdf pdf range =
  let remove_trim_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/TrimBox")}
  in
    Cpdfpage.process_pages (ppstub remove_trim_page) pdf range

let remove_art_pdf pdf range =
  let remove_art_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/ArtBox")}
  in
    Cpdfpage.process_pages (ppstub remove_art_page) pdf range

let remove_bleed_pdf pdf range =
  let remove_bleed_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/BleedBox")}
  in
    Cpdfpage.process_pages (ppstub remove_bleed_page) pdf range

(* \section{Rotating pages} *)
let rotate_pdf r pdf range =
  let rotate_page _ page =
    {page with Pdfpage.rotate =
       Pdfpage.rotation_of_int r}
  in
    Cpdfpage.process_pages (ppstub rotate_page) pdf range

let rotate_pdf_by r pdf range =
  let rotate_page_by _ page =
    {page with Pdfpage.rotate =
       Pdfpage.rotation_of_int ((Pdfpage.int_of_rotation page.Pdfpage.rotate + r) mod 360)}
  in
    Cpdfpage.process_pages (ppstub rotate_page_by) pdf range

let rotate_page_contents ~fast rotpoint r pdf pnum page =
  let rotation_point =
    match rotpoint with
    | None ->
        let minx, miny, maxx, maxy = Pdf.parse_rectangle page.Pdfpage.mediabox in
          (minx +. maxx) /. 2.,  (miny +. maxy) /. 2.
    | Some point -> point
  in
    let tr =
      Pdftransform.matrix_of_op
        (Pdftransform.Rotate (rotation_point, -.(rad_of_deg r)))
    in let tr2 =
      Pdftransform.matrix_of_op
        (Pdftransform.Rotate (rotation_point, rad_of_deg r))
    in    
      let transform_op = Pdfops.Op_cm tr in
      let page = change_pattern_matrices_page pdf tr2 page in
        transform_annotations pdf tr page.Pdfpage.rest;
        (Pdfpage.prepend_operators pdf [transform_op] ~fast page, pnum, tr)

let rotate_contents ?(fast=false) r pdf range =
  Cpdfpage.process_pages (rotate_page_contents ~fast None r pdf) pdf range

(* Return the pages from the pdf in the range, unordered. *)
let select_pages range pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    option_map (function n -> try Some (select n pages) with _ -> None) range


(* Upright functionality *)

(* If all pages are already upright, and the mediabox is (0,0)-based, do nothing
to save time. *)
let allupright range pdf =
  let page_is_upright page =
    page.Pdfpage.rotate = Pdfpage.Rotate0 &&
      (let (minx, miny, _, _) = Pdf.parse_rectangle page.Pdfpage.mediabox in
         minx < 0.001 && miny < 0.001 && minx > ~-.0.001 && miny > ~-.0.001)
  in
    not (mem false (map page_is_upright (select_pages range pdf)))

let upright_transform page =
  let rotate =
    Pdfpage.int_of_rotation page.Pdfpage.rotate
  and cx, cy =
    let minx, miny, maxx, maxy = Pdf.parse_rectangle page.Pdfpage.mediabox in
      (minx +. maxx) /. 2., (miny +. maxy) /. 2.
  in
    Pdftransform.mkrotate (cx, cy) (rad_of_deg (~-.(float rotate)))

let transform_boxes tr pdf page =
  let f (minx, miny, maxx, maxy) =
    let minx, miny = Pdftransform.transform_matrix tr (minx, miny)
    and maxx, maxy = Pdftransform.transform_matrix tr (maxx, maxy) in
      (minx, miny, maxx, maxy)
  in
    change_boxes f pdf page

let transform_contents ?(fast=false) tr pdf page =
  let transform_op = Pdfops.Op_cm tr in
    let page = change_pattern_matrices_page pdf (Pdftransform.matrix_invert tr) page in
      transform_annotations pdf tr page.Pdfpage.rest;
      Pdfpage.prepend_operators pdf [transform_op] ~fast page

let upright ?(fast=false) range pdf =
  if allupright range pdf then pdf else
    let upright_page _ pnum page =
      let tr = upright_transform page in
        let page = transform_boxes tr pdf page in
          let page = transform_contents ~fast tr pdf page in
            (rectify_boxes ~fast pdf {page with Pdfpage.rotate = Pdfpage.Rotate0}, pnum, tr)
    in
      Cpdfpage.process_pages (upright_page pdf) pdf range

(* \section{Scale page data} *)
let scale_pdf ?(fast=false) sxsylist pdf range =
  let scale_page pnum page =
    let sx, sy = List.nth sxsylist (pnum - 1) in
      let f (xmin, ymin, xmax, ymax) =
        xmin *. sx, ymin *. sy, xmax *. sx, ymax *. sy
      in
        let page = change_boxes f pdf page
        and matrix = Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), sx, sy)) in
          let transform_op =
            Pdfops.Op_cm matrix
          and page =
            change_pattern_matrices_page pdf (Pdftransform.matrix_invert matrix) page
          in
           transform_annotations pdf matrix page.Pdfpage.rest;
           (Pdfpage.prepend_operators pdf ~fast [transform_op] page, pnum, matrix)
      in
        Cpdfpage.process_pages scale_page pdf range

(* Scale to fit page of size x * y *)
let scale_to_fit_pdf ?(fast=false) position input_scale xylist op pdf range =
  let scale_page_to_fit pnum page =
    let x, y = List.nth xylist (pnum - 1) in
    let matrix =
      let (minx, miny, maxx, maxy) =
        (* Use cropbox if available *)
        Pdf.parse_rectangle
          (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
          | Some r -> r
          | None -> page.Pdfpage.mediabox)
      in
        if maxx <= 0. || maxy <= 0. then failwith "Zero-sized pages are invalid" else
          let fx = x /. maxx in let fy = y /. maxy in
            let scale = fmin fx fy *. input_scale in
              let trans_x =
                match position with
                  Cpdfposition.Left _ -> 0.
                | Cpdfposition.Right _ -> (x -. (maxx *. scale))
                | _ -> (x -. (maxx *. scale)) /. 2.
              and trans_y =
                match position with
                | Cpdfposition.Top _ -> (y -. (maxy *. scale))
                | Cpdfposition.Bottom _ -> 0.
                | _ -> (y -. (maxy *. scale)) /. 2.
              in
                (Pdftransform.matrix_of_transform
                   [Pdftransform.Translate (trans_x, trans_y);
                    Pdftransform.Scale ((0., 0.), scale, scale)])
    in
      let page =
        change_boxes
          (function (minx, miny, maxx, maxy) -> 0., 0., x, y)
          pdf page
      in
        transform_annotations pdf matrix page.Pdfpage.rest;
        (Pdfpage.prepend_operators pdf [Pdfops.Op_cm matrix] ~fast
         (change_pattern_matrices_page pdf (Pdftransform.matrix_invert matrix) page), pnum, matrix)
  in
    Cpdfpage.process_pages scale_page_to_fit pdf range

(* Scale contents *)
let scale_page_contents ?(fast=false) scale position pdf pnum page =
  let (minx, miny, maxx, maxy) as box =
    (* Use cropbox if available *)
    Pdf.parse_rectangle
      (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
       | Some r -> r
       | None -> page.Pdfpage.mediabox)
  in
    let sx, sy, _ = Cpdfposition.calculate_position true 0. box Horizontal position in
      let tx, ty =
        let open Cpdfposition in
        match position with
        | Top t -> 0., -.t
        | TopLeft t -> t, -.t
        | TopRight t -> -.t, -.t
        | Left t -> t, 0.
        | BottomLeft t -> t, t
        | Bottom t -> 0., t
        | BottomRight t -> -.t, t
        | Right t -> -.t, 0.
        | _ -> 0., 0. (* centre it... FIXME: We will add a center position, eventually, for text and this... *)
    in
      let transform =
        Pdftransform.matrix_of_transform
          [Pdftransform.Translate (tx, ty);
           Pdftransform.Scale ((sx, sy), scale, scale)]
      in
        let transform_op = Pdfops.Op_cm transform in
          let page = change_pattern_matrices_page pdf transform page in
          transform_annotations pdf transform page.Pdfpage.rest;
          (Pdfpage.prepend_operators pdf [transform_op] ~fast page, pnum, transform)

let scale_contents ?(fast=false) position scale pdf range =
  Cpdfpage.process_pages (scale_page_contents ~fast scale position pdf) pdf range

(* Imposition *)

(* Union two rest dictionaries from the same PDF. *)
let combine_pdf_rests pdf a b =
  let a_entries =
    match a with
    | Pdf.Dictionary entries -> entries
    | _ -> []
  in let b_entries =
    match b with
    | Pdf.Dictionary entries -> entries
    | _ -> []
  in
    let keys_to_combine = ["/Annots"] in
      let combine_entries key =
        let a_entries =
          match Pdf.lookup_direct pdf key a with
          | Some (Pdf.Array d) -> d
          | _ -> []
        in let b_entries =
          match Pdf.lookup_direct pdf key b with
          | Some (Pdf.Array d) -> d
          | _ -> []
        in
          if a_entries = [] && b_entries = [] then
            None
          else
            Some (key, Pdf.Array (a_entries @ b_entries))
      in
        let unknown_keys_a = lose (fun (k, _) -> mem k keys_to_combine) a_entries in
        let unknown_keys_b = lose (fun (k, _) -> mem k keys_to_combine) b_entries in
        let combined_known_entries = option_map combine_entries keys_to_combine in
          fold_left
            (fun dict (k, v) -> Pdf.add_dict_entry dict k v)
            (Pdf.Dictionary [])
            (unknown_keys_a @ unknown_keys_b @ combined_known_entries)

(* Calculate the transformation matrices for a single imposed output page. *)

(* make margins by scaling for a fitted impose. *)
let make_margin output_mediabox margin tr =
  if margin = 0. then tr else
    let width, height =
      match Pdf.parse_rectangle output_mediabox with
        xmin, ymin, xmax, ymax -> xmax -. xmin, ymax -. ymin
    in
    if margin > width /. 2. || margin > height /. 2. then error "margin would fill whole page!" else
      let hfactor = (width -. margin -. margin) /. width in
      let vfactor = (height -. margin -. margin) /. height in
      let factor = fmin hfactor vfactor in
      let scale = Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), factor, factor)) in
      let shift =
        Pdftransform.matrix_of_op (Pdftransform.Translate ((width -. width *. factor) /. 2.,
                                                           (height -. height *. factor) /. 2.))
      in
        (Pdftransform.matrix_compose shift (Pdftransform.matrix_compose scale tr))

(* FIXME fixup -center for next release. For now it has been disabled. *)
let impose_transforms fit fx fy columns rtl btt center margin mediabox output_mediabox fit_extra_hspace fit_extra_vspace len =
  let width, height =
    match Pdf.parse_rectangle mediabox with
      xmin, ymin, xmax, ymax -> xmax -. xmin, ymax -. ymin
  in
  let trs = ref [] in
  let len = ref len in
  let cent_extra_x = ref 0. in
  let cent_extra_y = ref 0. in
  let addtr x y row col px py =
    let cex, cey =
      (if rtl then ~-.(!cent_extra_x) else !cent_extra_x), (if btt then ~-.(!cent_extra_y) else !cent_extra_y)
    in
      let spacecol = if rtl then x - col - 1 else col in
      let total_fit_extra_hspace = fit_extra_hspace *. (float_of_int spacecol +. 1.) in
      let total_fit_extra_vspace = fit_extra_vspace *. (float_of_int row +. 1.) in
      (*Printf.printf "row = %i, py = %f, ey = %f, fit_extra_vspace = %f, total_fit_extra_vspace = %f\n" row py cey fit_extra_vspace total_fit_extra_vspace;*)
      trs :=
        Pdftransform.matrix_of_transform
          [Pdftransform.Translate (px +. cex +. total_fit_extra_hspace, py +. cey +. total_fit_extra_vspace)]
        ::!trs
  in
  let x = int_of_float fx in
  let y = int_of_float fy in
  let final_full_cols = !len mod x in
  let final_full_rows = !len mod y in
  let order row col =
    ((if btt then y - row - 1 else row), (if rtl then x - col - 1 else col))
  in
  if columns then
    for col = 0 to x - 1 do
      if center && !len < y then if !cent_extra_y = 0. then cent_extra_y := ~-.(height *. float_of_int (y - !len)) /. 2.;
      for row = y - 1 downto 0 do
        let original_row = row in
        let row, col = order row col in
         let adjusted_row =
           let final_empty_rows = y - final_full_rows in
             if center && !len <= final_full_rows then original_row + (y - 1 - 1 - (final_empty_rows / 2)) else original_row
         in
           if !len > 0 then addtr x y adjusted_row col (width *. float_of_int col) (height *. float_of_int row);
           len := !len - 1
      done
    done
  else
    for row = y - 1 downto 0 do
      if center && !len < x then if !cent_extra_x = 0. then cent_extra_x := (width *. float_of_int (x - !len)) /. 2.;
      for col = 0 to x - 1 do
        let original_col = col in
        let row, col = order row col in
          let adjusted_col =
            let final_empty_cols = x - final_full_cols in
              if center && !len <= final_full_cols then original_col + (x - 1 - 1 - (final_empty_cols / 2)) else original_col
          in
            if !len > 0 then addtr x y row adjusted_col (width *. float_of_int col) (height *. float_of_int row);
            len := !len - 1
      done
    done;
  map (if fit then make_margin output_mediabox margin else Fun.id) (rev !trs)

(* Combine two pages into one throughout the document. The pages have already
had their objects renumbered so as not to clash. *)
let impose_pages fit x y columns rtl btt center margin output_mediabox fast fit_extra_hspace fit_extra_vspace pdf = function
  | [] -> assert false
  | (h::_) as pages ->
     let transforms = 
       impose_transforms
         fit x y columns rtl btt center margin h.Pdfpage.mediabox
         output_mediabox fit_extra_hspace fit_extra_vspace (length pages)
     in
       (* Change the pattern matrices before combining resources *)
       let pages, h =
         let r = map2 (fun p t -> change_pattern_matrices_page pdf t p) pages transforms in
           (r, List.hd r)
       in
     let resources' = pair_reduce (combine_pdf_resources pdf) (map (fun p -> p.Pdfpage.resources) pages) in
     let rest' = pair_reduce (combine_pdf_rests pdf) (map (fun p -> p.Pdfpage.rest) pages) in
       let content' =
          let transform_stream transform contents =
            (* If fast, no mismatched q/Q protection and no parsing of operators. *)
            if fast then
              [Pdfops.stream_of_ops [Pdfops.Op_q; Pdfops.Op_cm transform]] @ contents @ [Pdfops.stream_of_ops [Pdfops.Op_Q]]
            else
            (* If slow, use protect from Pdfpage. *)
            let ops = Pdfpage.protect pdf resources' contents @ Pdfops.parse_operators pdf resources' contents in
              [Pdfops.stream_of_ops ([Pdfops.Op_q] @ [Pdfops.Op_cm transform] @ ops @ [Pdfops.Op_Q])]
          in
            flatten
              (map2
                (fun p t -> transform_annotations pdf t p.Pdfpage.rest; transform_stream t p.Pdfpage.content)
                pages
                transforms)
       in
         {Pdfpage.mediabox = output_mediabox;
          Pdfpage.rotate = h.Pdfpage.rotate;
          Pdfpage.content = content';
          Pdfpage.resources = resources';
          Pdfpage.rest = rest'}

(* For fit, we scale contents, move to middle and retain page size. For xy, we
   expand mediabox and move contents to middle. This function also does the hard boxing. *)
let make_space fit ~fast spacing pdf =
  let endpage = Pdfpage.endpage pdf in
  let all = ilist 1 endpage in
  let pdf = hard_box pdf all "/MediaBox" false fast in
  if spacing = 0. then pdf else
  let margin = spacing /. 2. in
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let width, height =
    match Pdf.parse_rectangle firstpage.Pdfpage.mediabox with
      xmin, ymin, xmax, ymax -> (xmax -. xmin, ymax -. ymin)
  in
  if fit then
    (shift_pdf
      ~fast
      (many (margin, margin) endpage)
      (scale_contents ~fast (Cpdfposition.BottomLeft 0.) ((width -. spacing) /. width) pdf all)
      all)
  else
    (set_mediabox
      (many (0., 0., width +. spacing, height +. spacing) endpage)
      (shift_pdf ~fast (many (margin, margin) endpage) pdf all) all)

(* We add the border as a thick unfilled rectangle just inside the page edge,
   only if its linewidth is > 0 since, for us, 0 means none, not single-pixel
   like in PDF. *)
let add_border linewidth ~fast pdf =
  if linewidth = 0. then pdf else
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let _, _, w, h = Pdf.parse_rectangle firstpage.Pdfpage.mediabox in
    Cpdfaddtext.addrectangle
      fast (w -. linewidth, h -. linewidth) (RGB (0., 0., 0.)) true linewidth 1. (Cpdfposition.BottomLeft (linewidth /. 2.))
      false false (ilist 1 (Pdfpage.endpage pdf)) pdf

let impose ~x ~y ~fit ~columns ~rtl ~btt ~center ~margin ~spacing ~linewidth ~fast pdf =
  let endpage = Pdfpage.endpage pdf in
  let pagenums = ilist 1 endpage in
  let pdf = copy_cropbox_to_mediabox pdf pagenums in
  let pdf = remove_cropping_pdf pdf pagenums in
  let pdf = upright pagenums pdf in
  let pdf = add_border linewidth ~fast pdf in
  let pdf = make_space fit ~fast spacing pdf in 
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let _, _, w, h = Pdf.parse_rectangle firstpage.Pdfpage.mediabox in
  let ix = int_of_float x in
  let iy = int_of_float y in
  let n, ix, iy, fit_extra_hspace, fit_extra_vspace =
      if fit then
        (* +. 0.001 ensures a page always fits on itself, or on another page of same height or width. *)
        let across = int_of_float (floor (x /. w +. 0.001)) in
        let down = int_of_float (floor (y /. h +. 0.001)) in
          if across < 1 || down < 1 then error "Not even a single page would fit." else
          let excess_hspace = x -. float_of_int across *. w in
          let excess_vspace = y -. float_of_int down *. h in
            (*Printf.printf "across = %i, down =%i, excess_hspace = %f, excess_hspace = %f\n" across down excess_hspace excess_vspace;*)
            (across * down,
             across,
             down,
             excess_hspace /. (float_of_int across +. 1.),
             excess_vspace /. (float_of_int down +. 1.))
      else
        if ix = 0 && iy = 0 then error "impose-xy: both dimensions cannot be zero" else
        if ix = 0 then (endpage, endpage, 1, 0., 0.)
        else if iy = 0 then (endpage, 1, endpage, 0., 0.)
        else (ix * iy, ix, iy, 0., 0.)
  in
  let mediabox' =
    if fit then Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real x; Pdf.Real y] else
      let m2 = margin *. 2. in
      if x = 0.0 then Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real (w *. float_of_int endpage +. m2); Pdf.Real (h +. m2)]
      else if y = 0.0 then Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real (w +. m2); Pdf.Real (h *. float_of_int endpage +. m2)]
      else Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real (w *. x +. m2); Pdf.Real (h *. y +. m2)]
  in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pagesets = splitinto n pages in
  let renumbered = map (Pdfpage.renumber_pages pdf) pagesets in
  let pages =
     map
       (impose_pages fit (float_of_int ix) (float_of_int iy) columns rtl btt
        center margin mediabox' fast fit_extra_hspace fit_extra_vspace pdf)
       renumbered
  in
  let changes = map (fun x -> (x, (x + (n - 1)) / n)) pagenums in
  let pdf = Pdfpage.change_pages ~changes true pdf pages in
  if fit then pdf else shift_pdf ~fast (many (margin, margin) (length pages)) pdf (ilist 1 (Pdfpage.endpage pdf))

(* Legacy -twoup-stack. Impose 2x1 on a page twice the size then rotate. *)
let twoup_stack fast pdf =
  let pdf =
    impose
      ~x:2. ~y:1. ~fit:false ~columns:false ~rtl:false ~btt:false ~center:false
      ~margin:0. ~spacing:0. ~linewidth:0. ~fast pdf
  in
   let all = ilist 1 (Pdfpage.endpage pdf) in
    upright ~fast all (rotate_pdf ~-90 pdf all)

(* Legacy -two-up. Rotate the pages and shrink them so as to fit 2x1 on a page the same size. *)
let twoup fast pdf =
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let width, height =
    match Pdf.parse_rectangle firstpage.Pdfpage.mediabox with
      xmin, ymin, xmax, ymax -> xmax -. xmin, ymax -. ymin
  in
    let width_exceeds_height = width > height in
      let sc =
        if width_exceeds_height
          then fmin (height /. width) ((width /. 2.) /. height)
          else fmin (width /. height) ((height /. 2.) /. width)
      in
        let endpage = Pdfpage.endpage pdf in
        let all = ilist 1 endpage in
        let pdf = scale_pdf ~fast (many (sc, sc) endpage) pdf all in
        let pdf =
          impose
            ~x:2. ~y:1. ~fit:false ~columns:false ~rtl:false ~btt:false ~center:true
            ~margin:0. ~spacing:0. ~linewidth:0. ~fast pdf
        in
        let endpage = Pdfpage.endpage pdf in
        let all = ilist 1 endpage in
        let pdf = upright all (rotate_pdf ~-90 pdf all) in
          scale_to_fit_pdf ~fast Cpdfposition.Diagonal 1. (many (width, height) endpage) () pdf all

(* \section{Blacken text} *)

(*
   \begin{verbatim}
    Algorithm: Change
     BT
     <ops>
    ET
 
    ...to...
 
    BT
    Op_g 0.
    <ops minus any color, shading or gs operators>
    ET
    <ops minus any text positioning or text rendering ones>
    \end{verbatim}
*)
let blacktext_ops colour pdf resources content =
  let not_text = function
    | Pdfops.Op_Tj _ | Pdfops.Op_TJ _
    | Pdfops.Op_' _ | Pdfops.Op_'' (_, _, _)
    | Pdfops.Op_Td (_, _) | Pdfops.Op_TD (_, _)
    | Pdfops.Op_Tm _ | Pdfops.Op_T'
    | Pdfops.Op_Tc _
    | Pdfops.Op_Tw _
    | Pdfops.Op_Tz _
    | Pdfops.Op_TL _
    | Pdfops.Op_Tf (_, _)
    | Pdfops.Op_Tr _
    | Pdfops.Op_Ts _ -> false
    | _ -> true
  in let textlevel = ref 0
  in let removed = ref []
  in let operators =
    Pdfops.parse_operators pdf resources content
  in
    let rec remove_colourops prev = function
      | [] -> rev prev
      | Pdfops.Op_BT::more ->
          incr textlevel;
          remove_colourops
            (Cpdfaddtext.colour_op colour::Pdfops.Op_BT::prev)
            more
      | Pdfops.Op_ET::more ->
          decr textlevel;
          let prev' = !removed @ Pdfops.Op_ET::prev in
            removed := [];
            remove_colourops prev' more
      | (Pdfops.Op_G _
         | Pdfops.Op_g _ 
         | Pdfops.Op_RG (_, _, _)
         | Pdfops.Op_rg (_, _, _)
         | Pdfops.Op_k (_, _, _, _)
         | Pdfops.Op_K (_, _, _, _)
         | Pdfops.Op_SCN _
         | Pdfops.Op_SC _
         | Pdfops.Op_scn _
         | Pdfops.Op_sc _
         | Pdfops.Op_SCNName (_, _)
         | Pdfops.Op_scnName (_, _)
         | Pdfops.Op_CS _
         | Pdfops.Op_cs _
         | Pdfops.Op_sh _
         | Pdfops.Op_gs _)
        as op::more ->
          if !textlevel > 0
            then
              begin
                removed =| op;
                remove_colourops prev more
              end
            else remove_colourops (op::prev) more
      | op::more ->
          if !textlevel > 0 && not_text op then removed =| op;
          remove_colourops (op::prev) more
    in
      let operators' = remove_colourops [] operators in
        [Pdfops.stream_of_ops operators']

(* Blacken a form xobject, writing it to the same object. *)

let blacktext c range pdf =
  let blacktext_page _ page =
    let content' =
      blacktext_ops c pdf page.Pdfpage.resources page.Pdfpage.content
    in
      process_xobjects pdf page (blacktext_ops c);
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (ppstub blacktext_page) pdf range

(* \section{Blacken lines} *)
let blacklines_ops c pdf resources content =
  let rec blacken_strokeops prev = function
    | [] -> rev prev
    | Pdfops.Op_CS _::t ->
        blacken_strokeops (Pdfops.Op_CS "/DeviceRGB"::prev) t
    | (Pdfops.Op_SC _ | Pdfops.Op_SCN _ | Pdfops.Op_SCNName _ | Pdfops.Op_G _
       | Pdfops.Op_RG _ | Pdfops.Op_K _)::t ->
           blacken_strokeops (Cpdfaddtext.colour_op_stroke c::prev) t
    | h::t -> blacken_strokeops (h::prev) t
  and operators =
    Pdfops.parse_operators pdf resources content
  in
    let operators' = blacken_strokeops [] operators in
      [Pdfops.stream_of_ops operators']

let blacklines c range pdf =
  let blacklines_page _ page =
    let content' =
      blacklines_ops c pdf page.Pdfpage.resources page.Pdfpage.content
    in
      process_xobjects pdf page (blacklines_ops c);
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (ppstub blacklines_page) pdf range

(* \section{Blacken Fills} *)
let blackfills_ops c pdf resources content =
  let rec blacken_fillops prev = function
    | [] -> rev prev
    | Pdfops.Op_cs _::t ->
        blacken_fillops (Pdfops.Op_cs "/DeviceRGB"::prev) t
    | (Pdfops.Op_sc _ | Pdfops.Op_scn _ | Pdfops.Op_scnName _ | Pdfops.Op_g _
       | Pdfops.Op_rg _ | Pdfops.Op_k _)::t ->
           blacken_fillops (Cpdfaddtext.colour_op c::prev) t
    | h::t -> blacken_fillops (h::prev) t
  and operators =
    Pdfops.parse_operators pdf resources content
  in
    let operators' = blacken_fillops [] operators in
      [Pdfops.stream_of_ops operators']

let blackfills c range pdf =
  let blackfills_page _ page =
    let content' =
      blackfills_ops c pdf page.Pdfpage.resources page.Pdfpage.content
    in
      process_xobjects pdf page (blackfills_ops c);
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (ppstub blackfills_page) pdf range

(* \section{Set a minimum line width to avoid dropout} *)
let thinlines range width pdf =
  let thinpage _ page =
    let operators =
      Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content
    in
      let ctmstack = ref [ref Pdftransform.i_matrix] in
        let scaleof_ctm () =
          try
            match Pdftransform.decompose (!(hd !ctmstack)) with
              (scale, _, _, _, _, _) ->
                 scale
          with
            Failure _ (*"hd"*) -> 1.
        in
          let rec replace_operators prev = function
            | [] -> rev prev
            | (Pdfops.Op_w w)::more ->
              (* Alter width. *)
              let width' = width /. scaleof_ctm () in
                let w' =
                  if w >= width' then Pdfops.Op_w w else Pdfops.Op_w width'
                in
                  replace_operators (w'::prev) more
            | (Pdfops.Op_cm m)::more ->
                (* Update CTM *)
                begin try 
                  let top = hd !ctmstack in
                    top := Pdftransform.matrix_compose !top m
                with
                  Failure _ (*"hd"*) -> error "Malformed file."
                end;
                replace_operators ((Pdfops.Op_cm m)::prev) more
            | Pdfops.Op_q::more ->
                (* Push stack *)
                begin try
                  ctmstack =| ref (!(hd !ctmstack))
                with
                  Failure _ (*"hd"*) -> error "Malformed file"
                end;
                replace_operators (Pdfops.Op_q::prev) more
            | Pdfops.Op_Q::more ->
                (* Pop stack *)
                begin try
                  ctmstack := tl !ctmstack
                with
                  Failure _ (*"tl"*) -> error "Malformed file"
                end;
                replace_operators (Pdfops.Op_Q::prev) more
            | (Pdfops.Op_gs gsname)::more ->
                (* Perhaps insert [Op_w]. *)
                let opw =
                  match Pdf.lookup_direct pdf "/ExtGState" page.Pdfpage.resources with
                  | None -> []
                  | Some ext_state_dict ->
                      match Pdf.lookup_direct pdf gsname ext_state_dict with
                      | None -> []
                      | Some gdict ->
                          match Pdf.lookup_direct pdf "/LW" gdict with
                          | Some s -> (try [Pdfops.Op_w (Pdf.getnum s)] with _ -> [])
                          | None -> []
                in
                  replace_operators (opw @ ((Pdfops.Op_gs gsname)::prev)) more
            | x::more -> replace_operators (x::prev) more
          in
            let operators = replace_operators [] operators in
              (* 2. Add an initial 'w' if width more than default width *)
              let operators =
                if width > 1. then (Pdfops.Op_w width)::operators else operators
              in
                let content' = [Pdfops.stream_of_ops operators] in
                  {page with Pdfpage.content = content'} 
  in
    Cpdfpage.process_pages (ppstub thinpage) pdf range

(* Parse the new content to make sure syntactically ok, append
 * as required. Rewrite the content *)
let append_page_content_page fast s before pdf n page =
  let ops =
    Pdfops.parse_stream pdf page.Pdfpage.resources [bytes_of_string s] 
  in
    (if before then Pdfpage.prepend_operators else Pdfpage.postpend_operators)
    pdf ops ~fast page

let append_page_content s before fast range pdf =
  Cpdfpage.process_pages (ppstub (append_page_content_page fast s before pdf)) pdf range

(* Add rectangles on top of pages to show Media, Crop, Art, Trim, Bleed boxes.
 *
 * We use different dash lengths and colours to help distinguish coincident
 * boxes The sequence of operators is postpended to the page content,
 * appropriately protected to prevent pollution of matrices.
 *
 * /MediaBox: Solid red line
 * /CropBox: Dashed 7 on 7 off green line
 * /ArtBox: Dashed 5 on 5 off blue line
 * /TrimBox: Dashed 3 on 3 off orange line
 * /BleedBox: Dashed 2 on 2 off pink line *)
let get_rectangle pdf page box =
  if box = "/MediaBox" then
    match page.Pdfpage.mediabox with
      Pdf.Array [a; b; c; d] as r -> Some (Pdf.parse_rectangle r)
    | _ -> None
  else
    match Pdf.lookup_direct pdf box page.Pdfpage.rest with
      Some (Pdf.Array [a; b; c; d] as r) -> Some (Pdf.parse_rectangle r)
    | _ -> None

let show_boxes_page fast pdf _ page =
  let make_ops (r, g, b) on off boxname =
    match get_rectangle pdf page boxname with
      Some (r1, r2, r3, r4) ->
        [Pdfops.Op_q;
         Pdfops.Op_RG (r /. 255., g /. 255., b /. 255.);
         Pdfops.Op_w 1.;
         Pdfops.Op_d ((if on = 0. && off = 0. then [] else [on; off]), 0.);
         Pdfops.Op_re (r1, r2, r3 -. r1, r4 -. r2);
         Pdfops.Op_S;
         Pdfops.Op_Q]
    | None -> []
  in
    let ops =
        make_ops (255., 0., 0.) 0. 0. "/MediaBox"
      @ make_ops (0., 255., 0.) 7. 7. "/CropBox"
      @ make_ops (0., 0., 255.) 5. 5. "/ArtBox"
      @ make_ops (255.,150.,0.) 3. 3. "/TrimBox"
      @ make_ops (255.,9.,147.) 2. 2. "/BleedBox"
    in
      Pdfpage.postpend_operators pdf ops ~fast page

let show_boxes ?(fast=false) pdf range =
  Cpdfpage.process_pages (ppstub (show_boxes_page fast pdf)) pdf range



let allowance = 9.

let line (x0, y0, x1, y1) =
  [Pdfops.Op_m (x0, y0);
   Pdfops.Op_l (x1, y1);
   Pdfops.Op_s]

let trim_marks_page fast pdf n page =
  match get_rectangle pdf page "/TrimBox", get_rectangle pdf page "/MediaBox" with
  | Some (tminx, tminy, tmaxx, tmaxy), Some (minx, miny, maxx, maxy) ->
      let ops =
        [Pdfops.Op_q;
         Pdfops.Op_K (1., 1., 1., 1.);
         Pdfops.Op_w 1.]
         @ line (minx, tmaxy, tminy -. allowance, tmaxy) (* top left *)
         @ line (tminx, tmaxy +. allowance, tminx, maxy)
         @ line (tmaxx +. allowance, tmaxy, maxx, tmaxy) (* top right *)
         @ line (tmaxx, tmaxy +. allowance, tmaxx, maxy)
         @ line (tmaxx +. allowance, tminy, maxx, tminy) (* bottom right *)
         @ line (tmaxx, tminy -. allowance, tmaxx, miny)
         @ line (tminx -. allowance, tminy, minx, tminy) (* bottom left *)
         @ line (tminx, tminy -. allowance, tminx, miny)
         @ [Pdfops.Op_Q]
      in
        Pdfpage.postpend_operators pdf ops ~fast page
  | _, _ ->
      (*Printf.eprintf "warning: no /TrimBox found on page %i\n%!" n;*)
      page

let trim_marks ?(fast=false) pdf range =
  Cpdfpage.process_pages (ppstub (trim_marks_page fast pdf)) pdf range

let rec remove_all_text_ops pdf resources content =
  let is_textop = function
    Pdfops.Op_Tj _ | Pdfops.Op_' _ | Pdfops.Op_'' _ | Pdfops.Op_TJ _ -> true
  | _ -> false
  in
    let content' =
      let ops = Pdfops.parse_operators pdf resources content in
        Pdfops.stream_of_ops
          (option_map (function x -> if is_textop x then None else Some x) ops) 
    in
      [content']

let remove_all_text_page pdf p =
  let resources = p.Pdfpage.resources in
  let content = p.Pdfpage.content in
    process_xobjects pdf p remove_all_text_ops;
    {p with Pdfpage.content = remove_all_text_ops pdf resources content}, pdf

let remove_all_text range pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pagenums = indx pages in
    let pdf = ref pdf in
    let pages' = ref [] in
      iter2 
        (fun p pagenum ->
          let p', pdf' =
            if mem pagenum range
              then remove_all_text_page !pdf p
              else p, !pdf
          in
            pdf := pdf';
            pages' =| p')
        pages
        pagenums;
      Pdfpage.change_pages true !pdf (rev !pages')

(* 1. Extend remove_dict_entry with search term
   2. Implement replace_dict_entry by analogy to remove_dict_entry *)
let rec dict_entry_single_object f pdf = function
  | (Pdf.Dictionary d) -> f (Pdf.recurse_dict (dict_entry_single_object f pdf) d)
  | (Pdf.Stream {contents = (Pdf.Dictionary dict, data)}) ->
      f (Pdf.Stream {contents = (Pdf.recurse_dict (dict_entry_single_object f pdf) dict, data)})
  | Pdf.Array a -> Pdf.recurse_array (dict_entry_single_object f pdf) a
  | x -> x

(* FIXME are we sure that functional values can never appear in the equality here? *)
let remove_dict_entry pdf key search =
  let f d =
    match search with
    | None -> Pdf.remove_dict_entry d key
    | Some s ->
        match Pdf.lookup_direct pdf key d with
        | Some v when v = s -> Pdf.remove_dict_entry d key
        | _ -> d
  in
    Pdf.objselfmap (dict_entry_single_object f pdf) pdf;
    pdf.Pdf.trailerdict <- dict_entry_single_object f pdf pdf.Pdf.trailerdict

let replace_dict_entry pdf key value search =
  let f d =
    match search with
    | None -> Pdf.replace_dict_entry d key value
    | Some s ->
        match Pdf.lookup_direct pdf key d with
        | Some v when v = s -> Pdf.replace_dict_entry d key value
        | _ -> d
  in
    Pdf.objselfmap (dict_entry_single_object f pdf) pdf;
    pdf.Pdf.trailerdict <- dict_entry_single_object f pdf pdf.Pdf.trailerdict

(* FIXME no need to self map here, since nothing changes *)
let print_dict_entry pdf key =
  let f d =
    match Pdf.lookup_direct pdf key d with
    | Some v ->
        (* We use a double newline as a separator. *)
        Printf.printf "%s\n\n" (Cpdfyojson.Safe.to_string (Cpdfjson.json_of_object pdf (fun _ -> ()) false false v));
        d
    | None -> d
  in
    Pdf.objselfmap (dict_entry_single_object f pdf) pdf;
    pdf.Pdf.trailerdict <- dict_entry_single_object f pdf pdf.Pdf.trailerdict

let remove_clipping_ops pdf resources content =
  let ops = Pdfops.parse_operators pdf resources content in
    let rec process a = function
      Pdfops.Op_W::Pdfops.Op_n::t -> process (Pdfops.Op_n::a) t
    | h::t -> process (h::a) t
    | [] -> rev a
    in
      [Pdfops.stream_of_ops (process [] ops)] 

let remove_clipping pdf range =
  let remove_clipping_page _ page =
    let content' =
      remove_clipping_ops pdf page.Pdfpage.resources page.Pdfpage.content
    in
      process_xobjects pdf page remove_clipping_ops;
      {page with Pdfpage.content = content'}
  in
    Cpdfpage.process_pages (ppstub remove_clipping_page) pdf range
 
(* copy the contents of the box f to the box t. If mediabox_if_missing is set,
the contents of the mediabox will be used if the from fox is not available. If
mediabox_is_missing is false, the page is unaltered. *)
let copy_box f t mediabox_if_missing pdf range =
  Cpdfpage.process_pages
    (ppstub (fun _ page ->
       if f = "/MediaBox" then
         {page with Pdfpage.rest =
            (Pdf.add_dict_entry page.Pdfpage.rest t (page.Pdfpage.mediabox))}
       else
         match Pdf.lookup_direct pdf f page.Pdfpage.rest with
         | Some pdfobject ->
             if t = "/MediaBox"
               then {page with
                       Pdfpage.mediabox = Pdf.direct pdf pdfobject}
               else {page with Pdfpage.rest =
                       (Pdf.add_dict_entry page.Pdfpage.rest t (Pdf.direct pdf pdfobject))}
         | None ->
             if mediabox_if_missing
               then {page with Pdfpage.rest = Pdf.add_dict_entry page.Pdfpage.rest t page.Pdfpage.mediabox}
               else page))
    pdf
    range

let remove_unused_resources_page pdf n page =
  let xobjects, all_names =
    match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
    | Some (Pdf.Dictionary d) -> Pdf.Dictionary d, map fst d
    | _ -> Pdf.Dictionary [], []
  in
    let names_to_keep =
      option_map
        (function Pdfops.Op_Do n -> Some n | _ -> None)
        (Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content)
    in
      let names_to_remove = lose (mem' names_to_keep) all_names in
        let xobjdict = fold_left (Pdf.remove_dict_entry) xobjects names_to_remove in
          {page with Pdfpage.resources = Pdf.add_dict_entry page.Pdfpage.resources  "/XObject" xobjdict}

let remove_unused_resources pdf =
  Cpdfpage.process_pages (ppstub (remove_unused_resources_page pdf)) pdf (ilist 1 (Pdfpage.endpage pdf))
