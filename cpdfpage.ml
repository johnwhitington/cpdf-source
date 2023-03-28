open Pdfutil
open Cpdferror

(* When we transfor a page by wrapping in an [Op_cm], we must also
invert any /Matrix entries in pattern dictionaries, including inside xobjects *)
let rec change_pattern_matrices_resources pdf tr resources =
  try
    begin match Pdf.lookup_direct pdf "/XObject" resources with
    | Some (Pdf.Dictionary elts) ->
        iter
          (fun (k, v) -> 
             match v with
             | Pdf.Indirect i -> change_pattern_matrices_xobject pdf tr k v i
             | _ -> raise (Pdf.PDFError "change_pattern_matrices_page"))
          elts
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/Pattern" resources with
    | Some (Pdf.Dictionary patterns) ->
        let entries =
          map
            (fun (name, p) ->
              Printf.printf "Changing matrices of pattern %s\n" name;
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

and change_pattern_matrices_xobject pdf tr k v i =
  match Pdf.lookup_direct pdf "/Subtype" v with
  | Some (Pdf.Name "/Form") ->
      Printf.printf "Processing form xobject %s for patterns\n" k;
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

let change_pattern_matrices_page pdf tr page =
  let page =
    {page with Pdfpage.resources = change_pattern_matrices_resources pdf tr page.Pdfpage.resources}
  in
    match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
    | Some (Pdf.Dictionary elts) ->
        iter
          (fun (k, v) -> 
             match v with
             | Pdf.Indirect i -> change_pattern_matrices_xobject pdf tr k v i
             | _ -> raise (Pdf.PDFError "change_pattern_matrices_page"))
          elts;
        page
    | _ -> page

(* Output information for each page *)
let output_page_info pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf
  and labels = Pdfpagelabels.read pdf in
    let getbox page box =
      if box = "/MediaBox" then
        match page.Pdfpage.mediabox with
        | Pdf.Array [a; b; c; d] ->
           Printf.sprintf "%f %f %f %f"
             (Pdf.getnum pdf a) (Pdf.getnum pdf b) (Pdf.getnum pdf c) (Pdf.getnum pdf d)
        | _ -> ""
      else
        match Pdf.lookup_direct pdf box page.Pdfpage.rest with
        | Some (Pdf.Array [a; b; c; d]) ->
           Printf.sprintf "%f %f %f %f"
             (Pdf.getnum pdf a) (Pdf.getnum pdf b) (Pdf.getnum pdf c) (Pdf.getnum pdf d)
        | _ -> ""
    and rotation page =
      Pdfpage.int_of_rotation page.Pdfpage.rotate
    in
      iter
        (fun pnum ->
           let page = select pnum pages in
             Printf.printf "Page %i:\n" pnum;
             Printf.printf "Label: %s\n"
               (try Pdfpagelabels.pagelabeltext_of_pagenumber pnum labels with Not_found -> "");
             Printf.printf "MediaBox: %s\n" (getbox page "/MediaBox");
             Printf.printf "CropBox: %s\n" (getbox page "/CropBox");
             Printf.printf "BleedBox: %s\n" (getbox page "/BleedBox");
             Printf.printf "TrimBox: %s\n" (getbox page "/TrimBox");
             Printf.printf "ArtBox: %s\n" (getbox page "/ArtBox");
             Printf.printf "Rotation: %i\n" (rotation page))
        range

let process_pages f pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pages', pagenumbers, matrices = (* new page objects, page number, matrix *)
      split3
        (map2
          (fun n p -> if mem n range then f n p else (p, n, Pdftransform.i_matrix))
          (ilist 1 (length pages))
          pages)
    in
      Pdfpage.change_pages ~matrices:(combine pagenumbers matrices) true pdf pages'

let iter_pages f pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
    iter2
      (fun n p -> if mem n range then f n p)
      (ilist 1 (length pages))
      pages

let map_pages f pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
    option_map2
      (fun n p -> if mem n range then Some (f n p) else None)
      (ilist 1 (length pages))
      pages

(* Clip a page to one of its boxes, or the media box if that box is not
 * present. This is a hard clip, done by using a clipping rectangle, so that
 * the page may then be used as a stamp without extraneous material reapearing.
 * *)
let hard_box pdf range boxname mediabox_if_missing fast =
  process_pages
    (Pdfpage.ppstub (fun pagenum page ->
       let minx, miny, maxx, maxy =
         if boxname = "/MediaBox" then
           Pdf.parse_rectangle pdf page.Pdfpage.mediabox
         else
           match Pdf.lookup_direct pdf boxname page.Pdfpage.rest with
           | Some a -> Pdf.parse_rectangle pdf a
           | _ ->
               if mediabox_if_missing
                 then Pdf.parse_rectangle pdf page.Pdfpage.mediabox
                 else error (Printf.sprintf "hard_box: box %s not found" boxname)
       in
         let ops = [Pdfops.Op_re (minx, miny, maxx -. minx, maxy -. miny); Pdfops.Op_W; Pdfops.Op_n] in
           Pdfpage.prepend_operators pdf ops ~fast page))
    pdf
    range

let shift_page ?(fast=false) dxdylist pdf pnum page =
  let dx, dy = List.nth dxdylist (pnum - 1) in
    let transform_op =
      Pdfops.Op_cm (Pdftransform.matrix_of_op (Pdftransform.Translate (dx, dy)))
    in
      let page =
        change_pattern_matrices_page pdf (Pdftransform.mktranslate ~-.dx ~-.dy) page
      in
        Pdfannot.transform_annotations pdf (Pdftransform.mktranslate dx dy) page.Pdfpage.rest;
        (Pdfpage.prepend_operators pdf [transform_op] ~fast page, pnum, Pdftransform.mktranslate dx dy)

let shift_pdf ?(fast=false) dxdylist pdf range =
  process_pages (shift_page ~fast dxdylist pdf) pdf range

(* Shift page data *)
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
                   make_mediabox (f (Pdf.parse_rectangle pdf v))
                 in
                   Pdf.replace_dict_entry e k v)
              page.Pdfpage.rest
              toreplace
          in
            {page with
               Pdfpage.mediabox =
                 make_mediabox (f (Pdf.parse_rectangle pdf page.Pdfpage.mediabox));
               Pdfpage.rest = rest'}


(* Scale contents *)
let scale_page_contents ?(fast=false) scale position pdf pnum page =
  let (minx, miny, maxx, maxy) as box =
    (* Use cropbox if available *)
    Pdf.parse_rectangle
      pdf
      (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
       | Some r -> r
       | None -> page.Pdfpage.mediabox)
  in
    let sx, sy, _ = Cpdfposition.calculate_position true 0. box position in
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
          let page = change_pattern_matrices_page pdf (Pdftransform.matrix_invert transform) page in
          Pdfannot.transform_annotations pdf transform page.Pdfpage.rest;
          (Pdfpage.prepend_operators pdf [transform_op] ~fast page, pnum, transform)

let scale_contents ?(fast=false) position scale pdf range =
  process_pages (scale_page_contents ~fast scale position pdf) pdf range

(* Set media box *)
let set_mediabox xywhlist pdf range =
  let crop_page pnum page =
    let x, y, w, h = List.nth xywhlist (pnum - 1) in
    {page with
       Pdfpage.mediabox =
        (Pdf.Array
           [Pdf.Real x; Pdf.Real y;
            Pdf.Real (x +.  w); Pdf.Real (y +. h)])}
  in
    process_pages (Pdfpage.ppstub crop_page) pdf range

(* If a cropbox exists, make it the mediabox. If not, change nothing. *)
let copy_cropbox_to_mediabox pdf range =
  process_pages
    (Pdfpage.ppstub (fun _ page ->
       match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
       | Some pdfobject -> {page with Pdfpage.mediabox = Pdf.direct pdf pdfobject}
       | None -> page))
    pdf
    range

let remove_cropping_pdf pdf range =
  let remove_cropping_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/CropBox")}
  in
    process_pages (Pdfpage.ppstub remove_cropping_page) pdf range

let remove_trim_pdf pdf range =
  let remove_trim_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/TrimBox")}
  in
    process_pages (Pdfpage.ppstub remove_trim_page) pdf range

let remove_art_pdf pdf range =
  let remove_art_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/ArtBox")}
  in
    process_pages (Pdfpage.ppstub remove_art_page) pdf range

let remove_bleed_pdf pdf range =
  let remove_bleed_page _ page =
    {page with
       Pdfpage.rest =
         (Pdf.remove_dict_entry page.Pdfpage.rest "/BleedBox")}
  in
    process_pages (Pdfpage.ppstub remove_bleed_page) pdf range

(* Upright functionality *)

(* Return the pages from the pdf in the range, unordered. *)
let select_pages range pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    option_map (function n -> try Some (select n pages) with _ -> None) range

(* If all pages are already upright, and the mediabox is (0,0)-based, do nothing
to save time. *)
let allupright range pdf =
  let page_is_upright page =
    page.Pdfpage.rotate = Pdfpage.Rotate0 &&
      (let (minx, miny, _, _) = Pdf.parse_rectangle pdf page.Pdfpage.mediabox in
         minx < 0.001 && miny < 0.001 && minx > ~-.0.001 && miny > ~-.0.001)
  in
    not (mem false (map page_is_upright (select_pages range pdf)))

let upright_transform pdf page =
  let rotate =
    Pdfpage.int_of_rotation page.Pdfpage.rotate
  and cx, cy =
    let minx, miny, maxx, maxy = Pdf.parse_rectangle pdf page.Pdfpage.mediabox in
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
      Pdfannot.transform_annotations pdf tr page.Pdfpage.rest;
      Pdfpage.prepend_operators pdf [transform_op] ~fast page

(* Change a page's media box so its minimum x and y are 0, making other
operations simpler to think about. Any shift that is done is reflected in
other boxes (clip etc.) *)
let rectify_boxes ?(fast=false) pdf page =
  let minx, miny, _, _ =
    Pdf.parse_rectangle pdf page.Pdfpage.mediabox
  in
    let f (iminx, iminy, imaxx, imaxy) =
      iminx -. minx, iminy -. miny, imaxx -. minx, imaxy -. miny
    in
      let page = change_boxes f pdf page in
        if minx <> 0. || miny <> 0.
          then
            begin let p, _, _ = shift_page ~fast [(-.minx),(-.miny)] pdf 1 page in p end
          else page

let upright ?(fast=false) range pdf =
  if allupright range pdf then pdf else
    let upright_page _ pnum page =
      let tr = upright_transform pdf page in
        let page = transform_boxes tr pdf page in
          let page = transform_contents ~fast tr pdf page in
            (rectify_boxes ~fast pdf {page with Pdfpage.rotate = Pdfpage.Rotate0}, pnum, tr)
    in
      process_pages (upright_page pdf) pdf range

(* Rotating pages *)
let rotate_pdf r pdf range =
  let rotate_page _ page =
    {page with Pdfpage.rotate =
       Pdfpage.rotation_of_int r}
  in
    process_pages (Pdfpage.ppstub rotate_page) pdf range

let rotate_pdf_by r pdf range =
  let rotate_page_by _ page =
    {page with Pdfpage.rotate =
       Pdfpage.rotation_of_int ((Pdfpage.int_of_rotation page.Pdfpage.rotate + r) mod 360)}
  in
    process_pages (Pdfpage.ppstub rotate_page_by) pdf range

let rotate_page_contents ~fast rotpoint r pdf pnum page =
  let rotation_point =
    match rotpoint with
    | None ->
        let minx, miny, maxx, maxy = Pdf.parse_rectangle pdf page.Pdfpage.mediabox in
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
        Pdfannot.transform_annotations pdf tr page.Pdfpage.rest;
        (Pdfpage.prepend_operators pdf [transform_op] ~fast page, pnum, tr)

let rotate_contents ?(fast=false) r pdf range =
  process_pages (rotate_page_contents ~fast None r pdf) pdf range

(* Scale page data *)
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
           Pdfannot.transform_annotations pdf matrix page.Pdfpage.rest;
           (Pdfpage.prepend_operators pdf ~fast [transform_op] page, pnum, matrix)
      in
        process_pages scale_page pdf range

(* Scale to fit page of size x * y *)
let scale_to_fit_pdf ?(fast=false) position input_scale xylist op pdf range =
  let scale_page_to_fit pnum page =
    let x, y = List.nth xylist (pnum - 1) in
    let matrix =
      let (minx, miny, maxx, maxy) =
        (* Use cropbox if available *)
        Pdf.parse_rectangle
          pdf
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
        Pdfannot.transform_annotations pdf matrix page.Pdfpage.rest;
        (Pdfpage.prepend_operators pdf [Pdfops.Op_cm matrix] ~fast
         (change_pattern_matrices_page pdf (Pdftransform.matrix_invert matrix) page), pnum, matrix)
  in
    process_pages scale_page_to_fit pdf range

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

(* Does the page have a defined box e.g "/CropBox" *)
let hasbox pdf page boxname =
  let pages = Pdfpage.pages_of_pagetree pdf in
    if page > length pages || page < 1 then raise (Failure "hasbox: bad page") else
      let p = select page pages in
        match Pdf.lookup_direct pdf boxname p.Pdfpage.rest with
        | Some _ -> true
        | _ -> false

(* Flip pages *)
let flip_page ?(fast=false) transform_op pdf pnum page =
  let minx, miny, maxx, maxy =
    Pdf.parse_rectangle pdf page.Pdfpage.mediabox
  in
    let tr = transform_op minx miny maxx maxy in
      let page = change_pattern_matrices_page pdf (Pdftransform.matrix_invert tr) page in
        Pdfannot.transform_annotations pdf tr page.Pdfpage.rest;
        (Pdfpage.prepend_operators pdf [Pdfops.Op_cm tr] ~fast page, pnum, tr)

let vflip_pdf ?(fast=false) pdf range =
  let transform_op _ miny _ maxy =
    Pdftransform.matrix_of_op
      (Pdftransform.Scale ((0., ((miny +. maxy) /. 2.)), 1., -.1.))
  in
    process_pages (flip_page ~fast transform_op pdf) pdf range

let hflip_pdf ?(fast=false) pdf range =
  let transform_op minx _ maxx _ =
    Pdftransform.matrix_of_op
      (Pdftransform.Scale (((minx +. maxx) /. 2., 0.), -.1., 1.))
  in
    process_pages (flip_page ~fast transform_op pdf) pdf range

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
      pdf
      (match Pdf.lookup_direct pdf "/CropBox" o.Pdfpage.rest with | Some r -> r | None -> o.Pdfpage.mediabox)
  in let txmin, tymin, txmax, tymax =
    Pdf.parse_rectangle
      pdf
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
                    Pdfannot.transform_annotations pdf matrix o.Pdfpage.rest;
                    let r = Pdfpage.prepend_operators pdf [Pdfops.Op_cm matrix] ~fast o in
                      change_pattern_matrices_page pdf (Pdftransform.matrix_invert matrix) r
      else
        let sw = sxmax -. sxmin and sh = symax -. symin
        and w = txmax -. txmin and h = tymax -. tymin in
          let dx, dy = stamp_shift_of_position topline midline sw sh w h position in
            let matrix =
              (Pdftransform.matrix_of_transform
                ((if relative_to_cropbox then [Pdftransform.Translate (txmin, tymin)] else []) @
                [Pdftransform.Translate (dx, dy)]))
            in
              Pdfannot.transform_annotations pdf matrix o.Pdfpage.rest;
              let r = Pdfpage.prepend_operators pdf [Pdfops.Op_cm matrix] ~fast o in
                change_pattern_matrices_page pdf (Pdftransform.matrix_invert matrix) r
    in
      {u with
         Pdfpage.content =
           (if isover then ( @ ) else ( @@ ))
           (protect fast pdf u.Pdfpage.resources u.Pdfpage.content)
           (protect fast pdf o.Pdfpage.resources o.Pdfpage.content);
         Pdfpage.rest =
           combine_page_items pdf u.Pdfpage.rest o.Pdfpage.rest; 
         Pdfpage.resources =
           Pdfpage.combine_pdf_resources pdf u.Pdfpage.resources o.Pdfpage.resources}

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


(* Just used by cpdflib for historical reasons *)
let setBox box minx maxx miny maxy pdf range =
  let set_box_page _ page =
    {page with
       Pdfpage.rest =
         Pdf.add_dict_entry
           page.Pdfpage.rest box
           (Pdf.Array [Pdf.Real minx; Pdf.Real miny; Pdf.Real maxx; Pdf.Real maxy])}
  in
    process_pages (Pdfpage.ppstub set_box_page) pdf range

(* Cropping *)
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
    process_pages (Pdfpage.ppstub crop_page) pdf range

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
      Pdf.Array [a; b; c; d] as r -> Some (Pdf.parse_rectangle pdf r)
    | _ -> None
  else
    match Pdf.lookup_direct pdf box page.Pdfpage.rest with
      Some (Pdf.Array [a; b; c; d] as r) -> Some (Pdf.parse_rectangle pdf r)
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
  process_pages (Pdfpage.ppstub (show_boxes_page fast pdf)) pdf range

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
  process_pages (Pdfpage.ppstub (trim_marks_page fast pdf)) pdf range

(* copy the contents of the box f to the box t. If mediabox_if_missing is set,
the contents of the mediabox will be used if the from fox is not available. If
mediabox_is_missing is false, the page is unaltered. *)
let copy_box f t mediabox_if_missing pdf range =
  process_pages
    (Pdfpage.ppstub (fun _ page ->
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

