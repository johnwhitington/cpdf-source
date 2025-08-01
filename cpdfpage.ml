open Pdfutil
open Cpdferror

(*(* FIXME: Need to take account of inherited resources (among Xobjects and their children - pages
   are regularized upon loading). Would be nice to see a failing example first though.
   FIXME: What would happen if a pattern was used in a transforming and non-transforming way - we
   would have to dedupulicate - again, no failing example available. *)

(* Transparency group soft masks appear to need altering with the inverse of
   the transformation matrix. We find them all, deduplicate, and then process
   in place. *)

(* For each xobject, look in /Resources -> /ExtGState -> /G, and get object number. *)
let rec change_softmask_matrices_xobject pdf xobject =
  let objnums = ref [] in
    begin match xobject with
    | Pdf.Indirect i ->
        let dict = Pdf.lookup_obj pdf i in
          begin match Pdf.lookup_direct pdf "/Resources" dict with
          | Some d ->
              begin match Pdf.lookup_direct pdf "/ExtGState" d with
              | Some (Pdf.Dictionary extgstates) ->
                  iter
                    (function extgstate ->
                       match Pdf.direct pdf extgstate with
                       | Pdf.Dictionary d ->
                           begin match Pdf.lookup_direct pdf "/SMask" (Pdf.Dictionary d) with
                           | Some (Pdf.Dictionary d) ->
                               begin match lookup "/G" d with
                               | Some (Pdf.Indirect i) ->
                                   objnums := i::!objnums
                               | _ -> ()
                               end
                           | _ -> ()
                           end
                       | _ -> ())
                    (map snd extgstates)
              | _ -> ()
              end
          | _ -> ()
          end
    | _ -> ()
    end;
    let subxobjects =
      match Pdf.lookup_direct pdf "/Resources" xobject with
      | Some d ->
          begin match Pdf.lookup_direct pdf "/XObject" d with
          | Some (Pdf.Dictionary d) -> map snd d
          | _ -> []
          end
      | _ -> []
    in
    let descendants =
      flatten (map (change_softmask_matrices_xobject pdf) subxobjects)
    in
     descendants @ !objnums

let change_softmask_matrices_page pdf tr page =
  let xobjects =
    match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
    | Some (Pdf.Dictionary d) -> d
    | _ -> []
  in
    let objnums = setify (flatten (map (change_softmask_matrices_xobject pdf) (map snd xobjects))) in
      iter
        (fun objnum ->
           let dict = Pdf.lookup_obj pdf objnum in
           let matrix = Pdf.parse_matrix pdf "/Matrix" dict in
           let matrix' = Pdftransform.matrix_compose (Pdftransform.matrix_invert tr) matrix in
           let dict = Pdf.add_dict_entry dict "/Matrix" (Pdf.make_matrix matrix') in
             Pdf.addobj_given_num pdf (objnum, dict))
        objnums*)

(* When we transform a page by wrapping in an [Op_cm], we must also
change any /Matrix entries in (some) pattern dictionaries, including inside
xobjects. We only change the ones used with scn, to avoid pattern dictionaries
used in other ways, which must not be transformed. *)
let patterns_used pdf content resources =
  let used = null_hash () in
  match Pdf.lookup_direct pdf "/Pattern" resources with
  | None -> used
  | Some _ ->
      let ops = Pdfops.parse_operators pdf resources content in
        iter
          (function Pdfops.Op_scnName (x, []) | Pdfops.Op_SCNName (x, []) -> Hashtbl.replace used x () | _ -> ())
          ops;
        used

let pats_done = null_hash ()

let rec change_pattern_matrices_resources pdf tr resources names_used_with_scn =
  begin match Pdf.lookup_direct pdf "/XObject" resources with
  | Some (Pdf.Dictionary elts) ->
      iter
        (fun (k, v) -> 
           match v with
           | Pdf.Indirect i ->
               (*Printf.printf "Processing form xobject %s for patterns\n%!" k;*)
               change_pattern_matrices_xobject pdf tr v i
           | _ -> raise (Pdf.PDFError "change_pattern_matrices_page"))
        elts
  | _ -> ()
  end;
  begin match Pdf.lookup_direct pdf "/Pattern" resources with
  | Some (Pdf.Dictionary patterns) ->
      let entries =
        map
          (fun (name, p) ->
            match Hashtbl.find names_used_with_scn name with
            | exception Not_found -> (name, p)
            | _ ->
                (*Printf.printf "Changing matrices of pattern %s\n%!" name;*)
                let old_pattern = Pdf.direct pdf p in
                  let new_pattern =
                    let existing_tr = Pdf.parse_matrix pdf "/Matrix" old_pattern in
                      let new_tr = Pdftransform.matrix_compose tr existing_tr in
                        Pdf.add_dict_entry old_pattern "/Matrix" (Pdf.make_matrix new_tr)
                  in
                    name, Pdf.Indirect (Pdf.addobj pdf new_pattern))
          patterns
       in
         Pdf.add_dict_entry resources "/Pattern" (Pdf.Dictionary entries)
  | _ -> resources
  end

and change_pattern_matrices_xobject pdf tr xobj xobjnum =
  match xobj with
  | Pdf.Indirect i when (try ignore (Hashtbl.find pats_done i); true with Not_found -> false) -> ()
  | _ ->
      begin match xobj with Pdf.Indirect i -> Hashtbl.add pats_done i () | _ -> () end;
      let xobj = Pdf.direct pdf xobj in
      match Pdf.lookup_direct pdf "/Subtype" xobj with
      | Some (Pdf.Name "/Form") ->
          Pdfcodec.decode_pdfstream pdf xobj;
          let resources = match Pdf.lookup_direct pdf "/Resources" xobj with Some d -> d | None -> Pdf.Dictionary [] in
          let used = patterns_used pdf [xobj] resources in
          begin match Pdf.lookup_direct pdf "/Resources" xobj with
          | Some resources ->
              let xobj' =
                Pdf.add_dict_entry xobj "/Resources" (change_pattern_matrices_resources pdf tr resources used)  
              in
                Pdf.addobj_given_num pdf (xobjnum, xobj')
          | _ -> ()
          end
      | _ -> ()

let change_pattern_matrices_page pdf tr page =
  Hashtbl.clear pats_done;
  (*change_softmask_matrices_page pdf tr page;*)
  let used = patterns_used pdf page.Pdfpage.content page.Pdfpage.resources in
    (*Printf.printf "Patterns for translation, due to being used as cs / CS";
    Hashtbl.iter (fun x _ -> Printf.printf "%s " x) used;
    Printf.printf "\n%!";*)
    {page with Pdfpage.resources = change_pattern_matrices_resources pdf tr page.Pdfpage.resources used}

(* Output information for each page *)
exception Exceptjson of Cpdfyojson.Safe.t

let output_page_info ?(json=false) ?(raisejson=false) unit pdf range =
  let ugetnum pdf o =
    let n = Pdf.getnum pdf o in
      match unit with
      | Pdfunits.PdfPoint -> n
      | Pdfunits.Inch -> Pdfunits.inches n Pdfunits.PdfPoint
      | Pdfunits.Centimetre -> Pdfunits.centimetres n Pdfunits.PdfPoint
      | Pdfunits.Millimetre -> Pdfunits.millimetres n Pdfunits.PdfPoint
  in
  let pages = Pdfpage.pages_of_pagetree pdf in
  let labels = Pdfpagelabels.read pdf in
    let getbox page box =
      if box = "/MediaBox" then
        match page.Pdfpage.mediabox with
        | Pdf.Array [a; b; c; d] ->
           Printf.sprintf "%f %f %f %f"
             (ugetnum pdf a) (ugetnum pdf b) (ugetnum pdf c) (ugetnum pdf d)
        | _ -> ""
      else
        match Pdf.lookup_direct pdf box page.Pdfpage.rest with
        | Some (Pdf.Array [a; b; c; d]) ->
           Printf.sprintf "%f %f %f %f"
             (ugetnum pdf a) (ugetnum pdf b) (ugetnum pdf c) (ugetnum pdf d)
        | _ -> ""
    in
    let rotation page =
      Pdfpage.int_of_rotation page.Pdfpage.rotate
    in
    let num_annots page =
      match Pdf.lookup_direct pdf "/Annots" page.Pdfpage.rest with
      | Some (Pdf.Array a) -> length a
      | _ -> 0
    in
      let json_entry_of_pnum pnum =
        let getbox_json page box =
          match getbox page box with
          | "" -> `Null
          | s ->
            let a, b, c, d = Cpdfcoord.parse_rectangle (Pdf.empty ()) s in
              `List [`Float a; `Float b; `Float c; `Float d]
        in
        let page = select pnum pages in
          `Assoc
            [("Page", `Int pnum);
             ("Label", (`String (try Pdfpagelabels.pagelabeltext_of_pagenumber pnum labels with Not_found -> "")));
             ("MediaBox", getbox_json page "/MediaBox");
             ("CropBox", getbox_json page "/CropBox");
             ("BleedBox", getbox_json page "/BleedBox");
             ("TrimBox", getbox_json page "/TrimBox");
             ("ArtBox", getbox_json page "/ArtBox");
             ("Rotation", `Int (rotation page));
             ("Annotations", `Int (num_annots page))]
      in
        if json then
          let thejson = `List (map json_entry_of_pnum range) in
            if raisejson then
              raise (Exceptjson thejson)
            else
              flprint (Cpdfyojson.Safe.pretty_to_string thejson)
        else
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
                 Printf.printf "Rotation: %i\n" (rotation page);
                 Printf.printf "Annotations: %i\n" (num_annots page))
            range

let json_page_info pdf range unit =
  try output_page_info ~json:true ~raisejson:true unit pdf range; `List [] with
    Exceptjson j -> j

let process_pages f pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pages', pagenumbers, matrices = (* new page objects, page number, matrix *)
      split3
        (map2
          (fun n p -> if mem n range then
            begin
              Cpdfutil.progress_page n; 
              let r = f n p in
              Cpdfutil.progress_endpage ();
              r
            end
          else (p, n, Pdftransform.i_matrix))
          (ilist 1 (length pages))
          pages)
    in
      let r = Pdfpage.change_pages ~matrices:(combine pagenumbers matrices) true pdf pages' in
        Cpdfutil.progress_done ();
        r

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
    let transform_op = Pdfops.Op_cm (Pdftransform.matrix_of_op (Pdftransform.Translate (dx, dy))) in
    let tr = Pdftransform.mktranslate dx dy in
    let page = change_pattern_matrices_page pdf tr page in
      Pdfannot.transform_annotations pdf tr page.Pdfpage.rest;
      (Pdfpage.prepend_operators pdf [transform_op] ~fast page, pnum, tr)

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

let shift_boxes dxdylist pdf range =
  let dx, dy = match dxdylist with (a, b)::_ -> a, b | _ -> 0.0, 0.0 in
  let f (xmin, ymin, xmax, ymax) = (xmin +. dx, ymin +. dy, xmax +. dx, ymax +. dy) in
  let fpage _ p = change_boxes f pdf p in
    process_pages (Pdfpage.ppstub fpage) pdf range

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
        | TopLeft (a, b) -> a, -.b
        | TopRight (a, b) -> -.a, -.b
        | Left t -> t, 0.
        | BottomLeft (a, b) -> a, b
        | Bottom t -> 0., t
        | BottomRight (a, b) -> -.a, b
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

(* Same, but don't care about mediabox origin. This is for -scale-to-fit, now that
it is ok with nonzero origins. *)
let alluprightonly range pdf =
  let page_is_upright page = page.Pdfpage.rotate = Pdfpage.Rotate0 in
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
    let page = change_pattern_matrices_page pdf tr page in
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
      Pdftransform.matrix_of_op (Pdftransform.Rotate (rotation_point, -.(rad_of_deg r)))
    in
    let transform_op = Pdfops.Op_cm tr in
    let page = change_pattern_matrices_page pdf tr page in
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
          in let page =
            change_pattern_matrices_page pdf matrix page
          in
           Pdfannot.transform_annotations pdf matrix page.Pdfpage.rest;
           (Pdfpage.prepend_operators pdf ~fast [transform_op] page, pnum, matrix)
      in
        process_pages scale_page pdf range

(* Scale without regard to aspect ratio. *)
let stretch ?(fast=false) sxsylist pdf range =
  let stretch_page pnum page =
    let sx, sy = List.nth sxsylist (pnum - 1) in
    let (minx, miny, maxx, maxy) =
      Pdf.parse_rectangle
        pdf
        (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
        | Some r -> r
        | None -> page.Pdfpage.mediabox)
    in
    let scale_x, scale_y = sx /. (maxx -. minx), sy /. (maxy -. miny) in
    let f (xmin, ymin, xmax, ymax) = xmin *. scale_x, ymin *. scale_y, xmax *. scale_x, ymax *. scale_y in
    let page = change_boxes f pdf page in
    let matrix = Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), scale_x, scale_y)) in
    let transform_op = Pdfops.Op_cm matrix in
    let page = change_pattern_matrices_page pdf matrix page in
      Pdfannot.transform_annotations pdf matrix page.Pdfpage.rest;
      (Pdfpage.prepend_operators pdf ~fast [transform_op] page, pnum, matrix)
  in
    process_pages stretch_page pdf range

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

(* Centre page content (crop box) on given page size, with no scaling. *)
let center_to_fit sxsylist pdf range =
  let dxdylist =
    let tx, ty = hd sxsylist in
      map
        (fun page ->
           let (minx, miny, maxx, maxy) =
             Pdf.parse_rectangle
               pdf
               (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
               | Some r -> r
               | None -> page.Pdfpage.mediabox)
           in
             (~-.((tx -. (maxx -. minx)) /. 2.),
              ~-.((ty -. (maxy -. miny)) /. 2.)))
        (Pdfpage.pages_of_pagetree pdf)
  in
    let list4 = map (fun (x, y) -> (0., 0., x, y)) sxsylist in
    let progress = !Cpdfutil.progress in
    clear Cpdfutil.progress;
    let pdf = set_mediabox list4 pdf range in
    let pdf = crop_pdf list4 pdf range in
    let pdf = remove_bleed_pdf pdf range in
    let pdf = remove_art_pdf pdf range in
    let pdf = remove_bleed_pdf pdf range in
      Cpdfutil.progress := progress;
      shift_boxes dxdylist pdf range
  
(* Scale to fit page of size x * y *)
let scale_to_fit_pdf ?(fast=false) position input_scale xylist op pdf range =
  let scale_page_to_fit pnum page =
    let x, y = List.nth xylist (pnum - 1) in
    let matrix =
      let (minx, miny, maxx, maxy) =
        Pdf.parse_rectangle
          pdf
          (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
          | Some r -> r
          | None -> page.Pdfpage.mediabox)
      in
        if maxx <= 0. || maxy <= 0. then failwith "Zero-sized pages are invalid" else
          let fx = x /. (maxx -. minx) in let fy = y /. (maxy -. miny) in
            let scale = fmin fx fy *. input_scale in
              let trans_x = (x -. (maxx *. scale)) /. 2. in
              let trans_y = (y -. (maxy *. scale)) /. 2. in
              let fixup_trans_x = -. (minx *. scale) /. 2. in
              let fixup_trans_y = -. (miny *. scale) /. 2. in 
              let position_trans_x =
                let dx = (x -. ((maxx -. minx) *. scale)) /. 2. in
                  match position with
                  | Cpdfposition.Left _ -> ~-. dx
                  | Cpdfposition.Right _ -> dx
                  | _ -> 0.
              in
              let position_trans_y =
                let dy = (y -. ((maxy -. miny) *. scale)) /. 2. in
                  match position with
                  | Cpdfposition.Top _ -> dy
                  | Cpdfposition.Bottom _ -> ~-. dy
                  | _ -> 0.
              in
                (Pdftransform.matrix_of_transform
                   [Pdftransform.Translate (fixup_trans_x, fixup_trans_y);
                    Pdftransform.Translate (position_trans_x, position_trans_y);
                    Pdftransform.Translate (trans_x, trans_y);
                    Pdftransform.Scale ((0., 0.), scale, scale)])
    in
      let page =
        change_boxes
          (function (minx, miny, maxx, maxy) -> 0., 0., x, y)
          pdf page
      in
        Pdfannot.transform_annotations pdf matrix page.Pdfpage.rest;
        (Pdfpage.prepend_operators pdf [Pdfops.Op_cm matrix] ~fast
         (change_pattern_matrices_page pdf matrix page), pnum, matrix)
  in
    process_pages scale_page_to_fit pdf range

(* For each page. If it is the wrong orientation for the xylist entry, rotate
   it the specified way (90 degrees, -90 degrees) and run upright (again) too. *)
let scale_to_fit_rotate ?(fast=false) xylist rotation pdf range =
  let scale_to_fit_rotate_page pnum page =
    let tw, th = List.nth xylist (pnum - 1) in
      let (minx, miny, maxx, maxy) =
        Pdf.parse_rectangle
          pdf
          (match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
          | Some r -> r
          | None -> page.Pdfpage.mediabox)
      in
        let pw, ph = maxx -. miny, maxy -. miny in
          if pw < ph && tw > th || pw > ph && tw < th then
            {page with Pdfpage.rotate = Pdfpage.rotation_of_int ((Pdfpage.int_of_rotation page.Pdfpage.rotate + rotation) mod 360)}
          else
            page
  in
    let pdf = process_pages (Pdfpage.ppstub scale_to_fit_rotate_page) pdf range in
      upright ~fast range pdf

(* Add stack operators to a content stream to ensure it is composeable. On
-fast, we don't check for Q deficit, assuming PDF is ISO. *)
let protect fast pdf resources content =
  let deficit =
    if fast then 0 else
      let ops = Pdfops.parse_operators pdf resources content in
      let qs = length (keep (eq Pdfops.Op_q) ops) in
      let bigqs = length (keep (eq Pdfops.Op_Q) ops) in
      let deficit = if qs > bigqs then qs - bigqs else 0 in
        if deficit <> 0 then Pdfe.log (Printf.sprintf "Q Deficit was nonzero. Fixing. %i\n" deficit);
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
      let page = change_pattern_matrices_page pdf tr page in
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
      | TopLeft (a, b) -> a, h -. sh -. b -. dy
      | TopRight (a, b) -> w -. sw -. a, h -. sh -. b -. dy
      | Left o -> o, half h -. half sh -. dy
      | BottomLeft (a, b) -> a, b -. dy
      | Bottom o -> half w -. half sw, o -. dy
      | BottomRight (a, b) -> w -. sw -. a, b -. dy
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
              Pdfannot.transform_annotations pdf matrix o.Pdfpage.rest;
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
           Pdfpage.combine_pdf_resources pdf u.Pdfpage.resources o.Pdfpage.resources}

let remove_struct_tree pdf =
  Cpdfutil.remove_dict_entry pdf "/StructTreeRoot" None;
  Cpdfutil.remove_dict_entry pdf "/StructParent" None;
  Cpdfutil.remove_dict_entry pdf "/StructParents" None;
  let remove_struct_tree_ops pdf resources content =
    let operators = Pdfops.parse_operators pdf resources content in
    (* In fact, we remove all marked content regions. Acceptable in the circumstances. *)
    let remove_mcids =
      lose
        (function
         | Pdfops.Op_MP _
         | Pdfops.Op_DP _
         | Pdfops.Op_BMC _
         | Pdfops.Op_BDC _
         | Pdfops.Op_EMC -> true | _ -> false)
    in
    let operators' = remove_mcids operators in
      [Pdfops.stream_of_ops operators']
  in
  let remove_struct_tree_page _ page =
    let content' = remove_struct_tree_ops pdf page.Pdfpage.resources page.Pdfpage.content in
      Pdfpage.process_xobjects pdf page remove_struct_tree_ops;
      {page with Pdfpage.content = content'}
  in
    process_pages (Pdfpage.ppstub remove_struct_tree_page) pdf (ilist 1 (Pdfpage.endpage pdf))

let mark_all_as_artifact pdf =
  let mark_all_as_artifact_ops pdf resources content =
    let operators = Pdfops.parse_operators pdf resources content in
    let operators' = [Pdfops.Op_BMC "/Artifact"] @ operators @ [Pdfops.Op_EMC] in
      [Pdfops.stream_of_ops operators']
  in
  let remove_struct_tree_page _ page =
    let content' = mark_all_as_artifact_ops pdf page.Pdfpage.resources page.Pdfpage.content in
      Pdfpage.process_xobjects pdf page mark_all_as_artifact_ops;
      {page with Pdfpage.content = content'}
  in
    process_pages (Pdfpage.ppstub remove_struct_tree_page) pdf (ilist 1 (Pdfpage.endpage pdf))

let stamp ~process_struct_tree relative_to_cropbox position topline midline fast scale_to_fit isover range over pdf =
  let over = if process_struct_tree then mark_all_as_artifact (remove_struct_tree over) else over in
  let prefix = Pdfpage.shortest_unused_prefix pdf in
  Pdfpage.add_prefix over prefix;
  let marks = Pdfmarks.read_bookmarks ~preserve_actions:true pdf in
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
                      Cpdfutil.progress_page pageseq;
                      let r = do_stamp relative_to_cropbox fast position topline midline scale_to_fit isover merged
                      (if mem pageseq range then over_page else Pdfpage.blankpage Pdfpaper.a4) under_page over
                      in
                      Cpdfutil.progress_endpage (); r)
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
                      Cpdfutil.progress_done ();
                      Pdfmarks.add_bookmarks new_marks changed

(* Combine pages from two PDFs. *)

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

let combine_pages ~process_struct_tree fast under over scaletofit over_is_under =
  let over = if process_struct_tree then mark_all_as_artifact (remove_struct_tree over) else over in
  let debug_combine_pages = false in
  let debug_pdf pdf n =
    if debug_combine_pages then
      begin Pdf.remove_unreferenced pdf; Pdfwrite.pdf_to_file pdf n end
  in
  Pdfpage.add_prefix over (Pdfpage.shortest_unused_prefix under);
  let marks_under, marks_over = Pdfmarks.read_bookmarks ~preserve_actions:true under, Pdfmarks.read_bookmarks ~preserve_actions:true over in
  let under, over = equalize_pages under over in
  let under_length, over_length = Pdfpage.endpage under, Pdfpage.endpage over in
    if under_length <> over_length then
      raise (Pdf.PDFError "combine_pages: not of equal length")
    else
      let pageseqs_under = ilist 1 (Pdfpage.endpage under) in
      let pageseqs_over = ilist 1 (Pdfpage.endpage over) in
      Cpdfutil.progress_line "Merging PDFs...";
      let merged =
        Pdfmerge.merge_pdfs
          false false ["a"; "b"] [under; over] [pageseqs_under; pageseqs_over]
      in
        debug_pdf merged "merged.pdf";
        let under_pages, over_pages =
          cleave (Pdfpage.pages_of_pagetree merged) under_length
        in
          let new_pages =
            map3
              (fun o u n ->
                 Cpdfutil.progress_page n;
                 let r =
                   do_stamp false fast (BottomLeft (0., 0.)) false false scaletofit (not over_is_under) merged o u over
                 in
                   Cpdfutil.progress_endpage ();
                   r)
              over_pages under_pages (indx over_pages)
          in
            (* Build the changes. 123456 -> 123123 *)
            let changes =
              let len = length new_pages in
                combine (ilist 1 (len * 2)) (let x = ilist 1 len in x @ x)
            in
              let changed = Pdfpage.change_pages ~changes true merged new_pages in
                let r = Pdfmarks.add_bookmarks (marks_under @ marks_over) changed in
                   debug_pdf r "final.pdf";
                   Cpdfutil.progress_done ();
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
        [Pdfops.begin_artifact]
      @ make_ops (255., 0., 0.) 0. 0. "/MediaBox"
      @ make_ops (0., 255., 0.) 7. 7. "/CropBox"
      @ make_ops (0., 0., 255.) 5. 5. "/ArtBox"
      @ make_ops (255.,150.,0.) 3. 3. "/TrimBox"
      @ make_ops (255.,9.,147.) 2. 2. "/BleedBox"
      @ [Pdfops.end_artifact]
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
         @ [Pdfops.begin_artifact]
         @ line (minx, tmaxy, tminx -. allowance, tmaxy) (* top left *)
         @ line (tminx, tmaxy +. allowance, tminx, maxy)
         @ line (tmaxx +. allowance, tmaxy, maxx, tmaxy) (* top right *)
         @ line (tmaxx, tmaxy +. allowance, tmaxx, maxy)
         @ line (tmaxx +. allowance, tminy, maxx, tminy) (* bottom right *)
         @ line (tmaxx, tminy -. allowance, tmaxx, miny)
         @ line (tminx -. allowance, tminy, minx, tminy) (* bottom left *)
         @ line (tminx, tminy -. allowance, tminx, miny)
         @ [Pdfops.end_artifact]
         @ [Pdfops.Op_Q]
      in
        Pdfpage.postpend_operators pdf ops ~fast page
  | _, _ ->
      (*Pdfe.log (Printf.sprintf "warning: no /TrimBox found on page %i\n" n);*)
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

let redact ~process_struct_tree pdf range =
  let pdf =
    process_pages
      (Pdfpage.ppstub
         (fun pnum page ->
            if mem pnum range then
              {page with
                 Pdfpage.content = [];
                 Pdfpage.resources = Pdf.Dictionary [];
                 Pdfpage.rest = Pdf.remove_dict_entry page.Pdfpage.rest "/Annots"}
            else
              page))
      pdf
      range
  in
    if process_struct_tree then Pdfst.trim_structure_tree pdf (Cpdfpagespec.invert_range (Pdfpage.endpage pdf) range);
    pdf
