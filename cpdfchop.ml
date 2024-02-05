open Pdfutil
open Cpdferror

(* Chop a single page into pieces. We prefer the cropbox when available. We set
   mediabox only, and delete any other boxes. We delete /Annots, since
   duplicate annotations are not allowed. *)

let get_box pdf page =
  match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
  | Some r -> Pdf.parse_rectangle pdf r
  | None -> Pdf.parse_rectangle pdf page.Pdfpage.mediabox

let erase_boxes d =
  let f = Pdf.remove_dict_entry in
    f (f (f (f (f d "/CropBox") "/BleedBox") "/TrimBox") "/ArtBox") "/Annots"

let make_pages x y columns btt rtl w h ps move_page =
  if columns then
    let column tx =
      if btt then
        for ty = 0 to y - 1 do ps =| move_page (w *. float_of_int tx) (h *. float_of_int ty) done
      else
        for ty = y - 1 downto 0 do ps =| move_page (w *. float_of_int tx) (h *. float_of_int ty) done
    in
      if rtl then for tx = x - 1 downto 0 do column tx done else for tx = 0 to x - 1 do column tx done
  else
    let row ty =
      if rtl then
        for tx = x - 1 downto 0 do ps =| move_page (w *. float_of_int tx) (h *. float_of_int ty) done
      else
        for tx = 0 to x - 1 do ps =| move_page (w *. float_of_int tx) (h *. float_of_int ty) done
    in
      if btt then for ty = 0 to y - 1 do row ty done else for ty = y - 1 downto 0 do row ty done

let chop_boxes pdf x y columns btt rtl p =
  if x < 1 || y < 1 then error "chop_boxes bad specification" else
    let move_page mx my p w h dx dy =
      let nminx, nminy, nmaxx, nmaxy = (mx +. dx, my +. dy, mx +. w +. dx, my +. h +. dy) in
        {p with
          Pdfpage.mediabox = Pdf.Array [Pdf.Real nminx; Pdf.Real nminy; Pdf.Real nmaxx; Pdf.Real nmaxy];
          Pdfpage.rest = erase_boxes p.Pdfpage.rest}
    in
    let minx, miny, maxx, maxy = get_box pdf p in
    let w, h = (maxx -. minx) /. float_of_int x, (maxy -. miny) /. float_of_int y in
    let ps = ref [] in
      make_pages x y columns btt rtl w h ps (move_page minx miny p w h);
      rev !ps

let chop ~x ~y ~columns ~btt ~rtl pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pages_out =
    flatten
      (map2
        (fun n p -> if mem n range then chop_boxes pdf x y columns btt rtl p else [p])
        (ilist 1 (Pdfpage.endpage pdf))
        pages)
  in
  let changes =
    let q = ref 0 in
      flatten
        (map2
           (fun n p ->
              if mem n range
                then (q += 1; let r = combine (many n (x * y)) (ilist !q (!q + x * y - 1)) in q += (x * y - 1); r)
                else (q += 1; [(n, !q)]))
           (ilist 1 (Pdfpage.endpage pdf))
           pages)
  in
    Pdfpage.change_pages ~changes true pdf pages_out


let chop_hv ~is_h ~p ~columns pdf range =
  pdf
