open Pdfutil
open Cpdferror

(* FIXME Resources - what to do with each. Bookmarks? Annotations - must be duplicated... *)
(* FIXME Test how the sharing affects other cpdf operations - especially with -fast! *)

(* Chop a single page into pieces. We prefer the cropbox when available. We set
   mediabox only, and delete any other boxes. *)
(* FIXME btt / rtl *)
let get_box pdf page =
  match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
  | Some r -> Pdf.parse_rectangle pdf r
  | None -> Pdf.parse_rectangle pdf page.Pdfpage.mediabox

let erase_boxes d =
  let f = Pdf.remove_dict_entry in
    f (f (f (f d "/CropBox") "/BleedBox") "/TrimBox") "/ArtBox"

let chop_boxes pdf x y p =
  if x < 1 || y < 1 then Cpdferror.error "chop_boxes bad specification" else
    let move_page mx my p w h dx dy =
      (*Printf.printf "move_page by %f %f\n" dx dy;*)
      let nminx, nminy, nmaxx, nmaxy = (mx +. dx, my +. dy, mx +. w +. dx, my +. h +. dy) in
        (*Printf.printf "new box: %f, %f, %f, %f\n" nminx nminy nmaxx nmaxy;*)
        {p with
          Pdfpage.mediabox = Pdf.Array [Pdf.Real nminx; Pdf.Real nminy; Pdf.Real nmaxx; Pdf.Real nmaxy];
          Pdfpage.rest = erase_boxes p.Pdfpage.rest}
    in
    let minx, miny, maxx, maxy = get_box pdf p in
    (*Printf.printf "minx, miny, maxx, maxy = %f, %f, %f, %f\n" minx miny maxx maxy;*)
    let w, h = (maxx -. minx) /. float_of_int x, (maxy -. miny) /. float_of_int y in
    let ps = ref [] in
      for ty = y - 1 downto 0 do
        for tx = 0 to x - 1 do
          ps =| move_page minx miny p w h (w *. float_of_int tx) (h *. float_of_int ty)
        done
      done;
      rev !ps

(* Chop pages in the range into pieces *)
let chop ~x ~y pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pages =
    flatten
      (map2
        (fun n p -> if mem n range then (chop_boxes pdf x y p) else [p])
        (ilist 1 (Pdfpage.endpage pdf))
        pages)
  in
  let changes = [] in (* FIXME *)
    Pdfpage.change_pages ~changes true pdf pages
