open Pdfutil
open Cpdferror

(* Resources - what to do with each. Bookmarks? Annotations - must be duplicated... *)
(* Test how the sharing affects other cpdf operations - especially with -fast! *)
(* Allow different orders - btt / ltr etc. *)

(* Chop a single page into pieces. We prefer the cropbox when available. We set
   mediabox only, and delete any other boxes. *)
(* FIXME check non-zero based boxes *)
let chop_boxes x y p =
  if x < 1 || y < 1 then Cpdferror.error "chop_boxes bad specification" else
    let move_page p dx dy = Printf.printf "move_page %f %f\n" dx dy; p in
    let minx, miny, maxx, maxy = 0., 0., 540., 666. in
    let w, h = (maxx -. minx) /. float_of_int x, (maxy -. miny) /. float_of_int y in
    let ps = ref [] in
      for ty = 0 to x do
        for tx = 0 to y do
          ps =| move_page p (w *. float_of_int tx) (h *. float_of_int ty *. ~-.1.)
        done
      done;
      rev !ps

(* Chop pages in the range into pieces *)
let chop ~x ~y pdf range =
  let pages = Pdfpage.pages_of_pagetree pdf in
  let pages =
    flatten
      (map2
        (fun n p -> if mem n range then (chop_boxes x y p) else [p])
        (ilist 1 (Pdfpage.endpage pdf))
        pages)
  in
  let changes = [] in (* FIXME *)
    Pdfpage.change_pages ~changes true pdf pages
