open Pdfutil
open Cpdferror

(* Resources - what to do with each. Bookmarks? Annotations - must be duplicated... *)
(* Test how the sharing affects other cpdf operations. *)

(* Chop a single page into pieces. We prefer the cropbox when available. We set
   mediabox only, and delete any other boxes. *)
let chop_boxes x y p = [p; p]

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
