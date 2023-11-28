open Pdfutil
open Cpdferror

(* 1. FIXME btt / rtl / columns *)
(* 2. FIXME Test/document how the sharing affects other cpdf operations - especially with -fast! What do we do with fast/slow, and what does it affect, and does -fast need documentation about what happens in general with shared content streams. *)
(* 3. Annotations must be duplicated *)
(* 4. What is the effect on bookmarks and links annotations? *)
(* 5. Any other page meta items? *)

(* Chop a single page into pieces. We prefer the cropbox when available. We set
   mediabox only, and delete any other boxes. *)
let get_box pdf page =
  match Pdf.lookup_direct pdf "/CropBox" page.Pdfpage.rest with
  | Some r -> Pdf.parse_rectangle pdf r
  | None -> Pdf.parse_rectangle pdf page.Pdfpage.mediabox

let erase_boxes d =
  let f = Pdf.remove_dict_entry in
    f (f (f (f d "/CropBox") "/BleedBox") "/TrimBox") "/ArtBox"

let chop_boxes pdf x y columns btt rtl p =
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
    let move_page = move_page minx miny p w h in
      (* columns, btt, rtl *)
      for ty = y - 1 downto 0 do for tx = 0 to x - 1 do
        ps =| move_page (w *. float_of_int tx) (h *. float_of_int ty)
      done done;
      rev !ps

(* Chop pages in the range into pieces *)
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
    (*iter (fun (a, b) -> Printf.printf "%i -> %i\n" a b) changes;*)
    Pdfpage.change_pages ~changes true pdf pages_out
