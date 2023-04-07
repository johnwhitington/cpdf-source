open Pdfutil
open Cpdferror

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
let make_margin pdf output_mediabox margin tr =
  if margin = 0. then tr else
    let width, height =
      match Pdf.parse_rectangle pdf output_mediabox with
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
let impose_transforms pdf fit fx fy columns rtl btt center margin mediabox output_mediabox fit_extra_hspace fit_extra_vspace len =
  let width, height =
    match Pdf.parse_rectangle pdf mediabox with
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
  map (if fit then make_margin pdf output_mediabox margin else Fun.id) (rev !trs)

let impose_pages fit x y columns rtl btt center margin output_mediabox fast fit_extra_hspace fit_extra_vspace pdf = function
  | [] -> assert false
  | (h::_) as pages ->
     let transforms = 
       impose_transforms
         pdf fit x y columns rtl btt center margin h.Pdfpage.mediabox
         output_mediabox fit_extra_hspace fit_extra_vspace (length pages)
     in
       (* Change the pattern matrices before combining resources *)
       let pages, h =
         let r = map2 (fun p t -> Cpdfpage.change_pattern_matrices_page pdf t p) pages transforms in
           (r, List.hd r)
       in
     let resources' = pair_reduce (Pdfpage.combine_pdf_resources pdf) (map (fun p -> p.Pdfpage.resources) pages) in
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
                (fun p t -> Pdfannot.transform_annotations pdf t p.Pdfpage.rest; transform_stream t p.Pdfpage.content)
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
  let pdf = Cpdfpage.hard_box pdf all "/MediaBox" false fast in
  if spacing = 0. then pdf else
  let margin = spacing /. 2. in
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let width, height =
    match Pdf.parse_rectangle pdf firstpage.Pdfpage.mediabox with
      xmin, ymin, xmax, ymax -> (xmax -. xmin, ymax -. ymin)
  in
  if fit then
    (Cpdfpage.shift_pdf
      ~fast
      (many (margin, margin) endpage)
      (Cpdfpage.scale_contents ~fast (Cpdfposition.BottomLeft (0., 0.)) ((width -. spacing) /. width) pdf all)
      all)
  else
    (Cpdfpage.set_mediabox
      (many (0., 0., width +. spacing, height +. spacing) endpage)
      (Cpdfpage.shift_pdf ~fast (many (margin, margin) endpage) pdf all) all)

(* We add the border as a thick unfilled rectangle just inside the page edge,
   only if its linewidth is > 0 since, for us, 0 means none, not single-pixel
   like in PDF. *)
let add_border linewidth ~fast pdf =
  if linewidth = 0. then pdf else
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let _, _, w, h = Pdf.parse_rectangle pdf firstpage.Pdfpage.mediabox in
    Cpdfaddtext.addrectangle
      fast (w -. linewidth, h -. linewidth) (RGB (0., 0., 0.)) true linewidth 1. (Cpdfposition.BottomLeft (linewidth /. 2., linewidth /. 2.))
      false false (ilist 1 (Pdfpage.endpage pdf)) pdf

let impose ~x ~y ~fit ~columns ~rtl ~btt ~center ~margin ~spacing ~linewidth ~fast pdf =
  let endpage = Pdfpage.endpage pdf in
  let pagenums = ilist 1 endpage in
  let pdf = Cpdfpage.copy_cropbox_to_mediabox pdf pagenums in
  let pdf = Cpdfpage.remove_cropping_pdf pdf pagenums in
  let pdf = Cpdfpage.upright pagenums pdf in
  let pdf = add_border linewidth ~fast pdf in
  let pdf = make_space fit ~fast spacing pdf in 
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let _, _, w, h = Pdf.parse_rectangle pdf firstpage.Pdfpage.mediabox in
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
  if fit then pdf else Cpdfpage.shift_pdf ~fast (many (margin, margin) (length pages)) pdf (ilist 1 (Pdfpage.endpage pdf))

(* Legacy -twoup-stack. Impose 2x1 on a page twice the size then rotate. *)
let twoup_stack fast pdf =
  let pdf =
    impose
      ~x:2. ~y:1. ~fit:false ~columns:false ~rtl:false ~btt:false ~center:false
      ~margin:0. ~spacing:0. ~linewidth:0. ~fast pdf
  in
   let all = ilist 1 (Pdfpage.endpage pdf) in
    Cpdfpage.upright ~fast all (Cpdfpage.rotate_pdf ~-90 pdf all)

(* Legacy -two-up. Rotate the pages and shrink them so as to fit 2x1 on a page the same size. *)
let twoup fast pdf =
  let firstpage = hd (Pdfpage.pages_of_pagetree pdf) in
  let width, height =
    match Pdf.parse_rectangle pdf firstpage.Pdfpage.mediabox with
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
        let pdf = Cpdfpage.scale_pdf ~fast (many (sc, sc) endpage) pdf all in
        let pdf =
          impose
            ~x:2. ~y:1. ~fit:false ~columns:false ~rtl:false ~btt:false ~center:true
            ~margin:0. ~spacing:0. ~linewidth:0. ~fast pdf
        in
        let endpage = Pdfpage.endpage pdf in
        let all = ilist 1 endpage in
        let pdf = Cpdfpage.upright all (Cpdfpage.rotate_pdf ~-90 pdf all) in
          Cpdfpage.scale_to_fit_pdf ~fast Cpdfposition.Diagonal 1. (many (width, height) endpage) () pdf all
