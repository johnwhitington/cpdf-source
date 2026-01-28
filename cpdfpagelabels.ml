open Pdfutil

(* Split the given range (which is in order) into multiple contiguous ones. *)
let rec ranges_of_range curr prev = function
  | [] -> begin match curr with [] -> rev prev | _ -> rev (rev curr::prev) end
  | x::xs ->
      match curr with
      | [] -> ranges_of_range [x] prev xs
      | c::cs when x = c + 1 -> ranges_of_range (x::curr) prev xs
      | cs -> ranges_of_range [x] (rev cs::prev) xs

(* Predicate which is true if at least one page range starts at page 1 *)
let page1 labels =
  mem true (map (function l -> l.Pdfpagelabels.startpage = 1) labels)

let add_page_labels pdf progress style prefix startval range =
  let ranges = map extremes (ranges_of_range [] [] range)
  and labels = Pdfpagelabels.read pdf in
    assert (length ranges > 0);
    let startval_additions =
      let r = ref [] in
      let sofar = ref 0 in
        iter (fun (s, e) -> r := !sofar :: !r; sofar := e - s + 1 + !sofar) ranges;
        rev !r
    in
    let labels =
      if not (page1 labels) then
        ref
          ({Pdfpagelabels.labelstyle = Pdfpagelabels.DecimalArabic;
            Pdfpagelabels.labelprefix = None;
            Pdfpagelabels.startpage = 1;
            Pdfpagelabels.startvalue = 1}::labels)
      else
        ref labels
    in
      iter2
        (fun (s, e) addition ->
           let label =
             {Pdfpagelabels.labelstyle = style;
              Pdfpagelabels.labelprefix = prefix;
              Pdfpagelabels.startpage = s;
              Pdfpagelabels.startvalue = startval + if progress then addition else 0}
           in
             labels := Pdfpagelabels.add_label (Pdfpage.endpage pdf) !labels label e)
        ranges
        startval_additions;
        Pdfpagelabels.write pdf !labels

let add_page_labels_json pdf json = ()

