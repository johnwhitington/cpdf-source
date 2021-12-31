open Pdfutil

(* Padding with blank pages. *)
let insert_after pos page pages =
  let before, after = cleave pages pos in
    before @ [page] @ after

(* Insert many. *)
let rec insert_after_many pages = function
  | [] -> pages
  | (pos, page)::more ->
      let pages' = insert_after pos page pages in
        insert_after_many pages' (map (fun (p, pa) -> p + 1, pa) more)

(* For each pagenum in the range, increment the count by padsize, and carry on. e.g
insert_after_many_changes 1 0 [2] [1; 2; 3] --> [(1, 1); (2, 2); (3, 4)] *)
let rec insert_after_many_changes isbefore padsize offset range = function
  [] -> []
| h::t ->
    let item = (h, h + offset + if isbefore && mem h range then 1 else 0) in
      if mem h range then
        item::insert_after_many_changes isbefore padsize (offset + padsize) range t
      else
        item::insert_after_many_changes isbefore padsize offset range t

let print_changes =
  iter (fun (f, t) -> Printf.printf "%i --> %i\n" f t)

let pad_with_pdf (range : int list) (pdf : Pdf.t) (isbefore : bool) (padfile : Pdf.t) =
  let range = sort compare (setify range) in
  let merged =
    Pdfmerge.merge_pdfs
      false false ["a"; "b"] [pdf; padfile] [ilist 1 (Pdfpage.endpage pdf); ilist 1 (Pdfpage.endpage padfile)]
  in
  let original_pages, padpages =
    cleave (Pdfpage.pages_of_pagetree merged) (Pdfpage.endpage pdf) 
  in
    let newpages =
      map
        (fun (pagenum, page) ->
           if mem pagenum range then
             (if isbefore then padpages @ [page] else [page] @ padpages)
           else
             [page])
        (combine (indx original_pages) original_pages)
    in
      (* FIXME Provide ~changes here? *)
      Pdfpage.change_pages false merged (flatten newpages)

let pad padwith range pdf isbefore =
  match padwith with
    Some padfile -> pad_with_pdf range pdf isbefore padfile
  | None ->
      let i = if isbefore then 1 else 0 in
      let pages = Pdfpage.pages_of_pagetree pdf in
        let blankpages =
          map
            (fun n ->
               {Pdfpage.content = [];
                Pdfpage.mediabox = (select (n + i) pages).Pdfpage.mediabox;
                Pdfpage.resources = Pdf.Dictionary [];
                Pdfpage.rotate = (select (n + i) pages).Pdfpage.rotate;
                Pdfpage.rest = Pdf.remove_dict_entry ((select (n + i) pages).Pdfpage.rest) "/Annots"})
            range
        in
          let pages' = insert_after_many pages (combine range blankpages) in
          let changes =
            insert_after_many_changes
              isbefore 1 0 (map (fun x -> x + i) range) (ilist 1 (length pages))
          in
            Pdfpage.change_pages ~changes true pdf pages'

let padafter ?padwith range pdf =
  let isinpdf n = mem n (ilist 1 (Pdfpage.endpage pdf)) in
    if not (fold_left ( && ) true (map isinpdf range)) then
      raise (Failure "padafter: range contains pages not present in pdf");
    pad padwith range pdf false

let padbefore ?padwith range pdf =
  let isinpdf n = mem n (ilist 1 (Pdfpage.endpage pdf)) in
    if not (fold_left ( && ) true (map isinpdf range)) then
      raise (Failure "padbefore: range contains pages not present in pdf");
    pad padwith (map pred range) pdf true

let padmultiple n pdf =
  let neg, n = n < 0, if n < 0 then -n else n in
  let pages = Pdfpage.pages_of_pagetree pdf in
    let len = length pages in
      let pages_to_add = if len / n * n = len then 0 else n - (len mod n) in
        if pages_to_add > 0 then
          let blankpages =
            many
              {Pdfpage.content = [];
               Pdfpage.mediabox = (select len pages).Pdfpage.mediabox;
               Pdfpage.resources = Pdf.Dictionary [];
               Pdfpage.rotate = (select len pages).Pdfpage.rotate;
               Pdfpage.rest = Pdf.Dictionary []}
              pages_to_add
          in
            let changes = map (fun x -> (x, x)) (ilist 1 (length pages)) in
              Pdfpage.change_pages ~changes true pdf (if neg then blankpages @ pages else pages @ blankpages)
        else
          pdf

