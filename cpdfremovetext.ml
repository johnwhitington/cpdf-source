open Pdfutil

let removetext range pdf =
  let rec remove_until_last_EMC level = function
    | [] -> []
    | Pdfops.Op_BMC "/CPDFSTAMP"::more ->
        remove_until_last_EMC (level + 1) more
    | Pdfops.Op_EMC::more ->
        if level = 1
          then more
          else remove_until_last_EMC (level - 1) more
    | _::more ->
        remove_until_last_EMC level more
  in
    let rec remove_stamps prev = function
      | [] -> rev prev
      | Pdfops.Op_BMC "/CPDFSTAMP"::more ->
          let rest = remove_until_last_EMC 1 more in
            remove_stamps prev rest
      | h::t -> remove_stamps (h::prev) t
    in
      let removetext_page _ page =
        {page with
           Pdfpage.content =
             let ops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content in
               [Pdfops.stream_of_ops (remove_stamps [] ops)]}
      in
        Cpdfpage.process_pages (Pdfpage.ppstub removetext_page) pdf range

let remove_all_text_ops ?(only_invisible=false) trm pdf resources content =
  let is_textop = function
    Pdfops.Op_Tj _ | Pdfops.Op_' _ | Pdfops.Op_'' _ | Pdfops.Op_TJ _ -> true
  | _ -> false
  in
    let trm = ref trm in
    let content' =
      let ops = Pdfops.parse_operators pdf resources content in
      let ops' =
        option_map
          (function op ->
            if is_textop op && !trm = 3 then None else
              match op with
              | Pdfops.Op_Tr n -> trm := n; Some op
              | op -> Some op)
          ops
      in
        Pdfops.stream_of_ops ops'

    in
      [content']

let remove_all_text_page ?(only_invisible=false) pdf p =
  let resources = p.Pdfpage.resources in
  let content = p.Pdfpage.content in
    (* TODO In the case of only_invisible, a devious file could legitimately
       set the text rendering mode to invisible in the main page, then have the
       text sections in XObjects, and we wouldn't catch it. When or if we have
       an example file for this, we can replace process_xobjects here with
       explicit handling of Op_Do in the main loop. (In fact, a very devious
       file could set the text rendering mode multiple times to different
       values and call the same XObject, but that would require deduplication
       of any xobjects where the state upon entry differs. We don't have that
       facility yet either. *)
    Pdfpage.process_xobjects pdf p (remove_all_text_ops ~only_invisible 0);
    {p with Pdfpage.content = (remove_all_text_ops ~only_invisible 0) pdf resources content}, pdf

let remove_all_text ?(only_invisible=false) range pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pagenums = indx pages in
    let pdf = ref pdf in
    let pages' = ref [] in
      iter2 
        (fun p pagenum ->
          Cpdfutil.progress_page pagenum;
          let p', pdf' =
            if mem pagenum range
              then remove_all_text_page !pdf p
              else p, !pdf
          in
            Cpdfutil.progress_endpage ();
            pdf := pdf';
            pages' =| p')
        pages
        pagenums;
      let r = Pdfpage.change_pages true !pdf (rev !pages') in
        Cpdfutil.progress_done ();
        r
