open Pdfutil
open Pdfio
open Cpdferror

let add_xobject_to_page xobjname xobjnum page pdf =
  let resources' =
    let xobjects =
      match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
      | Some xobjects -> xobjects
      | _ -> Pdf.Dictionary []
    in
    let new_xobjects =
      Pdf.add_dict_entry xobjects xobjname (Pdf.Indirect xobjnum)
    in
      Pdf.add_dict_entry page.Pdfpage.resources "/XObject" new_xobjects
  in
    {page with Pdfpage.resources = resources'}

let add_page_as_xobject pdf range page name =
  let xobject_data =
    match Pdfops.stream_of_ops (Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content) with
      Pdf.Stream {contents = (_, Got b)} -> b
    | _ -> assert false
  in
  let xobject_dict =
     ["/Type", Pdf.Name "/XObject";
      "/Subtype", Pdf.Name "/Form";
      "/BBox", page.Pdfpage.mediabox;
      "/Resources", page.Pdfpage.resources;
      "/Length", Pdf.Integer (bytes_size xobject_data)]
  in
    let xobject =
      Pdf.Stream {contents = (Pdf.Dictionary xobject_dict, Pdf.Got xobject_data)}
    in
      let xobject_objnum = Pdf.addobj pdf xobject in
      let pages = Pdfpage.pages_of_pagetree pdf in
      let new_pages =
        List.map2
          (fun page pnum ->
             if mem pnum range
               then add_xobject_to_page name xobject_objnum page pdf
               else page)
          pages
          (indx pages)
      in
        Pdfpage.change_pages true pdf new_pages

(* n.b the use of change_pages here ensures no inheritable resources in the
 * stamp, therefore creation of xobject from page is as simple as expected. *)
let stamp_as_xobject pdf range over =
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
                let new_pages = under_pages in
                  let changed =
                    let changes =
                      map (fun x -> (x, x)) (ilist 1 (length new_pages))
                    in
                      Pdfpage.change_pages ~changes true merged new_pages
                  in
                    let new_refnumbers = Pdf.page_reference_numbers changed in
                    let changetable = hashtable_of_dictionary (combine marks_refnumbers new_refnumbers) in
                    let new_marks = map (Cpdfbookmarks.change_bookmark changetable) marks in
                    let pdf = Pdfmarks.add_bookmarks new_marks changed in
                    let name = "/" ^ Pdfpage.shortest_unused_prefix pdf ^ "CPDFXObj" in
                      (add_page_as_xobject pdf range over_page name, name)
