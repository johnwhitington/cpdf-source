(* Build PDF Presentations *)
open Pdfutil

let change_page_effect t d horizontal inward direction effect_duration page =
  let checkname = function
    | "Split" | "Blinds" | "Box" | "Wipe" | "Dissolve" | "Glitter" -> ()
    | _ -> Cpdferror.error "Unknown presentation type"
  in
    let rest = page.Pdfpage.rest in
      let transdict =
        match t with
        | None ->
            Pdf.Dictionary []
        | Some name ->
            checkname name;
            Pdf.Dictionary [("/S", Pdf.Name ("/" ^ name))]
      in
        let transdict = Pdf.add_dict_entry transdict "/D" (Pdf.Real effect_duration) in
          let transdict =
            match t with
            | Some ("Split" | "Blinds") ->
                Pdf.add_dict_entry
                  transdict "/Dm" (Pdf.Name (if horizontal then "/H" else "/V"))
            | _ -> transdict
          in
           let transdict =
             match t with
             | Some ("Split" | "Box") ->
                 Pdf.add_dict_entry
                   transdict "/M" (Pdf.Name (if inward then "/I" else "/O"))
             | _ -> transdict
           in 
             let transdict =
               match t with
               | Some ("Wipe" | "Glitter") ->
                   Pdf.add_dict_entry transdict "/Di" (Pdf.Integer direction)
               | _ -> transdict
             in
              let rest = Pdf.add_dict_entry rest "/Trans" transdict in
                let rest =
                  match d with
                  | None -> Pdf.remove_dict_entry rest "/Dur"
                  | Some delay -> Pdf.add_dict_entry rest "/Dur" (Pdf.Real delay)
                in
                  {page with Pdfpage.rest = rest}

let presentation range t d h i dir effect_dur pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pages' =
      map2
        (fun page num ->
          if mem num range
            then change_page_effect t d h i dir effect_dur page
            else page)
        pages
        (indx pages)
    in
      Pdfpage.change_pages true pdf pages'

