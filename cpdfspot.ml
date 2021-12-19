open Pdfutil

let print_spot_colour n s =
  Printf.printf "%i %s\n" n s

let list_spot_colours pdf =
  Pdf.objiter
    (fun _ obj ->
       match obj with
         Pdf.Array (Pdf.Name "/Separation"::x::_) ->
           begin match Pdf.direct pdf x with
             Pdf.Name col -> Printf.printf "%s\n" col
           | _ -> ()
           end
       | _ -> ())
    pdf

