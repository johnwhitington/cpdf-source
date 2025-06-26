open Pdfutil
open Pdfio

type entry =
  {filename : string;
   relationship : string option;
   description : string option}

let process_afrelationship = function
  | None -> "/Unspecified"
  | Some x -> "/" ^ x

(* Build a PDF portfolio a.k.a collection *)
let portfolio pdf filenames =
  let embedded_files =
    map
      (fun e ->
        let basename = Filename.basename e.filename in
        let bytes = bytes_of_string (contents_of_file e.filename) in
        let contents =
          Pdf.addobj pdf
            (Pdf.Stream {contents = Pdf.Dictionary
              [("/Params", Pdf.Dictionary [("/ModDate", Pdf.String (Cpdfmetadata.expand_date "now")); ("/Size", Pdf.Integer (bytes_size bytes))]);
               ("/Length", Pdf.Integer (bytes_size bytes))], Pdf.Got bytes})
        in
          Pdf.Indirect (Pdf.addobj pdf (Pdf.Dictionary
            [("/Type", Pdf.Name "/FileSpec");
             ("/Desc", Pdf.String (match e.description with Some x -> x | None -> basename));
             ("/F", Pdf.String basename);
             ("/UF", Pdf.String basename);
                          ("/AFRelationship", Pdf.Name (process_afrelationship e.relationship));
             ("/EF", Pdf.Dictionary [("/F", Pdf.Indirect contents); ("/UF", Pdf.Indirect contents)])])))
      filenames
  in
  (* We need unique keys, even if basenames are not unique... *)
  let keys = map string_of_int (indx0 filenames) in
  let name_tree =
    Pdf.Indirect (Pdf.addobj pdf (Pdftree.build_name_tree false pdf (map3 (fun k {filename} e -> (k, e)) keys filenames embedded_files)))
  in
    Pdf.replace_chain pdf ["/Root"; "/Names"; "/EmbeddedFiles"] name_tree;
    Pdf.replace_chain pdf ["/Root"; "/Collection"] (Pdf.Dictionary [("/View", Pdf.Name "/T")])
