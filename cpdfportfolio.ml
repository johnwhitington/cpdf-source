open Pdfutil
open Pdfio

type entry =
  {filename : string;
   relationship : Pdf.pdfobject;
   description : string}

(* Assumes no duplicate file names. *)
let portfolio pdf filenames =
  let embedded_files =
    map
      (fun {filename} ->
        let basename = Filename.basename filename in
        let bytes = bytes_of_string (contents_of_file filename) in
        let contents =
          Pdf.addobj pdf
            (Pdf.Stream {contents = Pdf.Dictionary
              [("/Params", Pdf.Dictionary [("/Size", Pdf.Integer (bytes_size bytes))]);
               ("/Length", Pdf.Integer (bytes_size bytes))], Pdf.Got bytes})
        in
          Pdf.Indirect (Pdf.addobj pdf (Pdf.Dictionary
            [("/Type", Pdf.Name "/FileSpec");
             ("/Desc", Pdf.String basename);
             ("/F", Pdf.String basename);
             ("/UF", Pdf.String basename);
                          ("/AFRelationship", Pdf.Name "/Unspecified");
             ("/EF", Pdf.Dictionary [("/F", Pdf.Indirect contents); ("/UF", Pdf.Indirect contents)])])))
      filenames
  in
  let name_tree =
    Pdf.Indirect (Pdf.addobj pdf (Pdftree.build_name_tree false pdf (map2 (fun {filename} e -> (Filename.basename filename, e)) filenames embedded_files)))
  in
    Pdf.replace_chain pdf ["/Root"; "/Names"; "/EmbeddedFiles"] name_tree;
    Pdf.replace_chain pdf ["/Root"; "/Collection"] (Pdf.Dictionary [("/View", Pdf.Name "/T")])
