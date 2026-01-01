open Pdfutil
open Pdfio

type t =
  {filename : string;
   relationship : string option;
   description : string option}

let process_afrelationship = function
  | None -> "/Unspecified"
  | Some x -> "/" ^ x

(* Build a PDF portfolio a.k.a collection *)
let portfolio pdf entries =
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
      entries
  in
  (* We need unique keys, even if basenames are not unique. We use a number
     followed by a forward slash. Since the forward slash is illegal in a
     filename, prefixing with a number cannot accidentally produce another of
     the names. *)
  let keys = map (fun (n, e) -> string_of_int n ^ "/" ^ e.filename) (combine (indx0 entries) entries) in
  assert (length (setify keys) = length keys);
  let name_tree =
    Pdf.Indirect (Pdf.addobj pdf (Pdftree.build_name_tree false pdf (map3 (fun k {filename} e -> (k, e)) keys entries embedded_files)))
  in
    Pdf.replace_chain pdf ["/Root"; "/Names"; "/EmbeddedFiles"] name_tree;
    Pdf.replace_chain pdf ["/Root"; "/Collection"] (Pdf.Dictionary [("/View", Pdf.Name "/T")])
