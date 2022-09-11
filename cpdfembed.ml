(* Truetype font embedding example *)
open Pdfutil

(* For the first stage of our embedder, we are only allowing standard encodings, and we don't actually subset.
  a) Get a list of Unicode codepoints;
  b) See which of them are in the glyph list;
  c) See which of those are in (StdEncoding|MacRomanEncoding|WinAnsiEncoding), and get their codes;
  d) Build a font to do just those;
  e) We put question marks for any character not in the encoding
  (* FUTURE *)
  1) Actually subset the font to save size
  2) Allow characters not in the standard encodings by builing one or more secondary subsets *)

(* UTF8 Input text *)
let text = "Noto Sans Black Àë"
let encoding = Pdftext.MacRomanEncoding

let unicodepoints = Pdftext.codepoints_of_utf8 text

let glyphlist_table = Pdfglyphlist.reverse_glyph_hashes ()
  
let encoding_table = Pdftext.reverse_table_of_encoding encoding

(*let () =
  iter
    (fun u ->
       Printf.printf "unicode %i --> " u;
       let glyphname = Hashtbl.find glyphlist_table [u] in
         Printf.printf "glyph name %s --> " glyphname;
         let pdfcode = Hashtbl.find encoding_table glyphname in
         Printf.printf "pdf code %i\n" pdfcode)
    unicodepoints
 
let pdfcode_of_unicode_codepoint u =
  try
    Some (Hashtbl.find encoding_table (Hashtbl.find glyphlist_table [u]))
  with
    Not_found -> None

let tj_text =
  implode
    (map
      (fun x -> match pdfcode_of_unicode_codepoint x with Some c -> char_of_int c | None -> '?')
      unicodepoints)

let calc_accepted_unicodepoints codepoints =
  setify
    (option_map
       (fun u -> match pdfcode_of_unicode_codepoint u with Some _ -> Some u | None -> None)
       codepoints)

let accepted_unicodepoints =
  map
    (fun u -> (u, pdfcode_of_unicode_codepoint u))
    (calc_accepted_unicodepoints unicodepoints)

let contents_of_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let fontname = "NotoSans-Black"
let fontstr = contents_of_file (fontname ^ ".ttf")

let f =
  Cpdftruetype.parse ~subset:accepted_unicodepoints (Pdfio.bytes_of_string fontstr)

let contents =
  "1 0 0 1 50 770 cm BT/TT1 36 Tf(" ^ tj_text ^ ")Tj ET"

let widths =
  Pdf.Array (map (fun x -> Pdf.Integer x) (Array.to_list f.Cpdftruetype.widths))

let fontnum = ref 0

let basename () =
  incr fontnum;
  "AAAAA" ^ string_of_char (char_of_int (!fontnum + 65))

let name_1 = basename ()

let string_of_encoding = function
  | Pdftext.WinAnsiEncoding -> "/WinAnsiEncoding"
  | Pdftext.MacRomanEncoding -> "/MacRomanEncoding"
  | Pdftext.StandardEncoding -> "/StandardEncoding"
  | _ -> failwith "unknown encoding"

let font =
  Pdf.add_dict_entry
    (Pdfread.parse_single_object
      (Printf.sprintf "<</Type/Font/Subtype/TrueType/BaseFont/%s+%s/FontDescriptor 6 0 R/Encoding%s/FirstChar %i/LastChar %i>>" name_1 fontname (string_of_encoding encoding) f.Cpdftruetype.firstchar f.Cpdftruetype.lastchar))
    "/Widths"
    widths

let fontdescriptor =
  Pdfread.parse_single_object
    (Printf.sprintf "<</Type/FontDescriptor/FontName/%s+%s/Flags %i/FontBBox[%i %i %i %i]/ItalicAngle %i/Ascent %i/Descent %i/CapHeight %i/StemV %i/XHeight %i/AvgWidth %i/MaxWidth %i/FontFile2 7 0 R>>"
    name_1 fontname f.Cpdftruetype.flags f.Cpdftruetype.minx f.Cpdftruetype.miny f.Cpdftruetype.maxx f.Cpdftruetype.maxy f.Cpdftruetype.italicangle
    f.Cpdftruetype.ascent f.Cpdftruetype.descent f.Cpdftruetype.capheight f.Cpdftruetype.stemv f.Cpdftruetype.xheight f.Cpdftruetype.avgwidth f.Cpdftruetype.maxwidth)

let fontfile =
  let len = String.length fontstr in
  Pdf.Stream
    {contents =
       (Pdf.Dictionary [("/Length", Pdf.Integer len); ("/Length1", Pdf.Integer len)],
        Pdf.Got (Pdfio.bytes_of_string fontstr))}
*)

(*let objects =
  [(1, Pdfread.parse_single_object "<</Type/Catalog/Pages 2 0 R>>");
   (2, Pdfread.parse_single_object "<</Type/Pages/Kids[3 0 R]/Count 1>>");
   (3, Pdfread.parse_single_object "<</Type/Page/Resources<</Font <</TT1 5 0 R>>>>/Parent 2 0 R/MediaBox[0 0 595 842]/Rotate 0/Contents[4 0 R]>>");
   (4, Pdf.Stream
         {contents = (Pdf.Dictionary [("/Length", Pdf.Integer (String.length contents))],
                     (Pdf.Got (Pdfio.bytes_of_string contents)))});
   (5, font);
   (6, fontdescriptor);
   (7, fontfile);
  ]

let root = 1

let trailerdict =
  Pdfread.parse_single_object (Printf.sprintf "<</Root 1 0 R/Size %i>>" (length objects + 1))

let pdf =
  let pdf =
   {Pdf.major = 2;
    Pdf.minor = 0;
    Pdf.root = root;
    Pdf.objects =
      {Pdf.maxobjnum = 0;
       Pdf.parse = None;
       Pdf.pdfobjects = Pdf.pdfobjmap_empty ();
       Pdf.object_stream_ids = null_hash ()};
    Pdf.trailerdict = trailerdict;
    Pdf.was_linearized = false;
    Pdf.saved_encryption = None}
  in
    iter (Pdf.addobj_given_num pdf) objects;
    pdf

let () =
  Pdfwrite.pdf_to_file pdf "subset.pdf"*)
