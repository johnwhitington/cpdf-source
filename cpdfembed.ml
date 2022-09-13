(* Embed a font *)
open Pdfutil

(* For the first stage of our embedder, we are only allowing Latin, and we don't actually subset.
  a) Get a list of Unicode codepoints;
  b) See which of them are in the glyph list;
  c) See which of those are in (StdEncoding|MacRomanEncoding|WinAnsiEncoding), and get their codes;
  d) Build a font to do just those;
  e) We put missing glyph or similar for any character not in the encoding
  (* FUTURE *)
  1) Actually subset the font to save size
  2) Allow characters not in the standard encodings by builing one or more secondary subsets *)

(*let () =
  iter
    (fun u ->
       Printf.printf "unicode %i --> " u;
       let glyphname = Hashtbl.find glyphlist_table [u] in
         Printf.printf "glyph name %s --> " glyphname;
         let pdfcode = Hashtbl.find encoding_table glyphname in
         Printf.printf "pdf code %i\n" pdfcode)
    unicodepoints *)

let pdfcode_of_unicode_codepoint encoding_table glyphlist_table u =
  try
    Some (Hashtbl.find encoding_table (Hashtbl.find glyphlist_table [u]))
  with
    Not_found -> None

let calc_accepted_unicodepoints encoding_table glyphlist_table codepoints =
  setify
    (option_map
       (fun u ->
          match
            pdfcode_of_unicode_codepoint encoding_table glyphlist_table u
          with
          | Some _ -> Some u
          | None -> None)
       codepoints)

let fontnum = ref 0

let basename () =
  incr fontnum;
  "AAAAA" ^ string_of_char (char_of_int (!fontnum + 65))

let string_of_encoding = function
  | Pdftext.WinAnsiEncoding -> "/WinAnsiEncoding"
  | Pdftext.MacRomanEncoding -> "/MacRomanEncoding"
  | Pdftext.StandardEncoding -> "/StandardEncoding"
  | _ -> failwith "unknown encoding"

(* FIXME add "" = full subset *)
let embed_truetype pdf ~fontfile ~fontname ~text ~encoding =
  let unicodepoints = Pdftext.codepoints_of_utf8 text in
  let glyphlist_table = Pdfglyphlist.reverse_glyph_hashes () in 
  let encoding_table = Pdftext.reverse_table_of_encoding encoding in
  let accepted_unicodepoints =
    map
      (fun u ->
        (u, pdfcode_of_unicode_codepoint encoding_table glyphlist_table u))
      (calc_accepted_unicodepoints
         encoding_table glyphlist_table unicodepoints)
  in
  let f = Cpdftruetype.parse ~subset:accepted_unicodepoints fontfile in
  let widths =
    Pdf.Array
      (map (fun x -> Pdf.Integer x)
      (Array.to_list f.Cpdftruetype.widths))
  in
  let name_1 = basename () in
  let fontfile =
    let len = Pdfio.bytes_size fontfile in
    Pdf.Stream
      {contents =
         (Pdf.Dictionary
            [("/Length", Pdf.Integer len); ("/Length1", Pdf.Integer len)],
          Pdf.Got fontfile)}
  in
  let fontfile_num = Pdf.addobj pdf fontfile in
  let module TT = Cpdftruetype in
  let fontdescriptor =
    Pdfread.parse_single_object
      (Printf.sprintf
         "<</Type/FontDescriptor/FontName/%s+%s/Flags %i/FontBBox[%i %i %i %i] \
            /ItalicAngle %i/Ascent %i/Descent %i/CapHeight %i/StemV %i/XHeight \
            %i/AvgWidth %i/MaxWidth %i/FontFile2 %i 0 R>>"
         name_1 fontname f.TT.flags f.TT.minx f.TT.miny f.TT.maxx f.TT.maxy
         f.TT.italicangle f.TT.ascent f.TT.descent f.TT.capheight f.TT.stemv
         f.TT.xheight f.TT.avgwidth f.TT.maxwidth fontfile_num)
  in
  let fontdesc_num = Pdf.addobj pdf fontdescriptor in
  let font =
    Pdf.add_dict_entry
      (Pdfread.parse_single_object
        (Printf.sprintf
           "<</Type/Font/Subtype/TrueType/BaseFont/%s+%s/FontDescriptor %i 0 R\
              /Encoding%s/FirstChar %i/LastChar %i>>"
           name_1 fontname fontdesc_num (string_of_encoding encoding)
           f.TT.firstchar f.TT.lastchar))
      "/Widths"
      widths
  in
    Pdf.addobj pdf font

(* For now, to get a Pdftext.font, we build it with the function above using an empty (i.e. full) subset, put it in an empty PDF and then read it back. This will be fixed later. *)
let font_of_truetype ~fontfile ~fontname ~encoding =
  let pdf = Pdf.empty () in
  let fontobjnum = embed_truetype pdf ~fontfile ~fontname ~text:"" ~encoding in
  Pdftext.read_font pdf (Pdf.lookup_obj pdf fontobjnum)
