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
  let open Pdftext in
  let fontmetrics =
    let a = Array.make 256 0. in
      for x = f.TT.firstchar to f.TT.lastchar do
        a.(x) <- float_of_int (f.TT.widths.(x - f.TT.firstchar))
      done;
      a
  in
  SimpleFont
    {fonttype = Truetype;
     basefont = Printf.sprintf "/%s+%s" name_1 fontname;
     fontmetrics = Some fontmetrics;
     firstchar = f.TT.firstchar;
     lastchar = f.TT.lastchar;
     widths = f.TT.widths;
     fontdescriptor = Some
       {ascent = float_of_int f.TT.ascent;
        descent = float_of_int f.TT.descent;
        avgwidth = float_of_int f.TT.avgwidth;
        maxwidth = float_of_int f.TT.maxwidth;
        flags = f.TT.flags;
        italicangle = float_of_int f.TT.italicangle;
        capheight = float_of_int f.TT.capheight;
        xheight = float_of_int f.TT.xheight;
        stemv = float_of_int f.TT.stemv;
        fontbbox = (float_of_int f.TT.minx, float_of_int f.TT.miny,
                    float_of_int f.TT.maxx, float_of_int f.TT.maxy);
        fontfile = Some (FontFile2 fontfile_num);
        charset = None;
        tounicode = None};
     encoding} 
