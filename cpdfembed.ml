(* Embed a font *)
open Pdfutil

type t = Pdftext.font list * (int, int * int) Hashtbl.t

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

let make_single_font ~fontname ~encoding pdf f = 
  let name_1 = basename () in
  let module TT = Cpdftruetype in
  let fontfile =
    let len = Pdfio.bytes_size f.TT.subset_fontfile in
    Pdf.Stream
      {contents =
         (Pdf.Dictionary
            [("/Length", Pdf.Integer len); ("/Length1", Pdf.Integer len)],
          Pdf.Got f.TT.subset_fontfile)}
  in
  let fontfile_num = Pdf.addobj pdf fontfile in
  let open Pdftext in
  let fontmetrics =
    let a = Array.make 256 0. in
      for x = f.TT.firstchar to f.TT.lastchar do
        a.(x) <- float_of_int (f.TT.widths.(x - f.TT.firstchar))
      done;
      a
  in
  (f.TT.subset,
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
        tounicode = f.TT.tounicode};
     encoding})

let make_fontpack_hashtable fs =
  let indexes = indx0 fs in
  let table = null_hash () in
  iter2
    (fun i (subset, f) ->
      let charcode_extractor = Pdftext.charcode_extractor_of_font_real f in
        iter
          (fun u -> Hashtbl.add table u (i, unopt (charcode_extractor u)))
          subset)
    indexes fs;
  table

let embed_truetype pdf ~fontfile ~fontname ~codepoints ~encoding =
  let glyphlist_table = Pdfglyphlist.reverse_glyph_hashes () in 
  let encoding_table = Pdftext.reverse_table_of_encoding encoding in
  let accepted_unicodepoints = calc_accepted_unicodepoints encoding_table glyphlist_table codepoints in
  let fs = Cpdftruetype.parse ~subset:accepted_unicodepoints fontfile encoding in
  let subsets_and_their_fonts =  map (make_single_font ~fontname ~encoding pdf) fs in
    (map snd subsets_and_their_fonts, make_fontpack_hashtable subsets_and_their_fonts)
