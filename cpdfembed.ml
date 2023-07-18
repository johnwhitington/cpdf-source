(* Embed a font *)
open Pdfutil

type t = Pdftext.font list * (int, int * int) Hashtbl.t (* Table returns font number and charcode for given unicode codepoint *)

type cpdffont =
  PreMadeFontPack of t
| EmbedInfo of {fontfile : Pdfio.bytes; fontname : string; encoding : Pdftext.encoding}
| ExistingNamedFont

let fontpack_of_standardfont sf =
  let te = Pdftext.text_extractor_of_font_real sf in
  let table = null_hash () in
  for x = 0 to 255 do
    let u = hd (Pdftext.codepoints_of_text te (string_of_char (char_of_int x))) in
      Hashtbl.add table u (0, x)
  done;
  ([sf], table)

let get_char (fonts, table) u =
  match Hashtbl.find table u with
  | (n, charcode) -> Some (charcode, n, List.nth fonts n)
  | exception Not_found -> None

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
          (fun u ->
            match charcode_extractor u with
            | Some x -> Hashtbl.add table u (i, x)
            | None -> Printf.printf "charcode_extractor could not find char in make_fontpack_hashtable\n")
          subset)
    indexes fs;
  table

let embed_truetype pdf ~fontfile ~fontname ~codepoints ~encoding =
  let fs = Cpdftruetype.parse ~subset:codepoints fontfile encoding in
  let subsets_and_their_fonts = map (make_single_font ~fontname ~encoding pdf) fs in
    (map snd subsets_and_their_fonts, make_fontpack_hashtable subsets_and_their_fonts)

let rec collate_runs cfn a = function
  | [] -> rev (map rev a)
  | (charcode, fontnum, font) as h::t ->
      match a with
      | [] -> collate_runs fontnum [[h]] t
      | this::rest ->
          if fontnum = cfn
            then collate_runs cfn ((h::this)::rest) t
            else collate_runs fontnum ([h]::this::rest) t

let collate_runs = function
  | [] -> []
  | (_, fontnum, _)::_ as l -> collate_runs fontnum [] l
