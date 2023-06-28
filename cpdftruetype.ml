(* Truetype font parsing and subsetting *)
open Pdfutil
open Pdfio

(* FIXME No need for bitstream - everything is byte based, so we can use a normal input *)
(* FIXME Don't create a second font unless we have to *)
(* FIXME Create third, fourth etc. font when we need to *)
(* FIXME Get rid of double-calling of this code to 1) make font then 2) collect chars then 3) subset it i.e the subset = [] stuff *)
let dbg = ref true

type t =
  {flags : int;
   minx : int;
   miny : int;
   maxx : int;
   maxy : int;
   italicangle : int;
   ascent : int;
   descent : int;
   capheight : int;
   stemv : int;
   xheight : int;
   avgwidth : int;
   maxwidth : int;
   firstchar : int;
   lastchar : int;
   widths : int array;
   subset_fontfile : Pdfio.bytes;
   subset : int list;
   tounicode : (int, string) Hashtbl.t option}

let debug_t t =
  let hex u =
    let b = Buffer.create 32 in
    String.iter (fun x -> Buffer.add_string b (Printf.sprintf "%02X" (int_of_char x))) u;
    Buffer.contents b
  in
    Printf.printf "firstchar: %i\n" t.firstchar;
    Printf.printf "lastchar: %i\n" t.lastchar;
    Printf.printf "widths:"; Array.iter (Printf.printf " %i") t.widths; Printf.printf "\n";
    Printf.printf "fontfile of length %i\n" (Pdfio.bytes_size t.subset_fontfile);
    Printf.printf "subset:"; iter (Printf.printf " U+%04X") t.subset; Printf.printf "\n";
    Printf.printf "tounicode:\n";
    begin match t.tounicode with
    | None -> Printf.printf "None";
    | Some table -> iter (fun (k, v) -> Printf.printf "%i --> U+%s\n" k (hex v)) (sort compare (list_of_hashtbl table))
    end;
    Printf.printf "\n"

let required_tables =
  ["head"; "hhea"; "loca"; "cmap"; "maxp"; "cvt "; "glyf"; "prep"; "hmtx"; "fpgm"]

(* 32-bit signed fixed-point number (16.16) returned as two ints *)
let read_fixed b =
  let a = getval_31 b 16 in
    let b = getval_31 b 16 in
      a, b

(* 16-bit unsigned integer *)
let read_ushort b = getval_31 b 16

(* 32-bit unsigned integer *)
let read_ulong b = getval_32 b 32

(* Signed byte *)
let read_byte b = getval_31 b 8

(* Signed short *)
let read_short b = sign_extend 16 (getval_31 b 16)

(* f2dot14 - 2 bit signed integer part, 14 bit unsigned fraction *)
let read_f2dot14 b =
  let v = read_ushort b in
    float_of_int (sign_extend 2 (v lsr 14)) +. (float_of_int (v land 0x3FFF) /. 16384.)

(* discard n bytes *)
let discard_bytes b n =
  for x = 1 to n do ignore (getval_31 b 8) done

let pdf_unit unitsPerEm x =
  int_of_float (float_of_int x *. 1000. /. float_of_int unitsPerEm +. 0.5)

let string_of_tag t =
  Printf.sprintf "%c%c%c%c"
    (char_of_int (i32toi (Int32.shift_right t 24)))
    (char_of_int (i32toi (Int32.logand 0x000000FFl (Int32.shift_right t 16))))
    (char_of_int (i32toi (Int32.logand 0x000000FFl (Int32.shift_right t 8))))
    (char_of_int (i32toi (Int32.logand 0x000000FFl t)))

let read_format_6_encoding_table b =
  let firstCode = read_ushort b in
  let entryCount = read_ushort b in
  let t = null_hash () in
    try
      for x = firstCode to firstCode + entryCount - 1 do
        Hashtbl.add t x (read_ushort b)
      done;
      t
    with
      e -> failwith ("bad format 6 table: " ^ Printexc.to_string e ^ "\n")

let read_magic_formula b glyphIndexArrayStart seg segCount ro c sc =
  if !dbg then Printf.printf "read_magic_formula: seg = %i, setCount = %i, ro = %i, c = %i, sc = %i\n" seg segCount ro c sc;
  let position = seg - segCount + ro / 2 + (c - sc) in
    if !dbg then Printf.printf "position is %i\n" position;
    b.input.seek_in (glyphIndexArrayStart + position * 2);
    b.bit <- 0;
    b.bitsread <- 0;
    b.currbyte <- 0;
    read_short b

let read_format_4_encoding_table b =
  let t = null_hash () in
  let segCountX2 = read_ushort b in
  let segCount = segCountX2 / 2 in
  let searchRange = read_ushort b in
  let entrySelector = read_ushort b in
  let rangeShift = read_ushort b in
  let endCodes = Array.init segCount (fun _ -> read_ushort b) in
  let _ (* reservedPad *) = read_ushort b in
  let startCodes = Array.init segCount (fun _ -> read_ushort b) in
  let idDelta = Array.init segCount (fun _ -> read_ushort b) in
  let idRangeOffset = Array.init segCount (fun _ -> read_ushort b) in
  let glyphIndexArrayStart = b.input.pos_in () in
    if !dbg then Printf.printf "glyphIndexArrayStart = %i\n" glyphIndexArrayStart;
    if !dbg then
    begin
    Printf.printf "segCount = %i, searchRange = %i, entrySelector = %i, rangeShift = %i\n" segCount searchRange entrySelector rangeShift;
    Printf.printf "endCodes\n";
    print_ints (Array.to_list endCodes);
    Printf.printf "startCodes\n";
    print_ints (Array.to_list startCodes);
    Printf.printf "idDelta\n";
    print_ints (Array.to_list idDelta);
    Printf.printf "idRangeOffset\n";
    print_ints (Array.to_list idRangeOffset);
    end;
    for seg = 0 to segCount - 1 do
      if !dbg then Printf.printf "Segment %i\n" seg;
      let ec = endCodes.(seg) in
      let sc = startCodes.(seg) in
      let del = idDelta.(seg) in
      let ro = idRangeOffset.(seg) in
        if !dbg then Printf.printf "sc = %i, ec = %i, del = %i, ro = %i\n" sc ec del ro;
        for c = sc to ec do
          if !dbg then Printf.printf "Code %i\n" c;
          if c != 0xFFFF then
            if ro = 0 then
              Hashtbl.add t c ((c + del) mod 65536)
            else
              begin
              if !dbg then flprint "format 4 magic required\n";
              let v = read_magic_formula b glyphIndexArrayStart seg segCount ro c sc in
                if !dbg then Printf.printf "Value %i returned\n" v;
                if v = 0
                  then Hashtbl.add t c (let r = (c + del) mod 65536 in if !dbg then Printf.printf "into hash %i\n" r; r)
                  else Hashtbl.add t c (let r = (v + del) mod 65536 in if !dbg then Printf.printf "into hash %i\n" r; r)
              end
        done
    done;
    t

let print_encoding_table format (table : (int, int) Hashtbl.t) =
  let unicodedata = Cpdfunicodedata.unicodedata () in
  let unicodetable = Hashtbl.create 16000 in
   iter
    (fun x ->
       Hashtbl.add unicodetable x.Cpdfunicodedata.code_value x.Cpdfunicodedata.character_name)
    unicodedata;
  let l = sort compare (list_of_hashtbl table) in
  if !dbg then Printf.printf "Format table %i: There are %i characters in this font\n" format (length l);
  iter
    (fun (c, gi) ->
      let str = Printf.sprintf "%04X" c in
        if !dbg then Printf.printf "Char %s (%s) is at glyph index %i\n" str (try Hashtbl.find unicodetable str with Not_found -> "Not_found") gi)
    l

let read_encoding_table fmt length version b =
  if !dbg then Printf.printf "********** format %i table has length, version %i, %i\n" fmt length version;
  match fmt with
  | 0 ->
      if !dbg then Printf.printf "read_encoding_table: format 0\n";
      let t = null_hash () in
        for x = 0 to 255 do Hashtbl.add t x (read_byte b) done;
        (*print_encoding_table 0 t;*)
        t
  | 4 ->
      if !dbg then Printf.printf "read_encoding_table: format 4\n";
      read_format_4_encoding_table b;
  | 6 ->
      if !dbg then Printf.printf "read_encoding_table: format 6\n";
      read_format_6_encoding_table b;
  | n -> raise (Pdf.PDFError "read_encoding_table: format %i not known\n")

let read_loca_table indexToLocFormat numGlyphs b =
  match indexToLocFormat with
  | 0 -> Array.init (numGlyphs + 1) (function _ -> i32ofi (read_ushort b * 2))
  | 1 -> Array.init (numGlyphs + 1) (function _ -> read_ulong b)
  | _ -> raise (Pdf.PDFError "Unknown indexToLocFormat in read_loca_table")

let write_loca_table subset cmap indexToLocFormat bs loca =
  let locnums = null_hash () in
    Hashtbl.add locnums 0 (); (* .notdef *)
    iter
      (fun u ->
         let locnum = Hashtbl.find cmap u in
           if !dbg then Printf.printf "write_loca_table: Unicode U+%04X is at location number %i\n" u locnum;
           Hashtbl.add locnums locnum ())
      subset;
  let write_entry loc position =
    match indexToLocFormat with
    | 0 -> putval bs 16 (i32div position 2l)
    | 1 -> putval bs 32 position
    | _ -> raise (Pdf.PDFError "Unknown indexToLocFormat in write_loca_table")
  in
  let pos = ref 0l in
  let pairs =
    map
      (fun loc ->
         let len = i32sub loca.(loc + 1) loca.(loc) in
         let r = (loc, !pos) in
           pos := i32add !pos len;
           r)
      (sort compare (map fst (list_of_hashtbl locnums)))
  in
    let pairs = Array.of_list (pairs @ [(Array.length loca - 1, !pos)]) in
    Array.iteri
      (fun i (loc, off) ->
         if i <> Array.length pairs - 1 then
           begin
             write_entry loc off;
             let loc', off' = pairs.(i + 1) in
             for x = 0 to loc' - loc - 2 do write_entry (loc + x) off' done
           end
         else
           write_entry loc off)
      pairs

(* Write the notdef glyf, and any others in the subset *)
let write_glyf_table subset cmap bs mk_b glyfoffset loca =
  if !dbg then Printf.printf "***write_glyf_table\n";
  let locnums = null_hash () in
    Hashtbl.add locnums 0 (); (* .notdef *)
    iter
      (fun u ->
         let locnum = Hashtbl.find cmap u in
         if !dbg then Printf.printf "write_glyf_table: Unicode U+%04X is at location number %i\n" u locnum;
           Hashtbl.add locnums locnum ())
      subset;
  let locnums = sort compare (map fst (list_of_hashtbl locnums)) in
  if !dbg then (Printf.printf "We want glyfs for locations: "; iter (Printf.printf "%i ") locnums; Printf.printf "\n");
    let byteranges = map (fun x -> (loca.(x), loca.(x + 1))) locnums in
    if !dbg then (Printf.printf "Byte ranges: "; iter (fun (a, b) -> Printf.printf "(%li, %li) " a b) byteranges; Printf.printf "\n");
  let len = List.fold_left i32add 0l (map (fun (a, b) -> i32sub b a) byteranges) in
  let write_bytes bs a l =
    if !dbg then Printf.printf "glyf: write_bytes %li %li\n" a l;
    let b = mk_b (i32toi (i32add glyfoffset a)) in
      for x = 1 to i32toi l do putval bs 8 (getval_32 b 8) done
  in
    iter (fun (a, b) -> write_bytes bs a (i32sub b a)) byteranges;
    let padding = 4 - i32toi len mod 4 in
    for x = 1 to padding do putval bs 8 0l done;
    len

let write_cmap_table subset cmap bs =
  if !dbg then Printf.printf "***write_cmap_table\n";
  let glyphindexes = [130; 131; 132] in
  putval bs 16 0l; (* table version number *)
  putval bs 16 1l; (* number of encoding tables *)
  putval bs 16 1l; (* platform ID *)
  putval bs 16 1l; (* platform encoding ID *)
  putval bs 32 12l; (* subtable offset = 12 bytes from beginning of table *)
  putval bs 16 6l; (* Table format 6 *)
  putval bs 16 (i32ofi (10 + 2 * length glyphindexes)); (* subtable length *)
  putval bs 16 0l;
  putval bs 16 33l; (* first character code *)
  putval bs 16 (i32ofi (length glyphindexes)); (* number of character codes *)
  iter (fun gi -> putval bs 16 (i32ofi gi)) glyphindexes; (* glyph indexes *)
  let len = i32ofi (22 + 2 * length glyphindexes) in
  let padding = 4 - i32toi len mod 4 in
  for x = 1 to padding do putval bs 8 0l done;
  len

let read_os2_table unitsPerEm b blength =
  let version = read_ushort b in
  if !dbg then Printf.printf "OS/2 table blength = %i bytes, version number = %i\n" blength version;
  let xAvgCharWidth = pdf_unit unitsPerEm (read_short b) in
  discard_bytes b 64; (* discard 14 entries usWeightClass...fsLastCharIndex *)
  (* -- end of original OS/2 Version 0 Truetype table. Must check length before reading now. *)
  let sTypoAscender = if blength > 68 then pdf_unit unitsPerEm (read_short b) else 0 in
  let sTypoDescender = if blength > 68 then pdf_unit unitsPerEm (read_short b) else 0 in
  discard_bytes b 6; (* discard sTypoLineGap...usWinDescent *)
  (* -- end of OpenType version 0 table *)
  discard_bytes b 8; (* discard ulCodePageRange1, ulCodePageRange2 *)
  (* -- end of OpenType version 1 table *)
  let sxHeight = if version < 2 then 0 else pdf_unit unitsPerEm (read_short b) in
  let sCapHeight = if version < 2 then 0 else pdf_unit unitsPerEm (read_short b) in
    (sTypoAscender, sTypoDescender, sCapHeight, sxHeight, xAvgCharWidth)

let read_post_table b =
  discard_bytes b 4; (* discard version *)
  let italicangle, n = read_fixed b in
    italicangle

(* (nb bit 1 is actualy bit 0 etc.) *)
let calculate_flags symbolic italicangle =
  let italic = if italicangle <> 0 then 1 else 0 in 
  let symbolic, nonsymbolic = if symbolic then 1, 0 else 0, 1 in
    32 lor (italic lsl 6) lor (symbolic lsl 3) lor (nonsymbolic lsl 5)

let calculate_limits subset =
  if subset = [] then (0, 255) else
    extremes (sort compare subset)

let calculate_stemv () = 0

let read_hhea_table b =
  discard_bytes b 34;
  read_ushort b (* numOfLongHorMetrics *)

let read_hmtx_table numOfLongHorMetrics b =
  Array.init
    numOfLongHorMetrics
    (fun _ -> let r = read_ushort b in ignore (read_short b); r)

(* For widths, we need the unicode code, not the unencoded byte *)
let unicode_codepoint_of_pdfcode encoding_table glyphlist_table p =
  try
    hd (Hashtbl.find glyphlist_table (Hashtbl.find encoding_table p))
  with
    Not_found -> 0

let calculate_widths unitsPerEm encoding firstchar lastchar subset cmapdata hmtxdata =
  if lastchar < firstchar then failwith "lastchar < firstchar" else
  (*if !dbg then List.iter (fun (a, b) -> Printf.printf "%i -> %i\n" a b) (sort compare (list_of_hashtbl cmapdata));*)
  let encoding_table = Pdftext.table_of_encoding encoding in
  let glyphlist_table = Pdfglyphlist.glyph_hashes () in
  Array.init
    (lastchar - firstchar + 1)
    (fun pos ->
       let code = pos + firstchar in
       if !dbg then Printf.printf "code %i --> " code;
       let code = unicode_codepoint_of_pdfcode encoding_table glyphlist_table code in
       if !dbg then Printf.printf "unicode %i --> " code;
       if subset <> [] && not (mem code subset) then 0 else
       try
         let glyphnum = Hashtbl.find cmapdata code in
           if !dbg then Printf.printf "glyph number %i --> " glyphnum;
           let width = hmtxdata.(glyphnum) in
           if !dbg then Printf.printf "width %i\n" width;
             pdf_unit unitsPerEm width
       with e -> if !dbg then Printf.printf "no width for %i\n" code; 0)

let calculate_width_higher unitsPerEm firstchar lastchar subset cmapdata hmtxdata =
 let subset = Array.of_list subset in
   Array.init
     (lastchar - firstchar + 1)
     (fun pos ->
        let glyphnum = Hashtbl.find cmapdata subset.(pos) in
        if !dbg then Printf.printf "glyph number %i --> " glyphnum;
          let width = hmtxdata.(glyphnum) in
          if !dbg then Printf.printf "width %i\n" width;
            pdf_unit unitsPerEm width)

let calculate_maxwidth unitsPerEm hmtxdata =
  pdf_unit unitsPerEm (hd (sort (fun a b -> compare b a) (Array.to_list hmtxdata)))

let padword n =
  let n = i32toi n in
  let r = n + (if n mod 4 = 0 then 0 else 4 - n mod 4) in
    i32ofi r

let fonumr = ref (-1)

let fonum () = fonumr += 1; !fonumr

let subset_font major minor tables indexToLocFormat subset encoding cmap loca mk_b glyfoffset data =
  let tables = Array.of_list (sort (fun (_, _, o, _) (_, _, o', _) -> compare o o') tables) in
  let tablesout = ref [] in
  let cut = ref 0l in
  if !dbg then Printf.printf "***Input:\n";
  Array.iteri
    (fun i (tag, checkSum, offset, ttlength) ->
      if !dbg then Printf.printf "tag = %li = %s, offset = %li\n" tag (string_of_tag tag) offset;
      if mem (string_of_tag tag) required_tables then
         tablesout := (tag, checkSum, i32sub offset !cut, ttlength)::!tablesout
      else
        cut := i32add !cut (match tables.(i + 1) with (_, _, offset', _) -> i32sub offset' offset))
    tables;
  (* Reduce offsets by the reduction in header table size *)
  let header_size_reduction = i32ofi (16 * (Array.length tables - length !tablesout)) in
  let glyf_table_size_reduction = ref 0l in
  let cmap_table_size_reduction = ref 0l in
  let newtables =
    Array.of_list
      (map
        (fun (tag, checksum, offset, ttlength) ->
          let ttlength =
            if string_of_tag tag = "glyf" && subset <> [] then
              let bs = make_write_bitstream () in
                let newlen = write_glyf_table subset cmap bs mk_b glyfoffset loca in
                let paddedlen = i32ofi (bytes_size (bytes_of_write_bitstream bs)) in
                  if !dbg then Printf.printf "new glyf table length = %li\n" newlen;
                  glyf_table_size_reduction := i32sub (padword ttlength) paddedlen;
                  newlen
              else if string_of_tag tag = "cmap" && subset <> [] then
                let bs = make_write_bitstream () in
                  let newlen = write_cmap_table subset cmap bs in
                  let paddedlen = i32ofi (bytes_size (bytes_of_write_bitstream bs)) in
                    if !dbg then Printf.printf "new cmap table length = %li\n" newlen;
                    cmap_table_size_reduction := i32sub (padword ttlength) paddedlen;
                    newlen
              else
                ttlength
          in
            (* Don't reduce by a table size reduction we have just set, but otherwise do. *)
            let offset' =
              i32sub
                (i32sub offset header_size_reduction)
                (if string_of_tag tag = "glyf" then !cmap_table_size_reduction else
                 if string_of_tag tag = "cmap" then !glyf_table_size_reduction else
                   i32add !cmap_table_size_reduction !glyf_table_size_reduction)
            in
              (tag, checksum, offset', ttlength))
        (rev !tablesout))
  in
  if !dbg then Printf.printf "***Reduced:\n";
  Array.iter
    (fun (tag, checkSum, offset, ttlength) -> 
      if !dbg then Printf.printf "tag = %li = %s, offset = %li, length = %li\n" tag (string_of_tag tag) offset ttlength)
    newtables;
  let bs = make_write_bitstream () in
  (* table directory *)
  let numtables = Array.length newtables in
  putval bs 16 (i32ofi major);
  putval bs 16 (i32ofi minor);
  putval bs 16 (i32ofi numtables); (* numTables *)
  putval bs 16 (i32ofi (16 * pow2lt numtables)); (* searchRange *)
  putval bs 16 (i32ofi (int_of_float (log (float_of_int (pow2lt numtables))))); (* entrySelector *)
  putval bs 16 (i32ofi (numtables * 16)); (* rangeShift *)
  Array.iter
    (fun (tag, checkSum, offset, ttlength) ->
      putval bs 32 tag;
      putval bs 32 checkSum;
      putval bs 32 offset;
      putval bs 32 ttlength)
    newtables;
  (* find each table in original data, calculating the length from the next offset.
     On the last, copy until we run out of data *)
  let findtag tag =
    let off = ref 0l in
    let len = ref None in
    begin try
      for x = 0 to Array.length tables - 1 do
        let t, _, offset, _ = tables.(x) in
          if t = tag then
            begin
              off := offset; 
              if x < Array.length tables - 1 then
                len := Some (let _, _, nextoffset, _ = tables.(x + 1) in i32sub nextoffset offset);
              raise Exit
            end
      done;
      failwith "failed to find table"
    with
      Exit -> (!off, !len)
    end
  in
  Array.iter
    (fun (tag, _, _, _) ->
      if !dbg then Printf.printf "Writing %s table\n" (string_of_tag tag);
      if string_of_tag tag = "loca" && subset <> [] then
        write_loca_table subset cmap indexToLocFormat bs loca
      else if string_of_tag tag = "glyf" && subset <> [] then
        ignore (write_glyf_table subset cmap bs mk_b glyfoffset loca)
      else
        match findtag tag with
        | (og_off, Some len) ->
            let b = mk_b (i32toi og_off) in
              for x = 0 to i32toi len - 1 do putval bs 8 (getval_32 b 8) done
        | (og_off, None) ->
            let b = mk_b (i32toi og_off) in
              try
                while true do putval bs 8 (getval_32 b 8) done
              with
                _ -> ())
    newtables;
  let obs = bytes_of_write_bitstream bs in
    if !dbg then
      begin
        Printf.printf "Made subset font of length %i bytes\n" (bytes_size obs);
        let o = open_out_bin ("fontout" ^ string_of_int (fonum ()) ^ ".ttf") in
          output_string o (string_of_bytes obs);
          close_out o
      end;
    obs

(* FIXME: remove *)
let _ =
  Pdfe.logger := (fun s -> print_string s; flush stdout)

let write_font filename data =
  let fh = open_out_bin filename in
    output_string fh (Pdfio.string_of_bytes data);
    close_out fh

let find_main encoding subset =
  let encoding_table = Pdftext.table_of_encoding encoding in
    List.partition
      (fun u -> try ignore (Hashtbl.find encoding_table u); true with Not_found -> false)
      subset

let parse ?(subset=[]) data encoding =
  if !dbg then
    begin
      Printf.printf "********Cpdftruetype.parse SUBSET is ";
      iter (Printf.printf "U+%04X ") subset;
      Printf.printf "\n"
    end;
  let mk_b byte_offset = bitbytes_of_input (let i = input_of_bytes data in i.seek_in byte_offset; i) in
  let b = mk_b 0 in
  let major, minor = read_fixed b in
    if !dbg then Printf.printf "Truetype font version %i.%i\n" major minor;
    let numTables = read_ushort b in
    let searchRange = read_ushort b in
    let entrySelector = read_ushort b in
    let rangeShift = read_ushort b in
      if !dbg then Printf.printf "numTables = %i, searchRange = %i, entrySelector = %i, rangeShift = %i\n"
        numTables searchRange entrySelector rangeShift;
      let tables = ref [] in
        for x = 1 to numTables do
          let tag = read_ulong b in
          let checkSum = read_ulong b in
          let offset = read_ulong b in
          let ttlength = read_ulong b in
            if !dbg then Printf.printf "tag = %li = %s, checkSum = %li, offset = %li, ttlength = %li\n"
            tag (string_of_tag tag) checkSum offset ttlength;
            tables =| (tag, checkSum, offset, ttlength);
        done;
          let headoffset, headlength =
            match keep (function (t, _, _, _) -> string_of_tag t = "head") !tables with
            | (_, _, o, l)::_ -> o, l
            | [] -> raise (Pdf.PDFError "No maxp table found in TrueType font")
          in
            let b = mk_b (i32toi headoffset) in
              discard_bytes b 18;
              let unitsPerEm = read_ushort b in
              discard_bytes b 16;
              let minx = pdf_unit unitsPerEm (read_short b) in
              let miny = pdf_unit unitsPerEm (read_short b) in
              let maxx = pdf_unit unitsPerEm (read_short b) in
              let maxy = pdf_unit unitsPerEm (read_short b) in
              discard_bytes b 6;
              let indexToLocFormat = read_short b in
              let _ (*glyphDataFormat*) = read_short b in
                if !dbg then Printf.printf "head table: indexToLocFormat is %i\n" indexToLocFormat;
                if !dbg then Printf.printf "box %i %i %i %i\n" minx miny maxx maxy;
        let os2 =
          match keep (function (t, _, _, _) -> string_of_tag t = "OS/2") !tables with
          | (_, _, o, l)::_ -> Some (o, l)
          | [] -> None
        in
        let ascent, descent, capheight, xheight, avgwidth =
          match os2 with
          | None -> (0, 0, 0, 0, 0) (* FIXME raise (Pdf.PDFError "No os/2 table found in truetype font") *)
          | Some (o, l) -> let b = mk_b (i32toi o) in read_os2_table unitsPerEm b (i32toi l)
        in
        let italicangle =
          match keep (function (t, _, _, _) -> string_of_tag t = "post") !tables with
          | (_, _, o, _)::_ -> read_post_table (mk_b (i32toi o))
          | _ -> 0
        in
        if !dbg then
          Printf.printf "ascent %i descent %i capheight %i xheight %i avgwidth %i\n"
            ascent descent capheight xheight avgwidth;
        let cmap =
          match keep (function (t, _, _, _) -> string_of_tag t = "cmap") !tables with
          | (_, _, o, l)::_ -> Some (o, l)
          | [] -> None
        in
        let glyphcodes = ref (null_hash ()) in
          begin match cmap with
          | None ->
              for x = 0 to 255 do Hashtbl.add !glyphcodes x x done
          | Some (cmapoffset, cmaplength) -> 
              let b = mk_b (i32toi cmapoffset) in
                let cmap_version = read_ushort b in
                let num_encoding_tables = read_ushort b in
                  if !dbg then Printf.printf "cmap version %i. There are %i encoding tables\n"
                    cmap_version num_encoding_tables;
                  for x = 1 to num_encoding_tables do
                    let platform_id = read_ushort b in
                    let encoding_id = read_ushort b in
                    let subtable_offset = read_ulong b in
                      if !dbg then Printf.printf "subtable %i. platform_id = %i, encoding_id = %i, subtable_offset = %li\n"
                        x platform_id encoding_id subtable_offset;
                      let b = mk_b (i32toi cmapoffset + i32toi subtable_offset) in
                        let fmt = read_ushort b in
                        let lngth = read_ushort b in
                        let version = read_ushort b in
                          if !dbg then Printf.printf "subtable has format %i, length %i, version %i\n" fmt lngth version;
                            let got_glyphcodes = read_encoding_table fmt lngth version b in
                              if fmt = 4 then print_encoding_table 4 got_glyphcodes;
                              if fmt = 4 then Hashtbl.iter (Hashtbl.add !glyphcodes) got_glyphcodes
                  done;
          end;
          let maxpoffset, maxplength =
            match keep (function (t, _, _, _) -> string_of_tag t = "maxp") !tables with
            | (_, _, o, l)::_ -> o, l
            | [] -> raise (Pdf.PDFError "No maxp table found in TrueType font")
          in
          let b = mk_b (i32toi maxpoffset) in
            let mmajor, mminor = read_fixed b in
            let numGlyphs = read_ushort b in
              if !dbg then Printf.printf "maxp table version %i.%i: This font has %i glyphs\n" mmajor mminor numGlyphs;
          let locaoffset, localength =
            match keep (function (t, _, _, _) -> string_of_tag t = "loca") !tables with
            | (_, _, o, l)::_ -> o, l
            | [] -> raise (Pdf.PDFError "No loca table found in TrueType font")
          in
            let subset_1, subset_2 = find_main encoding subset in
            (*if !dbg && subset <> [] then*)
              begin
                Printf.printf "***********Chars for main WinAnsiEncoding subset:\n";
                iter (Printf.printf "U+%04X ") subset_1;
                Printf.printf "\n***********Chars for higher subset:\n";
                iter (Printf.printf "U+%04X ") subset_2;
                Printf.printf "\n";
              end;
            let flags_1 = calculate_flags false italicangle in
            let flags_2 = calculate_flags true italicangle in
            let firstchar_1, lastchar_1 = calculate_limits subset_1 in
            let firstchar_2, lastchar_2 = (33, length subset_2 + 33 - 1) in
            let numOfLongHorMetrics =
              match keep (function (t, _, _, _) -> string_of_tag t = "hhea") !tables with
              | (_, _, o, l)::_ -> let b = mk_b (i32toi o) in read_hhea_table b
              | _ -> 0
            in
            let hmtxdata =
              match keep (function (t, _, _, _) -> string_of_tag t = "hmtx") !tables with
              | (_, _, o, _)::_ -> read_hmtx_table numOfLongHorMetrics (mk_b (i32toi o))
              | [] -> raise (Pdf.PDFError "No hmtx table found in TrueType font")
            in
            Printf.printf "firstchar_1, lastchar_1, firstchar_2, lastchar_2 = %i, %i, %i%, %i\n" firstchar_1 lastchar_1 firstchar_2 lastchar_2;
            let widths_1 = calculate_widths unitsPerEm encoding firstchar_1 lastchar_1 subset_1 !glyphcodes hmtxdata in
            let widths_2 = calculate_width_higher unitsPerEm firstchar_2 lastchar_2 subset_2 !glyphcodes hmtxdata in
            let maxwidth = calculate_maxwidth unitsPerEm hmtxdata in
            let stemv = calculate_stemv () in
            let b = mk_b (i32toi locaoffset) in
            let loca = read_loca_table indexToLocFormat numGlyphs b in
            let glyfoffset, glyflength =
              match keep (function (t, _, _, _) -> string_of_tag t = "glyf") !tables with
              | (_, _, o, l)::_ -> o, l
              | [] -> raise (Pdf.PDFError "No glyf table found in TrueType font")
            in
            Printf.printf "Calculate main subset\n";
            let main_subset =
              subset_font major minor !tables indexToLocFormat subset_1
              encoding !glyphcodes loca mk_b glyfoffset data
            in
            Printf.printf "Calculate higher subset\n";
            let second_subset =
              subset_font major minor !tables indexToLocFormat subset_2
              encoding !glyphcodes loca mk_b glyfoffset data
            in
              let second_tounicode =
                if subset = [] then None else
                  let h = null_hash () in
                    List.iter2
                      (fun n u ->
                         let s = implode (tl (tl (explode (Pdftext.utf16be_of_codepoints [u])))) in
                           Hashtbl.add h n s)
                      (map (( + ) 33) (indx0 subset_2))
                      subset_2;
                    Some h
              in
              Printf.printf "returning the fonts. Job done.\n";
              let one = 
                {flags = flags_1; minx; miny; maxx; maxy; italicangle; ascent; descent;
                capheight; stemv; xheight; avgwidth; maxwidth; firstchar = firstchar_1; lastchar = lastchar_1;
                widths = widths_1; subset_fontfile = main_subset; subset = subset_1; tounicode = None}
              in
              let two =
               {flags = flags_2; minx; miny; maxx; maxy; italicangle; ascent; descent;
                capheight; stemv; xheight; avgwidth; maxwidth; firstchar = firstchar_2; lastchar = lastchar_2;
                widths = widths_2; subset_fontfile = second_subset; subset = subset_2;
                tounicode = second_tounicode}
              in
                Printf.printf "\nMain subset:\n";
                debug_t one;
                write_font "one.ttf" one.subset_fontfile;
                Printf.printf "\nHigher subset:\n";
                debug_t two;
                write_font "two.ttf" two.subset_fontfile;
                [one; two]
