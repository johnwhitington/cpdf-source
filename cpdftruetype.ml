(* Truetype font parsing and subsetting *)
open Pdfutil
open Pdfio

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
   subset : Pdfio.bytes}

let dbg = ref false (* text-based debug *)

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

(* fword *)
let read_fword = read_short
let read_ufword = read_ushort

(* f2dot14 - 2 bit signed integer part, 14 bit unsigned fraction *)
let read_f2dot14 b =
  let v = read_ushort b in
    float_of_int (sign_extend 2 (v lsr 14)) +. (float_of_int (v land 0x3FFF) /. 16384.)

(* discard n bytes *)
let discard_bytes b n =
  for x = 1 to n do ignore (getval_31 b 8) done

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
    (* FIXME: This format can address glyphs > 255, but we don't support that
    elsewhere yet --- but we read the whole format table nonethless *)
    try
      for x = firstCode to firstCode + entryCount - 1 do
        Hashtbl.add t x (read_ushort b)
      done;
      t
    with
      e -> failwith ("bad format 6 table: " ^ Printexc.to_string e ^ "\n")

(* fixme might need indexToLocFormat here, to undo the "clever" formula. *)
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
      let ec = endCodes.(seg) in
      let sc = startCodes.(seg) in
      let del = idDelta.(seg) in
      let ro = idRangeOffset.(seg) in
        for c = sc to ec do
          if ro = 0 then
            Hashtbl.add t c ((c + del) mod 65536)
          else
            let sum = (c - sc) + del in
              ()
        done
    done;
    t

let read_encoding_table fmt length version b =
  match fmt with
  | 0 ->
      let t = null_hash () in
        for x = 0 to 255 do Hashtbl.add t x (read_byte b) done;
        t
  | 4 -> read_format_4_encoding_table b
  | 6 -> read_format_6_encoding_table b
  | n -> raise (Pdf.PDFError "read_encoding_table: format %i not known\n%!")

let read_loca_table indexToLocFormat numGlyphs b =
  let fix_empties arr =
    for x = 1 to Array.length arr - 1 do
      if arr.(x) = arr.(x - 1) then arr.(x - 1) <- -1l
    done;
    if arr <> [||] then arr.(Array.length arr - 1) <- -1l
  in
    match indexToLocFormat with
    | 0 ->
        let arr = Array.init (numGlyphs + 1) (function _ -> i32ofi (read_ushort b * 2)) in
          fix_empties arr; arr
    | 1 ->
        let arr = Array.init (numGlyphs + 1) (function _ -> read_ulong b) in
          fix_empties arr; arr
    | _ -> raise (Pdf.PDFError "Unknown indexToLocFormat in read_loca_table")

let read_os2_table b blength =
  let version = read_ushort b in
  if !dbg then Printf.printf "OS/2 table blength = %i bytes, version number = %i\n" blength version;
  if version < 2 then failwith "read_os2_table: version number too low" else
  let xAvgCharWidth = read_short b in
  discard_bytes b 64; (* discard 14 entries usWeightClass...fsLastCharIndex *)
  (* -- end of original OS/2 Version 0 Truetype table. Must check length before reading now. *)
  let sTypoAscender = read_short b in
  let sTypoDescender = read_short b in
  discard_bytes b 6; (* discard sTypoLineGap...usWinDescent *)
  (* -- end of OpenType version 0 table *)
  discard_bytes b 8; (* discard ulCodePageRange1, ulCodePageRange2 *)
  (* -- end of OpenType version 1 table *)
  let sxHeight = read_short b in
  let sCapHeight = read_short b in
    (sTypoAscender, sTypoDescender, sCapHeight, sxHeight, xAvgCharWidth)

let read_post_table b =
  discard_bytes b 4; (* discard version *)
  let italicangle, n = read_fixed b in
    italicangle

(* Eventually:
Set bit 6 for non symbolic. (nb bit 1 is actualy bit 0 etc.)
Set bit 7 if italicangle <> 0
Set bit 2 if serif ?
Set bit 1 if fixed pitch (calculate from widths) *)
let calculate_flags italicangle =
  let italic = if italicangle <> 0 then 1 else 0 in 
    32 lor italic lsl 6

let calculate_limits subset =
  if subset = [] then (0, 255) else
    extremes (sort compare subset)

let calculate_stemv () = 80

let read_hhea_table b =
  discard_bytes b 34;
  read_ushort b (* numOfLongHorMetrics *)

let read_hmtx_table numOfLongHorMetrics b =
  Array.init
    numOfLongHorMetrics
    (fun _ -> let r = read_ushort b in ignore (read_short b); r)

let calculate_widths firstchar lastchar subset (cmapdata : (int, int) Hashtbl.t) (hmtxdata : int array) =
  if lastchar < firstchar then failwith "lastchar < firschar" else
  if !dbg then List.iter (fun (a, b) -> Printf.printf "%i -> %i\n" a b) (sort compare (list_of_hashtbl cmapdata));
  Array.init
    (lastchar - firstchar + 1)
    (fun pos ->
       let code = pos + firstchar in
       if !dbg then Printf.printf "code %i --> " code;
       if subset <> [] && not (mem code subset) then 0 else
       try
         let glyphnum = Hashtbl.find cmapdata code in
         if !dbg then Printf.printf "glyph number %i --> " glyphnum;
           let width = hmtxdata.(glyphnum) in
           if !dbg then Printf.printf "width %i\n" width;
             width
       with e -> if !dbg then Printf.printf "no width for %i\n" code; 0)

let calculate_maxwidth hmtxdata =
  hd (sort (fun a b -> compare b a) (Array.to_list hmtxdata))

let parse ?(subset=[]) data =
  let subset = map fst subset in
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
        let os2 =
          match keep (function (t, _, _, _) -> string_of_tag t = "OS/2") !tables with
          | (_, _, o, l)::_ -> Some (o, l)
          | [] -> None
        in
        let ascent, descent, capheight, xheight, avgwidth =
          match os2 with
          | None -> raise (Pdf.PDFError "No os/2 table found in truetype font")
          | Some (o, l) -> let b = mk_b (i32toi o) in read_os2_table b (i32toi l)
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
              let t = null_hash () in
                for x = 0 to 255 do Hashtbl.add t x x done;
                glyphcodes := t
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
                          let got_glyphcodes = read_encoding_table fmt length version b in
                            glyphcodes := got_glyphcodes
                  done;
          end;
          let maxpoffset, maxplength =
            match keep (function (t, _, _, _) -> string_of_tag t = "maxp") !tables with
            | (_, _, o, l)::_ -> o, l
            | [] -> raise (Pdf.PDFError "No maxp table found in TrueType font")
          in
          let b = mk_b (i32toi maxpoffset) in
            let major, minor = read_fixed b in
            let numGlyphs = read_ushort b in
              if !dbg then Printf.printf "maxp table version %i.%i: This font has %i glyphs\n" major minor numGlyphs;
          let headoffset, headlength =
            match keep (function (t, _, _, _) -> string_of_tag t = "head") !tables with
            | (_, _, o, l)::_ -> o, l
            | [] -> raise (Pdf.PDFError "No maxp table found in TrueType font")
          in
            let b = mk_b (i32toi headoffset) in
              discard_bytes b 36;
              let minx = read_fword b in
              let miny = read_fword b in
              let maxx = read_fword b in
              let maxy = read_fword b in
              discard_bytes b 6;
              let indexToLocFormat = read_short b in
              let _ (*glyphDataFormat*) = read_short b in
                if !dbg then Printf.printf "head table: indexToLocFormat is %i\n" indexToLocFormat;
                if !dbg then Printf.printf "box %i %i %i %i\n" minx miny maxx maxy;
          let locaoffset, localength =
            match keep (function (t, _, _, _) -> string_of_tag t = "loca") !tables with
            | (_, _, o, l)::_ -> o, l
            | [] -> raise (Pdf.PDFError "No loca table found in TrueType font")
          in
            let flags = calculate_flags italicangle in
            let firstchar, lastchar = calculate_limits subset in
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
            let widths = calculate_widths firstchar lastchar subset !glyphcodes hmtxdata in
            let maxwidth = calculate_maxwidth hmtxdata in
            let stemv = calculate_stemv () in
            let b = mk_b (i32toi locaoffset) in
            let offsets = read_loca_table indexToLocFormat numGlyphs b in
            let subset = data in
              {flags; minx; miny; maxx; maxy; italicangle; ascent; descent;
              capheight; stemv; xheight; avgwidth; maxwidth; firstchar; lastchar;
              widths; subset}
