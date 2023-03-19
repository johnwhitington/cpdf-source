(* Read and write non-interlaced, non-transparent 24 bit PNGs. Such a PNG may
   be loaded into a PDF simply by extracting its width and height from the
   IHDR, and concatenating all its IDAT data sections together. *)
open Pdfutil
open Pdfio

type t =
  {width : int;
   height : int;
   idat : bytes}

(* Writing *)
let crc_table =
  let f n =
    let c = ref (i32ofi n) in
      for _ = 0 to 7 do
        c := lxor32 (lsr32 !c 1) (land32 0xedb88320l (i32succ (lnot32 (land32 !c 1l))))
      done;
      !c
  in
    Array.init 256 f

let update_crc crc buf len =
  let c = ref crc in
    for n = 0 to len - 1 do
      let e = i32ofi (int_of_char buf.[n]) in
        c := lxor32 crc_table.(i32toi (land32 (lxor32 !c e) 0xffl)) (lsr32 !c 8)
    done;
    !c

let png_crc buf len =
  lnot32 (update_crc 0xffffffffl buf len)

let bytes_of_word x =
  i32toi (Int32.shift_right x 24),
  i32toi (land32 0x000000FFl (Int32.shift_right x 16)),
  i32toi (land32 0x000000FFl (Int32.shift_right x 8)),
  i32toi (land32 0x000000FFl x)

let write_crc o ctype cdata =
  let crc = update_crc 0xffffffffl ctype 4 in
  let crc = update_crc crc cdata (String.length cdata) in
  let a, b, c, d = bytes_of_word crc in
    o.output_byte a;
    o.output_byte b;
    o.output_byte c;
    o.output_byte d

let write_chunk o ctype data =
  for x = 0 to 3 do o.output_byte (int_of_char ctype.[x]) done;
  o.output_string (Bytes.unsafe_to_string data);
  write_crc o ctype (Bytes.unsafe_to_string data)

let write_word b p n =
  let p, q, r, s = bytes_of_word n in
  Bytes.set b p (char_of_int p);
  Bytes.set b (p + 1) (char_of_int q);
  Bytes.set b (p + 2) (char_of_int r);
  Bytes.set b (p + 3) (char_of_int s)

let write_png png o =
  if bytes_size png.idat > 2147483647 then raise (Invalid_argument "write_png: too large") else
  (* Signature *)
  o.output_string "\137\080\078\071\013\010\026\010";
  let ihdr = Bytes.create 13 in
  write_word ihdr 0 (i32ofi png.width);
  write_word ihdr 4 (i32ofi png.height);
  Bytes.set ihdr 8 (char_of_int 8); (* bit depth *)
  Bytes.set ihdr 9 (char_of_int 2); (* colour type *)
  Bytes.set ihdr 10 (char_of_int 0); (* compression method *)
  Bytes.set ihdr 11 (char_of_int 0); (* filter method *)
  Bytes.set ihdr 12 (char_of_int 0); (* interlace method *)
  write_chunk o "IHDR" ihdr;
  write_chunk o "IDAT" (Bytes.unsafe_of_string (Pdfio.string_of_bytes png.idat));
  write_chunk o "IEND" (Bytes.create 0)

(* Reading *)
let string_of_tag t =
  Printf.sprintf "%c%c%c%c"
    (char_of_int (i32toi (Int32.shift_right t 24)))
    (char_of_int (i32toi (Int32.logand 0x000000FFl (Int32.shift_right t 16))))
    (char_of_int (i32toi (Int32.logand 0x000000FFl (Int32.shift_right t 8))))
    (char_of_int (i32toi (Int32.logand 0x000000FFl t)))

let read_unsigned_4byte i =
  let a = i32ofi (i.input_byte ()) in
  let b = i32ofi (i.input_byte ()) in
  let c = i32ofi (i.input_byte ()) in
  let d = i32ofi (i.input_byte ()) in
    lor32 (lor32 (lsl32 a 24) (lsl32 b 16)) (lor32 (lsl32 c 8) d)

let read_data l i =
  let l = i32toi l in
  let b = mkbytes l in 
    setinit i b 0 l;
    b

let read_chunk i =
  let chunklen = read_unsigned_4byte i in
  let chunktype = read_unsigned_4byte i in
  let chunkdata = read_data chunklen i in
  let _ (* crc *) = read_unsigned_4byte i in
    (string_of_tag chunktype, chunkdata) 

let concat_bytes ss =
  let total_length = sum (map bytes_size ss) in
    let s' = mkbytes total_length in
      let p = ref 0 in
        iter
          (fun s ->
             for x = 0 to bytes_size s - 1 do bset_unsafe s' !p (bget s x); incr p done)
          ss;
        s'

let read_png i =
  try
    i.seek_in 8;
    let ihdr, ihdrdata = read_chunk i in
    if ihdr <> "IHDR" then raise (Pdf.PDFError "read_png: first table not IHDR") else
    let hdr = input_of_bytes ihdrdata in
    let width = read_unsigned_4byte hdr in
    let height = read_unsigned_4byte hdr in
    let bitdepth = hdr.input_byte () in
    if bitdepth <> 8 then failwith "read_png: bit depth not 8" else
    let colortype = hdr.input_byte () in
    if colortype <> 2 then failwith "read_png: only 24 bit non-alpha PNGs" else
    let _ (*compressionmethod*) = hdr.input_byte () in
    let _ (*filtermethod*) = hdr.input_byte () in
    let interlacemethod = hdr.input_byte () in
    if interlacemethod <> 0 then failwith "read_png: interlaced PDFs not supported" else
    let idat = ref [] in
      begin try
        while true do
          let chunkname, chunkdata = read_chunk i in
            if chunkname = "IDAT" then idat := chunkdata::!idat
        done
      with
        _ -> ()
      end;
      let r =
        {width = i32toi width; height = i32toi height; idat = concat_bytes (rev !idat)}
      in
        let ch = open_out_bin "out.png" in
          write_png r (Pdfio.output_of_channel ch);
          r
  with
    e -> raise (Pdf.PDFError (Printf.sprintf "read_png: failed on %s" (Printexc.to_string e)))
