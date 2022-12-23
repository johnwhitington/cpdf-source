(* Read a non-interlaced, non-transparent 24 bit PNG for inclusion in a PDF file *)
open Pdfutil
open Pdfio

type t =
  {width : int;
   height : int;
   idat : bytes}

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
  let total_length = fold_left ( + ) 0 (map bytes_size ss) in
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
      {width = i32toi width; height = i32toi height; idat = concat_bytes (rev !idat)}
  with
    e -> raise (Pdf.PDFError (Printf.sprintf "read_png: failed on %s" (Printexc.to_string e)))
