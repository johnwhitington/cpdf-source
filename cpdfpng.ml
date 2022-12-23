(* Read a non-interlaced, non-transparent 24 bit PNG for inclusion in a PDF file *)
open Pdfutil
open Pdfio

type t =
  {width : int;
   height : int;
   idat : bytes}

exception BadPNG of string

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
  Printf.printf "chunklen = %li\n" chunklen;
  let chunktype = read_unsigned_4byte i in
  Printf.printf "chunktype = %s\n" (string_of_tag chunktype);
  let chunkdata = read_data chunklen i in
  let _ (* crc *) = read_unsigned_4byte i in
    (string_of_tag chunktype, chunkdata) 

let read_png i =
  try
    i.seek_in 8;
    let ihdr, ihdrdata = read_chunk i in
    if ihdr <> "IHDR" then raise (Pdf.PDFError "read_png: first table not IHDR") else
    let width = 0 in
    let height = 0 in
    let bitdepth = 0 in
    let colortype = 0 in
    let compressionmethod = 0 in
    let filtermethod = 0 in
    let interlacemethod = 0 in
    let idat = ref None in
      begin try
        while true do
          let chunkname, chunkdata = read_chunk i in
            if chunkname = "IDAT" then
              idat := Some chunkdata
        done
      with
        _ -> ()
      end;
      {width; height; idat = unopt !idat}
  with
    e -> raise (Pdf.PDFError (Printf.sprintf "read_png: failed on %s" (Printexc.to_string e)))

let _ =
  read_png
    (input_of_string
      (Pdfutil.contents_of_file "/Users/john/Desktop/cpdfdraw/Untitled.png"))
