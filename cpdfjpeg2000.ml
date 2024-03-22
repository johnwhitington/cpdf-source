open Pdfutil
open Pdfio
open Cpdferror

let read_word i =
  let a = i32ofi (i.input_byte ()) in
  let b = i32ofi (i.input_byte ()) in
  let c = i32ofi (i.input_byte ()) in
  let d = i32ofi (i.input_byte ()) in
    lor32 (lor32 (lor32 (lsl32 a 24) (lsl32 b 16)) (lsl32 c 8)) d

exception Answer of int * int

let id_IMAGE_HEADER_BOX = 0x69686472l (* ihdr *)

(* Return the width and height of a JPEG2000 (.jp2 or .jpx) image. *)
let jpeg2000_dimensions_inner bs =
  let i = Pdfio.input_of_bytes bs in
  let a = read_word i in
  let b = read_word i in
  let c = read_word i in
    if a <> 0x0000000cl || b <> 0x6a502020l || c <> 0x0d0a870al then
      Cpdferror.error "bad JPEG2000 header"
    else
    while true do
      let box = read_word i in
      if box = id_IMAGE_HEADER_BOX then
        begin
          let h = read_word i in
          let w = read_word i in
          raise (Answer (i32toi w, i32toi h))
        end;
      (* Read words starting at each byte until we find ihdr. *)
      i.seek_in (i.pos_in () - 3)
    done

let jpeg2000_dimensions bs =
  try jpeg2000_dimensions_inner bs; (0, 0) with
  | Answer (w, h) -> (w, h)
  | Exit -> error "could not determine JPEG2000 dimensions"
