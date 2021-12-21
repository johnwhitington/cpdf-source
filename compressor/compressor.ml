(* compressor <infile> <outfile> [<compress>] substitutes data files specified
   by __DATA:<filename>\n into the <infile> template, writing to <outfile>. The
   data is printed in way which meets OCaml's syntax. It is compressed by zlib.
*)
open Pdfutil

let contents_of_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let contents_to_file filename contents =
  let ch = open_out_bin filename in
    output_string ch contents;
    close_out ch

let rec process a = function
  | '_'::'_'::'D'::'A'::'T'::'A'::':'::more ->
      let filename, rest = cleavewhile (neq '\n') more in
      let data = all_but_last (explode (contents_of_file (implode filename))) in
      let compressed = Pdfio.string_of_bytes (Pdfcodec.encode_flate (Pdfio.bytes_of_string (implode data))) in
      let ocaml = explode (Printf.sprintf "%S" compressed) in
        process (rev ocaml @ a) rest
  | h::t -> process (h::a) t
  | [] -> rev a

let go infile outfile compress =
  let indata = explode (contents_of_file infile) in
  let processed = process [] indata in
    contents_to_file outfile (implode processed)

let () =
  match Sys.argv with
  | [|_; infile; outfile|] -> go infile outfile false
  | [|_; infile; outfile; "compress"|] -> go infile outfile true
  | _ -> Printf.eprintf "compressor: unknown command line\n"
