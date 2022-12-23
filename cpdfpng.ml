(* Read a non-interlaced, non-transparent 24 bit PNG for inclusion in a PDF file *)
type t =
  {width : int;
   height : int;
   idat : Pdfio.bytes}

exception BadPNG of string

let read_png i =
  (* File signature *)
  (* IHDR *)
  (* IDAT *)
  (* IEND *)
  {width = 0;
   height = 0;
   idat = Pdfio.bytes_of_string ""}

let _ =
  read_png
    (Pdfio.input_of_string
      (Pdfutil.contents_of_file "/Users/john/Desktop/cpdfdraw/Untitled.png"))
