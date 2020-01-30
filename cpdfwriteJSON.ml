module J = Tjjson

let test = J.Array [J.Number "100"; J.String "foo"]

let write fh parse_content pdf =
  let b = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer b in
    Tjjson.format formatter test;
    Format.pp_print_flush formatter ();
    output_string fh (Buffer.contents b)


