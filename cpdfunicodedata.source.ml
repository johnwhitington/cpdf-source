open Pdfutil

let unicodedata_source = __DATA:UnicodeData.txt

type t =
  {code_value : string;
   character_name : string;
   general_category : string;
   canonical_combining_classes : string;
   bidirectional_category : string;
   character_decomposition_mapping : string;
   decimal_digit_value : string;
   digit_value : string;
   numeric_value : string;
   mirrored : string;
   unicode_10_name : string;
   iso_10646_comment_field : string;
   uppercase_mapping : string;
   lowercase_mapping : string;
   titlecase_mapping : string}

let get_single_field i =
  let r = implode (Pdfread.getuntil true (function c -> c = ';' || c = '\n') i) in
    Pdfio.nudge i;
    r

let parse_entry i =
  let code_value = get_single_field i in
  let character_name = get_single_field i in
  let general_category = get_single_field i in
  let canonical_combining_classes = get_single_field i in
  let bidirectional_category = get_single_field i in
  let character_decomposition_mapping = get_single_field i in
  let decimal_digit_value = get_single_field i in
  let digit_value = get_single_field i in
  let numeric_value = get_single_field i in
  let mirrored = get_single_field i in
  let unicode_10_name = get_single_field i in
  let iso_10646_comment_field = get_single_field i in
  let uppercase_mapping = get_single_field i in
  let lowercase_mapping = get_single_field i in
  let titlecase_mapping = get_single_field i in
    {code_value;
     character_name;
     general_category;
     canonical_combining_classes;
     bidirectional_category;
     character_decomposition_mapping;
     decimal_digit_value;
     digit_value;
     numeric_value;
     mirrored;
     unicode_10_name;
     iso_10646_comment_field;
     uppercase_mapping;
     lowercase_mapping;
     titlecase_mapping}

let rec parse_unicodedata a i =
  if i.Pdfio.pos_in () = i.Pdfio.in_channel_length + 2 (* it's been nudged *)
    then rev a
    else parse_unicodedata (parse_entry i::a) i

let print_entry e =
  Printf.printf
    "{{%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s}}\n"
    e.code_value
    e.character_name
    e.general_category
    e.canonical_combining_classes
    e.bidirectional_category
    e.character_decomposition_mapping
    e.decimal_digit_value
    e.digit_value
    e.numeric_value
    e.mirrored
    e.unicode_10_name
    e.iso_10646_comment_field
    e.uppercase_mapping
    e.lowercase_mapping
    e.titlecase_mapping

let unicodedata =
  memoize
    (fun () ->
       let r = 
          unicodedata_source
       |> Pdfio.bytes_of_string
       |> Pdfcodec.decode_flate
       |> Pdfio.string_of_bytes
       |> Pdfio.input_of_string
       |> parse_unicodedata []
       in (*iter print_entry r;*) r)
