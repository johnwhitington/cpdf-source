val unicodedata_source : string

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

val unicodedata : unit -> t list
