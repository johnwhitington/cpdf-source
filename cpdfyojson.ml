(* Copyright (c) 2010-2012, Martin Jambon All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

Neither the name of nor the names of its contributors may be used to endorse or
promote products derived from this software without specific prior written
permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. *)

# 1 "common.ml"
let version = "%%VERSION%%"

exception Json_error of string

let json_error s = raise (Json_error s)

exception End_of_array
exception End_of_object
exception End_of_tuple
exception End_of_input

type in_param = {
  string_buf : Buffer.t
}

let create_in_param ?(len = 256) () = {
  string_buf = Buffer.create len
}


let utf8_of_code buf x =
  let add = Buffer.add_char in

  (* Straight <= doesn't work with signed 31-bit ints *)
  let maxbits n x = x lsr n = 0 in

  if maxbits 7 x then
    (* 7 *)
    add buf (Char.chr x)
  else if maxbits 11 x then (
    (* 5 + 6 *)
    add buf (Char.chr (0b11000000 lor ((x lsr 6) land 0b00011111)));
    add buf (Char.chr (0b10000000 lor (x         land 0b00111111)))
  )
  else if maxbits 16 x then (
    (* 4 + 6 + 6 *)
    add buf (Char.chr (0b11100000 lor ((x lsr 12) land 0b00001111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)))
  )
  else if maxbits 21 x then (
    (* 3 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11110000 lor ((x lsr 18) land 0b00000111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
  )
  else if maxbits 26 x then (
    (* 2 + 6 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11111000 lor ((x lsr 24) land 0b00000011)));
    add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
  )
  else (
    assert (maxbits 31 x);
    (* 1 + 6 + 6 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11111100 lor ((x lsr 30) land 0b00000001)));
    add buf (Char.chr (0b10000000 lor ((x lsr 24) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
  )

let code_of_surrogate_pair i j =
  let high10 = i - 0xD800 in
  let low10 = j - 0xDC00 in
  0x10000 + ((high10 lsl 10) lor low10)

let utf8_of_surrogate_pair buf i j =
  utf8_of_code buf (code_of_surrogate_pair i j)

let is_object_or_array x =
  match x with
      `List _
    | `Assoc _ -> true
    | _ -> false


type lexer_state = {
  buf : Buffer.t;
    (* Buffer used to accumulate substrings *)

  mutable lnum : int;
    (* Current line number (starting from 1) *)

  mutable bol : int;
    (* Absolute position of the first character of the current line
       (starting from 0) *)

  mutable fname : string option;
    (* Name describing the input file *)
}

module Lexer_state =
struct
  type t = lexer_state = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }
end

let init_lexer ?buf ?fname ?(lnum = 1) () =
  let buf =
    match buf with
	None -> Buffer.create 256
      | Some buf -> buf
  in
  {
    buf = buf;
    lnum = lnum;
    bol = 0;
    fname = fname
  }

# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 8 "type.ml"
    | `Int of int
    
# 11 "type.ml"
    | `Intlit of string
    
# 14 "type.ml"
    | `Float of float
    
# 17 "type.ml"
    | `Floatlit of string
    
# 20 "type.ml"
    | `String of string
    
# 23 "type.ml"
    | `Stringlit of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 28 "type.ml"
    | `Tuple of t list
    
# 31 "type.ml"
    | `Variant of (string * t option)
    
# 33 "type.ml"
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)
# 12 "yojson.cppo.ml"
type json_max = t
# 1 "write.ml"
(* included: type.ml *)

let hex n =
  Char.chr (
    if n < 10 then n + 48
    else n + 87
  )

let write_special src start stop ob str =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob str;
  start := stop + 1

let write_control_char src start stop ob c =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob "\\u00";
  Buffer.add_char ob (hex (Char.code c lsr 4));
  Buffer.add_char ob (hex (Char.code c land 0xf));
  start := stop + 1

let finish_string src start ob =
  try
    Buffer.add_substring ob src !start (String.length src - !start)
  with exc ->
    Printf.eprintf "src=%S start=%i len=%i\n%!"
      src !start (String.length src - !start);
    raise exc

let write_string_body ob s =
  let start = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '"' -> write_special s start i ob "\\\""
      | '\\' -> write_special s start i ob "\\\\"
      | '\b' -> write_special s start i ob "\\b"
      | '\012' -> write_special s start i ob "\\f"
      | '\n' -> write_special s start i ob "\\n"
      | '\r' -> write_special s start i ob "\\r"
      | '\t' -> write_special s start i ob "\\t"
      | '\x00'..'\x1F'
      | '\x7F' as c -> write_control_char s start i ob c
      | _ -> ()
  done;
  finish_string s start ob

let write_string ob s =
  Buffer.add_char ob '"';
  write_string_body ob s;
  Buffer.add_char ob '"'

let json_string_of_string s =
  let ob = Buffer.create 10 in
  write_string ob s;
  Buffer.contents ob

let test_string () =
  let s = Bytes.create 256 in
  for i = 0 to 255 do
    Bytes.set s i (Char.chr i)
  done;
  json_string_of_string (Bytes.to_string s)


let write_null ob () =
  Buffer.add_string ob "null"

let write_bool ob x =
  Buffer.add_string ob (if x then "true" else "false")


let max_digits =
  max
    (String.length (string_of_int max_int))
    (String.length (string_of_int min_int))

let dec n =
  Char.chr (n + 48)

let rec write_digits s x =
  if x = 0 then ()
  else
    let d = x mod 10 in
    write_digits s (x / 10);
    Buffer.add_char s (dec (abs d))

let write_int ob x =
  if x > 0 then
    write_digits ob x
  else if x < 0 then (
    Buffer.add_char ob '-';
    write_digits ob x
  )
  else
    Buffer.add_char ob '0'


let json_string_of_int i =
  string_of_int i


(*
  Ensure that the float is not printed as an int.
  This is not required by JSON, but useful in order to guarantee
  reversibility.
*)
let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with
          '0'..'9' | '-' -> ()
        | _ -> raise Exit
    done;
    true
  with Exit ->
    false

(*
  Both write_float_fast and write_float guarantee
  that a sufficient number of digits are printed in order to
  allow reversibility.

  The _fast version is faster but often produces unnecessarily long numbers.
*)
let write_float_fast ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s = Printf.sprintf "%.17g" x in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_float ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_normal_float_prec significant_figures ob x =
  let open Printf in
  let s =
    match significant_figures with
        1 -> sprintf "%.1g" x
      | 2 -> sprintf "%.2g" x
      | 3 -> sprintf "%.3g" x
      | 4 -> sprintf "%.4g" x
      | 5 -> sprintf "%.5g" x
      | 6 -> sprintf "%.6g" x
      | 7 -> sprintf "%.7g" x
      | 8 -> sprintf "%.8g" x
      | 9 -> sprintf "%.9g" x
      | 10 -> sprintf "%.10g" x
      | 11 -> sprintf "%.11g" x
      | 12 -> sprintf "%.12g" x
      | 13 -> sprintf "%.13g" x
      | 14 -> sprintf "%.14g" x
      | 15 -> sprintf "%.15g" x
      | 16 -> sprintf "%.16g" x
      | _ -> sprintf "%.17g" x
  in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

let write_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      write_normal_float_prec significant_figures ob x

let json_string_of_float x =
  let ob = Buffer.create 20 in
  write_float ob x;
  Buffer.contents ob


let write_std_float_fast ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s = Printf.sprintf "%.17g" x in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_std_float ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_std_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      write_normal_float_prec significant_figures ob x

let std_json_string_of_float x =
  let ob = Buffer.create 20 in
  write_std_float ob x;
  Buffer.contents ob


let test_float () =
  let l = [ 0.; 1.; -1. ] in
  let l = l @ List.map (fun x -> 2. *. x +. 1.) l in
  let l = l @ List.map (fun x -> x /. sqrt 2.) l in
  let l = l @ List.map (fun x -> x *. sqrt 3.) l in
  let l = l @ List.map cos l in
  let l = l @ List.map (fun x -> x *. 1.23e50) l in
  let l = l @ [ infinity; neg_infinity ] in
  List.iter (
    fun x ->
      let s = Printf.sprintf "%.17g" x in
      let y = float_of_string s in
      Printf.printf "%g %g %S %B\n" x y s (x = y)
  )
    l

(*
let () = test_float ()
*)

let write_intlit = Buffer.add_string
let write_floatlit = Buffer.add_string
let write_stringlit = Buffer.add_string

let rec iter2_aux f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_sep x;
      f_elt x y;
      iter2_aux f_elt f_sep x l

let iter2 f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_elt x y;
      iter2_aux f_elt f_sep x l

let f_sep ob =
  Buffer.add_char ob ','

let rec write_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    
# 293 "write.ml"
    | `Int i -> write_int ob i
    
# 296 "write.ml"
    | `Intlit s -> Buffer.add_string ob s
    
# 299 "write.ml"
    | `Float f -> write_float ob f
    
# 302 "write.ml"
    | `Floatlit s -> Buffer.add_string ob s
    
# 305 "write.ml"
    | `String s -> write_string ob s
    
# 308 "write.ml"
    | `Stringlit s -> Buffer.add_string ob s
    
# 310 "write.ml"
    | `Assoc l -> write_assoc ob l
    | `List l -> write_list ob l
    
# 313 "write.ml"
    | `Tuple l -> write_tuple ob l
    
# 316 "write.ml"
    | `Variant (s, o) -> write_variant ob s o

# 319 "write.ml"
and write_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_list ob l =
  Buffer.add_char ob '[';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ']'

# 335 "write.ml"
and write_tuple ob l =
  Buffer.add_char ob '(';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ')'

# 342 "write.ml"
and write_variant ob s o =
  Buffer.add_char ob '<';
  write_string ob s;
  (match o with
       None -> ()
     | Some x ->
         Buffer.add_char ob ':';
         write_json ob x
  );
  Buffer.add_char ob '>'

# 354 "write.ml"
let write_t = write_json

let rec write_std_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    
# 361 "write.ml"
    | `Int i -> write_int ob i
    
# 364 "write.ml"
    | `Intlit s -> Buffer.add_string ob s
    
# 367 "write.ml"
    | `Float f -> write_std_float ob f
    
# 370 "write.ml"
    | `Floatlit s -> Buffer.add_string ob s
    
# 373 "write.ml"
    | `String s -> write_string ob s
    
# 376 "write.ml"
    | `Stringlit s -> Buffer.add_string ob s
    
# 378 "write.ml"
    | `Assoc l -> write_std_assoc ob l
    | `List l -> write_std_list ob l
    
# 381 "write.ml"
    | `Tuple l -> write_std_tuple ob l
    
# 384 "write.ml"
    | `Variant (s, o) -> write_std_variant ob s o

# 387 "write.ml"
and write_std_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_std_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_std_list ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'

and write_std_tuple ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'

# 408 "write.ml"
and write_std_variant ob s o =
  match o with
      None -> write_string ob s
    | Some x ->
        Buffer.add_char ob '[';
        write_string ob s;
        Buffer.add_char ob ',';
        write_std_json ob x;
        Buffer.add_char ob ']'


# 420 "write.ml"
let to_buffer ?(std = false) ob x =
  if std then (
    if not (is_object_or_array x) then
      json_error "Root is not an object or array"
    else
      write_std_json ob x
  )
  else
    write_json ob x


let to_string ?buf ?(len = 256) ?std x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  to_buffer ?std ob x;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let to_channel ?buf ?(len=4096) ?std oc x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  to_buffer ?std ob x;
  Buffer.output_buffer oc ob

let to_output ?buf ?(len=4096) ?std out x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  to_buffer ?std ob x;
  out#output (Buffer.contents ob) 0 (Buffer.length ob);
  ()

let to_file ?len ?std file x =
  let oc = open_out file in
  try
    to_channel ?len ?std oc x;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let stream_to_buffer ?std ob st =
  Stream.iter (to_buffer ?std ob) st

let stream_to_string ?buf ?(len = 256) ?std st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  stream_to_buffer ?std ob st;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let stream_to_channel ?buf ?(len=2096) ?std oc st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  stream_to_buffer ?std ob st

let stream_to_file ?len ?std file st =
  let oc = open_out file in
  try
    stream_to_channel ?len ?std oc st;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e


let rec sort = function
  | `Assoc l ->
      let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
      `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
  | `List l ->
      `List (List.rev (List.rev_map sort l))
  
# 513 "write.ml"
  | `Tuple l ->
      `Tuple (List.rev (List.rev_map sort l))
  
# 517 "write.ml"
  | `Variant (k, Some v) as x ->
      let v' = sort v in
      if v == v' then x
      else
        `Variant (k, Some v')
  
# 523 "write.ml"
  | x -> x
# 1 "monomorphic.ml"
let rec pp fmt =
  function
  | `Null -> Format.pp_print_string fmt "`Null"
  | `Bool x ->
    Format.fprintf fmt "`Bool (@[<hov>";
    Format.fprintf fmt "%B" x;
    Format.fprintf fmt "@])"
  
# 9 "monomorphic.ml"
  | `Int x ->
    Format.fprintf fmt "`Int (@[<hov>";
    Format.fprintf fmt "%d" x;
    Format.fprintf fmt "@])"
  
# 15 "monomorphic.ml"
  | `Intlit x ->
    Format.fprintf fmt "`Intlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 21 "monomorphic.ml"
  | `Float x ->
    Format.fprintf fmt "`Float (@[<hov>";
    Format.fprintf fmt "%F" x;
    Format.fprintf fmt "@])"
  
# 27 "monomorphic.ml"
  | `Floatlit x ->
    Format.fprintf fmt "`Floatlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 33 "monomorphic.ml"
  | `String x ->
    Format.fprintf fmt "`String (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 39 "monomorphic.ml"
  | `Stringlit x ->
    Format.fprintf fmt "`Stringlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 44 "monomorphic.ml"
  | `Assoc xs ->
    Format.fprintf fmt "`Assoc (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep (key, value) ->
        if sep then
          Format.fprintf fmt ";@ ";
          Format.fprintf fmt "(@[";
          Format.fprintf fmt "%S" key;
          Format.fprintf fmt ",@ ";
          pp fmt value;
          Format.fprintf fmt "@])";
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  | `List xs ->
    Format.fprintf fmt "`List (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep x ->
        if sep then
          Format.fprintf fmt ";@ ";
          pp fmt x;
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  
# 71 "monomorphic.ml"
  | `Tuple tup ->
    Format.fprintf fmt "`Tuple (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep e ->
         if sep then
           Format.fprintf fmt ";@ ";
           pp fmt e;
           true) false tup);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  
# 84 "monomorphic.ml"
  | `Variant (name, value) ->
    Format.fprintf fmt "`Variant (@[<hov>";
    Format.fprintf fmt "(@[";
    Format.fprintf fmt "%S" name;
    Format.fprintf fmt ",@ ";
    (match value with
      | None -> Format.pp_print_string fmt "None"
      | Some x ->
        Format.pp_print_string fmt "(Some ";
        pp fmt x;
        Format.pp_print_string fmt ")");
    Format.fprintf fmt "@])";
    Format.fprintf fmt "@])"

# 99 "monomorphic.ml"
let show x =
  Format.asprintf "%a" pp x

let rec equal a b =
  match a, b with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
  
# 107 "monomorphic.ml"
  | `Int a, `Int b -> a = b
    
# 110 "monomorphic.ml"
    | `Intlit a, `Intlit b -> a = b
    
# 113 "monomorphic.ml"
    | `Float a, `Float b -> a = b
    
# 116 "monomorphic.ml"
    | `Floatlit a, `Floatlit b -> a = b
    
# 119 "monomorphic.ml"
    | `String a, `String b -> a = b
    
# 122 "monomorphic.ml"
    | `Stringlit a, `Stringlit b -> a = b
    
# 124 "monomorphic.ml"
    | `Assoc xs, `Assoc ys ->
      let compare_keys = fun (key, _) (key', _) -> String.compare key key' in
      let xs = List.stable_sort compare_keys xs in
      let ys = List.stable_sort compare_keys ys in
      (match List.for_all2 (fun (key, value) (key', value') ->
        match key = key' with
        | false -> false
        | true -> equal value value') xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    
# 137 "monomorphic.ml"
    | `Tuple xs, `Tuple ys
    
# 139 "monomorphic.ml"
    | `List xs, `List ys ->
      (match List.for_all2 equal xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    
# 146 "monomorphic.ml"
    | `Variant (name, value), `Variant (name', value') ->
      (match name = name' with
      | false -> false
      | true ->
        match value, value' with
        | None, None -> true
        | Some x, Some y -> equal x y
        | _ -> false)
    
# 155 "monomorphic.ml"
    | _ -> false
# 15 "yojson.cppo.ml"
module Pretty =
struct

# 2 "pretty.ml"
let pp_list sep ppx out l =
  let pp_sep out () = Format.fprintf out "%s@ " sep in
  Format.pp_print_list ~pp_sep ppx out l

let rec format std (out:Format.formatter) (x : t) : unit =
  match x with
    | `Null -> Format.pp_print_string out "null"
    | `Bool x -> Format.pp_print_bool out x
    | `Int x -> Format.pp_print_string out (json_string_of_int x)
    | `Float x ->
        let s =
          if std then std_json_string_of_float x
          else json_string_of_float x
        in
        Format.pp_print_string out s
    | `String s -> Format.pp_print_string out (json_string_of_string s)
    | `Intlit s
    | `Floatlit s
    | `Stringlit s -> Format.pp_print_string out s
    | `List [] -> Format.pp_print_string out "[]"
    | `List l -> Format.fprintf out "[@;<1 0>@[<hov>%a@]@;<1 -2>]" (pp_list "," (format std)) l
    | `Assoc [] -> Format.pp_print_string out "{}"
    | `Assoc l ->
      Format.fprintf out "{@;<1 0>%a@;<1 -2>}" (pp_list "," (format_field std)) l
    | `Tuple l ->
        if std then
          format std out (`List l)
        else
          if l = [] then
            Format.pp_print_string out "()"
          else
            Format.fprintf out "(@,%a@;<0 -2>)" (pp_list "," (format std)) l

    | `Variant (s, None) ->
        if std then
          format std out (`String s)
        else
          Format.fprintf out "<%s>" (json_string_of_string s)

    | `Variant (s, Some x) ->
        if std then
          format std out (`List [ `String s; x ])
        else
          let op = json_string_of_string s in
          Format.fprintf out "<@[<hv2>%s: %a@]>" op (format std) x

and format_field std out (name, x) =
  Format.fprintf out "@[<hv2>%s: %a@]" (json_string_of_string name) (format std) x

let pp ?(std = false) out x =
  if std && not (is_object_or_array x) then
    json_error
      "Root is not an object or array as requested by the JSON standard"
  else
    Format.fprintf out "@[<hv2>%a@]" (format std) (x :> t)

let to_string ?std x =
  Format.asprintf "%a" (pp ?std) x

let to_channel ?std oc x =
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@?" (pp ?std) x
# 18 "yojson.cppo.ml"
end

# 2 "write2.ml"
let pretty_print ?std out (x : t) =
  Pretty.pp ?std out (x :> json_max)

let pretty_to_string ?std (x : t) =
  Pretty.to_string ?std (x :> json_max)

let pretty_to_channel ?std oc (x : t) =
  Pretty.to_channel ?std oc (x :> json_max)

# 29 "yojson.cppo.ml"
module Basic =
struct
# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 8 "type.ml"
    | `Int of int
    
# 14 "type.ml"
    | `Float of float
    
# 20 "type.ml"
    | `String of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 33 "type.ml"
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)
# 1 "write.ml"
(* included: type.ml *)

let hex n =
  Char.chr (
    if n < 10 then n + 48
    else n + 87
  )

let write_special src start stop ob str =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob str;
  start := stop + 1

let write_control_char src start stop ob c =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob "\\u00";
  Buffer.add_char ob (hex (Char.code c lsr 4));
  Buffer.add_char ob (hex (Char.code c land 0xf));
  start := stop + 1

let finish_string src start ob =
  try
    Buffer.add_substring ob src !start (String.length src - !start)
  with exc ->
    Printf.eprintf "src=%S start=%i len=%i\n%!"
      src !start (String.length src - !start);
    raise exc

let write_string_body ob s =
  let start = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '"' -> write_special s start i ob "\\\""
      | '\\' -> write_special s start i ob "\\\\"
      | '\b' -> write_special s start i ob "\\b"
      | '\012' -> write_special s start i ob "\\f"
      | '\n' -> write_special s start i ob "\\n"
      | '\r' -> write_special s start i ob "\\r"
      | '\t' -> write_special s start i ob "\\t"
      | '\x00'..'\x1F'
      | '\x7F' as c -> write_control_char s start i ob c
      | _ -> ()
  done;
  finish_string s start ob

let write_string ob s =
  Buffer.add_char ob '"';
  write_string_body ob s;
  Buffer.add_char ob '"'

let json_string_of_string s =
  let ob = Buffer.create 10 in
  write_string ob s;
  Buffer.contents ob

let test_string () =
  let s = Bytes.create 256 in
  for i = 0 to 255 do
    Bytes.set s i (Char.chr i)
  done;
  json_string_of_string (Bytes.to_string s)


let write_null ob () =
  Buffer.add_string ob "null"

let write_bool ob x =
  Buffer.add_string ob (if x then "true" else "false")


let max_digits =
  max
    (String.length (string_of_int max_int))
    (String.length (string_of_int min_int))

let dec n =
  Char.chr (n + 48)

let rec write_digits s x =
  if x = 0 then ()
  else
    let d = x mod 10 in
    write_digits s (x / 10);
    Buffer.add_char s (dec (abs d))

let write_int ob x =
  if x > 0 then
    write_digits ob x
  else if x < 0 then (
    Buffer.add_char ob '-';
    write_digits ob x
  )
  else
    Buffer.add_char ob '0'


let json_string_of_int i =
  string_of_int i


(*
  Ensure that the float is not printed as an int.
  This is not required by JSON, but useful in order to guarantee
  reversibility.
*)
let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with
          '0'..'9' | '-' -> ()
        | _ -> raise Exit
    done;
    true
  with Exit ->
    false

(*
  Both write_float_fast and write_float guarantee
  that a sufficient number of digits are printed in order to
  allow reversibility.

  The _fast version is faster but often produces unnecessarily long numbers.
*)
let write_float_fast ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s = Printf.sprintf "%.17g" x in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_float ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_normal_float_prec significant_figures ob x =
  let open Printf in
  let s =
    match significant_figures with
        1 -> sprintf "%.1g" x
      | 2 -> sprintf "%.2g" x
      | 3 -> sprintf "%.3g" x
      | 4 -> sprintf "%.4g" x
      | 5 -> sprintf "%.5g" x
      | 6 -> sprintf "%.6g" x
      | 7 -> sprintf "%.7g" x
      | 8 -> sprintf "%.8g" x
      | 9 -> sprintf "%.9g" x
      | 10 -> sprintf "%.10g" x
      | 11 -> sprintf "%.11g" x
      | 12 -> sprintf "%.12g" x
      | 13 -> sprintf "%.13g" x
      | 14 -> sprintf "%.14g" x
      | 15 -> sprintf "%.15g" x
      | 16 -> sprintf "%.16g" x
      | _ -> sprintf "%.17g" x
  in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

let write_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      write_normal_float_prec significant_figures ob x

let json_string_of_float x =
  let ob = Buffer.create 20 in
  write_float ob x;
  Buffer.contents ob


let write_std_float_fast ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s = Printf.sprintf "%.17g" x in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_std_float ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_std_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      write_normal_float_prec significant_figures ob x

let std_json_string_of_float x =
  let ob = Buffer.create 20 in
  write_std_float ob x;
  Buffer.contents ob


let test_float () =
  let l = [ 0.; 1.; -1. ] in
  let l = l @ List.map (fun x -> 2. *. x +. 1.) l in
  let l = l @ List.map (fun x -> x /. sqrt 2.) l in
  let l = l @ List.map (fun x -> x *. sqrt 3.) l in
  let l = l @ List.map cos l in
  let l = l @ List.map (fun x -> x *. 1.23e50) l in
  let l = l @ [ infinity; neg_infinity ] in
  List.iter (
    fun x ->
      let s = Printf.sprintf "%.17g" x in
      let y = float_of_string s in
      Printf.printf "%g %g %S %B\n" x y s (x = y)
  )
    l

(*
let () = test_float ()
*)

let write_intlit = Buffer.add_string
let write_floatlit = Buffer.add_string
let write_stringlit = Buffer.add_string

let rec iter2_aux f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_sep x;
      f_elt x y;
      iter2_aux f_elt f_sep x l

let iter2 f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_elt x y;
      iter2_aux f_elt f_sep x l

let f_sep ob =
  Buffer.add_char ob ','

let rec write_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    
# 293 "write.ml"
    | `Int i -> write_int ob i
    
# 299 "write.ml"
    | `Float f -> write_float ob f
    
# 305 "write.ml"
    | `String s -> write_string ob s
    
# 310 "write.ml"
    | `Assoc l -> write_assoc ob l
    | `List l -> write_list ob l

# 319 "write.ml"
and write_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_list ob l =
  Buffer.add_char ob '[';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ']'



# 354 "write.ml"
let write_t = write_json

let rec write_std_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    
# 361 "write.ml"
    | `Int i -> write_int ob i
    
# 367 "write.ml"
    | `Float f -> write_std_float ob f
    
# 373 "write.ml"
    | `String s -> write_string ob s
    
# 378 "write.ml"
    | `Assoc l -> write_std_assoc ob l
    | `List l -> write_std_list ob l

# 387 "write.ml"
and write_std_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_std_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_std_list ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'

and write_std_tuple ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'



# 420 "write.ml"
let to_buffer ?(std = false) ob x =
  if std then (
    if not (is_object_or_array x) then
      json_error "Root is not an object or array"
    else
      write_std_json ob x
  )
  else
    write_json ob x


let to_string ?buf ?(len = 256) ?std x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  to_buffer ?std ob x;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let to_channel ?buf ?(len=4096) ?std oc x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  to_buffer ?std ob x;
  Buffer.output_buffer oc ob

let to_output ?buf ?(len=4096) ?std out x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  to_buffer ?std ob x;
  out#output (Buffer.contents ob) 0 (Buffer.length ob);
  ()

let to_file ?len ?std file x =
  let oc = open_out file in
  try
    to_channel ?len ?std oc x;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let stream_to_buffer ?std ob st =
  Stream.iter (to_buffer ?std ob) st

let stream_to_string ?buf ?(len = 256) ?std st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  stream_to_buffer ?std ob st;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let stream_to_channel ?buf ?(len=2096) ?std oc st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  stream_to_buffer ?std ob st

let stream_to_file ?len ?std file st =
  let oc = open_out file in
  try
    stream_to_channel ?len ?std oc st;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e


let rec sort = function
  | `Assoc l ->
      let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
      `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
  | `List l ->
      `List (List.rev (List.rev_map sort l))
  
# 523 "write.ml"
  | x -> x
# 1 "monomorphic.ml"
let rec pp fmt =
  function
  | `Null -> Format.pp_print_string fmt "`Null"
  | `Bool x ->
    Format.fprintf fmt "`Bool (@[<hov>";
    Format.fprintf fmt "%B" x;
    Format.fprintf fmt "@])"
  
# 9 "monomorphic.ml"
  | `Int x ->
    Format.fprintf fmt "`Int (@[<hov>";
    Format.fprintf fmt "%d" x;
    Format.fprintf fmt "@])"
  
# 21 "monomorphic.ml"
  | `Float x ->
    Format.fprintf fmt "`Float (@[<hov>";
    Format.fprintf fmt "%F" x;
    Format.fprintf fmt "@])"
  
# 33 "monomorphic.ml"
  | `String x ->
    Format.fprintf fmt "`String (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 44 "monomorphic.ml"
  | `Assoc xs ->
    Format.fprintf fmt "`Assoc (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep (key, value) ->
        if sep then
          Format.fprintf fmt ";@ ";
          Format.fprintf fmt "(@[";
          Format.fprintf fmt "%S" key;
          Format.fprintf fmt ",@ ";
          pp fmt value;
          Format.fprintf fmt "@])";
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  | `List xs ->
    Format.fprintf fmt "`List (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep x ->
        if sep then
          Format.fprintf fmt ";@ ";
          pp fmt x;
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"

# 99 "monomorphic.ml"
let show x =
  Format.asprintf "%a" pp x

let rec equal a b =
  match a, b with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
  
# 107 "monomorphic.ml"
  | `Int a, `Int b -> a = b
    
# 113 "monomorphic.ml"
    | `Float a, `Float b -> a = b
    
# 119 "monomorphic.ml"
    | `String a, `String b -> a = b
    
# 124 "monomorphic.ml"
    | `Assoc xs, `Assoc ys ->
      let compare_keys = fun (key, _) (key', _) -> String.compare key key' in
      let xs = List.stable_sort compare_keys xs in
      let ys = List.stable_sort compare_keys ys in
      (match List.for_all2 (fun (key, value) (key', value') ->
        match key = key' with
        | false -> false
        | true -> equal value value') xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    
# 139 "monomorphic.ml"
    | `List xs, `List ys ->
      (match List.for_all2 equal xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    
# 155 "monomorphic.ml"
    | _ -> false

# 2 "write2.ml"
let pretty_print ?std out (x : t) =
  Pretty.pp ?std out (x :> json_max)

let pretty_to_string ?std (x : t) =
  Pretty.to_string ?std (x :> json_max)

let pretty_to_channel ?std oc (x : t) =
  Pretty.to_channel ?std oc (x :> json_max)

# 1 "lib/read.mll"
 
  
# 2 "lib/read.mll"
  module Lexing =
    (*
      We override Lexing.engine in order to avoid creating a new position
      record each time a rule is matched.
      This reduces total parsing time by about 31%.
    *)
  struct
    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
        buf.lex_start_p <- buf.lex_curr_p;
        buf.lex_curr_p <- {buf.lex_curr_p
                           with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end

  open Printf
  open Lexing

  (* see description in common.mli *)
  type lexer_state = Lexer_state.t = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }

  let dec c =
    Char.code c - 48

  let hex c =
    match c with
        '0'..'9' -> int_of_char c - int_of_char '0'
      | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
      | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
      | _ -> assert false

  let custom_error descr v lexbuf =
    let offs = lexbuf.lex_abs_pos - 1 in
    let bol = v.bol in
    let pos1 = offs + lexbuf.lex_start_pos - bol - 1 in
    let pos2 = max pos1 (offs + lexbuf.lex_curr_pos - bol) in
    let file_line =
      match v.fname with
          None -> "Line"
        | Some s ->
            sprintf "File %s, line" s
    in
    let bytes =
      if pos1 = pos2 then
        sprintf "byte %i" (pos1+1)
      else
        sprintf "bytes %i-%i" (pos1+1) (pos2+1)
    in
    let msg = sprintf "%s %i, %s:\n%s" file_line v.lnum bytes descr in
    json_error msg


  let lexer_error descr v lexbuf =
    custom_error
      (sprintf "%s '%s'" descr (Lexing.lexeme lexbuf))
      v lexbuf

  let read_junk = ref (fun _ -> assert false)

  let long_error descr v lexbuf =
    let junk = Lexing.lexeme lexbuf in
    let extra_junk = !read_junk lexbuf in
    custom_error
      (sprintf "%s '%s%s'" descr junk extra_junk)
      v lexbuf

  let min10 = min_int / 10 - (if min_int mod 10 = 0 then 0 else 1)
  let max10 = max_int / 10 + (if max_int mod 10 = 0 then 0 else 1)

  exception Int_overflow

  let extract_positive_int lexbuf =
    let start = lexbuf.lex_start_pos in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n >= max10 then
        raise Int_overflow
      else
        n := 10 * !n + dec (Bytes.get s i)
    done;
    if !n < 0 then
      raise Int_overflow
    else
      !n

  let make_positive_int v lexbuf =
      
# 104 "lib/read.mll"
      try `Int (extract_positive_int lexbuf)
      with Int_overflow ->
        
# 110 "lib/read.mll"
        lexer_error "Int overflow" v lexbuf

  
# 113 "lib/read.mll"
  let extract_negative_int lexbuf =
    let start = lexbuf.lex_start_pos + 1 in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n <= min10 then
        raise Int_overflow
      else
        n := 10 * !n - dec (Bytes.get s i)
    done;
    if !n > 0 then
      raise Int_overflow
    else
      !n

  let make_negative_int v lexbuf =
      
# 131 "lib/read.mll"
      try `Int (extract_negative_int lexbuf)
      with Int_overflow ->
        
# 137 "lib/read.mll"
        lexer_error "Int overflow" v lexbuf


  
# 141 "lib/read.mll"
  let set_file_name v fname =
    v.fname <- fname

  let newline v lexbuf =
    v.lnum <- v.lnum + 1;
    v.bol <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos

  let add_lexeme buf lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_subbytes buf lexbuf.lex_buffer lexbuf.lex_start_pos len

  let map_lexeme f lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    f (Bytes.to_string lexbuf.lex_buffer) lexbuf.lex_start_pos len

  type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]
  type tuple_kind = [ `Parenthesis | `Square_bracket ]


# 161 "lib/read.ml"
# 161 "lib/read.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\236\255\237\255\003\000\239\255\016\000\242\255\243\255\
    \244\255\245\255\000\000\031\000\249\255\085\000\001\000\000\000\
    \000\000\001\000\000\000\001\000\002\000\255\255\000\000\000\000\
    \003\000\254\255\001\000\004\000\253\255\011\000\252\255\003\000\
    \001\000\003\000\002\000\003\000\000\000\251\255\021\000\097\000\
    \010\000\022\000\020\000\016\000\022\000\012\000\008\000\250\255\
    \119\000\129\000\139\000\161\000\171\000\181\000\193\000\209\000\
    \240\255\011\000\038\000\252\255\065\000\254\255\255\255\110\000\
    \252\255\163\000\254\255\255\255\234\000\247\255\248\255\048\001\
    \250\255\251\255\252\255\253\255\254\255\255\255\071\001\126\001\
    \149\001\249\255\039\000\253\255\254\255\038\000\187\001\210\001\
    \248\001\015\002\255\255\220\000\253\255\255\255\245\000\039\002\
    \109\002\014\001\088\002\164\002\187\002\225\002\013\000\252\255\
    \253\255\254\255\255\255\014\000\253\255\254\255\255\255\030\000\
    \253\255\254\255\255\255\015\000\253\255\254\255\255\255\017\001\
    \251\255\252\255\253\255\254\255\255\255\019\000\252\255\253\255\
    \254\255\015\000\255\255\016\000\255\255\008\001\005\000\253\255\
    \023\000\254\255\020\000\255\255\046\000\253\255\254\255\042\000\
    \052\000\053\000\255\255\053\000\048\000\091\000\092\000\255\255\
    \027\001\250\255\251\255\137\000\104\000\089\000\088\000\106\000\
    \255\255\143\000\137\000\177\000\254\255\183\000\168\000\166\000\
    \183\000\002\000\253\255\177\000\172\000\187\000\004\000\252\255\
    \053\002\251\255\252\255\253\255\103\001\255\255\248\002\254\255\
    \006\003\030\003\252\255\253\255\254\255\255\255\040\003\050\003\
    \074\003\252\255\253\255\254\255\255\255\061\003\084\003\108\003\
    \249\255\250\255\251\255\244\000\120\003\142\003\179\000\194\000\
    \015\000\255\255\190\000\188\000\187\000\193\000\183\000\179\000\
    \254\255\191\000\201\000\200\000\196\000\203\000\193\000\189\000\
    \253\255\157\003\095\003\174\003\196\003\206\003\216\003\228\003\
    \239\003\060\000\253\255\254\255\255\255\012\004\252\255\253\255\
    \087\004\255\255\145\004\252\255\253\255\221\004\255\255\229\000\
    \253\255\254\255\255\255\231\000\253\255\254\255\255\255\002\000\
    \255\255\018\001\252\255\253\255\254\255\255\255\034\001\253\255\
    \254\255\255\255\000\000\255\255\003\000\254\255\255\255\038\001\
    \252\255\253\255\254\255\255\255\120\001\251\255\252\255\253\255\
    \254\255\255\255\208\000\253\255\254\255\255\255\211\000\253\255\
    \254\255\255\255\189\000\255\255\143\001\252\255\253\255\254\255\
    \255\255\013\001\253\255\254\255\255\255\095\001\252\255\253\255\
    \254\255\255\255\050\001\253\255\254\255\255\255\026\001\253\255\
    \254\255\255\255\233\000\253\255\254\255\255\255\222\000\253\255\
    \254\255\255\255\079\005\237\255\238\255\010\000\240\255\044\001\
    \243\255\244\255\245\255\246\255\061\001\002\004\249\255\045\005\
    \209\000\228\000\211\000\232\000\225\000\223\000\240\000\255\255\
    \235\000\234\000\008\001\254\255\004\001\023\001\253\255\054\001\
    \252\255\031\001\029\001\032\001\039\001\049\001\045\001\251\255\
    \057\001\082\001\080\001\078\001\084\001\074\001\086\001\250\255\
    \110\005\012\004\123\005\155\005\165\005\177\005\187\005\197\005\
    \241\255\199\001\077\002\253\255\255\255\154\002\222\005\209\005\
    \155\002\239\005\053\006\076\006\114\006\016\002\252\255\253\255\
    \254\255\255\255\152\006\252\255\253\255\227\006\255\255\085\007\
    \244\255\245\255\011\000\247\255\076\002\250\255\251\255\252\255\
    \253\255\254\255\031\002\243\005\051\007\100\001\115\001\104\001\
    \133\001\118\001\154\001\171\001\255\255\173\001\176\001\191\001\
    \185\001\187\001\253\001\230\001\230\001\234\001\247\001\237\001\
    \234\001\009\002\019\002\019\002\015\002\021\002\011\002\007\002\
    \142\006\152\006\116\007\170\007\180\007\190\007\200\007\210\007\
    \248\255\120\002\167\002\253\255\255\255\216\002\082\007\220\007\
    \236\002\244\007\058\008\081\008\119\008\076\002\252\255\253\255\
    \254\255\255\255\157\008\252\255\253\255\232\008\255\255\135\002\
    \120\002\253\255\100\002\254\255\182\002\255\255\011\002\255\255\
    \204\002\252\255\253\255\254\255\255\255\046\002\255\255\178\002\
    \252\255\253\255\254\255\255\255\023\000\255\255\183\002\252\255\
    \253\255\254\255\255\255\187\002\253\255\254\255\255\255\121\002\
    \253\255\254\255\255\255\184\002\252\255\253\255\254\255\019\000\
    \255\255\140\001\146\001\255\255\150\001\151\001\154\001\168\001\
    \170\001\171\001\172\001\173\001\181\001\184\001\185\001\187\001\
    \191\001\193\001\195\001\196\001\197\001\200\001\203\001\223\001\
    \225\001\228\001\249\001\251\001\002\002\004\002\011\002\012\002\
    \013\002\000\000";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\017\000\255\255\019\000\255\255\255\255\
    \255\255\255\255\007\000\007\000\255\255\019\000\019\000\019\000\
    \019\000\019\000\019\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\008\000\008\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\009\000\255\255\009\000\255\255\009\000\255\255\
    \255\255\014\000\255\255\255\255\002\000\255\255\255\255\255\255\
    \255\255\002\000\255\255\255\255\255\255\255\255\255\255\007\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\001\000\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\255\255\001\000\255\255\004\000\003\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\
    \255\255\255\255\255\255\001\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\004\000\004\000\004\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\255\255\000\000\255\255\
    \001\000\255\255\255\255\255\255\255\255\255\255\000\000\002\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\002\000\255\255\
    \255\255\255\255\255\255\003\000\003\000\005\000\005\000\005\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\003\000\255\255\003\000\255\255\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \001\000\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\002\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\255\255\018\000\
    \255\255\255\255\255\255\255\255\007\000\007\000\255\255\018\000\
    \018\000\018\000\018\000\018\000\018\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\008\000\255\255\008\000\255\255\008\000\255\255\
    \255\255\013\000\255\255\255\255\255\255\001\000\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\009\000\255\255\011\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\255\255\000\000\255\255\
    \255\255\006\000\255\255\255\255\255\255\001\000\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\004\000\
    \003\000\255\255\255\255\255\255\255\255\255\255\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\001\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\000\000\255\255\000\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\057\000\060\000\000\000\060\000\000\000\000\000\065\000\
    \000\000\065\000\000\000\000\000\070\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\084\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\094\000\000\000\000\000\097\000\255\255\
    \255\255\097\000\255\255\255\255\255\255\255\255\104\000\000\000\
    \000\000\000\000\000\000\109\000\000\000\000\000\000\000\113\000\
    \000\000\000\000\000\000\117\000\000\000\000\000\000\000\121\000\
    \000\000\000\000\000\000\000\000\000\000\126\000\000\000\000\000\
    \000\000\255\255\000\000\255\255\000\000\255\255\255\255\000\000\
    \255\255\000\000\138\000\000\000\142\000\000\000\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \154\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \178\000\000\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \255\255\187\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \194\000\000\000\000\000\000\000\000\000\255\255\255\255\201\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\235\000\000\000\000\000\000\000\239\000\000\000\000\000\
    \255\255\000\000\244\000\000\000\000\000\255\255\000\000\249\000\
    \000\000\000\000\000\000\253\000\000\000\000\000\000\000\255\255\
    \000\000\003\001\000\000\000\000\000\000\000\000\008\001\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\017\001\
    \000\000\000\000\000\000\000\000\022\001\000\000\000\000\000\000\
    \000\000\000\000\028\001\000\000\000\000\000\000\032\001\000\000\
    \000\000\000\000\255\255\000\000\038\001\000\000\000\000\000\000\
    \000\000\043\001\000\000\000\000\000\000\047\001\000\000\000\000\
    \000\000\000\000\052\001\000\000\000\000\000\000\056\001\000\000\
    \000\000\000\000\060\001\000\000\000\000\000\000\064\001\000\000\
    \000\000\000\000\067\001\000\000\000\000\255\255\000\000\255\255\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\121\001\125\001\000\000\000\000\128\001\255\255\255\255\
    \128\001\255\255\255\255\255\255\255\255\135\001\000\000\000\000\
    \000\000\000\000\140\001\000\000\000\000\255\255\000\000\144\001\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\193\001\197\001\000\000\000\000\200\001\255\255\255\255\
    \200\001\255\255\255\255\255\255\255\255\207\001\000\000\000\000\
    \000\000\000\000\212\001\000\000\000\000\255\255\000\000\255\255\
    \255\255\000\000\255\255\000\000\220\001\000\000\255\255\000\000\
    \226\001\000\000\000\000\000\000\000\000\255\255\000\000\233\001\
    \000\000\000\000\000\000\000\000\255\255\000\000\240\001\000\000\
    \000\000\000\000\000\000\245\001\000\000\000\000\000\000\249\001\
    \000\000\000\000\000\000\252\001\000\000\000\000\000\000\255\255\
    \000\000\002\002\004\002\000\000\005\002\006\002\007\002\008\002\
    \009\002\010\002\011\002\012\002\013\002\014\002\015\002\016\002\
    \017\002\018\002\019\002\020\002\021\002\022\002\023\002\024\002\
    \025\002\026\002\027\002\028\002\029\002\030\002\031\002\032\002\
    \033\002\003\002";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\004\000\000\000\003\000\003\000\134\000\000\000\
    \003\000\000\000\134\000\069\001\146\001\255\255\000\000\069\001\
    \146\001\000\000\000\000\000\000\000\000\127\000\139\000\000\000\
    \003\000\000\000\012\000\003\000\170\000\134\000\175\000\000\000\
    \007\000\011\001\069\001\146\001\014\001\013\000\049\000\005\000\
    \010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\056\000\118\000\006\000\129\000\130\000\057\000\
    \237\001\137\000\000\002\049\000\000\000\048\000\138\000\106\000\
    \062\000\014\000\110\000\105\000\000\000\049\000\015\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\030\000\048\000\008\000\114\000\209\000\236\000\000\001\
    \013\001\029\000\022\000\255\255\048\000\048\000\017\000\021\000\
    \025\000\032\000\033\000\035\000\023\000\027\000\016\000\031\000\
    \028\000\034\000\019\000\024\000\018\000\026\000\020\000\036\000\
    \041\000\037\000\048\000\009\000\042\000\043\000\044\000\045\000\
    \046\000\047\000\061\000\085\000\048\000\038\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\049\000\
    \067\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\086\000\143\000\255\255\040\000\144\000\
    \145\000\146\000\055\000\148\000\055\000\149\000\048\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\255\255\048\000\150\000\
    \151\000\161\000\066\000\158\000\053\000\159\000\053\000\160\000\
    \051\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\165\000\
    \051\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\162\000\163\000\166\000\093\000\255\255\
    \002\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\255\255\077\000\103\000\108\000\116\000\
    \132\000\134\000\135\000\128\000\139\000\134\000\164\000\093\000\
    \171\000\077\000\167\000\168\000\169\000\172\000\112\000\173\000\
    \174\000\210\000\226\000\208\000\211\000\212\000\059\000\083\000\
    \134\000\213\000\214\000\215\000\216\000\218\000\141\000\219\000\
    \093\000\220\000\221\000\123\000\222\000\223\000\224\000\136\000\
    \095\000\225\000\035\001\065\001\234\000\155\000\005\001\097\001\
    \250\000\255\255\254\000\057\001\061\001\095\001\077\000\044\001\
    \092\001\088\001\009\001\029\001\076\000\124\000\033\001\018\001\
    \075\000\098\000\019\001\085\001\086\001\087\001\120\001\089\001\
    \074\000\225\000\053\001\121\001\073\000\090\001\072\000\071\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\098\000\113\001\122\000\091\001\064\000\004\001\
    \093\001\078\000\078\000\078\000\078\000\078\000\078\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\156\000\112\001\094\001\096\001\098\001\099\001\049\001\
    \079\000\079\000\079\000\079\000\079\000\079\000\100\001\157\000\
    \101\001\078\000\078\000\078\000\078\000\078\000\078\000\183\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\024\001\112\001\255\255\025\001\102\001\103\001\105\001\
    \079\000\079\000\079\000\079\000\079\000\079\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \106\001\107\001\048\001\040\001\108\001\109\001\110\001\080\000\
    \080\000\080\000\080\000\080\000\080\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\111\001\
    \027\001\255\255\171\001\031\001\170\001\023\001\081\000\081\000\
    \081\000\081\000\081\000\081\000\092\000\168\001\063\001\080\000\
    \080\000\080\000\080\000\080\000\080\000\248\000\165\001\252\000\
    \162\001\059\001\069\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\255\255\081\000\081\000\
    \081\000\081\000\081\000\081\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\039\001\042\001\255\255\163\001\
    \164\001\120\000\002\001\088\000\088\000\088\000\088\000\088\000\
    \088\000\166\001\055\001\153\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\007\001\167\001\164\001\169\001\016\001\164\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\051\001\088\000\088\000\088\000\088\000\088\000\
    \088\000\089\000\089\000\089\000\089\000\089\000\089\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\097\000\137\001\164\001\172\001\185\001\136\001\173\001\
    \090\000\090\000\090\000\090\000\090\000\090\000\097\000\179\000\
    \174\001\089\000\089\000\089\000\089\000\089\000\089\000\046\001\
    \175\001\176\001\180\000\164\001\184\001\181\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\124\001\
    \090\000\090\000\090\000\090\000\090\000\090\000\192\001\178\001\
    \021\001\179\001\097\000\193\001\180\001\181\001\182\001\183\001\
    \164\001\216\001\255\255\097\000\184\001\216\001\209\001\097\000\
    \223\001\097\000\208\001\230\001\003\002\097\000\219\001\037\001\
    \216\001\217\001\003\002\220\001\216\001\097\000\003\002\003\002\
    \216\001\097\000\003\002\097\000\096\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\216\001\
    \003\002\126\001\003\002\003\002\003\002\003\002\099\000\099\000\
    \099\000\099\000\099\000\099\000\097\000\003\002\218\001\250\001\
    \003\002\003\002\097\000\003\002\124\001\124\001\097\000\003\002\
    \221\001\003\002\253\001\003\002\003\002\003\002\097\000\255\255\
    \003\002\196\001\097\000\003\002\097\000\096\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\235\001\003\002\
    \241\001\003\002\255\001\242\001\003\002\100\000\100\000\100\000\
    \100\000\100\000\100\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\246\001\129\001\129\001\
    \228\001\003\002\196\001\003\002\101\000\101\000\101\000\101\000\
    \101\000\101\000\003\002\198\001\003\002\100\000\100\000\100\000\
    \100\000\100\000\100\000\003\002\003\002\003\002\196\001\234\001\
    \134\001\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\000\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\000\000\000\000\201\001\177\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \188\000\000\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \201\001\227\001\000\000\191\000\206\001\123\001\189\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\189\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\195\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\198\000\
    \255\255\248\001\196\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\196\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\202\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\205\000\255\255\255\255\203\000\204\000\204\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\226\000\195\001\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\232\001\000\000\000\000\206\000\221\001\239\001\
    \254\001\000\000\207\000\244\001\000\000\225\000\203\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \232\000\000\000\232\000\000\000\225\001\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\217\000\
    \255\255\000\000\000\000\000\000\000\000\225\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\230\000\
    \000\000\230\000\000\000\228\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\000\000\228\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\186\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\000\000\000\000\000\000\000\000\000\000\241\000\000\000\
    \113\001\000\000\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\114\001\000\000\112\001\
    \000\000\000\000\193\000\000\000\000\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\112\001\
    \000\000\000\000\000\000\240\000\200\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\000\000\246\000\000\000\000\000\240\000\000\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\000\000\000\000\000\000\000\000\
    \245\000\000\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\238\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \000\000\000\000\000\000\000\000\245\000\000\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \069\001\070\001\000\000\000\000\069\001\076\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\069\001\
    \000\000\078\001\000\000\000\000\000\000\000\000\104\001\073\001\
    \000\000\000\000\000\000\000\000\079\001\000\000\071\001\076\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\000\000\000\000\072\001\000\000\000\000\000\000\000\000\
    \000\000\243\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \080\001\119\001\000\000\119\001\000\000\081\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\118\001\
    \000\000\000\000\074\001\114\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\083\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\082\001\000\000\000\000\
    \115\001\000\000\000\000\084\001\000\000\000\000\117\001\000\000\
    \117\001\000\000\075\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\000\000\
    \115\001\116\001\116\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\000\000\
    \128\001\130\001\130\001\130\001\130\001\130\001\130\001\130\001\
    \130\001\130\001\130\001\000\000\000\000\128\001\000\000\000\000\
    \000\000\128\001\130\001\130\001\130\001\130\001\130\001\130\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\128\001\000\000\
    \000\000\185\001\000\000\155\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\000\000\000\000\000\000\
    \000\000\000\000\130\001\130\001\130\001\130\001\130\001\130\001\
    \184\001\000\000\128\001\000\000\000\000\000\000\000\000\000\000\
    \128\001\000\000\000\000\000\000\128\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\128\001\128\001\000\000\000\000\068\001\
    \128\001\128\001\128\001\127\001\000\000\128\001\000\000\000\000\
    \184\001\000\000\000\000\000\000\000\000\128\001\000\000\000\000\
    \000\000\128\001\000\000\128\001\127\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\131\001\131\001\
    \131\001\131\001\131\001\131\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\132\001\132\001\132\001\
    \132\001\132\001\132\001\000\000\000\000\000\000\131\001\131\001\
    \131\001\131\001\131\001\131\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\128\001\128\001\128\001\128\001\128\001\128\001\
    \128\001\128\001\128\001\128\001\000\000\132\001\132\001\132\001\
    \132\001\132\001\132\001\128\001\128\001\128\001\128\001\128\001\
    \128\001\191\001\142\001\191\001\000\000\000\000\190\001\190\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\000\000\128\001\128\001\128\001\128\001\128\001\
    \128\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\000\000\000\000\000\000\000\000\141\001\
    \000\000\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\000\000\000\000\
    \000\000\000\000\141\001\000\000\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\146\001\147\001\
    \000\000\000\000\146\001\154\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\200\001\146\001\000\000\153\001\
    \000\000\000\000\000\000\000\000\177\001\150\001\000\000\000\000\
    \000\000\200\001\156\001\000\000\148\001\154\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\155\001\155\001\000\000\
    \000\000\149\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \139\001\000\000\000\000\000\000\000\000\000\000\157\001\000\000\
    \000\000\000\000\000\000\158\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\200\001\000\000\
    \151\001\000\000\000\000\000\000\200\001\000\000\000\000\000\000\
    \200\001\187\001\000\000\160\001\000\000\000\000\000\000\000\000\
    \200\001\000\000\000\000\159\001\200\001\000\000\200\001\199\001\
    \000\000\161\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \152\001\000\000\000\000\000\000\000\000\189\001\000\000\189\001\
    \000\000\187\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\190\001\190\001\202\001\202\001\202\001\202\001\
    \202\001\202\001\202\001\202\001\202\001\202\001\200\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\202\001\202\001\202\001\
    \202\001\202\001\202\001\200\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\202\001\202\001\202\001\
    \202\001\202\001\202\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \200\001\000\000\000\000\000\000\000\000\145\001\200\001\000\000\
    \000\000\000\000\200\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\200\001\000\000\000\000\000\000\200\001\000\000\
    \200\001\199\001\203\001\203\001\203\001\203\001\203\001\203\001\
    \203\001\203\001\203\001\203\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\203\001\203\001\203\001\203\001\203\001\
    \203\001\204\001\204\001\204\001\204\001\204\001\204\001\204\001\
    \204\001\204\001\204\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\204\001\204\001\204\001\204\001\204\001\204\001\
    \000\000\000\000\000\000\203\001\203\001\203\001\203\001\203\001\
    \203\001\000\000\000\000\000\000\000\000\000\000\000\000\200\001\
    \200\001\200\001\200\001\200\001\200\001\200\001\200\001\200\001\
    \200\001\000\000\204\001\204\001\204\001\204\001\204\001\204\001\
    \200\001\200\001\200\001\200\001\200\001\200\001\000\000\214\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \200\001\200\001\200\001\200\001\200\001\200\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \000\000\000\000\000\000\000\000\213\001\000\000\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\000\000\000\000\000\000\000\000\213\001\
    \000\000\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\211\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\003\000\000\000\134\000\255\255\
    \003\000\255\255\134\000\069\001\146\001\057\000\255\255\069\001\
    \146\001\255\255\255\255\255\255\255\255\125\000\138\000\255\255\
    \000\000\255\255\000\000\003\000\169\000\134\000\174\000\255\255\
    \000\000\010\001\069\001\146\001\012\001\000\000\010\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\005\000\115\000\000\000\125\000\129\000\005\000\
    \236\001\136\000\255\001\038\000\255\255\010\000\136\000\102\000\
    \058\000\000\000\107\000\102\000\255\255\011\000\000\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\029\000\038\000\000\000\111\000\208\000\233\000\255\000\
    \012\001\015\000\017\000\060\000\011\000\010\000\000\000\020\000\
    \024\000\031\000\032\000\034\000\022\000\026\000\000\000\014\000\
    \027\000\033\000\018\000\023\000\000\000\016\000\019\000\035\000\
    \040\000\036\000\038\000\000\000\041\000\042\000\043\000\044\000\
    \045\000\046\000\058\000\082\000\011\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\039\000\
    \063\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\085\000\140\000\060\000\013\000\143\000\
    \144\000\145\000\048\000\147\000\048\000\148\000\039\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\065\000\039\000\149\000\
    \150\000\156\000\063\000\157\000\051\000\158\000\051\000\159\000\
    \050\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\155\000\
    \050\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\161\000\162\000\155\000\091\000\065\000\
    \000\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\057\000\068\000\102\000\107\000\115\000\
    \131\000\133\000\133\000\125\000\138\000\133\000\163\000\094\000\
    \165\000\068\000\166\000\167\000\168\000\171\000\111\000\172\000\
    \173\000\206\000\203\000\207\000\210\000\211\000\058\000\082\000\
    \133\000\212\000\213\000\214\000\215\000\217\000\140\000\218\000\
    \097\000\219\000\220\000\119\000\221\000\222\000\223\000\133\000\
    \091\000\203\000\034\001\062\001\233\000\152\000\001\001\080\001\
    \247\000\060\000\251\000\054\001\058\001\081\001\068\000\041\001\
    \082\001\083\001\006\001\026\001\068\000\119\000\030\001\015\001\
    \068\000\094\000\015\001\084\001\085\001\086\001\071\001\088\001\
    \068\000\203\000\050\001\071\001\068\000\089\001\068\000\068\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\097\000\076\001\119\000\090\001\063\000\001\001\
    \092\001\071\000\071\000\071\000\071\000\071\000\071\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\152\000\076\001\093\001\095\001\097\001\098\001\045\001\
    \078\000\078\000\078\000\078\000\078\000\078\000\099\001\152\000\
    \100\001\071\000\071\000\071\000\071\000\071\000\071\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\020\001\076\001\065\000\020\001\101\001\102\001\104\001\
    \078\000\078\000\078\000\078\000\078\000\078\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \105\001\106\001\045\001\036\001\107\001\108\001\109\001\079\000\
    \079\000\079\000\079\000\079\000\079\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\110\001\
    \026\001\121\001\157\001\030\001\158\001\020\001\080\000\080\000\
    \080\000\080\000\080\000\080\000\091\000\159\001\062\001\079\000\
    \079\000\079\000\079\000\079\000\079\000\247\000\160\001\251\000\
    \161\001\058\001\068\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\094\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\036\001\041\001\097\000\162\001\
    \163\001\119\000\001\001\087\000\087\000\087\000\087\000\087\000\
    \087\000\165\001\054\001\152\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\006\001\166\001\167\001\168\001\015\001\169\001\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\050\001\087\000\087\000\087\000\087\000\087\000\
    \087\000\088\000\088\000\088\000\088\000\088\000\088\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\095\000\133\001\170\001\171\001\154\001\133\001\172\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\095\000\176\000\
    \173\001\088\000\088\000\088\000\088\000\088\000\088\000\045\001\
    \174\001\175\001\176\000\176\001\154\001\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\122\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\148\001\177\001\
    \020\001\178\001\098\000\148\001\179\001\180\001\181\001\182\001\
    \183\001\216\001\193\001\095\000\154\001\216\001\205\001\098\000\
    \222\001\095\000\205\001\229\001\001\002\095\000\218\001\036\001\
    \215\001\215\001\002\002\218\001\215\001\095\000\004\002\005\002\
    \216\001\095\000\006\002\095\000\095\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\215\001\
    \007\002\122\001\008\002\009\002\010\002\011\002\096\000\096\000\
    \096\000\096\000\096\000\096\000\098\000\012\002\215\001\247\001\
    \013\002\014\002\098\000\015\002\125\001\128\001\098\000\016\002\
    \220\001\017\002\251\001\018\002\019\002\020\002\098\000\121\001\
    \021\002\194\001\098\000\022\002\098\000\098\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\231\001\023\002\
    \238\001\024\002\251\001\238\001\025\002\099\000\099\000\099\000\
    \099\000\099\000\099\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\243\001\125\001\128\001\
    \224\001\026\002\197\001\027\002\100\000\100\000\100\000\100\000\
    \100\000\100\000\028\002\194\001\029\002\099\000\099\000\099\000\
    \099\000\099\000\099\000\030\002\031\002\032\002\200\001\231\001\
    \133\001\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\255\255\100\000\100\000\100\000\100\000\
    \100\000\100\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\255\255\255\255\197\001\176\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \185\000\255\255\101\000\101\000\101\000\101\000\101\000\101\000\
    \200\001\224\001\255\255\185\000\205\001\122\001\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\192\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\192\000\
    \193\001\247\001\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\198\000\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\199\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\199\000\125\001\128\001\199\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\204\000\194\001\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\231\001\255\255\255\255\199\000\220\001\238\001\
    \251\001\255\255\199\000\243\001\255\255\204\000\205\000\205\000\
    \205\000\205\000\205\000\205\000\205\000\205\000\205\000\205\000\
    \225\000\255\255\225\000\255\255\224\001\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\225\000\225\000\225\000\205\000\
    \197\001\255\255\255\255\255\255\255\255\204\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \255\255\255\255\255\255\255\255\200\001\255\255\255\255\228\000\
    \255\255\228\000\255\255\227\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\255\255\227\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\185\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\255\255\255\255\255\255\255\255\255\255\237\000\255\255\
    \077\001\255\255\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\113\001\113\001\113\001\113\001\
    \113\001\113\001\113\001\113\001\113\001\113\001\255\255\077\001\
    \255\255\255\255\192\000\255\255\255\255\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\077\001\
    \255\255\255\255\255\255\237\000\199\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\255\255\242\000\255\255\255\255\240\000\255\255\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\255\255\255\255\255\255\255\255\
    \242\000\255\255\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\237\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \255\255\255\255\255\255\255\255\245\000\255\255\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \066\001\066\001\255\255\255\255\066\001\079\001\079\001\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\066\001\
    \255\255\066\001\255\255\255\255\255\255\255\255\079\001\066\001\
    \255\255\255\255\255\255\255\255\066\001\255\255\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\255\255\255\255\066\001\255\255\255\255\255\255\255\255\
    \255\255\242\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \066\001\112\001\255\255\112\001\255\255\066\001\112\001\112\001\
    \112\001\112\001\112\001\112\001\112\001\112\001\112\001\112\001\
    \255\255\255\255\066\001\114\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\066\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\066\001\255\255\255\255\
    \114\001\255\255\255\255\066\001\255\255\255\255\115\001\255\255\
    \115\001\255\255\066\001\115\001\115\001\115\001\115\001\115\001\
    \115\001\115\001\115\001\115\001\115\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\255\255\
    \114\001\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\117\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\119\001\119\001\119\001\
    \119\001\119\001\119\001\119\001\119\001\119\001\119\001\255\255\
    \126\001\127\001\127\001\127\001\127\001\127\001\127\001\127\001\
    \127\001\127\001\127\001\255\255\255\255\126\001\255\255\255\255\
    \255\255\129\001\127\001\127\001\127\001\127\001\127\001\127\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\129\001\255\255\
    \255\255\155\001\255\255\155\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\255\255\255\255\255\255\
    \255\255\255\255\127\001\127\001\127\001\127\001\127\001\127\001\
    \155\001\255\255\126\001\255\255\255\255\255\255\255\255\255\255\
    \126\001\255\255\255\255\255\255\126\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\129\001\126\001\255\255\255\255\066\001\
    \126\001\129\001\126\001\126\001\255\255\129\001\255\255\255\255\
    \155\001\255\255\255\255\255\255\255\255\129\001\255\255\255\255\
    \255\255\129\001\255\255\129\001\129\001\130\001\130\001\130\001\
    \130\001\130\001\130\001\130\001\130\001\130\001\130\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\130\001\130\001\
    \130\001\130\001\130\001\130\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\131\001\131\001\131\001\
    \131\001\131\001\131\001\255\255\255\255\255\255\130\001\130\001\
    \130\001\130\001\130\001\130\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\255\255\131\001\131\001\131\001\
    \131\001\131\001\131\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\184\001\138\001\184\001\255\255\255\255\184\001\184\001\
    \184\001\184\001\184\001\184\001\184\001\184\001\184\001\184\001\
    \185\001\185\001\185\001\185\001\185\001\185\001\185\001\185\001\
    \185\001\185\001\255\255\132\001\132\001\132\001\132\001\132\001\
    \132\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\255\255\255\255\255\255\255\255\138\001\
    \255\255\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\255\255\255\255\
    \255\255\255\255\141\001\255\255\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\143\001\143\001\
    \255\255\255\255\143\001\156\001\156\001\156\001\156\001\156\001\
    \156\001\156\001\156\001\156\001\156\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\198\001\143\001\255\255\143\001\
    \255\255\255\255\255\255\255\255\156\001\143\001\255\255\255\255\
    \255\255\198\001\143\001\255\255\143\001\143\001\143\001\143\001\
    \143\001\143\001\143\001\143\001\143\001\143\001\143\001\255\255\
    \255\255\143\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \138\001\255\255\255\255\255\255\255\255\255\255\143\001\255\255\
    \255\255\255\255\255\255\143\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\198\001\255\255\
    \143\001\255\255\255\255\255\255\198\001\255\255\255\255\255\255\
    \198\001\186\001\255\255\143\001\255\255\255\255\255\255\255\255\
    \198\001\255\255\255\255\143\001\198\001\255\255\198\001\198\001\
    \255\255\143\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \143\001\255\255\255\255\255\255\255\255\187\001\255\255\187\001\
    \255\255\186\001\187\001\187\001\187\001\187\001\187\001\187\001\
    \187\001\187\001\187\001\187\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\189\001\189\001\
    \189\001\189\001\189\001\189\001\189\001\189\001\189\001\189\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\191\001\191\001\191\001\191\001\191\001\191\001\
    \191\001\191\001\191\001\191\001\199\001\199\001\199\001\199\001\
    \199\001\199\001\199\001\199\001\199\001\199\001\201\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\199\001\199\001\199\001\
    \199\001\199\001\199\001\201\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\199\001\199\001\199\001\
    \199\001\199\001\199\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \201\001\255\255\255\255\255\255\255\255\143\001\201\001\255\255\
    \255\255\255\255\201\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\201\001\255\255\255\255\255\255\201\001\255\255\
    \201\001\201\001\202\001\202\001\202\001\202\001\202\001\202\001\
    \202\001\202\001\202\001\202\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\202\001\202\001\202\001\202\001\202\001\
    \202\001\203\001\203\001\203\001\203\001\203\001\203\001\203\001\
    \203\001\203\001\203\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\203\001\203\001\203\001\203\001\203\001\203\001\
    \255\255\255\255\255\255\202\001\202\001\202\001\202\001\202\001\
    \202\001\255\255\255\255\255\255\255\255\255\255\255\255\204\001\
    \204\001\204\001\204\001\204\001\204\001\204\001\204\001\204\001\
    \204\001\255\255\203\001\203\001\203\001\203\001\203\001\203\001\
    \204\001\204\001\204\001\204\001\204\001\204\001\255\255\210\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \204\001\204\001\204\001\204\001\204\001\204\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \255\255\255\255\255\255\255\255\210\001\255\255\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\255\255\255\255\255\255\255\255\213\001\
    \255\255\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\210\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read_json v lexbuf =
   __ocaml_lex_read_json_rec v lexbuf 0
and __ocaml_lex_read_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 188 "lib/read.mll"
                
# 188 "lib/read.mll"
                ( `Bool true )

# 1032 "lib/read.ml"

  
# 1033 "lib/read.ml"
  | 1 ->

# 189 "lib/read.mll"
                
# 189 "lib/read.mll"
                ( `Bool false )

# 1037 "lib/read.ml"

  
# 1038 "lib/read.ml"
  | 2 ->

# 190 "lib/read.mll"
                
# 190 "lib/read.mll"
                ( `Null )

# 1042 "lib/read.ml"

  
# 1043 "lib/read.ml"
  | 3 ->

# 191 "lib/read.mll"
                
# 191 "lib/read.mll"
                (
                    
# 193 "lib/read.mll"
                    `Float nan
                
# 197 "lib/read.mll"
                )

# 1053 "lib/read.ml"

  
# 1054 "lib/read.ml"
  | 4 ->

# 198 "lib/read.mll"
                
# 198 "lib/read.mll"
                (
                    
# 200 "lib/read.mll"
                    `Float infinity
                
# 204 "lib/read.mll"
                )

# 1064 "lib/read.ml"

  
# 1065 "lib/read.ml"
  | 5 ->

# 205 "lib/read.mll"
                
# 205 "lib/read.mll"
                (
                    
# 207 "lib/read.mll"
                    `Float neg_infinity
                
# 211 "lib/read.mll"
                )

# 1075 "lib/read.ml"

  
# 1076 "lib/read.ml"
  | 6 ->

# 212 "lib/read.mll"
                
# 212 "lib/read.mll"
                (
                    
# 214 "lib/read.mll"
                    Buffer.clear v.buf;
                    `String (finish_string v lexbuf)
                
# 219 "lib/read.mll"
                )

# 1087 "lib/read.ml"

  
# 1088 "lib/read.ml"
  | 7 ->

# 220 "lib/read.mll"
                         
# 220 "lib/read.mll"
                         ( make_positive_int v lexbuf )

# 1092 "lib/read.ml"

  
# 1093 "lib/read.ml"
  | 8 ->

# 221 "lib/read.mll"
                         
# 221 "lib/read.mll"
                         ( make_negative_int v lexbuf )

# 1097 "lib/read.ml"

  
# 1098 "lib/read.ml"
  | 9 ->

# 222 "lib/read.mll"
                
# 222 "lib/read.mll"
                (
                    
# 224 "lib/read.mll"
                    `Float (float_of_string (lexeme lexbuf))
                 
# 228 "lib/read.mll"
                 )

# 1108 "lib/read.ml"

  
# 1109 "lib/read.ml"
  | 10 ->

# 230 "lib/read.mll"
                 
# 230 "lib/read.mll"
                 ( let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     let field_name = read_ident v lexbuf in
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     acc := (field_name, read_json v lexbuf) :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       let field_name = read_ident v lexbuf in
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       acc := (field_name, read_json v lexbuf) :: !acc;
                     done;
                     assert false
                   with End_of_object ->
                     `Assoc (List.rev !acc)
                 )

# 1135 "lib/read.ml"

  
# 1136 "lib/read.ml"
  | 11 ->

# 254 "lib/read.mll"
                 
# 254 "lib/read.mll"
                 ( let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     acc := read_json v lexbuf :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       acc := read_json v lexbuf :: !acc;
                     done;
                     assert false
                   with End_of_array ->
                     `List (List.rev !acc)
                 )

# 1154 "lib/read.ml"

  
# 1155 "lib/read.ml"
  | 12 ->

# 270 "lib/read.mll"
                 
# 270 "lib/read.mll"
                 (
                     
# 287 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 289 "lib/read.mll"
                 )

# 1178 "lib/read.ml"

  
# 1179 "lib/read.ml"
  | 13 ->

# 291 "lib/read.mll"
                 
# 291 "lib/read.mll"
                 (
                     
# 298 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 300 "lib/read.mll"
                 )

# 1192 "lib/read.ml"

  
# 1193 "lib/read.ml"
  | 14 ->

# 302 "lib/read.mll"
                 
# 302 "lib/read.mll"
                 ( read_json v lexbuf )

# 1197 "lib/read.ml"

  
# 1198 "lib/read.ml"
  | 15 ->

# 303 "lib/read.mll"
                 
# 303 "lib/read.mll"
                 ( finish_comment v lexbuf; read_json v lexbuf )

# 1202 "lib/read.ml"

  
# 1203 "lib/read.ml"
  | 16 ->

# 304 "lib/read.mll"
                 
# 304 "lib/read.mll"
                 ( newline v lexbuf; read_json v lexbuf )

# 1207 "lib/read.ml"

  
# 1208 "lib/read.ml"
  | 17 ->

# 305 "lib/read.mll"
                 
# 305 "lib/read.mll"
                 ( read_json v lexbuf )

# 1212 "lib/read.ml"

  
# 1213 "lib/read.ml"
  | 18 ->

# 306 "lib/read.mll"
                 
# 306 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 1217 "lib/read.ml"

  
# 1218 "lib/read.ml"
  | 19 ->

# 307 "lib/read.mll"
                 
# 307 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 1222 "lib/read.ml"

  
# 1223 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_json_rec v lexbuf __ocaml_lex_state

and finish_string v lexbuf =
   __ocaml_lex_finish_string_rec v lexbuf 58
and __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 311 "lib/read.mll"
                  
# 311 "lib/read.mll"
                  ( Buffer.contents v.buf )

# 1234 "lib/read.ml"

  
# 1235 "lib/read.ml"
  | 1 ->

# 312 "lib/read.mll"
                  
# 312 "lib/read.mll"
                  ( finish_escaped_char v lexbuf;
                    finish_string v lexbuf )

# 1240 "lib/read.ml"

  
# 1241 "lib/read.ml"
  | 2 ->

# 314 "lib/read.mll"
                  
# 314 "lib/read.mll"
                  ( add_lexeme v.buf lexbuf;
                    finish_string v lexbuf )

# 1246 "lib/read.ml"

  
# 1247 "lib/read.ml"
  | 3 ->

# 316 "lib/read.mll"
                  
# 316 "lib/read.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )

# 1251 "lib/read.ml"

  
# 1252 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state

and map_string v f lexbuf =
   __ocaml_lex_map_string_rec v f lexbuf 63
and __ocaml_lex_map_string_rec v f lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 319 "lib/read.mll"
                  
# 319 "lib/read.mll"
                  ( let b = v.buf in
                    f (Buffer.contents b) 0 (Buffer.length b) )

# 1264 "lib/read.ml"

  
# 1265 "lib/read.ml"
  | 1 ->

# 321 "lib/read.mll"
                  
# 321 "lib/read.mll"
                  ( finish_escaped_char v lexbuf;
                    map_string v f lexbuf )

# 1270 "lib/read.ml"

  
# 1271 "lib/read.ml"
  | 2 ->

# 323 "lib/read.mll"
                  
# 323 "lib/read.mll"
                  ( add_lexeme v.buf lexbuf;
                    map_string v f lexbuf )

# 1276 "lib/read.ml"

  
# 1277 "lib/read.ml"
  | 3 ->

# 325 "lib/read.mll"
                  
# 325 "lib/read.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )

# 1281 "lib/read.ml"

  
# 1282 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_map_string_rec v f lexbuf __ocaml_lex_state

and finish_escaped_char v lexbuf =
   __ocaml_lex_finish_escaped_char_rec v lexbuf 68
and __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let

# 330 "lib/read.mll"
           
# 330 "lib/read.mll"
           c

# 1294 "lib/read.ml"
# 1294 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in

# 330 "lib/read.mll"
             
# 330 "lib/read.mll"
             ( Buffer.add_char v.buf c )

# 1298 "lib/read.ml"

  
# 1299 "lib/read.ml"
  | 1 ->

# 331 "lib/read.mll"
         
# 331 "lib/read.mll"
         ( Buffer.add_char v.buf '\b' )

# 1303 "lib/read.ml"

  
# 1304 "lib/read.ml"
  | 2 ->

# 332 "lib/read.mll"
         
# 332 "lib/read.mll"
         ( Buffer.add_char v.buf '\012' )

# 1308 "lib/read.ml"

  
# 1309 "lib/read.ml"
  | 3 ->

# 333 "lib/read.mll"
         
# 333 "lib/read.mll"
         ( Buffer.add_char v.buf '\n' )

# 1313 "lib/read.ml"

  
# 1314 "lib/read.ml"
  | 4 ->

# 334 "lib/read.mll"
         
# 334 "lib/read.mll"
         ( Buffer.add_char v.buf '\r' )

# 1318 "lib/read.ml"

  
# 1319 "lib/read.ml"
  | 5 ->

# 335 "lib/read.mll"
         
# 335 "lib/read.mll"
         ( Buffer.add_char v.buf '\t' )

# 1323 "lib/read.ml"

  
# 1324 "lib/read.ml"
  | 6 ->
let

# 336 "lib/read.mll"
                
# 336 "lib/read.mll"
                a

# 1329 "lib/read.ml"
# 1329 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and

# 336 "lib/read.mll"
                           
# 336 "lib/read.mll"
                           b

# 1334 "lib/read.ml"
# 1334 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and

# 336 "lib/read.mll"
                                      
# 336 "lib/read.mll"
                                      c

# 1339 "lib/read.ml"
# 1339 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and

# 336 "lib/read.mll"
                                                 
# 336 "lib/read.mll"
                                                 d

# 1344 "lib/read.ml"
# 1344 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4) in

# 337 "lib/read.mll"
         
# 337 "lib/read.mll"
         ( let x =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if x >= 0xD800 && x <= 0xDBFF then
             finish_surrogate_pair v x lexbuf
           else
             utf8_of_code v.buf x
         )

# 1355 "lib/read.ml"

  
# 1356 "lib/read.ml"
  | 7 ->

# 345 "lib/read.mll"
         
# 345 "lib/read.mll"
         ( long_error "Invalid escape sequence" v lexbuf )

# 1360 "lib/read.ml"

  
# 1361 "lib/read.ml"
  | 8 ->

# 346 "lib/read.mll"
         
# 346 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1365 "lib/read.ml"

  
# 1366 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state

and finish_surrogate_pair v x lexbuf =
   __ocaml_lex_finish_surrogate_pair_rec v x lexbuf 82
and __ocaml_lex_finish_surrogate_pair_rec v x lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let

# 349 "lib/read.mll"
                  
# 349 "lib/read.mll"
                  a

# 1378 "lib/read.ml"
# 1378 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and

# 349 "lib/read.mll"
                             
# 349 "lib/read.mll"
                             b

# 1383 "lib/read.ml"
# 1383 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and

# 349 "lib/read.mll"
                                        
# 349 "lib/read.mll"
                                        c

# 1388 "lib/read.ml"
# 1388 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4)
and

# 349 "lib/read.mll"
                                                   
# 349 "lib/read.mll"
                                                   d

# 1393 "lib/read.ml"
# 1393 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 5) in

# 350 "lib/read.mll"
         
# 350 "lib/read.mll"
         ( let y =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if y >= 0xDC00 && y <= 0xDFFF then
             utf8_of_surrogate_pair v.buf x y
           else
             long_error "Invalid low surrogate for code point beyond U+FFFF"
               v lexbuf
         )

# 1405 "lib/read.ml"

  
# 1406 "lib/read.ml"
  | 1 ->

# 359 "lib/read.mll"
         
# 359 "lib/read.mll"
         ( long_error "Missing escape sequence representing low surrogate \
                       for code point beyond U+FFFF" v lexbuf )

# 1411 "lib/read.ml"

  
# 1412 "lib/read.ml"
  | 2 ->

# 361 "lib/read.mll"
         
# 361 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1416 "lib/read.ml"

  
# 1417 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_surrogate_pair_rec v x lexbuf __ocaml_lex_state

and finish_stringlit v lexbuf =
   __ocaml_lex_finish_stringlit_rec v lexbuf 91
and __ocaml_lex_finish_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 366 "lib/read.mll"
         
# 366 "lib/read.mll"
         ( let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
           let s = Bytes.create (len+1) in
           Bytes.set s 0 '"';
           Bytes.blit lexbuf.lex_buffer lexbuf.lex_start_pos s 1 len;
           Bytes.to_string s
         )

# 1433 "lib/read.ml"

  
# 1434 "lib/read.ml"
  | 1 ->

# 372 "lib/read.mll"
         
# 372 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 1438 "lib/read.ml"

  
# 1439 "lib/read.ml"
  | 2 ->

# 373 "lib/read.mll"
         
# 373 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1443 "lib/read.ml"

  
# 1444 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_stringlit_rec v lexbuf __ocaml_lex_state

and finish_variant v lexbuf =
   __ocaml_lex_finish_variant_rec v lexbuf 102
and __ocaml_lex_finish_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 376 "lib/read.mll"
         
# 376 "lib/read.mll"
         ( let x = read_json v lexbuf in
           read_space v lexbuf;
           read_gt v lexbuf;
           Some x )

# 1458 "lib/read.ml"

  
# 1459 "lib/read.ml"
  | 1 ->

# 380 "lib/read.mll"
         
# 380 "lib/read.mll"
         ( None )

# 1463 "lib/read.ml"

  
# 1464 "lib/read.ml"
  | 2 ->

# 381 "lib/read.mll"
         
# 381 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 1468 "lib/read.ml"

  
# 1469 "lib/read.ml"
  | 3 ->

# 382 "lib/read.mll"
         
# 382 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1473 "lib/read.ml"

  
# 1474 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_variant_rec v lexbuf __ocaml_lex_state

and read_lt v lexbuf =
   __ocaml_lex_read_lt_rec v lexbuf 107
and __ocaml_lex_read_lt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 385 "lib/read.mll"
             
# 385 "lib/read.mll"
             ( () )

# 1485 "lib/read.ml"

  
# 1486 "lib/read.ml"
  | 1 ->

# 386 "lib/read.mll"
             
# 386 "lib/read.mll"
             ( long_error "Expected '<' but found" v lexbuf )

# 1490 "lib/read.ml"

  
# 1491 "lib/read.ml"
  | 2 ->

# 387 "lib/read.mll"
             
# 387 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1495 "lib/read.ml"

  
# 1496 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lt_rec v lexbuf __ocaml_lex_state

and read_gt v lexbuf =
   __ocaml_lex_read_gt_rec v lexbuf 111
and __ocaml_lex_read_gt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 390 "lib/read.mll"
         
# 390 "lib/read.mll"
         ( () )

# 1507 "lib/read.ml"

  
# 1508 "lib/read.ml"
  | 1 ->

# 391 "lib/read.mll"
         
# 391 "lib/read.mll"
         ( long_error "Expected '>' but found" v lexbuf )

# 1512 "lib/read.ml"

  
# 1513 "lib/read.ml"
  | 2 ->

# 392 "lib/read.mll"
         
# 392 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1517 "lib/read.ml"

  
# 1518 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_gt_rec v lexbuf __ocaml_lex_state

and read_comma v lexbuf =
   __ocaml_lex_read_comma_rec v lexbuf 115
and __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 395 "lib/read.mll"
         
# 395 "lib/read.mll"
         ( () )

# 1529 "lib/read.ml"

  
# 1530 "lib/read.ml"
  | 1 ->

# 396 "lib/read.mll"
         
# 396 "lib/read.mll"
         ( long_error "Expected ',' but found" v lexbuf )

# 1534 "lib/read.ml"

  
# 1535 "lib/read.ml"
  | 2 ->

# 397 "lib/read.mll"
         
# 397 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1539 "lib/read.ml"

  
# 1540 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state

and start_any_variant v lexbuf =
   __ocaml_lex_start_any_variant_rec v lexbuf 119
and __ocaml_lex_start_any_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 400 "lib/read.mll"
             
# 400 "lib/read.mll"
             ( `Edgy_bracket )

# 1551 "lib/read.ml"

  
# 1552 "lib/read.ml"
  | 1 ->

# 401 "lib/read.mll"
             
# 401 "lib/read.mll"
             ( Buffer.clear v.buf;
               `Double_quote )

# 1557 "lib/read.ml"

  
# 1558 "lib/read.ml"
  | 2 ->

# 403 "lib/read.mll"
             
# 403 "lib/read.mll"
             ( `Square_bracket )

# 1562 "lib/read.ml"

  
# 1563 "lib/read.ml"
  | 3 ->

# 404 "lib/read.mll"
             
# 404 "lib/read.mll"
             ( long_error "Expected '<', '\"' or '[' but found" v lexbuf )

# 1567 "lib/read.ml"

  
# 1568 "lib/read.ml"
  | 4 ->

# 405 "lib/read.mll"
             
# 405 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1572 "lib/read.ml"

  
# 1573 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_start_any_variant_rec v lexbuf __ocaml_lex_state

and finish_comment v lexbuf =
   __ocaml_lex_finish_comment_rec v lexbuf 125
and __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 408 "lib/read.mll"
         
# 408 "lib/read.mll"
         ( () )

# 1584 "lib/read.ml"

  
# 1585 "lib/read.ml"
  | 1 ->

# 409 "lib/read.mll"
         
# 409 "lib/read.mll"
         ( long_error "Unterminated comment" v lexbuf )

# 1589 "lib/read.ml"

  
# 1590 "lib/read.ml"
  | 2 ->

# 410 "lib/read.mll"
         
# 410 "lib/read.mll"
         ( newline v lexbuf; finish_comment v lexbuf )

# 1594 "lib/read.ml"

  
# 1595 "lib/read.ml"
  | 3 ->

# 411 "lib/read.mll"
         
# 411 "lib/read.mll"
         ( finish_comment v lexbuf )

# 1599 "lib/read.ml"

  
# 1600 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state

and read_eof lexbuf =
   __ocaml_lex_read_eof_rec lexbuf 131
and __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 419 "lib/read.mll"
              
# 419 "lib/read.mll"
              ( true )

# 1611 "lib/read.ml"

  
# 1612 "lib/read.ml"
  | 1 ->

# 420 "lib/read.mll"
              
# 420 "lib/read.mll"
              ( false )

# 1616 "lib/read.ml"

  
# 1617 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state

and read_space v lexbuf =
   __ocaml_lex_read_space_rec v lexbuf 133
and __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 423 "lib/read.mll"
                             
# 423 "lib/read.mll"
                             ( newline v lexbuf; read_space v lexbuf )

# 1628 "lib/read.ml"

  
# 1629 "lib/read.ml"
  | 1 ->

# 424 "lib/read.mll"
                             
# 424 "lib/read.mll"
                             ( finish_comment v lexbuf; read_space v lexbuf )

# 1633 "lib/read.ml"

  
# 1634 "lib/read.ml"
  | 2 ->

# 425 "lib/read.mll"
                             
# 425 "lib/read.mll"
                             ( newline v lexbuf; read_space v lexbuf )

# 1638 "lib/read.ml"

  
# 1639 "lib/read.ml"
  | 3 ->

# 426 "lib/read.mll"
                             
# 426 "lib/read.mll"
                             ( read_space v lexbuf )

# 1643 "lib/read.ml"

  
# 1644 "lib/read.ml"
  | 4 ->

# 427 "lib/read.mll"
                             
# 427 "lib/read.mll"
                             ( () )

# 1648 "lib/read.ml"

  
# 1649 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state

and read_null v lexbuf =
   __ocaml_lex_read_null_rec v lexbuf 140
and __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 430 "lib/read.mll"
              
# 430 "lib/read.mll"
              ( () )

# 1660 "lib/read.ml"

  
# 1661 "lib/read.ml"
  | 1 ->

# 431 "lib/read.mll"
              
# 431 "lib/read.mll"
              ( long_error "Expected 'null' but found" v lexbuf )

# 1665 "lib/read.ml"

  
# 1666 "lib/read.ml"
  | 2 ->

# 432 "lib/read.mll"
              
# 432 "lib/read.mll"
              ( custom_error "Unexpected end of input" v lexbuf )

# 1670 "lib/read.ml"

  
# 1671 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state

and read_null_if_possible v lexbuf =
   __ocaml_lex_read_null_if_possible_rec v lexbuf 147
and __ocaml_lex_read_null_if_possible_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 435 "lib/read.mll"
              
# 435 "lib/read.mll"
              ( true )

# 1682 "lib/read.ml"

  
# 1683 "lib/read.ml"
  | 1 ->

# 436 "lib/read.mll"
              
# 436 "lib/read.mll"
              ( false )

# 1687 "lib/read.ml"

  
# 1688 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_null_if_possible_rec v lexbuf __ocaml_lex_state

and read_bool v lexbuf =
   __ocaml_lex_read_bool_rec v lexbuf 152
and __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 439 "lib/read.mll"
                
# 439 "lib/read.mll"
                ( true )

# 1699 "lib/read.ml"

  
# 1700 "lib/read.ml"
  | 1 ->

# 440 "lib/read.mll"
                
# 440 "lib/read.mll"
                ( false )

# 1704 "lib/read.ml"

  
# 1705 "lib/read.ml"
  | 2 ->

# 443 "lib/read.mll"
                
# 443 "lib/read.mll"
                ( true )

# 1709 "lib/read.ml"

  
# 1710 "lib/read.ml"
  | 3 ->

# 444 "lib/read.mll"
                
# 444 "lib/read.mll"
                ( false )

# 1714 "lib/read.ml"

  
# 1715 "lib/read.ml"
  | 4 ->

# 446 "lib/read.mll"
                
# 446 "lib/read.mll"
                ( long_error "Expected 'true' or 'false' but found" v lexbuf )

# 1719 "lib/read.ml"

  
# 1720 "lib/read.ml"
  | 5 ->

# 447 "lib/read.mll"
                
# 447 "lib/read.mll"
                ( custom_error "Unexpected end of input" v lexbuf )

# 1724 "lib/read.ml"

  
# 1725 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state

and read_int v lexbuf =
   __ocaml_lex_read_int_rec v lexbuf 176
and __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 450 "lib/read.mll"
                         
# 450 "lib/read.mll"
                         ( try extract_positive_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf )

# 1738 "lib/read.ml"

  
# 1739 "lib/read.ml"
  | 1 ->

# 453 "lib/read.mll"
                         
# 453 "lib/read.mll"
                         ( try extract_negative_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf )

# 1745 "lib/read.ml"

  
# 1746 "lib/read.ml"
  | 2 ->

# 456 "lib/read.mll"
                         
# 456 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             int_of_string s
                           with _ ->
                             custom_error
                               "Expected an integer but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1763 "lib/read.ml"

  
# 1764 "lib/read.ml"
  | 3 ->

# 470 "lib/read.mll"
                         
# 470 "lib/read.mll"
                         ( long_error "Expected integer but found" v lexbuf )

# 1768 "lib/read.ml"

  
# 1769 "lib/read.ml"
  | 4 ->

# 471 "lib/read.mll"
                         
# 471 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1773 "lib/read.ml"

  
# 1774 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state

and read_int32 v lexbuf =
   __ocaml_lex_read_int32_rec v lexbuf 185
and __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 474 "lib/read.mll"
                         
# 474 "lib/read.mll"
                         ( try Int32.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf )

# 1787 "lib/read.ml"

  
# 1788 "lib/read.ml"
  | 1 ->

# 477 "lib/read.mll"
                         
# 477 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int32.of_string s
                           with _ ->
                             custom_error
                               "Expected an int32 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1805 "lib/read.ml"

  
# 1806 "lib/read.ml"
  | 2 ->

# 491 "lib/read.mll"
                         
# 491 "lib/read.mll"
                         ( long_error "Expected int32 but found" v lexbuf )

# 1810 "lib/read.ml"

  
# 1811 "lib/read.ml"
  | 3 ->

# 492 "lib/read.mll"
                         
# 492 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1815 "lib/read.ml"

  
# 1816 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state

and read_int64 v lexbuf =
   __ocaml_lex_read_int64_rec v lexbuf 192
and __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 495 "lib/read.mll"
                         
# 495 "lib/read.mll"
                         ( try Int64.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf )

# 1829 "lib/read.ml"

  
# 1830 "lib/read.ml"
  | 1 ->

# 498 "lib/read.mll"
                         
# 498 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int64.of_string s
                           with _ ->
                             custom_error
                               "Expected an int64 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1847 "lib/read.ml"

  
# 1848 "lib/read.ml"
  | 2 ->

# 512 "lib/read.mll"
                         
# 512 "lib/read.mll"
                         ( long_error "Expected int64 but found" v lexbuf )

# 1852 "lib/read.ml"

  
# 1853 "lib/read.ml"
  | 3 ->

# 513 "lib/read.mll"
                         
# 513 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1857 "lib/read.ml"

  
# 1858 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state

and read_number v lexbuf =
   __ocaml_lex_read_number_rec v lexbuf 199
and __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 516 "lib/read.mll"
                
# 516 "lib/read.mll"
                ( nan )

# 1869 "lib/read.ml"

  
# 1870 "lib/read.ml"
  | 1 ->

# 517 "lib/read.mll"
                
# 517 "lib/read.mll"
                ( infinity )

# 1874 "lib/read.ml"

  
# 1875 "lib/read.ml"
  | 2 ->

# 518 "lib/read.mll"
                
# 518 "lib/read.mll"
                ( neg_infinity )

# 1879 "lib/read.ml"

  
# 1880 "lib/read.ml"
  | 3 ->

# 519 "lib/read.mll"
                
# 519 "lib/read.mll"
                ( float_of_string (lexeme lexbuf) )

# 1884 "lib/read.ml"

  
# 1885 "lib/read.ml"
  | 4 ->

# 520 "lib/read.mll"
                
# 520 "lib/read.mll"
                ( Buffer.clear v.buf;
                  let s = finish_string v lexbuf in
                  try
                    (* Any OCaml-compliant float will pass,
                       including hexadecimal and octal notations,
                       and embedded underscores. *)
                    float_of_string s
                  with _ ->
                    match s with
                        "NaN" -> nan
                      | "Infinity" -> infinity
                      | "-Infinity" -> neg_infinity
                      | _ ->
                          custom_error
                            "Expected a number but found a string that \
                             doesn't even represent a number"
                            v lexbuf
                )

# 1906 "lib/read.ml"

  
# 1907 "lib/read.ml"
  | 5 ->

# 538 "lib/read.mll"
                
# 538 "lib/read.mll"
                ( long_error "Expected number but found" v lexbuf )

# 1911 "lib/read.ml"

  
# 1912 "lib/read.ml"
  | 6 ->

# 539 "lib/read.mll"
                
# 539 "lib/read.mll"
                ( custom_error "Unexpected end of input" v lexbuf )

# 1916 "lib/read.ml"

  
# 1917 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state

and read_string v lexbuf =
   __ocaml_lex_read_string_rec v lexbuf 233
and __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 542 "lib/read.mll"
             
# 542 "lib/read.mll"
             ( Buffer.clear v.buf;
               finish_string v lexbuf )

# 1929 "lib/read.ml"

  
# 1930 "lib/read.ml"
  | 1 ->

# 544 "lib/read.mll"
             
# 544 "lib/read.mll"
             ( long_error "Expected '\"' but found" v lexbuf )

# 1934 "lib/read.ml"

  
# 1935 "lib/read.ml"
  | 2 ->

# 545 "lib/read.mll"
             
# 545 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1939 "lib/read.ml"

  
# 1940 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state

and read_ident v lexbuf =
   __ocaml_lex_read_ident_rec v lexbuf 237
and __ocaml_lex_read_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 548 "lib/read.mll"
             
# 548 "lib/read.mll"
             ( Buffer.clear v.buf;
               finish_string v lexbuf )

# 1952 "lib/read.ml"

  
# 1953 "lib/read.ml"
  | 1 ->
let

# 550 "lib/read.mll"
             
# 550 "lib/read.mll"
             s

# 1958 "lib/read.ml"
# 1958 "lib/read.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in

# 551 "lib/read.mll"
             
# 551 "lib/read.mll"
             ( s )

# 1962 "lib/read.ml"

  
# 1963 "lib/read.ml"
  | 2 ->

# 552 "lib/read.mll"
             
# 552 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 1967 "lib/read.ml"

  
# 1968 "lib/read.ml"
  | 3 ->

# 553 "lib/read.mll"
             
# 553 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1972 "lib/read.ml"

  
# 1973 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_ident_rec v lexbuf __ocaml_lex_state

and map_ident v f lexbuf =
   __ocaml_lex_map_ident_rec v f lexbuf 242
and __ocaml_lex_map_ident_rec v f lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 556 "lib/read.mll"
             
# 556 "lib/read.mll"
             ( Buffer.clear v.buf;
               map_string v f lexbuf )

# 1985 "lib/read.ml"

  
# 1986 "lib/read.ml"
  | 1 ->

# 559 "lib/read.mll"
             
# 559 "lib/read.mll"
             ( map_lexeme f lexbuf )

# 1990 "lib/read.ml"

  
# 1991 "lib/read.ml"
  | 2 ->

# 560 "lib/read.mll"
             
# 560 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 1995 "lib/read.ml"

  
# 1996 "lib/read.ml"
  | 3 ->

# 561 "lib/read.mll"
             
# 561 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2000 "lib/read.ml"

  
# 2001 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_map_ident_rec v f lexbuf __ocaml_lex_state

and read_sequence read_cell init_acc v lexbuf =
   __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf 247
and __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 564 "lib/read.mll"
             
# 564 "lib/read.mll"
             ( let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell !acc v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell !acc v lexbuf;
                 done;
                 assert false
               with End_of_array ->
                 !acc
             )

# 2026 "lib/read.ml"

  
# 2027 "lib/read.ml"
  | 1 ->

# 579 "lib/read.mll"
             
# 579 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2031 "lib/read.ml"

  
# 2032 "lib/read.ml"
  | 2 ->

# 580 "lib/read.mll"
             
# 580 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2036 "lib/read.ml"

  
# 2037 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf __ocaml_lex_state

and read_list_rev read_cell v lexbuf =
   __ocaml_lex_read_list_rev_rec read_cell v lexbuf 251
and __ocaml_lex_read_list_rev_rec read_cell v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 583 "lib/read.mll"
             
# 583 "lib/read.mll"
             ( let acc = ref [] in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell v lexbuf :: !acc;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell v lexbuf :: !acc;
                 done;
                 assert false
               with End_of_array ->
                 !acc
             )

# 2062 "lib/read.ml"

  
# 2063 "lib/read.ml"
  | 1 ->

# 598 "lib/read.mll"
             
# 598 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2067 "lib/read.ml"

  
# 2068 "lib/read.ml"
  | 2 ->

# 599 "lib/read.mll"
             
# 599 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2072 "lib/read.ml"

  
# 2073 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_list_rev_rec read_cell v lexbuf __ocaml_lex_state

and read_array_end lexbuf =
   __ocaml_lex_read_array_end_rec lexbuf 255
and __ocaml_lex_read_array_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 602 "lib/read.mll"
             
# 602 "lib/read.mll"
             ( raise End_of_array )

# 2084 "lib/read.ml"

  
# 2085 "lib/read.ml"
  | 1 ->

# 603 "lib/read.mll"
             
# 603 "lib/read.mll"
             ( () )

# 2089 "lib/read.ml"

  
# 2090 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_array_end_rec lexbuf __ocaml_lex_state

and read_array_sep v lexbuf =
   __ocaml_lex_read_array_sep_rec v lexbuf 257
and __ocaml_lex_read_array_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 606 "lib/read.mll"
             
# 606 "lib/read.mll"
             ( () )

# 2101 "lib/read.ml"

  
# 2102 "lib/read.ml"
  | 1 ->

# 607 "lib/read.mll"
             
# 607 "lib/read.mll"
             ( raise End_of_array )

# 2106 "lib/read.ml"

  
# 2107 "lib/read.ml"
  | 2 ->

# 608 "lib/read.mll"
             
# 608 "lib/read.mll"
             ( long_error "Expected ',' or ']' but found" v lexbuf )

# 2111 "lib/read.ml"

  
# 2112 "lib/read.ml"
  | 3 ->

# 609 "lib/read.mll"
             
# 609 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2116 "lib/read.ml"

  
# 2117 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_array_sep_rec v lexbuf __ocaml_lex_state

and read_tuple read_cell init_acc v lexbuf =
   __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf 262
and __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 613 "lib/read.mll"
                 
# 613 "lib/read.mll"
                 (
                     
# 633 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 635 "lib/read.mll"
                 )

# 2150 "lib/read.ml"

  
# 2151 "lib/read.ml"
  | 1 ->

# 636 "lib/read.mll"
             
# 636 "lib/read.mll"
             ( long_error "Expected ')' but found" v lexbuf )

# 2155 "lib/read.ml"

  
# 2156 "lib/read.ml"
  | 2 ->

# 637 "lib/read.mll"
             
# 637 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2160 "lib/read.ml"

  
# 2161 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf __ocaml_lex_state

and read_tuple_end lexbuf =
   __ocaml_lex_read_tuple_end_rec lexbuf 266
and __ocaml_lex_read_tuple_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 640 "lib/read.mll"
             
# 640 "lib/read.mll"
             ( raise End_of_tuple )

# 2172 "lib/read.ml"

  
# 2173 "lib/read.ml"
  | 1 ->

# 641 "lib/read.mll"
             
# 641 "lib/read.mll"
             ( () )

# 2177 "lib/read.ml"

  
# 2178 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_end_rec lexbuf __ocaml_lex_state

and read_tuple_end2 v std lexbuf =
   __ocaml_lex_read_tuple_end2_rec v std lexbuf 268
and __ocaml_lex_read_tuple_end2_rec v std lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 644 "lib/read.mll"
             
# 644 "lib/read.mll"
             ( if std then
                 long_error "Expected ')' or '' but found" v lexbuf
               else
                 raise End_of_tuple )

# 2192 "lib/read.ml"

  
# 2193 "lib/read.ml"
  | 1 ->

# 648 "lib/read.mll"
             
# 648 "lib/read.mll"
             ( if std then
                 raise End_of_tuple
               else
                 long_error "Expected ']' or '' but found" v lexbuf )

# 2200 "lib/read.ml"

  
# 2201 "lib/read.ml"
  | 2 ->

# 652 "lib/read.mll"
             
# 652 "lib/read.mll"
             ( () )

# 2205 "lib/read.ml"

  
# 2206 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_end2_rec v std lexbuf __ocaml_lex_state

and read_tuple_sep v lexbuf =
   __ocaml_lex_read_tuple_sep_rec v lexbuf 271
and __ocaml_lex_read_tuple_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 655 "lib/read.mll"
             
# 655 "lib/read.mll"
             ( () )

# 2217 "lib/read.ml"

  
# 2218 "lib/read.ml"
  | 1 ->

# 656 "lib/read.mll"
             
# 656 "lib/read.mll"
             ( raise End_of_tuple )

# 2222 "lib/read.ml"

  
# 2223 "lib/read.ml"
  | 2 ->

# 657 "lib/read.mll"
             
# 657 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 2227 "lib/read.ml"

  
# 2228 "lib/read.ml"
  | 3 ->

# 658 "lib/read.mll"
             
# 658 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2232 "lib/read.ml"

  
# 2233 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_sep_rec v lexbuf __ocaml_lex_state

and read_tuple_sep2 v std lexbuf =
   __ocaml_lex_read_tuple_sep2_rec v std lexbuf 276
and __ocaml_lex_read_tuple_sep2_rec v std lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 661 "lib/read.mll"
             
# 661 "lib/read.mll"
             ( () )

# 2244 "lib/read.ml"

  
# 2245 "lib/read.ml"
  | 1 ->

# 662 "lib/read.mll"
             
# 662 "lib/read.mll"
             ( if std then
                 long_error "Expected ',' or ']' but found" v lexbuf
               else
                 raise End_of_tuple )

# 2252 "lib/read.ml"

  
# 2253 "lib/read.ml"
  | 2 ->

# 666 "lib/read.mll"
             
# 666 "lib/read.mll"
             ( if std then
                 raise End_of_tuple
               else
                 long_error "Expected ',' or ')' but found" v lexbuf )

# 2260 "lib/read.ml"

  
# 2261 "lib/read.ml"
  | 3 ->

# 670 "lib/read.mll"
             
# 670 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 2265 "lib/read.ml"

  
# 2266 "lib/read.ml"
  | 4 ->

# 671 "lib/read.mll"
             
# 671 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2270 "lib/read.ml"

  
# 2271 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_sep2_rec v std lexbuf __ocaml_lex_state

and read_abstract_fields read_key read_field init_acc v lexbuf =
   __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf 282
and __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 675 "lib/read.mll"
             
# 675 "lib/read.mll"
             ( let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_object_end lexbuf;
                 let field_name = read_key v lexbuf in
                 read_space v lexbuf;
                 read_colon v lexbuf;
                 read_space v lexbuf;
                 acc := read_field !acc field_name v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_object_sep v lexbuf;
                   read_space v lexbuf;
                   let field_name = read_key v lexbuf in
                   read_space v lexbuf;
                   read_colon v lexbuf;
                   read_space v lexbuf;
                   acc := read_field !acc field_name v lexbuf;
                 done;
                 assert false
               with End_of_object ->
                 !acc
             )

# 2304 "lib/read.ml"

  
# 2305 "lib/read.ml"
  | 1 ->

# 698 "lib/read.mll"
             
# 698 "lib/read.mll"
             ( long_error "Expected '{' but found" v lexbuf )

# 2309 "lib/read.ml"

  
# 2310 "lib/read.ml"
  | 2 ->

# 699 "lib/read.mll"
             
# 699 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2314 "lib/read.ml"

  
# 2315 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf __ocaml_lex_state

and read_lcurl v lexbuf =
   __ocaml_lex_read_lcurl_rec v lexbuf 286
and __ocaml_lex_read_lcurl_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 702 "lib/read.mll"
             
# 702 "lib/read.mll"
             ( () )

# 2326 "lib/read.ml"

  
# 2327 "lib/read.ml"
  | 1 ->

# 703 "lib/read.mll"
             
# 703 "lib/read.mll"
             ( long_error "Expected '{' but found" v lexbuf )

# 2331 "lib/read.ml"

  
# 2332 "lib/read.ml"
  | 2 ->

# 704 "lib/read.mll"
             
# 704 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2336 "lib/read.ml"

  
# 2337 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lcurl_rec v lexbuf __ocaml_lex_state

and read_object_end lexbuf =
   __ocaml_lex_read_object_end_rec lexbuf 290
and __ocaml_lex_read_object_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 707 "lib/read.mll"
             
# 707 "lib/read.mll"
             ( raise End_of_object )

# 2348 "lib/read.ml"

  
# 2349 "lib/read.ml"
  | 1 ->

# 708 "lib/read.mll"
             
# 708 "lib/read.mll"
             ( () )

# 2353 "lib/read.ml"

  
# 2354 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_object_end_rec lexbuf __ocaml_lex_state

and read_object_sep v lexbuf =
   __ocaml_lex_read_object_sep_rec v lexbuf 292
and __ocaml_lex_read_object_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 711 "lib/read.mll"
             
# 711 "lib/read.mll"
             ( () )

# 2365 "lib/read.ml"

  
# 2366 "lib/read.ml"
  | 1 ->

# 712 "lib/read.mll"
             
# 712 "lib/read.mll"
             ( raise End_of_object )

# 2370 "lib/read.ml"

  
# 2371 "lib/read.ml"
  | 2 ->

# 713 "lib/read.mll"
             
# 713 "lib/read.mll"
             ( long_error "Expected ',' or '}' but found" v lexbuf )

# 2375 "lib/read.ml"

  
# 2376 "lib/read.ml"
  | 3 ->

# 714 "lib/read.mll"
             
# 714 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2380 "lib/read.ml"

  
# 2381 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_object_sep_rec v lexbuf __ocaml_lex_state

and read_colon v lexbuf =
   __ocaml_lex_read_colon_rec v lexbuf 297
and __ocaml_lex_read_colon_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 717 "lib/read.mll"
             
# 717 "lib/read.mll"
             ( () )

# 2392 "lib/read.ml"

  
# 2393 "lib/read.ml"
  | 1 ->

# 718 "lib/read.mll"
             
# 718 "lib/read.mll"
             ( long_error "Expected ':' but found" v lexbuf )

# 2397 "lib/read.ml"

  
# 2398 "lib/read.ml"
  | 2 ->

# 719 "lib/read.mll"
             
# 719 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2402 "lib/read.ml"

  
# 2403 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_colon_rec v lexbuf __ocaml_lex_state

and start_any_tuple v lexbuf =
   __ocaml_lex_start_any_tuple_rec v lexbuf 301
and __ocaml_lex_start_any_tuple_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 722 "lib/read.mll"
             
# 722 "lib/read.mll"
             ( false )

# 2414 "lib/read.ml"

  
# 2415 "lib/read.ml"
  | 1 ->

# 723 "lib/read.mll"
             
# 723 "lib/read.mll"
             ( true )

# 2419 "lib/read.ml"

  
# 2420 "lib/read.ml"
  | 2 ->

# 724 "lib/read.mll"
             
# 724 "lib/read.mll"
             ( long_error "Expected '(' or '[' but found" v lexbuf )

# 2424 "lib/read.ml"

  
# 2425 "lib/read.ml"
  | 3 ->

# 725 "lib/read.mll"
             
# 725 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2429 "lib/read.ml"

  
# 2430 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_start_any_tuple_rec v lexbuf __ocaml_lex_state

and read_lpar v lexbuf =
   __ocaml_lex_read_lpar_rec v lexbuf 306
and __ocaml_lex_read_lpar_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 728 "lib/read.mll"
             
# 728 "lib/read.mll"
             ( () )

# 2441 "lib/read.ml"

  
# 2442 "lib/read.ml"
  | 1 ->

# 729 "lib/read.mll"
             
# 729 "lib/read.mll"
             ( long_error "Expected '(' but found" v lexbuf )

# 2446 "lib/read.ml"

  
# 2447 "lib/read.ml"
  | 2 ->

# 730 "lib/read.mll"
             
# 730 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2451 "lib/read.ml"

  
# 2452 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lpar_rec v lexbuf __ocaml_lex_state

and read_rpar v lexbuf =
   __ocaml_lex_read_rpar_rec v lexbuf 310
and __ocaml_lex_read_rpar_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 733 "lib/read.mll"
             
# 733 "lib/read.mll"
             ( () )

# 2463 "lib/read.ml"

  
# 2464 "lib/read.ml"
  | 1 ->

# 734 "lib/read.mll"
             
# 734 "lib/read.mll"
             ( long_error "Expected ')' but found" v lexbuf )

# 2468 "lib/read.ml"

  
# 2469 "lib/read.ml"
  | 2 ->

# 735 "lib/read.mll"
             
# 735 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2473 "lib/read.ml"

  
# 2474 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rpar_rec v lexbuf __ocaml_lex_state

and read_lbr v lexbuf =
   __ocaml_lex_read_lbr_rec v lexbuf 314
and __ocaml_lex_read_lbr_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 738 "lib/read.mll"
             
# 738 "lib/read.mll"
             ( () )

# 2485 "lib/read.ml"

  
# 2486 "lib/read.ml"
  | 1 ->

# 739 "lib/read.mll"
             
# 739 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2490 "lib/read.ml"

  
# 2491 "lib/read.ml"
  | 2 ->

# 740 "lib/read.mll"
             
# 740 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2495 "lib/read.ml"

  
# 2496 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lbr_rec v lexbuf __ocaml_lex_state

and read_rbr v lexbuf =
   __ocaml_lex_read_rbr_rec v lexbuf 318
and __ocaml_lex_read_rbr_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 743 "lib/read.mll"
             
# 743 "lib/read.mll"
             ( () )

# 2507 "lib/read.ml"

  
# 2508 "lib/read.ml"
  | 1 ->

# 744 "lib/read.mll"
             
# 744 "lib/read.mll"
             ( long_error "Expected ']' but found" v lexbuf )

# 2512 "lib/read.ml"

  
# 2513 "lib/read.ml"
  | 2 ->

# 745 "lib/read.mll"
             
# 745 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2517 "lib/read.ml"

  
# 2518 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rbr_rec v lexbuf __ocaml_lex_state

and skip_json v lexbuf =
   __ocaml_lex_skip_json_rec v lexbuf 322
and __ocaml_lex_skip_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 751 "lib/read.mll"
                
# 751 "lib/read.mll"
                ( () )

# 2529 "lib/read.ml"

  
# 2530 "lib/read.ml"
  | 1 ->

# 752 "lib/read.mll"
                
# 752 "lib/read.mll"
                ( () )

# 2534 "lib/read.ml"

  
# 2535 "lib/read.ml"
  | 2 ->

# 753 "lib/read.mll"
                
# 753 "lib/read.mll"
                ( () )

# 2539 "lib/read.ml"

  
# 2540 "lib/read.ml"
  | 3 ->

# 754 "lib/read.mll"
                
# 754 "lib/read.mll"
                ( () )

# 2544 "lib/read.ml"

  
# 2545 "lib/read.ml"
  | 4 ->

# 755 "lib/read.mll"
                
# 755 "lib/read.mll"
                ( () )

# 2549 "lib/read.ml"

  
# 2550 "lib/read.ml"
  | 5 ->

# 756 "lib/read.mll"
                
# 756 "lib/read.mll"
                ( () )

# 2554 "lib/read.ml"

  
# 2555 "lib/read.ml"
  | 6 ->

# 757 "lib/read.mll"
                
# 757 "lib/read.mll"
                ( finish_skip_stringlit v lexbuf )

# 2559 "lib/read.ml"

  
# 2560 "lib/read.ml"
  | 7 ->

# 758 "lib/read.mll"
                          
# 758 "lib/read.mll"
                          ( () )

# 2564 "lib/read.ml"

  
# 2565 "lib/read.ml"
  | 8 ->

# 759 "lib/read.mll"
                
# 759 "lib/read.mll"
                ( () )

# 2569 "lib/read.ml"

  
# 2570 "lib/read.ml"
  | 9 ->

# 761 "lib/read.mll"
                 
# 761 "lib/read.mll"
                 ( try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     skip_ident v lexbuf;
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       skip_ident v lexbuf;
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with End_of_object ->
                     ()
                 )

# 2595 "lib/read.ml"

  
# 2596 "lib/read.ml"
  | 10 ->

# 784 "lib/read.mll"
                 
# 784 "lib/read.mll"
                 ( try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with End_of_array ->
                     ()
                 )

# 2613 "lib/read.ml"

  
# 2614 "lib/read.ml"
  | 11 ->

# 799 "lib/read.mll"
                 
# 799 "lib/read.mll"
                 (
                     
# 815 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 817 "lib/read.mll"
                 )

# 2636 "lib/read.ml"

  
# 2637 "lib/read.ml"
  | 12 ->

# 819 "lib/read.mll"
                 
# 819 "lib/read.mll"
                 (
                     
# 826 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 828 "lib/read.mll"
                 )

# 2650 "lib/read.ml"

  
# 2651 "lib/read.ml"
  | 13 ->

# 830 "lib/read.mll"
                 
# 830 "lib/read.mll"
                 ( skip_json v lexbuf )

# 2655 "lib/read.ml"

  
# 2656 "lib/read.ml"
  | 14 ->

# 831 "lib/read.mll"
                 
# 831 "lib/read.mll"
                 ( finish_comment v lexbuf; skip_json v lexbuf )

# 2660 "lib/read.ml"

  
# 2661 "lib/read.ml"
  | 15 ->

# 832 "lib/read.mll"
                 
# 832 "lib/read.mll"
                 ( newline v lexbuf; skip_json v lexbuf )

# 2665 "lib/read.ml"

  
# 2666 "lib/read.ml"
  | 16 ->

# 833 "lib/read.mll"
                 
# 833 "lib/read.mll"
                 ( skip_json v lexbuf )

# 2670 "lib/read.ml"

  
# 2671 "lib/read.ml"
  | 17 ->

# 834 "lib/read.mll"
                 
# 834 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 2675 "lib/read.ml"

  
# 2676 "lib/read.ml"
  | 18 ->

# 835 "lib/read.mll"
                 
# 835 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 2680 "lib/read.ml"

  
# 2681 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_json_rec v lexbuf __ocaml_lex_state

and finish_skip_stringlit v lexbuf =
   __ocaml_lex_finish_skip_stringlit_rec v lexbuf 378
and __ocaml_lex_finish_skip_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 841 "lib/read.mll"
         
# 841 "lib/read.mll"
         ( () )

# 2692 "lib/read.ml"

  
# 2693 "lib/read.ml"
  | 1 ->

# 842 "lib/read.mll"
         
# 842 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 2697 "lib/read.ml"

  
# 2698 "lib/read.ml"
  | 2 ->

# 843 "lib/read.mll"
         
# 843 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2702 "lib/read.ml"

  
# 2703 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_skip_stringlit_rec v lexbuf __ocaml_lex_state

and finish_skip_variant v lexbuf =
   __ocaml_lex_finish_skip_variant_rec v lexbuf 389
and __ocaml_lex_finish_skip_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 846 "lib/read.mll"
         
# 846 "lib/read.mll"
         ( skip_json v lexbuf;
           read_space v lexbuf;
           read_gt v lexbuf )

# 2716 "lib/read.ml"

  
# 2717 "lib/read.ml"
  | 1 ->

# 849 "lib/read.mll"
         
# 849 "lib/read.mll"
         ( () )

# 2721 "lib/read.ml"

  
# 2722 "lib/read.ml"
  | 2 ->

# 850 "lib/read.mll"
         
# 850 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 2726 "lib/read.ml"

  
# 2727 "lib/read.ml"
  | 3 ->

# 851 "lib/read.mll"
         
# 851 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2731 "lib/read.ml"

  
# 2732 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_skip_variant_rec v lexbuf __ocaml_lex_state

and skip_ident v lexbuf =
   __ocaml_lex_skip_ident_rec v lexbuf 394
and __ocaml_lex_skip_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 854 "lib/read.mll"
             
# 854 "lib/read.mll"
             ( finish_skip_stringlit v lexbuf )

# 2743 "lib/read.ml"

  
# 2744 "lib/read.ml"
  | 1 ->

# 855 "lib/read.mll"
             
# 855 "lib/read.mll"
             ( () )

# 2748 "lib/read.ml"

  
# 2749 "lib/read.ml"
  | 2 ->

# 856 "lib/read.mll"
             
# 856 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 2753 "lib/read.ml"

  
# 2754 "lib/read.ml"
  | 3 ->

# 857 "lib/read.mll"
             
# 857 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2758 "lib/read.ml"

  
# 2759 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_ident_rec v lexbuf __ocaml_lex_state

and buffer_json v lexbuf =
   __ocaml_lex_buffer_json_rec v lexbuf 399
and __ocaml_lex_buffer_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 870 "lib/read.mll"
                
# 870 "lib/read.mll"
                ( add_lexeme v.buf lexbuf )

# 2770 "lib/read.ml"

  
# 2771 "lib/read.ml"
  | 1 ->

# 872 "lib/read.mll"
                
# 872 "lib/read.mll"
                ( finish_buffer_stringlit v lexbuf )

# 2775 "lib/read.ml"

  
# 2776 "lib/read.ml"
  | 2 ->

# 873 "lib/read.mll"
                 
# 873 "lib/read.mll"
                 ( try
                     Buffer.add_char v.buf '{';
                     buffer_space v lexbuf;
                     buffer_object_end v lexbuf;
                     buffer_ident v lexbuf;
                     buffer_space v lexbuf;
                     buffer_colon v lexbuf;
                     buffer_space v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_object_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_ident v lexbuf;
                       buffer_space v lexbuf;
                       buffer_colon v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with End_of_object ->
                     ()
                 )

# 2802 "lib/read.ml"

  
# 2803 "lib/read.ml"
  | 3 ->

# 897 "lib/read.mll"
                 
# 897 "lib/read.mll"
                 ( try
                     Buffer.add_char v.buf '[';
                     buffer_space v lexbuf;
                     buffer_array_end v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_array_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with End_of_array ->
                     ()
                 )

# 2821 "lib/read.ml"

  
# 2822 "lib/read.ml"
  | 4 ->

# 913 "lib/read.mll"
                 
# 913 "lib/read.mll"
                 (
                     
# 930 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 932 "lib/read.mll"
                 )

# 2845 "lib/read.ml"

  
# 2846 "lib/read.ml"
  | 5 ->

# 934 "lib/read.mll"
                 
# 934 "lib/read.mll"
                 (
                     
# 942 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 944 "lib/read.mll"
                 )

# 2860 "lib/read.ml"

  
# 2861 "lib/read.ml"
  | 6 ->

# 946 "lib/read.mll"
                 
# 946 "lib/read.mll"
                 ( add_lexeme v.buf lexbuf; buffer_json v lexbuf )

# 2865 "lib/read.ml"

  
# 2866 "lib/read.ml"
  | 7 ->

# 947 "lib/read.mll"
                 
# 947 "lib/read.mll"
                 ( Buffer.add_string v.buf "/*";
                   finish_buffer_comment v lexbuf;
                   buffer_json v lexbuf )

# 2872 "lib/read.ml"

  
# 2873 "lib/read.ml"
  | 8 ->

# 950 "lib/read.mll"
                 
# 950 "lib/read.mll"
                 ( Buffer.add_char v.buf '\n';
                   newline v lexbuf;
                   buffer_json v lexbuf )

# 2879 "lib/read.ml"

  
# 2880 "lib/read.ml"
  | 9 ->

# 953 "lib/read.mll"
                 
# 953 "lib/read.mll"
                 ( add_lexeme v.buf lexbuf; buffer_json v lexbuf )

# 2884 "lib/read.ml"

  
# 2885 "lib/read.ml"
  | 10 ->

# 954 "lib/read.mll"
                 
# 954 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 2889 "lib/read.ml"

  
# 2890 "lib/read.ml"
  | 11 ->

# 955 "lib/read.mll"
                 
# 955 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 2894 "lib/read.ml"

  
# 2895 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_json_rec v lexbuf __ocaml_lex_state

and finish_buffer_stringlit v lexbuf =
   __ocaml_lex_finish_buffer_stringlit_rec v lexbuf 450
and __ocaml_lex_finish_buffer_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 961 "lib/read.mll"
         
# 961 "lib/read.mll"
         ( Buffer.add_char v.buf '"';
           add_lexeme v.buf lexbuf
         )

# 2908 "lib/read.ml"

  
# 2909 "lib/read.ml"
  | 1 ->

# 964 "lib/read.mll"
         
# 964 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 2913 "lib/read.ml"

  
# 2914 "lib/read.ml"
  | 2 ->

# 965 "lib/read.mll"
         
# 965 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2918 "lib/read.ml"

  
# 2919 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_stringlit_rec v lexbuf __ocaml_lex_state

and finish_buffer_variant v lexbuf =
   __ocaml_lex_finish_buffer_variant_rec v lexbuf 461
and __ocaml_lex_finish_buffer_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 968 "lib/read.mll"
         
# 968 "lib/read.mll"
         ( Buffer.add_char v.buf ':';
           buffer_json v lexbuf;
           buffer_space v lexbuf;
           buffer_gt v lexbuf )

# 2933 "lib/read.ml"

  
# 2934 "lib/read.ml"
  | 1 ->

# 972 "lib/read.mll"
         
# 972 "lib/read.mll"
         ( Buffer.add_char v.buf '>' )

# 2938 "lib/read.ml"

  
# 2939 "lib/read.ml"
  | 2 ->

# 973 "lib/read.mll"
         
# 973 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 2943 "lib/read.ml"

  
# 2944 "lib/read.ml"
  | 3 ->

# 974 "lib/read.mll"
         
# 974 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2948 "lib/read.ml"

  
# 2949 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_variant_rec v lexbuf __ocaml_lex_state

and buffer_ident v lexbuf =
   __ocaml_lex_buffer_ident_rec v lexbuf 466
and __ocaml_lex_buffer_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 977 "lib/read.mll"
             
# 977 "lib/read.mll"
             ( finish_buffer_stringlit v lexbuf )

# 2960 "lib/read.ml"

  
# 2961 "lib/read.ml"
  | 1 ->

# 978 "lib/read.mll"
             
# 978 "lib/read.mll"
             ( add_lexeme v.buf lexbuf )

# 2965 "lib/read.ml"

  
# 2966 "lib/read.ml"
  | 2 ->

# 979 "lib/read.mll"
             
# 979 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 2970 "lib/read.ml"

  
# 2971 "lib/read.ml"
  | 3 ->

# 980 "lib/read.mll"
             
# 980 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2975 "lib/read.ml"

  
# 2976 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_ident_rec v lexbuf __ocaml_lex_state

and buffer_space v lexbuf =
   __ocaml_lex_buffer_space_rec v lexbuf 471
and __ocaml_lex_buffer_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 983 "lib/read.mll"
                             
# 983 "lib/read.mll"
                             (
    add_lexeme v.buf lexbuf;
    newline v lexbuf;
    buffer_space v lexbuf )

# 2990 "lib/read.ml"

  
# 2991 "lib/read.ml"
  | 1 ->

# 987 "lib/read.mll"
                             
# 987 "lib/read.mll"
                             (
    Buffer.add_string v.buf "/*";
    finish_buffer_comment v lexbuf;
    buffer_space v lexbuf )

# 2998 "lib/read.ml"

  
# 2999 "lib/read.ml"
  | 2 ->

# 991 "lib/read.mll"
                             
# 991 "lib/read.mll"
                             (
    Buffer.add_char v.buf '\n';
    newline v lexbuf;
    buffer_space v lexbuf )

# 3006 "lib/read.ml"

  
# 3007 "lib/read.ml"
  | 3 ->

# 995 "lib/read.mll"
                             
# 995 "lib/read.mll"
                             (
    add_lexeme v.buf lexbuf;
    buffer_space v lexbuf )

# 3013 "lib/read.ml"

  
# 3014 "lib/read.ml"
  | 4 ->

# 998 "lib/read.mll"
                             
# 998 "lib/read.mll"
                             ( () )

# 3018 "lib/read.ml"

  
# 3019 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_space_rec v lexbuf __ocaml_lex_state

and buffer_object_end v lexbuf =
   __ocaml_lex_buffer_object_end_rec v lexbuf 478
and __ocaml_lex_buffer_object_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1001 "lib/read.mll"
             
# 1001 "lib/read.mll"
             (
      Buffer.add_char v.buf '}';
      raise End_of_object )

# 3032 "lib/read.ml"

  
# 3033 "lib/read.ml"
  | 1 ->

# 1004 "lib/read.mll"
             
# 1004 "lib/read.mll"
             ( () )

# 3037 "lib/read.ml"

  
# 3038 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_object_end_rec v lexbuf __ocaml_lex_state

and buffer_object_sep v lexbuf =
   __ocaml_lex_buffer_object_sep_rec v lexbuf 480
and __ocaml_lex_buffer_object_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1007 "lib/read.mll"
             
# 1007 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3049 "lib/read.ml"

  
# 3050 "lib/read.ml"
  | 1 ->

# 1008 "lib/read.mll"
             
# 1008 "lib/read.mll"
             ( Buffer.add_char v.buf '}'; raise End_of_object )

# 3054 "lib/read.ml"

  
# 3055 "lib/read.ml"
  | 2 ->

# 1009 "lib/read.mll"
             
# 1009 "lib/read.mll"
             ( long_error "Expected ',' or '}' but found" v lexbuf )

# 3059 "lib/read.ml"

  
# 3060 "lib/read.ml"
  | 3 ->

# 1010 "lib/read.mll"
             
# 1010 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3064 "lib/read.ml"

  
# 3065 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_object_sep_rec v lexbuf __ocaml_lex_state

and buffer_array_end v lexbuf =
   __ocaml_lex_buffer_array_end_rec v lexbuf 485
and __ocaml_lex_buffer_array_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1013 "lib/read.mll"
             
# 1013 "lib/read.mll"
             ( Buffer.add_char v.buf ']'; raise End_of_array )

# 3076 "lib/read.ml"

  
# 3077 "lib/read.ml"
  | 1 ->

# 1014 "lib/read.mll"
             
# 1014 "lib/read.mll"
             ( () )

# 3081 "lib/read.ml"

  
# 3082 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_array_end_rec v lexbuf __ocaml_lex_state

and buffer_array_sep v lexbuf =
   __ocaml_lex_buffer_array_sep_rec v lexbuf 487
and __ocaml_lex_buffer_array_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1017 "lib/read.mll"
             
# 1017 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3093 "lib/read.ml"

  
# 3094 "lib/read.ml"
  | 1 ->

# 1018 "lib/read.mll"
             
# 1018 "lib/read.mll"
             ( Buffer.add_char v.buf ']'; raise End_of_array )

# 3098 "lib/read.ml"

  
# 3099 "lib/read.ml"
  | 2 ->

# 1019 "lib/read.mll"
             
# 1019 "lib/read.mll"
             ( long_error "Expected ',' or ']' but found" v lexbuf )

# 3103 "lib/read.ml"

  
# 3104 "lib/read.ml"
  | 3 ->

# 1020 "lib/read.mll"
             
# 1020 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3108 "lib/read.ml"

  
# 3109 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_array_sep_rec v lexbuf __ocaml_lex_state

and buffer_tuple_end v lexbuf =
   __ocaml_lex_buffer_tuple_end_rec v lexbuf 492
and __ocaml_lex_buffer_tuple_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1023 "lib/read.mll"
             
# 1023 "lib/read.mll"
             (
      Buffer.add_char v.buf ')';
      raise End_of_tuple )

# 3122 "lib/read.ml"

  
# 3123 "lib/read.ml"
  | 1 ->

# 1026 "lib/read.mll"
             
# 1026 "lib/read.mll"
             ( () )

# 3127 "lib/read.ml"

  
# 3128 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_tuple_end_rec v lexbuf __ocaml_lex_state

and buffer_tuple_sep v lexbuf =
   __ocaml_lex_buffer_tuple_sep_rec v lexbuf 494
and __ocaml_lex_buffer_tuple_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1029 "lib/read.mll"
             
# 1029 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3139 "lib/read.ml"

  
# 3140 "lib/read.ml"
  | 1 ->

# 1030 "lib/read.mll"
             
# 1030 "lib/read.mll"
             ( Buffer.add_char v.buf ')'; raise End_of_tuple )

# 3144 "lib/read.ml"

  
# 3145 "lib/read.ml"
  | 2 ->

# 1031 "lib/read.mll"
             
# 1031 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 3149 "lib/read.ml"

  
# 3150 "lib/read.ml"
  | 3 ->

# 1032 "lib/read.mll"
             
# 1032 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3154 "lib/read.ml"

  
# 3155 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_tuple_sep_rec v lexbuf __ocaml_lex_state

and buffer_colon v lexbuf =
   __ocaml_lex_buffer_colon_rec v lexbuf 499
and __ocaml_lex_buffer_colon_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1035 "lib/read.mll"
             
# 1035 "lib/read.mll"
             ( Buffer.add_char v.buf ':' )

# 3166 "lib/read.ml"

  
# 3167 "lib/read.ml"
  | 1 ->

# 1036 "lib/read.mll"
             
# 1036 "lib/read.mll"
             ( long_error "Expected ':' but found" v lexbuf )

# 3171 "lib/read.ml"

  
# 3172 "lib/read.ml"
  | 2 ->

# 1037 "lib/read.mll"
             
# 1037 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3176 "lib/read.ml"

  
# 3177 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_colon_rec v lexbuf __ocaml_lex_state

and buffer_gt v lexbuf =
   __ocaml_lex_buffer_gt_rec v lexbuf 503
and __ocaml_lex_buffer_gt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1040 "lib/read.mll"
         
# 1040 "lib/read.mll"
         ( Buffer.add_char v.buf '>' )

# 3188 "lib/read.ml"

  
# 3189 "lib/read.ml"
  | 1 ->

# 1041 "lib/read.mll"
         
# 1041 "lib/read.mll"
         ( long_error "Expected '>' but found" v lexbuf )

# 3193 "lib/read.ml"

  
# 3194 "lib/read.ml"
  | 2 ->

# 1042 "lib/read.mll"
         
# 1042 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 3198 "lib/read.ml"

  
# 3199 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_gt_rec v lexbuf __ocaml_lex_state

and finish_buffer_comment v lexbuf =
   __ocaml_lex_finish_buffer_comment_rec v lexbuf 507
and __ocaml_lex_finish_buffer_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1045 "lib/read.mll"
         
# 1045 "lib/read.mll"
         ( Buffer.add_string v.buf "*/" )

# 3210 "lib/read.ml"

  
# 3211 "lib/read.ml"
  | 1 ->

# 1046 "lib/read.mll"
         
# 1046 "lib/read.mll"
         ( long_error "Unterminated comment" v lexbuf )

# 3215 "lib/read.ml"

  
# 3216 "lib/read.ml"
  | 2 ->

# 1047 "lib/read.mll"
         
# 1047 "lib/read.mll"
         ( Buffer.add_char v.buf '\n';
           newline v lexbuf;
           finish_buffer_comment v lexbuf )

# 3222 "lib/read.ml"

  
# 3223 "lib/read.ml"
  | 3 ->

# 1050 "lib/read.mll"
         
# 1050 "lib/read.mll"
         ( add_lexeme v.buf lexbuf; finish_buffer_comment v lexbuf )

# 3227 "lib/read.ml"

  
# 3228 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_comment_rec v lexbuf __ocaml_lex_state

and junk lexbuf =
   __ocaml_lex_junk_rec lexbuf 513
and __ocaml_lex_junk_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1053 "lib/read.mll"
             
# 1053 "lib/read.mll"
             ( Lexing.lexeme lexbuf )

# 3239 "lib/read.ml"

  
# 3240 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_junk_rec lexbuf __ocaml_lex_state

;;


# 1055 "lib/read.mll"
 
  
# 1056 "lib/read.mll"
  let _ = (read_json : lexer_state -> Lexing.lexbuf -> t)

  let read_t = read_json

  let () =
    read_junk := junk

  let read_int8 v lexbuf =
    let n = read_int v lexbuf in
    if n < 0 || n > 255 then
      lexer_error "Int8 overflow" v lexbuf
    else
      char_of_int n

  let read_list read_cell v lexbuf =
    List.rev (read_list_rev read_cell v lexbuf)

  let array_of_rev_list l =
    match l with
        [] -> [| |]
      | x :: tl ->
          let len = List.length l in
          let a = Array.make len x in
          let r = ref tl in
          for i = len - 2 downto 0 do
            a.(i) <- List.hd !r;
            r := List.tl !r
          done;
          a

  let read_array read_cell v lexbuf =
    let l = read_list_rev read_cell v lexbuf in
    array_of_rev_list l

  (* Read a JSON object, reading the keys into OCaml strings
     (provided for backward compatibility) *)
  let read_fields read_field init_acc v =
    read_abstract_fields read_ident read_field init_acc v

  let finish v lexbuf =
    read_space v lexbuf;
    if not (read_eof lexbuf) then
      long_error "Junk after end of JSON value:" v lexbuf

  let init_lexer = init_lexer

  let from_lexbuf v ?(stream = false) lexbuf =
    read_space v lexbuf;

    let x =
      if read_eof lexbuf then
        raise End_of_input
      else
        read_json v lexbuf
    in

    if not stream then
      finish v lexbuf;

    x


  let from_string ?buf ?fname ?lnum s =
    try
      let lexbuf = Lexing.from_string s in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with End_of_input ->
      json_error "Blank input data"

  let from_channel ?buf ?fname ?lnum ic =
    try
      let lexbuf = Lexing.from_channel ic in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with End_of_input ->
      json_error "Blank input data"

  let from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    try
      let x = from_channel ?buf ?fname ?lnum ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  exception Finally of exn * exn

  let stream_from_lexbuf v ?(fin = fun () -> ()) lexbuf =
    let stream = Some true in
    let f i =
      try Some (from_lexbuf v ?stream lexbuf)
      with
          End_of_input ->
            fin ();
            None
        | e ->
            (try fin () with fin_e -> raise (Finally (e, fin_e)));
            raise e
    in
    Stream.from f

  let stream_from_string ?buf ?fname ?lnum s =
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v (Lexing.from_string s)

  let stream_from_channel ?buf ?fin ?fname ?lnum ic =
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v ?fin lexbuf

  let stream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v ~fin lexbuf

  type json_line = [ `Json of t | `Exn of exn ]

  let linestream_from_channel
      ?buf ?(fin = fun () -> ()) ?fname ?lnum:(lnum0 = 1) ic =
    let buf =
      match buf with
          None -> Some (Buffer.create 256)
        | Some _ -> buf
    in
    let f i =
      try
        let line = input_line ic in
        let lnum = lnum0 + i in
        Some (`Json (from_string ?buf ?fname ~lnum line))
      with
          End_of_file -> fin (); None
        | e -> Some (`Exn e)
    in
    Stream.from f

  let linestream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    linestream_from_channel ?buf ~fin ?fname ?lnum ic

  let prettify ?std s =
    pretty_to_string ?std (from_string s)

  let compact ?std s =
    to_string (from_string s)

  let validate_json _path _value = None


# 3411 "lib/read.ml"
# 39 "yojson.cppo.ml"
module Util =
struct
# 1 "util.ml"
exception Type_error of string * t

let typeof = function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `Int _ -> "int"
  | `List _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"
  | `Intlit _ -> "intlit"
  | `Tuple _ -> "tuple"
  | `Variant _ -> "variant"

let typerr msg js = raise (Type_error (msg ^ typeof js, js))

exception Undefined of string * t

let assoc name obj =
  try List.assoc name obj
  with Not_found -> `Null

let member name = function
  | `Assoc obj -> assoc name obj
  | js -> typerr ("Can't get member '" ^ name ^ "' of non-object type ") js

let index i = function
  | `List l as js ->
      let len = List.length l in
      let wrapped_index = if i < 0 then len + i else i in
      if wrapped_index < 0 || wrapped_index >= len then
        raise (Undefined ("Index " ^ string_of_int i ^ " out of bounds", js))
      else List.nth l wrapped_index
  | js -> typerr ("Can't get index " ^ string_of_int i
                 ^ " of non-array type ") js

let map f = function
  | `List l -> `List (List.map f l)
  | js -> typerr "Can't map function over non-array type " js

let to_assoc = function
  | `Assoc obj -> obj
  | js -> typerr "Expected object, got " js

let to_option f = function
  | `Null -> None
  | x -> Some (f x)

let to_bool = function
  | `Bool b -> b
  | js -> typerr "Expected bool, got " js

let to_bool_option = function
  | `Bool b -> Some b
  | `Null -> None
  | js -> typerr "Expected bool or null, got " js

let to_number = function
  | `Int i -> float i
  | `Float f -> f
  | js -> typerr "Expected number, got " js

let to_number_option = function
  | `Int i -> Some (float i)
  | `Float f -> Some f
  | `Null -> None
  | js -> typerr "Expected number or null, got " js

let to_float = function
  | `Float f -> f
  | js -> typerr "Expected float, got " js

let to_float_option = function
  | `Float f -> Some f
  | `Null -> None
  | js -> typerr "Expected float or null, got " js

let to_int = function
  | `Int i -> i
  | js -> typerr "Expected int, got " js

let to_int_option = function
  | `Int i -> Some i
  | `Null -> None
  | js -> typerr "Expected int or null, got " js

let to_list = function
  | `List l -> l
  | js -> typerr "Expected array, got " js

let to_string = function
  | `String s -> s
  | js -> typerr "Expected string, got " js

let to_string_option = function
  | `String s -> Some s
  | `Null -> None
  | js -> typerr "Expected string or null, got " js

let convert_each f = function
  | `List l -> List.map f l
  | js -> typerr "Can't convert each element of non-array type " js


let rec rev_filter_map f acc l =
  match l with
      [] -> acc
    | x :: tl ->
        match f x with
            None -> rev_filter_map f acc tl
          | Some y -> rev_filter_map f (y :: acc) tl

let filter_map f l =
  List.rev (rev_filter_map f [] l)

let rec rev_flatten acc l =
  match l with
      [] -> acc
    | x :: tl ->
        match x with
            `List l2 -> rev_flatten (List.rev_append l2 acc) tl
          | _ -> rev_flatten acc tl

let flatten l =
  List.rev (rev_flatten [] l)

let filter_index i l =
  filter_map (
    function
        `List l ->
          (try Some (List.nth l i)
           with _ -> None)
      | _ -> None
  ) l

let filter_list l =
  filter_map (
    function
        `List l -> Some l
      | _ -> None
  ) l

let filter_member k l =
  filter_map (
    function
        `Assoc l ->
          (try Some (List.assoc k l)
           with _ -> None)
      | _ -> None
  ) l

let filter_assoc l =
  filter_map (
    function
        `Assoc l -> Some l
      | _ -> None
  ) l

let filter_bool l =
  filter_map (
    function
        `Bool x -> Some x
      | _ -> None
  ) l

let filter_int l =
  filter_map (
    function
        `Int x -> Some x
      | _ -> None
  ) l

let filter_float l =
  filter_map (
    function
        `Float x -> Some x
      | _ -> None
  ) l

let filter_number l =
  filter_map (
    function
        `Int x -> Some (float x)
      | `Float x -> Some x
      | _ -> None
  ) l

let filter_string l =
  filter_map (
    function
        `String x -> Some x
      | _ -> None
  ) l

let keys o =
  to_assoc o |> List.map (fun (key, _) -> key)

let values o =
  to_assoc o |> List.map (fun (_, value) -> value)

let combine (first : t) (second : t) =
  match (first, second) with
  | (`Assoc a, `Assoc b) -> (`Assoc (a @ b) :  t)
  | (a, b) -> raise (Invalid_argument "Expected two objects, check inputs")
# 42 "yojson.cppo.ml"
end
# 46 "yojson.cppo.ml"
end

module Safe =
struct
# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 8 "type.ml"
    | `Int of int
    
# 11 "type.ml"
    | `Intlit of string
    
# 14 "type.ml"
    | `Float of float
    
# 20 "type.ml"
    | `String of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 28 "type.ml"
    | `Tuple of t list
    
# 31 "type.ml"
    | `Variant of (string * t option)
    
# 33 "type.ml"
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)
# 1 "safe.ml"
let rec to_basic : t -> Basic.t = function
    `Null
  | `Bool _
  | `Int _
  | `Float _
  | `String _ as x -> x
  | `Intlit s -> `String s
  | `List l
  | `Tuple l ->
      `List (List.rev (List.rev_map to_basic l))
  | `Assoc l ->
      `Assoc (List.rev (List.rev_map (fun (k, v) -> (k, to_basic v)) l))
  | `Variant (k, None) -> `String k
  | `Variant (k, Some v) -> `List [ `String k; to_basic v ]
# 1 "write.ml"
(* included: type.ml *)

let hex n =
  Char.chr (
    if n < 10 then n + 48
    else n + 87
  )

let write_special src start stop ob str =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob str;
  start := stop + 1

let write_control_char src start stop ob c =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob "\\u00";
  Buffer.add_char ob (hex (Char.code c lsr 4));
  Buffer.add_char ob (hex (Char.code c land 0xf));
  start := stop + 1

let finish_string src start ob =
  try
    Buffer.add_substring ob src !start (String.length src - !start)
  with exc ->
    Printf.eprintf "src=%S start=%i len=%i\n%!"
      src !start (String.length src - !start);
    raise exc

let write_string_body ob s =
  let start = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '"' -> write_special s start i ob "\\\""
      | '\\' -> write_special s start i ob "\\\\"
      | '\b' -> write_special s start i ob "\\b"
      | '\012' -> write_special s start i ob "\\f"
      | '\n' -> write_special s start i ob "\\n"
      | '\r' -> write_special s start i ob "\\r"
      | '\t' -> write_special s start i ob "\\t"
      | '\x00'..'\x1F'
      | '\x7F' as c -> write_control_char s start i ob c
      | _ -> ()
  done;
  finish_string s start ob

let write_string ob s =
  Buffer.add_char ob '"';
  write_string_body ob s;
  Buffer.add_char ob '"'

let json_string_of_string s =
  let ob = Buffer.create 10 in
  write_string ob s;
  Buffer.contents ob

let test_string () =
  let s = Bytes.create 256 in
  for i = 0 to 255 do
    Bytes.set s i (Char.chr i)
  done;
  json_string_of_string (Bytes.to_string s)


let write_null ob () =
  Buffer.add_string ob "null"

let write_bool ob x =
  Buffer.add_string ob (if x then "true" else "false")


let max_digits =
  max
    (String.length (string_of_int max_int))
    (String.length (string_of_int min_int))

let dec n =
  Char.chr (n + 48)

let rec write_digits s x =
  if x = 0 then ()
  else
    let d = x mod 10 in
    write_digits s (x / 10);
    Buffer.add_char s (dec (abs d))

let write_int ob x =
  if x > 0 then
    write_digits ob x
  else if x < 0 then (
    Buffer.add_char ob '-';
    write_digits ob x
  )
  else
    Buffer.add_char ob '0'


let json_string_of_int i =
  string_of_int i


(*
  Ensure that the float is not printed as an int.
  This is not required by JSON, but useful in order to guarantee
  reversibility.
*)
let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with
          '0'..'9' | '-' -> ()
        | _ -> raise Exit
    done;
    true
  with Exit ->
    false

(*
  Both write_float_fast and write_float guarantee
  that a sufficient number of digits are printed in order to
  allow reversibility.

  The _fast version is faster but often produces unnecessarily long numbers.
*)
let write_float_fast ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s = Printf.sprintf "%.17g" x in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_float ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_normal_float_prec significant_figures ob x =
  let open Printf in
  let s =
    match significant_figures with
        1 -> sprintf "%.1g" x
      | 2 -> sprintf "%.2g" x
      | 3 -> sprintf "%.3g" x
      | 4 -> sprintf "%.4g" x
      | 5 -> sprintf "%.5g" x
      | 6 -> sprintf "%.6g" x
      | 7 -> sprintf "%.7g" x
      | 8 -> sprintf "%.8g" x
      | 9 -> sprintf "%.9g" x
      | 10 -> sprintf "%.10g" x
      | 11 -> sprintf "%.11g" x
      | 12 -> sprintf "%.12g" x
      | 13 -> sprintf "%.13g" x
      | 14 -> sprintf "%.14g" x
      | 15 -> sprintf "%.15g" x
      | 16 -> sprintf "%.16g" x
      | _ -> sprintf "%.17g" x
  in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

let write_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      write_normal_float_prec significant_figures ob x

let json_string_of_float x =
  let ob = Buffer.create 20 in
  write_float ob x;
  Buffer.contents ob


let write_std_float_fast ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s = Printf.sprintf "%.17g" x in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_std_float ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_std_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      write_normal_float_prec significant_figures ob x

let std_json_string_of_float x =
  let ob = Buffer.create 20 in
  write_std_float ob x;
  Buffer.contents ob


let test_float () =
  let l = [ 0.; 1.; -1. ] in
  let l = l @ List.map (fun x -> 2. *. x +. 1.) l in
  let l = l @ List.map (fun x -> x /. sqrt 2.) l in
  let l = l @ List.map (fun x -> x *. sqrt 3.) l in
  let l = l @ List.map cos l in
  let l = l @ List.map (fun x -> x *. 1.23e50) l in
  let l = l @ [ infinity; neg_infinity ] in
  List.iter (
    fun x ->
      let s = Printf.sprintf "%.17g" x in
      let y = float_of_string s in
      Printf.printf "%g %g %S %B\n" x y s (x = y)
  )
    l

(*
let () = test_float ()
*)

let write_intlit = Buffer.add_string
let write_floatlit = Buffer.add_string
let write_stringlit = Buffer.add_string

let rec iter2_aux f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_sep x;
      f_elt x y;
      iter2_aux f_elt f_sep x l

let iter2 f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_elt x y;
      iter2_aux f_elt f_sep x l

let f_sep ob =
  Buffer.add_char ob ','

let rec write_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    
# 293 "write.ml"
    | `Int i -> write_int ob i
    
# 296 "write.ml"
    | `Intlit s -> Buffer.add_string ob s
    
# 299 "write.ml"
    | `Float f -> write_float ob f
    
# 305 "write.ml"
    | `String s -> write_string ob s
    
# 310 "write.ml"
    | `Assoc l -> write_assoc ob l
    | `List l -> write_list ob l
    
# 313 "write.ml"
    | `Tuple l -> write_tuple ob l
    
# 316 "write.ml"
    | `Variant (s, o) -> write_variant ob s o

# 319 "write.ml"
and write_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_list ob l =
  Buffer.add_char ob '[';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ']'

# 335 "write.ml"
and write_tuple ob l =
  Buffer.add_char ob '(';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ')'

# 342 "write.ml"
and write_variant ob s o =
  Buffer.add_char ob '<';
  write_string ob s;
  (match o with
       None -> ()
     | Some x ->
         Buffer.add_char ob ':';
         write_json ob x
  );
  Buffer.add_char ob '>'

# 354 "write.ml"
let write_t = write_json

let rec write_std_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    
# 361 "write.ml"
    | `Int i -> write_int ob i
    
# 364 "write.ml"
    | `Intlit s -> Buffer.add_string ob s
    
# 367 "write.ml"
    | `Float f -> write_std_float ob f
    
# 373 "write.ml"
    | `String s -> write_string ob s
    
# 378 "write.ml"
    | `Assoc l -> write_std_assoc ob l
    | `List l -> write_std_list ob l
    
# 381 "write.ml"
    | `Tuple l -> write_std_tuple ob l
    
# 384 "write.ml"
    | `Variant (s, o) -> write_std_variant ob s o

# 387 "write.ml"
and write_std_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_std_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_std_list ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'

and write_std_tuple ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'

# 408 "write.ml"
and write_std_variant ob s o =
  match o with
      None -> write_string ob s
    | Some x ->
        Buffer.add_char ob '[';
        write_string ob s;
        Buffer.add_char ob ',';
        write_std_json ob x;
        Buffer.add_char ob ']'


# 420 "write.ml"
let to_buffer ?(std = false) ob x =
  if std then (
    if not (is_object_or_array x) then
      json_error "Root is not an object or array"
    else
      write_std_json ob x
  )
  else
    write_json ob x


let to_string ?buf ?(len = 256) ?std x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  to_buffer ?std ob x;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let to_channel ?buf ?(len=4096) ?std oc x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  to_buffer ?std ob x;
  Buffer.output_buffer oc ob

let to_output ?buf ?(len=4096) ?std out x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  to_buffer ?std ob x;
  out#output (Buffer.contents ob) 0 (Buffer.length ob);
  ()

let to_file ?len ?std file x =
  let oc = open_out file in
  try
    to_channel ?len ?std oc x;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let stream_to_buffer ?std ob st =
  Stream.iter (to_buffer ?std ob) st

let stream_to_string ?buf ?(len = 256) ?std st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  stream_to_buffer ?std ob st;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let stream_to_channel ?buf ?(len=2096) ?std oc st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  stream_to_buffer ?std ob st

let stream_to_file ?len ?std file st =
  let oc = open_out file in
  try
    stream_to_channel ?len ?std oc st;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e


let rec sort = function
  | `Assoc l ->
      let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
      `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
  | `List l ->
      `List (List.rev (List.rev_map sort l))
  
# 513 "write.ml"
  | `Tuple l ->
      `Tuple (List.rev (List.rev_map sort l))
  
# 517 "write.ml"
  | `Variant (k, Some v) as x ->
      let v' = sort v in
      if v == v' then x
      else
        `Variant (k, Some v')
  
# 523 "write.ml"
  | x -> x
# 1 "monomorphic.ml"
let rec pp fmt =
  function
  | `Null -> Format.pp_print_string fmt "`Null"
  | `Bool x ->
    Format.fprintf fmt "`Bool (@[<hov>";
    Format.fprintf fmt "%B" x;
    Format.fprintf fmt "@])"
  
# 9 "monomorphic.ml"
  | `Int x ->
    Format.fprintf fmt "`Int (@[<hov>";
    Format.fprintf fmt "%d" x;
    Format.fprintf fmt "@])"
  
# 15 "monomorphic.ml"
  | `Intlit x ->
    Format.fprintf fmt "`Intlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 21 "monomorphic.ml"
  | `Float x ->
    Format.fprintf fmt "`Float (@[<hov>";
    Format.fprintf fmt "%F" x;
    Format.fprintf fmt "@])"
  
# 33 "monomorphic.ml"
  | `String x ->
    Format.fprintf fmt "`String (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 44 "monomorphic.ml"
  | `Assoc xs ->
    Format.fprintf fmt "`Assoc (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep (key, value) ->
        if sep then
          Format.fprintf fmt ";@ ";
          Format.fprintf fmt "(@[";
          Format.fprintf fmt "%S" key;
          Format.fprintf fmt ",@ ";
          pp fmt value;
          Format.fprintf fmt "@])";
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  | `List xs ->
    Format.fprintf fmt "`List (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep x ->
        if sep then
          Format.fprintf fmt ";@ ";
          pp fmt x;
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  
# 71 "monomorphic.ml"
  | `Tuple tup ->
    Format.fprintf fmt "`Tuple (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep e ->
         if sep then
           Format.fprintf fmt ";@ ";
           pp fmt e;
           true) false tup);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  
# 84 "monomorphic.ml"
  | `Variant (name, value) ->
    Format.fprintf fmt "`Variant (@[<hov>";
    Format.fprintf fmt "(@[";
    Format.fprintf fmt "%S" name;
    Format.fprintf fmt ",@ ";
    (match value with
      | None -> Format.pp_print_string fmt "None"
      | Some x ->
        Format.pp_print_string fmt "(Some ";
        pp fmt x;
        Format.pp_print_string fmt ")");
    Format.fprintf fmt "@])";
    Format.fprintf fmt "@])"

# 99 "monomorphic.ml"
let show x =
  Format.asprintf "%a" pp x

let rec equal a b =
  match a, b with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
  
# 107 "monomorphic.ml"
  | `Int a, `Int b -> a = b
    
# 110 "monomorphic.ml"
    | `Intlit a, `Intlit b -> a = b
    
# 113 "monomorphic.ml"
    | `Float a, `Float b -> a = b
    
# 119 "monomorphic.ml"
    | `String a, `String b -> a = b
    
# 124 "monomorphic.ml"
    | `Assoc xs, `Assoc ys ->
      let compare_keys = fun (key, _) (key', _) -> String.compare key key' in
      let xs = List.stable_sort compare_keys xs in
      let ys = List.stable_sort compare_keys ys in
      (match List.for_all2 (fun (key, value) (key', value') ->
        match key = key' with
        | false -> false
        | true -> equal value value') xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    
# 137 "monomorphic.ml"
    | `Tuple xs, `Tuple ys
    
# 139 "monomorphic.ml"
    | `List xs, `List ys ->
      (match List.for_all2 equal xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    
# 146 "monomorphic.ml"
    | `Variant (name, value), `Variant (name', value') ->
      (match name = name' with
      | false -> false
      | true ->
        match value, value' with
        | None, None -> true
        | Some x, Some y -> equal x y
        | _ -> false)
    
# 155 "monomorphic.ml"
    | _ -> false

# 2 "write2.ml"
let pretty_print ?std out (x : t) =
  Pretty.pp ?std out (x :> json_max)

let pretty_to_string ?std (x : t) =
  Pretty.to_string ?std (x :> json_max)

let pretty_to_channel ?std oc (x : t) =
  Pretty.to_channel ?std oc (x :> json_max)

# 1 "lib/read.mll"
 
  
# 2 "lib/read.mll"
  module Lexing =
    (*
      We override Lexing.engine in order to avoid creating a new position
      record each time a rule is matched.
      This reduces total parsing time by about 31%.
    *)
  struct
    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
        buf.lex_start_p <- buf.lex_curr_p;
        buf.lex_curr_p <- {buf.lex_curr_p
                           with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end

  open Printf
  open Lexing

  (* see description in common.mli *)
  type lexer_state = Lexer_state.t = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }

  let dec c =
    Char.code c - 48

  let hex c =
    match c with
        '0'..'9' -> int_of_char c - int_of_char '0'
      | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
      | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
      | _ -> assert false

  let custom_error descr v lexbuf =
    let offs = lexbuf.lex_abs_pos - 1 in
    let bol = v.bol in
    let pos1 = offs + lexbuf.lex_start_pos - bol - 1 in
    let pos2 = max pos1 (offs + lexbuf.lex_curr_pos - bol) in
    let file_line =
      match v.fname with
          None -> "Line"
        | Some s ->
            sprintf "File %s, line" s
    in
    let bytes =
      if pos1 = pos2 then
        sprintf "byte %i" (pos1+1)
      else
        sprintf "bytes %i-%i" (pos1+1) (pos2+1)
    in
    let msg = sprintf "%s %i, %s:\n%s" file_line v.lnum bytes descr in
    json_error msg


  let lexer_error descr v lexbuf =
    custom_error
      (sprintf "%s '%s'" descr (Lexing.lexeme lexbuf))
      v lexbuf

  let read_junk = ref (fun _ -> assert false)

  let long_error descr v lexbuf =
    let junk = Lexing.lexeme lexbuf in
    let extra_junk = !read_junk lexbuf in
    custom_error
      (sprintf "%s '%s%s'" descr junk extra_junk)
      v lexbuf

  let min10 = min_int / 10 - (if min_int mod 10 = 0 then 0 else 1)
  let max10 = max_int / 10 + (if max_int mod 10 = 0 then 0 else 1)

  exception Int_overflow

  let extract_positive_int lexbuf =
    let start = lexbuf.lex_start_pos in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n >= max10 then
        raise Int_overflow
      else
        n := 10 * !n + dec (Bytes.get s i)
    done;
    if !n < 0 then
      raise Int_overflow
    else
      !n

  let make_positive_int v lexbuf =
      
# 104 "lib/read.mll"
      try `Int (extract_positive_int lexbuf)
      with Int_overflow ->
        
# 108 "lib/read.mll"
        `Intlit (lexeme lexbuf)

  
# 113 "lib/read.mll"
  let extract_negative_int lexbuf =
    let start = lexbuf.lex_start_pos + 1 in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n <= min10 then
        raise Int_overflow
      else
        n := 10 * !n - dec (Bytes.get s i)
    done;
    if !n > 0 then
      raise Int_overflow
    else
      !n

  let make_negative_int v lexbuf =
      
# 131 "lib/read.mll"
      try `Int (extract_negative_int lexbuf)
      with Int_overflow ->
        
# 135 "lib/read.mll"
        `Intlit (lexeme lexbuf)


  
# 141 "lib/read.mll"
  let set_file_name v fname =
    v.fname <- fname

  let newline v lexbuf =
    v.lnum <- v.lnum + 1;
    v.bol <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos

  let add_lexeme buf lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_subbytes buf lexbuf.lex_buffer lexbuf.lex_start_pos len

  let map_lexeme f lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    f (Bytes.to_string lexbuf.lex_buffer) lexbuf.lex_start_pos len

  type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]
  type tuple_kind = [ `Parenthesis | `Square_bracket ]


# 161 "lib/read.ml"
# 161 "lib/read.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\236\255\237\255\003\000\239\255\016\000\242\255\243\255\
    \244\255\245\255\000\000\031\000\249\255\085\000\001\000\000\000\
    \000\000\001\000\000\000\001\000\002\000\255\255\000\000\000\000\
    \003\000\254\255\001\000\004\000\253\255\011\000\252\255\003\000\
    \001\000\003\000\002\000\003\000\000\000\251\255\021\000\097\000\
    \010\000\022\000\020\000\016\000\022\000\012\000\008\000\250\255\
    \119\000\129\000\139\000\161\000\171\000\181\000\193\000\209\000\
    \240\255\011\000\038\000\252\255\065\000\254\255\255\255\110\000\
    \252\255\163\000\254\255\255\255\234\000\247\255\248\255\048\001\
    \250\255\251\255\252\255\253\255\254\255\255\255\071\001\126\001\
    \149\001\249\255\039\000\253\255\254\255\038\000\187\001\210\001\
    \248\001\015\002\255\255\220\000\253\255\255\255\245\000\039\002\
    \109\002\014\001\088\002\164\002\187\002\225\002\013\000\252\255\
    \253\255\254\255\255\255\014\000\253\255\254\255\255\255\030\000\
    \253\255\254\255\255\255\015\000\253\255\254\255\255\255\017\001\
    \251\255\252\255\253\255\254\255\255\255\019\000\252\255\253\255\
    \254\255\015\000\255\255\016\000\255\255\008\001\005\000\253\255\
    \023\000\254\255\020\000\255\255\046\000\253\255\254\255\042\000\
    \052\000\053\000\255\255\053\000\048\000\091\000\092\000\255\255\
    \027\001\250\255\251\255\137\000\104\000\089\000\088\000\106\000\
    \255\255\143\000\137\000\177\000\254\255\183\000\168\000\166\000\
    \183\000\002\000\253\255\177\000\172\000\187\000\004\000\252\255\
    \053\002\251\255\252\255\253\255\103\001\255\255\248\002\254\255\
    \006\003\030\003\252\255\253\255\254\255\255\255\040\003\050\003\
    \074\003\252\255\253\255\254\255\255\255\061\003\084\003\108\003\
    \249\255\250\255\251\255\244\000\120\003\142\003\179\000\194\000\
    \015\000\255\255\190\000\188\000\187\000\193\000\183\000\179\000\
    \254\255\191\000\201\000\200\000\196\000\203\000\193\000\189\000\
    \253\255\157\003\095\003\174\003\196\003\206\003\216\003\228\003\
    \239\003\060\000\253\255\254\255\255\255\012\004\252\255\253\255\
    \087\004\255\255\145\004\252\255\253\255\221\004\255\255\229\000\
    \253\255\254\255\255\255\231\000\253\255\254\255\255\255\002\000\
    \255\255\018\001\252\255\253\255\254\255\255\255\034\001\253\255\
    \254\255\255\255\000\000\255\255\003\000\254\255\255\255\038\001\
    \252\255\253\255\254\255\255\255\120\001\251\255\252\255\253\255\
    \254\255\255\255\208\000\253\255\254\255\255\255\211\000\253\255\
    \254\255\255\255\189\000\255\255\143\001\252\255\253\255\254\255\
    \255\255\013\001\253\255\254\255\255\255\095\001\252\255\253\255\
    \254\255\255\255\050\001\253\255\254\255\255\255\026\001\253\255\
    \254\255\255\255\233\000\253\255\254\255\255\255\222\000\253\255\
    \254\255\255\255\079\005\237\255\238\255\010\000\240\255\044\001\
    \243\255\244\255\245\255\246\255\061\001\002\004\249\255\045\005\
    \209\000\228\000\211\000\232\000\225\000\223\000\240\000\255\255\
    \235\000\234\000\008\001\254\255\004\001\023\001\253\255\054\001\
    \252\255\031\001\029\001\032\001\039\001\049\001\045\001\251\255\
    \057\001\082\001\080\001\078\001\084\001\074\001\086\001\250\255\
    \110\005\012\004\123\005\155\005\165\005\177\005\187\005\197\005\
    \241\255\199\001\077\002\253\255\255\255\154\002\222\005\209\005\
    \155\002\239\005\053\006\076\006\114\006\016\002\252\255\253\255\
    \254\255\255\255\152\006\252\255\253\255\227\006\255\255\085\007\
    \244\255\245\255\011\000\247\255\076\002\250\255\251\255\252\255\
    \253\255\254\255\031\002\243\005\051\007\100\001\115\001\104\001\
    \133\001\118\001\154\001\171\001\255\255\173\001\176\001\191\001\
    \185\001\187\001\253\001\230\001\230\001\234\001\247\001\237\001\
    \234\001\009\002\019\002\019\002\015\002\021\002\011\002\007\002\
    \142\006\152\006\116\007\170\007\180\007\190\007\200\007\210\007\
    \248\255\120\002\167\002\253\255\255\255\216\002\082\007\220\007\
    \236\002\244\007\058\008\081\008\119\008\076\002\252\255\253\255\
    \254\255\255\255\157\008\252\255\253\255\232\008\255\255\135\002\
    \120\002\253\255\100\002\254\255\182\002\255\255\011\002\255\255\
    \204\002\252\255\253\255\254\255\255\255\046\002\255\255\178\002\
    \252\255\253\255\254\255\255\255\023\000\255\255\183\002\252\255\
    \253\255\254\255\255\255\187\002\253\255\254\255\255\255\121\002\
    \253\255\254\255\255\255\184\002\252\255\253\255\254\255\019\000\
    \255\255\140\001\146\001\255\255\150\001\151\001\154\001\168\001\
    \170\001\171\001\172\001\173\001\181\001\184\001\185\001\187\001\
    \191\001\193\001\195\001\196\001\197\001\200\001\203\001\223\001\
    \225\001\228\001\249\001\251\001\002\002\004\002\011\002\012\002\
    \013\002\000\000";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\017\000\255\255\019\000\255\255\255\255\
    \255\255\255\255\007\000\007\000\255\255\019\000\019\000\019\000\
    \019\000\019\000\019\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\008\000\008\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\009\000\255\255\009\000\255\255\009\000\255\255\
    \255\255\014\000\255\255\255\255\002\000\255\255\255\255\255\255\
    \255\255\002\000\255\255\255\255\255\255\255\255\255\255\007\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\001\000\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\255\255\001\000\255\255\004\000\003\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\
    \255\255\255\255\255\255\001\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\004\000\004\000\004\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\255\255\000\000\255\255\
    \001\000\255\255\255\255\255\255\255\255\255\255\000\000\002\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\002\000\255\255\
    \255\255\255\255\255\255\003\000\003\000\005\000\005\000\005\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\003\000\255\255\003\000\255\255\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \001\000\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\002\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\255\255\018\000\
    \255\255\255\255\255\255\255\255\007\000\007\000\255\255\018\000\
    \018\000\018\000\018\000\018\000\018\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\008\000\255\255\008\000\255\255\008\000\255\255\
    \255\255\013\000\255\255\255\255\255\255\001\000\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\009\000\255\255\011\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\255\255\000\000\255\255\
    \255\255\006\000\255\255\255\255\255\255\001\000\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\004\000\
    \003\000\255\255\255\255\255\255\255\255\255\255\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\001\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\000\000\255\255\000\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\057\000\060\000\000\000\060\000\000\000\000\000\065\000\
    \000\000\065\000\000\000\000\000\070\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\084\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\094\000\000\000\000\000\097\000\255\255\
    \255\255\097\000\255\255\255\255\255\255\255\255\104\000\000\000\
    \000\000\000\000\000\000\109\000\000\000\000\000\000\000\113\000\
    \000\000\000\000\000\000\117\000\000\000\000\000\000\000\121\000\
    \000\000\000\000\000\000\000\000\000\000\126\000\000\000\000\000\
    \000\000\255\255\000\000\255\255\000\000\255\255\255\255\000\000\
    \255\255\000\000\138\000\000\000\142\000\000\000\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \154\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \178\000\000\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \255\255\187\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \194\000\000\000\000\000\000\000\000\000\255\255\255\255\201\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\235\000\000\000\000\000\000\000\239\000\000\000\000\000\
    \255\255\000\000\244\000\000\000\000\000\255\255\000\000\249\000\
    \000\000\000\000\000\000\253\000\000\000\000\000\000\000\255\255\
    \000\000\003\001\000\000\000\000\000\000\000\000\008\001\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\017\001\
    \000\000\000\000\000\000\000\000\022\001\000\000\000\000\000\000\
    \000\000\000\000\028\001\000\000\000\000\000\000\032\001\000\000\
    \000\000\000\000\255\255\000\000\038\001\000\000\000\000\000\000\
    \000\000\043\001\000\000\000\000\000\000\047\001\000\000\000\000\
    \000\000\000\000\052\001\000\000\000\000\000\000\056\001\000\000\
    \000\000\000\000\060\001\000\000\000\000\000\000\064\001\000\000\
    \000\000\000\000\067\001\000\000\000\000\255\255\000\000\255\255\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\121\001\125\001\000\000\000\000\128\001\255\255\255\255\
    \128\001\255\255\255\255\255\255\255\255\135\001\000\000\000\000\
    \000\000\000\000\140\001\000\000\000\000\255\255\000\000\144\001\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\193\001\197\001\000\000\000\000\200\001\255\255\255\255\
    \200\001\255\255\255\255\255\255\255\255\207\001\000\000\000\000\
    \000\000\000\000\212\001\000\000\000\000\255\255\000\000\255\255\
    \255\255\000\000\255\255\000\000\220\001\000\000\255\255\000\000\
    \226\001\000\000\000\000\000\000\000\000\255\255\000\000\233\001\
    \000\000\000\000\000\000\000\000\255\255\000\000\240\001\000\000\
    \000\000\000\000\000\000\245\001\000\000\000\000\000\000\249\001\
    \000\000\000\000\000\000\252\001\000\000\000\000\000\000\255\255\
    \000\000\002\002\004\002\000\000\005\002\006\002\007\002\008\002\
    \009\002\010\002\011\002\012\002\013\002\014\002\015\002\016\002\
    \017\002\018\002\019\002\020\002\021\002\022\002\023\002\024\002\
    \025\002\026\002\027\002\028\002\029\002\030\002\031\002\032\002\
    \033\002\003\002";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\004\000\000\000\003\000\003\000\134\000\000\000\
    \003\000\000\000\134\000\069\001\146\001\255\255\000\000\069\001\
    \146\001\000\000\000\000\000\000\000\000\127\000\139\000\000\000\
    \003\000\000\000\012\000\003\000\170\000\134\000\175\000\000\000\
    \007\000\011\001\069\001\146\001\014\001\013\000\049\000\005\000\
    \010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\056\000\118\000\006\000\129\000\130\000\057\000\
    \237\001\137\000\000\002\049\000\000\000\048\000\138\000\106\000\
    \062\000\014\000\110\000\105\000\000\000\049\000\015\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\030\000\048\000\008\000\114\000\209\000\236\000\000\001\
    \013\001\029\000\022\000\255\255\048\000\048\000\017\000\021\000\
    \025\000\032\000\033\000\035\000\023\000\027\000\016\000\031\000\
    \028\000\034\000\019\000\024\000\018\000\026\000\020\000\036\000\
    \041\000\037\000\048\000\009\000\042\000\043\000\044\000\045\000\
    \046\000\047\000\061\000\085\000\048\000\038\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\049\000\
    \067\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\086\000\143\000\255\255\040\000\144\000\
    \145\000\146\000\055\000\148\000\055\000\149\000\048\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\255\255\048\000\150\000\
    \151\000\161\000\066\000\158\000\053\000\159\000\053\000\160\000\
    \051\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\165\000\
    \051\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\162\000\163\000\166\000\093\000\255\255\
    \002\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\255\255\077\000\103\000\108\000\116\000\
    \132\000\134\000\135\000\128\000\139\000\134\000\164\000\093\000\
    \171\000\077\000\167\000\168\000\169\000\172\000\112\000\173\000\
    \174\000\210\000\226\000\208\000\211\000\212\000\059\000\083\000\
    \134\000\213\000\214\000\215\000\216\000\218\000\141\000\219\000\
    \093\000\220\000\221\000\123\000\222\000\223\000\224\000\136\000\
    \095\000\225\000\035\001\065\001\234\000\155\000\005\001\097\001\
    \250\000\255\255\254\000\057\001\061\001\095\001\077\000\044\001\
    \092\001\088\001\009\001\029\001\076\000\124\000\033\001\018\001\
    \075\000\098\000\019\001\085\001\086\001\087\001\120\001\089\001\
    \074\000\225\000\053\001\121\001\073\000\090\001\072\000\071\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\098\000\113\001\122\000\091\001\064\000\004\001\
    \093\001\078\000\078\000\078\000\078\000\078\000\078\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\156\000\112\001\094\001\096\001\098\001\099\001\049\001\
    \079\000\079\000\079\000\079\000\079\000\079\000\100\001\157\000\
    \101\001\078\000\078\000\078\000\078\000\078\000\078\000\183\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\024\001\112\001\255\255\025\001\102\001\103\001\105\001\
    \079\000\079\000\079\000\079\000\079\000\079\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \106\001\107\001\048\001\040\001\108\001\109\001\110\001\080\000\
    \080\000\080\000\080\000\080\000\080\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\111\001\
    \027\001\255\255\171\001\031\001\170\001\023\001\081\000\081\000\
    \081\000\081\000\081\000\081\000\092\000\168\001\063\001\080\000\
    \080\000\080\000\080\000\080\000\080\000\248\000\165\001\252\000\
    \162\001\059\001\069\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\255\255\081\000\081\000\
    \081\000\081\000\081\000\081\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\039\001\042\001\255\255\163\001\
    \164\001\120\000\002\001\088\000\088\000\088\000\088\000\088\000\
    \088\000\166\001\055\001\153\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\007\001\167\001\164\001\169\001\016\001\164\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\051\001\088\000\088\000\088\000\088\000\088\000\
    \088\000\089\000\089\000\089\000\089\000\089\000\089\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\097\000\137\001\164\001\172\001\185\001\136\001\173\001\
    \090\000\090\000\090\000\090\000\090\000\090\000\097\000\179\000\
    \174\001\089\000\089\000\089\000\089\000\089\000\089\000\046\001\
    \175\001\176\001\180\000\164\001\184\001\181\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\124\001\
    \090\000\090\000\090\000\090\000\090\000\090\000\192\001\178\001\
    \021\001\179\001\097\000\193\001\180\001\181\001\182\001\183\001\
    \164\001\216\001\255\255\097\000\184\001\216\001\209\001\097\000\
    \223\001\097\000\208\001\230\001\003\002\097\000\219\001\037\001\
    \216\001\217\001\003\002\220\001\216\001\097\000\003\002\003\002\
    \216\001\097\000\003\002\097\000\096\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\216\001\
    \003\002\126\001\003\002\003\002\003\002\003\002\099\000\099\000\
    \099\000\099\000\099\000\099\000\097\000\003\002\218\001\250\001\
    \003\002\003\002\097\000\003\002\124\001\124\001\097\000\003\002\
    \221\001\003\002\253\001\003\002\003\002\003\002\097\000\255\255\
    \003\002\196\001\097\000\003\002\097\000\096\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\235\001\003\002\
    \241\001\003\002\255\001\242\001\003\002\100\000\100\000\100\000\
    \100\000\100\000\100\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\246\001\129\001\129\001\
    \228\001\003\002\196\001\003\002\101\000\101\000\101\000\101\000\
    \101\000\101\000\003\002\198\001\003\002\100\000\100\000\100\000\
    \100\000\100\000\100\000\003\002\003\002\003\002\196\001\234\001\
    \134\001\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\000\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\000\000\000\000\201\001\177\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \188\000\000\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \201\001\227\001\000\000\191\000\206\001\123\001\189\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\189\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\195\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\198\000\
    \255\255\248\001\196\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\196\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\202\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\205\000\255\255\255\255\203\000\204\000\204\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\226\000\195\001\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\232\001\000\000\000\000\206\000\221\001\239\001\
    \254\001\000\000\207\000\244\001\000\000\225\000\203\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \232\000\000\000\232\000\000\000\225\001\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\217\000\
    \255\255\000\000\000\000\000\000\000\000\225\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\230\000\
    \000\000\230\000\000\000\228\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\000\000\228\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\186\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\000\000\000\000\000\000\000\000\000\000\241\000\000\000\
    \113\001\000\000\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\114\001\000\000\112\001\
    \000\000\000\000\193\000\000\000\000\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\112\001\
    \000\000\000\000\000\000\240\000\200\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\000\000\246\000\000\000\000\000\240\000\000\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\000\000\000\000\000\000\000\000\
    \245\000\000\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\238\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \000\000\000\000\000\000\000\000\245\000\000\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \069\001\070\001\000\000\000\000\069\001\076\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\069\001\
    \000\000\078\001\000\000\000\000\000\000\000\000\104\001\073\001\
    \000\000\000\000\000\000\000\000\079\001\000\000\071\001\076\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\000\000\000\000\072\001\000\000\000\000\000\000\000\000\
    \000\000\243\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \080\001\119\001\000\000\119\001\000\000\081\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\118\001\
    \000\000\000\000\074\001\114\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\083\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\082\001\000\000\000\000\
    \115\001\000\000\000\000\084\001\000\000\000\000\117\001\000\000\
    \117\001\000\000\075\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\000\000\
    \115\001\116\001\116\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\000\000\
    \128\001\130\001\130\001\130\001\130\001\130\001\130\001\130\001\
    \130\001\130\001\130\001\000\000\000\000\128\001\000\000\000\000\
    \000\000\128\001\130\001\130\001\130\001\130\001\130\001\130\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\128\001\000\000\
    \000\000\185\001\000\000\155\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\000\000\000\000\000\000\
    \000\000\000\000\130\001\130\001\130\001\130\001\130\001\130\001\
    \184\001\000\000\128\001\000\000\000\000\000\000\000\000\000\000\
    \128\001\000\000\000\000\000\000\128\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\128\001\128\001\000\000\000\000\068\001\
    \128\001\128\001\128\001\127\001\000\000\128\001\000\000\000\000\
    \184\001\000\000\000\000\000\000\000\000\128\001\000\000\000\000\
    \000\000\128\001\000\000\128\001\127\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\131\001\131\001\
    \131\001\131\001\131\001\131\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\132\001\132\001\132\001\
    \132\001\132\001\132\001\000\000\000\000\000\000\131\001\131\001\
    \131\001\131\001\131\001\131\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\128\001\128\001\128\001\128\001\128\001\128\001\
    \128\001\128\001\128\001\128\001\000\000\132\001\132\001\132\001\
    \132\001\132\001\132\001\128\001\128\001\128\001\128\001\128\001\
    \128\001\191\001\142\001\191\001\000\000\000\000\190\001\190\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\000\000\128\001\128\001\128\001\128\001\128\001\
    \128\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\000\000\000\000\000\000\000\000\141\001\
    \000\000\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\000\000\000\000\
    \000\000\000\000\141\001\000\000\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\146\001\147\001\
    \000\000\000\000\146\001\154\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\200\001\146\001\000\000\153\001\
    \000\000\000\000\000\000\000\000\177\001\150\001\000\000\000\000\
    \000\000\200\001\156\001\000\000\148\001\154\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\155\001\155\001\000\000\
    \000\000\149\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \139\001\000\000\000\000\000\000\000\000\000\000\157\001\000\000\
    \000\000\000\000\000\000\158\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\200\001\000\000\
    \151\001\000\000\000\000\000\000\200\001\000\000\000\000\000\000\
    \200\001\187\001\000\000\160\001\000\000\000\000\000\000\000\000\
    \200\001\000\000\000\000\159\001\200\001\000\000\200\001\199\001\
    \000\000\161\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \152\001\000\000\000\000\000\000\000\000\189\001\000\000\189\001\
    \000\000\187\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\190\001\190\001\202\001\202\001\202\001\202\001\
    \202\001\202\001\202\001\202\001\202\001\202\001\200\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\202\001\202\001\202\001\
    \202\001\202\001\202\001\200\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\202\001\202\001\202\001\
    \202\001\202\001\202\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \200\001\000\000\000\000\000\000\000\000\145\001\200\001\000\000\
    \000\000\000\000\200\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\200\001\000\000\000\000\000\000\200\001\000\000\
    \200\001\199\001\203\001\203\001\203\001\203\001\203\001\203\001\
    \203\001\203\001\203\001\203\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\203\001\203\001\203\001\203\001\203\001\
    \203\001\204\001\204\001\204\001\204\001\204\001\204\001\204\001\
    \204\001\204\001\204\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\204\001\204\001\204\001\204\001\204\001\204\001\
    \000\000\000\000\000\000\203\001\203\001\203\001\203\001\203\001\
    \203\001\000\000\000\000\000\000\000\000\000\000\000\000\200\001\
    \200\001\200\001\200\001\200\001\200\001\200\001\200\001\200\001\
    \200\001\000\000\204\001\204\001\204\001\204\001\204\001\204\001\
    \200\001\200\001\200\001\200\001\200\001\200\001\000\000\214\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \200\001\200\001\200\001\200\001\200\001\200\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \000\000\000\000\000\000\000\000\213\001\000\000\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\000\000\000\000\000\000\000\000\213\001\
    \000\000\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\211\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\003\000\000\000\134\000\255\255\
    \003\000\255\255\134\000\069\001\146\001\057\000\255\255\069\001\
    \146\001\255\255\255\255\255\255\255\255\125\000\138\000\255\255\
    \000\000\255\255\000\000\003\000\169\000\134\000\174\000\255\255\
    \000\000\010\001\069\001\146\001\012\001\000\000\010\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\005\000\115\000\000\000\125\000\129\000\005\000\
    \236\001\136\000\255\001\038\000\255\255\010\000\136\000\102\000\
    \058\000\000\000\107\000\102\000\255\255\011\000\000\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\029\000\038\000\000\000\111\000\208\000\233\000\255\000\
    \012\001\015\000\017\000\060\000\011\000\010\000\000\000\020\000\
    \024\000\031\000\032\000\034\000\022\000\026\000\000\000\014\000\
    \027\000\033\000\018\000\023\000\000\000\016\000\019\000\035\000\
    \040\000\036\000\038\000\000\000\041\000\042\000\043\000\044\000\
    \045\000\046\000\058\000\082\000\011\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\039\000\
    \063\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\085\000\140\000\060\000\013\000\143\000\
    \144\000\145\000\048\000\147\000\048\000\148\000\039\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\065\000\039\000\149\000\
    \150\000\156\000\063\000\157\000\051\000\158\000\051\000\159\000\
    \050\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\155\000\
    \050\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\161\000\162\000\155\000\091\000\065\000\
    \000\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\057\000\068\000\102\000\107\000\115\000\
    \131\000\133\000\133\000\125\000\138\000\133\000\163\000\094\000\
    \165\000\068\000\166\000\167\000\168\000\171\000\111\000\172\000\
    \173\000\206\000\203\000\207\000\210\000\211\000\058\000\082\000\
    \133\000\212\000\213\000\214\000\215\000\217\000\140\000\218\000\
    \097\000\219\000\220\000\119\000\221\000\222\000\223\000\133\000\
    \091\000\203\000\034\001\062\001\233\000\152\000\001\001\080\001\
    \247\000\060\000\251\000\054\001\058\001\081\001\068\000\041\001\
    \082\001\083\001\006\001\026\001\068\000\119\000\030\001\015\001\
    \068\000\094\000\015\001\084\001\085\001\086\001\071\001\088\001\
    \068\000\203\000\050\001\071\001\068\000\089\001\068\000\068\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\097\000\076\001\119\000\090\001\063\000\001\001\
    \092\001\071\000\071\000\071\000\071\000\071\000\071\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\152\000\076\001\093\001\095\001\097\001\098\001\045\001\
    \078\000\078\000\078\000\078\000\078\000\078\000\099\001\152\000\
    \100\001\071\000\071\000\071\000\071\000\071\000\071\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\020\001\076\001\065\000\020\001\101\001\102\001\104\001\
    \078\000\078\000\078\000\078\000\078\000\078\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \105\001\106\001\045\001\036\001\107\001\108\001\109\001\079\000\
    \079\000\079\000\079\000\079\000\079\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\110\001\
    \026\001\121\001\157\001\030\001\158\001\020\001\080\000\080\000\
    \080\000\080\000\080\000\080\000\091\000\159\001\062\001\079\000\
    \079\000\079\000\079\000\079\000\079\000\247\000\160\001\251\000\
    \161\001\058\001\068\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\094\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\036\001\041\001\097\000\162\001\
    \163\001\119\000\001\001\087\000\087\000\087\000\087\000\087\000\
    \087\000\165\001\054\001\152\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\006\001\166\001\167\001\168\001\015\001\169\001\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\050\001\087\000\087\000\087\000\087\000\087\000\
    \087\000\088\000\088\000\088\000\088\000\088\000\088\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\095\000\133\001\170\001\171\001\154\001\133\001\172\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\095\000\176\000\
    \173\001\088\000\088\000\088\000\088\000\088\000\088\000\045\001\
    \174\001\175\001\176\000\176\001\154\001\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\122\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\148\001\177\001\
    \020\001\178\001\098\000\148\001\179\001\180\001\181\001\182\001\
    \183\001\216\001\193\001\095\000\154\001\216\001\205\001\098\000\
    \222\001\095\000\205\001\229\001\001\002\095\000\218\001\036\001\
    \215\001\215\001\002\002\218\001\215\001\095\000\004\002\005\002\
    \216\001\095\000\006\002\095\000\095\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\215\001\
    \007\002\122\001\008\002\009\002\010\002\011\002\096\000\096\000\
    \096\000\096\000\096\000\096\000\098\000\012\002\215\001\247\001\
    \013\002\014\002\098\000\015\002\125\001\128\001\098\000\016\002\
    \220\001\017\002\251\001\018\002\019\002\020\002\098\000\121\001\
    \021\002\194\001\098\000\022\002\098\000\098\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\231\001\023\002\
    \238\001\024\002\251\001\238\001\025\002\099\000\099\000\099\000\
    \099\000\099\000\099\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\243\001\125\001\128\001\
    \224\001\026\002\197\001\027\002\100\000\100\000\100\000\100\000\
    \100\000\100\000\028\002\194\001\029\002\099\000\099\000\099\000\
    \099\000\099\000\099\000\030\002\031\002\032\002\200\001\231\001\
    \133\001\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\255\255\100\000\100\000\100\000\100\000\
    \100\000\100\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\255\255\255\255\197\001\176\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \185\000\255\255\101\000\101\000\101\000\101\000\101\000\101\000\
    \200\001\224\001\255\255\185\000\205\001\122\001\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\192\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\192\000\
    \193\001\247\001\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\198\000\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\199\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\199\000\125\001\128\001\199\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\204\000\194\001\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\231\001\255\255\255\255\199\000\220\001\238\001\
    \251\001\255\255\199\000\243\001\255\255\204\000\205\000\205\000\
    \205\000\205\000\205\000\205\000\205\000\205\000\205\000\205\000\
    \225\000\255\255\225\000\255\255\224\001\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\225\000\225\000\225\000\205\000\
    \197\001\255\255\255\255\255\255\255\255\204\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \255\255\255\255\255\255\255\255\200\001\255\255\255\255\228\000\
    \255\255\228\000\255\255\227\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\255\255\227\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\185\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\255\255\255\255\255\255\255\255\255\255\237\000\255\255\
    \077\001\255\255\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\113\001\113\001\113\001\113\001\
    \113\001\113\001\113\001\113\001\113\001\113\001\255\255\077\001\
    \255\255\255\255\192\000\255\255\255\255\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\077\001\
    \255\255\255\255\255\255\237\000\199\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\255\255\242\000\255\255\255\255\240\000\255\255\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\255\255\255\255\255\255\255\255\
    \242\000\255\255\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\237\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \255\255\255\255\255\255\255\255\245\000\255\255\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \066\001\066\001\255\255\255\255\066\001\079\001\079\001\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\066\001\
    \255\255\066\001\255\255\255\255\255\255\255\255\079\001\066\001\
    \255\255\255\255\255\255\255\255\066\001\255\255\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\255\255\255\255\066\001\255\255\255\255\255\255\255\255\
    \255\255\242\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \066\001\112\001\255\255\112\001\255\255\066\001\112\001\112\001\
    \112\001\112\001\112\001\112\001\112\001\112\001\112\001\112\001\
    \255\255\255\255\066\001\114\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\066\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\066\001\255\255\255\255\
    \114\001\255\255\255\255\066\001\255\255\255\255\115\001\255\255\
    \115\001\255\255\066\001\115\001\115\001\115\001\115\001\115\001\
    \115\001\115\001\115\001\115\001\115\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\255\255\
    \114\001\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\117\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\119\001\119\001\119\001\
    \119\001\119\001\119\001\119\001\119\001\119\001\119\001\255\255\
    \126\001\127\001\127\001\127\001\127\001\127\001\127\001\127\001\
    \127\001\127\001\127\001\255\255\255\255\126\001\255\255\255\255\
    \255\255\129\001\127\001\127\001\127\001\127\001\127\001\127\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\129\001\255\255\
    \255\255\155\001\255\255\155\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\255\255\255\255\255\255\
    \255\255\255\255\127\001\127\001\127\001\127\001\127\001\127\001\
    \155\001\255\255\126\001\255\255\255\255\255\255\255\255\255\255\
    \126\001\255\255\255\255\255\255\126\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\129\001\126\001\255\255\255\255\066\001\
    \126\001\129\001\126\001\126\001\255\255\129\001\255\255\255\255\
    \155\001\255\255\255\255\255\255\255\255\129\001\255\255\255\255\
    \255\255\129\001\255\255\129\001\129\001\130\001\130\001\130\001\
    \130\001\130\001\130\001\130\001\130\001\130\001\130\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\130\001\130\001\
    \130\001\130\001\130\001\130\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\131\001\131\001\131\001\
    \131\001\131\001\131\001\255\255\255\255\255\255\130\001\130\001\
    \130\001\130\001\130\001\130\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\255\255\131\001\131\001\131\001\
    \131\001\131\001\131\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\184\001\138\001\184\001\255\255\255\255\184\001\184\001\
    \184\001\184\001\184\001\184\001\184\001\184\001\184\001\184\001\
    \185\001\185\001\185\001\185\001\185\001\185\001\185\001\185\001\
    \185\001\185\001\255\255\132\001\132\001\132\001\132\001\132\001\
    \132\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\255\255\255\255\255\255\255\255\138\001\
    \255\255\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\255\255\255\255\
    \255\255\255\255\141\001\255\255\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\143\001\143\001\
    \255\255\255\255\143\001\156\001\156\001\156\001\156\001\156\001\
    \156\001\156\001\156\001\156\001\156\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\198\001\143\001\255\255\143\001\
    \255\255\255\255\255\255\255\255\156\001\143\001\255\255\255\255\
    \255\255\198\001\143\001\255\255\143\001\143\001\143\001\143\001\
    \143\001\143\001\143\001\143\001\143\001\143\001\143\001\255\255\
    \255\255\143\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \138\001\255\255\255\255\255\255\255\255\255\255\143\001\255\255\
    \255\255\255\255\255\255\143\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\198\001\255\255\
    \143\001\255\255\255\255\255\255\198\001\255\255\255\255\255\255\
    \198\001\186\001\255\255\143\001\255\255\255\255\255\255\255\255\
    \198\001\255\255\255\255\143\001\198\001\255\255\198\001\198\001\
    \255\255\143\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \143\001\255\255\255\255\255\255\255\255\187\001\255\255\187\001\
    \255\255\186\001\187\001\187\001\187\001\187\001\187\001\187\001\
    \187\001\187\001\187\001\187\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\189\001\189\001\
    \189\001\189\001\189\001\189\001\189\001\189\001\189\001\189\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\191\001\191\001\191\001\191\001\191\001\191\001\
    \191\001\191\001\191\001\191\001\199\001\199\001\199\001\199\001\
    \199\001\199\001\199\001\199\001\199\001\199\001\201\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\199\001\199\001\199\001\
    \199\001\199\001\199\001\201\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\199\001\199\001\199\001\
    \199\001\199\001\199\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \201\001\255\255\255\255\255\255\255\255\143\001\201\001\255\255\
    \255\255\255\255\201\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\201\001\255\255\255\255\255\255\201\001\255\255\
    \201\001\201\001\202\001\202\001\202\001\202\001\202\001\202\001\
    \202\001\202\001\202\001\202\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\202\001\202\001\202\001\202\001\202\001\
    \202\001\203\001\203\001\203\001\203\001\203\001\203\001\203\001\
    \203\001\203\001\203\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\203\001\203\001\203\001\203\001\203\001\203\001\
    \255\255\255\255\255\255\202\001\202\001\202\001\202\001\202\001\
    \202\001\255\255\255\255\255\255\255\255\255\255\255\255\204\001\
    \204\001\204\001\204\001\204\001\204\001\204\001\204\001\204\001\
    \204\001\255\255\203\001\203\001\203\001\203\001\203\001\203\001\
    \204\001\204\001\204\001\204\001\204\001\204\001\255\255\210\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \204\001\204\001\204\001\204\001\204\001\204\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \255\255\255\255\255\255\255\255\210\001\255\255\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\255\255\255\255\255\255\255\255\213\001\
    \255\255\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\210\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read_json v lexbuf =
   __ocaml_lex_read_json_rec v lexbuf 0
and __ocaml_lex_read_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 188 "lib/read.mll"
                
# 188 "lib/read.mll"
                ( `Bool true )

# 1032 "lib/read.ml"

  
# 1033 "lib/read.ml"
  | 1 ->

# 189 "lib/read.mll"
                
# 189 "lib/read.mll"
                ( `Bool false )

# 1037 "lib/read.ml"

  
# 1038 "lib/read.ml"
  | 2 ->

# 190 "lib/read.mll"
                
# 190 "lib/read.mll"
                ( `Null )

# 1042 "lib/read.ml"

  
# 1043 "lib/read.ml"
  | 3 ->

# 191 "lib/read.mll"
                
# 191 "lib/read.mll"
                (
                    
# 193 "lib/read.mll"
                    `Float nan
                
# 197 "lib/read.mll"
                )

# 1053 "lib/read.ml"

  
# 1054 "lib/read.ml"
  | 4 ->

# 198 "lib/read.mll"
                
# 198 "lib/read.mll"
                (
                    
# 200 "lib/read.mll"
                    `Float infinity
                
# 204 "lib/read.mll"
                )

# 1064 "lib/read.ml"

  
# 1065 "lib/read.ml"
  | 5 ->

# 205 "lib/read.mll"
                
# 205 "lib/read.mll"
                (
                    
# 207 "lib/read.mll"
                    `Float neg_infinity
                
# 211 "lib/read.mll"
                )

# 1075 "lib/read.ml"

  
# 1076 "lib/read.ml"
  | 6 ->

# 212 "lib/read.mll"
                
# 212 "lib/read.mll"
                (
                    
# 214 "lib/read.mll"
                    Buffer.clear v.buf;
                    `String (finish_string v lexbuf)
                
# 219 "lib/read.mll"
                )

# 1087 "lib/read.ml"

  
# 1088 "lib/read.ml"
  | 7 ->

# 220 "lib/read.mll"
                         
# 220 "lib/read.mll"
                         ( make_positive_int v lexbuf )

# 1092 "lib/read.ml"

  
# 1093 "lib/read.ml"
  | 8 ->

# 221 "lib/read.mll"
                         
# 221 "lib/read.mll"
                         ( make_negative_int v lexbuf )

# 1097 "lib/read.ml"

  
# 1098 "lib/read.ml"
  | 9 ->

# 222 "lib/read.mll"
                
# 222 "lib/read.mll"
                (
                    
# 224 "lib/read.mll"
                    `Float (float_of_string (lexeme lexbuf))
                 
# 228 "lib/read.mll"
                 )

# 1108 "lib/read.ml"

  
# 1109 "lib/read.ml"
  | 10 ->

# 230 "lib/read.mll"
                 
# 230 "lib/read.mll"
                 ( let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     let field_name = read_ident v lexbuf in
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     acc := (field_name, read_json v lexbuf) :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       let field_name = read_ident v lexbuf in
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       acc := (field_name, read_json v lexbuf) :: !acc;
                     done;
                     assert false
                   with End_of_object ->
                     `Assoc (List.rev !acc)
                 )

# 1135 "lib/read.ml"

  
# 1136 "lib/read.ml"
  | 11 ->

# 254 "lib/read.mll"
                 
# 254 "lib/read.mll"
                 ( let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     acc := read_json v lexbuf :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       acc := read_json v lexbuf :: !acc;
                     done;
                     assert false
                   with End_of_array ->
                     `List (List.rev !acc)
                 )

# 1154 "lib/read.ml"

  
# 1155 "lib/read.ml"
  | 12 ->

# 270 "lib/read.mll"
                 
# 270 "lib/read.mll"
                 (
                     
# 272 "lib/read.mll"
                     let acc = ref [] in
                     try
                       read_space v lexbuf;
                       read_tuple_end lexbuf;
                       acc := read_json v lexbuf :: !acc;
                       while true do
                         read_space v lexbuf;
                         read_tuple_sep v lexbuf;
                         read_space v lexbuf;
                         acc := read_json v lexbuf :: !acc;
                       done;
                       assert false
                     with End_of_tuple ->
                       `Tuple (List.rev !acc)
                 
# 289 "lib/read.mll"
                 )

# 1178 "lib/read.ml"

  
# 1179 "lib/read.ml"
  | 13 ->

# 291 "lib/read.mll"
                 
# 291 "lib/read.mll"
                 (
                     
# 293 "lib/read.mll"
                     read_space v lexbuf;
                     let cons = read_ident v lexbuf in
                     read_space v lexbuf;
                     `Variant (cons, finish_variant v lexbuf)
                 
# 300 "lib/read.mll"
                 )

# 1192 "lib/read.ml"

  
# 1193 "lib/read.ml"
  | 14 ->

# 302 "lib/read.mll"
                 
# 302 "lib/read.mll"
                 ( read_json v lexbuf )

# 1197 "lib/read.ml"

  
# 1198 "lib/read.ml"
  | 15 ->

# 303 "lib/read.mll"
                 
# 303 "lib/read.mll"
                 ( finish_comment v lexbuf; read_json v lexbuf )

# 1202 "lib/read.ml"

  
# 1203 "lib/read.ml"
  | 16 ->

# 304 "lib/read.mll"
                 
# 304 "lib/read.mll"
                 ( newline v lexbuf; read_json v lexbuf )

# 1207 "lib/read.ml"

  
# 1208 "lib/read.ml"
  | 17 ->

# 305 "lib/read.mll"
                 
# 305 "lib/read.mll"
                 ( read_json v lexbuf )

# 1212 "lib/read.ml"

  
# 1213 "lib/read.ml"
  | 18 ->

# 306 "lib/read.mll"
                 
# 306 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 1217 "lib/read.ml"

  
# 1218 "lib/read.ml"
  | 19 ->

# 307 "lib/read.mll"
                 
# 307 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 1222 "lib/read.ml"

  
# 1223 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_json_rec v lexbuf __ocaml_lex_state

and finish_string v lexbuf =
   __ocaml_lex_finish_string_rec v lexbuf 58
and __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 311 "lib/read.mll"
                  
# 311 "lib/read.mll"
                  ( Buffer.contents v.buf )

# 1234 "lib/read.ml"

  
# 1235 "lib/read.ml"
  | 1 ->

# 312 "lib/read.mll"
                  
# 312 "lib/read.mll"
                  ( finish_escaped_char v lexbuf;
                    finish_string v lexbuf )

# 1240 "lib/read.ml"

  
# 1241 "lib/read.ml"
  | 2 ->

# 314 "lib/read.mll"
                  
# 314 "lib/read.mll"
                  ( add_lexeme v.buf lexbuf;
                    finish_string v lexbuf )

# 1246 "lib/read.ml"

  
# 1247 "lib/read.ml"
  | 3 ->

# 316 "lib/read.mll"
                  
# 316 "lib/read.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )

# 1251 "lib/read.ml"

  
# 1252 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state

and map_string v f lexbuf =
   __ocaml_lex_map_string_rec v f lexbuf 63
and __ocaml_lex_map_string_rec v f lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 319 "lib/read.mll"
                  
# 319 "lib/read.mll"
                  ( let b = v.buf in
                    f (Buffer.contents b) 0 (Buffer.length b) )

# 1264 "lib/read.ml"

  
# 1265 "lib/read.ml"
  | 1 ->

# 321 "lib/read.mll"
                  
# 321 "lib/read.mll"
                  ( finish_escaped_char v lexbuf;
                    map_string v f lexbuf )

# 1270 "lib/read.ml"

  
# 1271 "lib/read.ml"
  | 2 ->

# 323 "lib/read.mll"
                  
# 323 "lib/read.mll"
                  ( add_lexeme v.buf lexbuf;
                    map_string v f lexbuf )

# 1276 "lib/read.ml"

  
# 1277 "lib/read.ml"
  | 3 ->

# 325 "lib/read.mll"
                  
# 325 "lib/read.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )

# 1281 "lib/read.ml"

  
# 1282 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_map_string_rec v f lexbuf __ocaml_lex_state

and finish_escaped_char v lexbuf =
   __ocaml_lex_finish_escaped_char_rec v lexbuf 68
and __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let

# 330 "lib/read.mll"
           
# 330 "lib/read.mll"
           c

# 1294 "lib/read.ml"
# 1294 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in

# 330 "lib/read.mll"
             
# 330 "lib/read.mll"
             ( Buffer.add_char v.buf c )

# 1298 "lib/read.ml"

  
# 1299 "lib/read.ml"
  | 1 ->

# 331 "lib/read.mll"
         
# 331 "lib/read.mll"
         ( Buffer.add_char v.buf '\b' )

# 1303 "lib/read.ml"

  
# 1304 "lib/read.ml"
  | 2 ->

# 332 "lib/read.mll"
         
# 332 "lib/read.mll"
         ( Buffer.add_char v.buf '\012' )

# 1308 "lib/read.ml"

  
# 1309 "lib/read.ml"
  | 3 ->

# 333 "lib/read.mll"
         
# 333 "lib/read.mll"
         ( Buffer.add_char v.buf '\n' )

# 1313 "lib/read.ml"

  
# 1314 "lib/read.ml"
  | 4 ->

# 334 "lib/read.mll"
         
# 334 "lib/read.mll"
         ( Buffer.add_char v.buf '\r' )

# 1318 "lib/read.ml"

  
# 1319 "lib/read.ml"
  | 5 ->

# 335 "lib/read.mll"
         
# 335 "lib/read.mll"
         ( Buffer.add_char v.buf '\t' )

# 1323 "lib/read.ml"

  
# 1324 "lib/read.ml"
  | 6 ->
let

# 336 "lib/read.mll"
                
# 336 "lib/read.mll"
                a

# 1329 "lib/read.ml"
# 1329 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and

# 336 "lib/read.mll"
                           
# 336 "lib/read.mll"
                           b

# 1334 "lib/read.ml"
# 1334 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and

# 336 "lib/read.mll"
                                      
# 336 "lib/read.mll"
                                      c

# 1339 "lib/read.ml"
# 1339 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and

# 336 "lib/read.mll"
                                                 
# 336 "lib/read.mll"
                                                 d

# 1344 "lib/read.ml"
# 1344 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4) in

# 337 "lib/read.mll"
         
# 337 "lib/read.mll"
         ( let x =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if x >= 0xD800 && x <= 0xDBFF then
             finish_surrogate_pair v x lexbuf
           else
             utf8_of_code v.buf x
         )

# 1355 "lib/read.ml"

  
# 1356 "lib/read.ml"
  | 7 ->

# 345 "lib/read.mll"
         
# 345 "lib/read.mll"
         ( long_error "Invalid escape sequence" v lexbuf )

# 1360 "lib/read.ml"

  
# 1361 "lib/read.ml"
  | 8 ->

# 346 "lib/read.mll"
         
# 346 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1365 "lib/read.ml"

  
# 1366 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state

and finish_surrogate_pair v x lexbuf =
   __ocaml_lex_finish_surrogate_pair_rec v x lexbuf 82
and __ocaml_lex_finish_surrogate_pair_rec v x lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let

# 349 "lib/read.mll"
                  
# 349 "lib/read.mll"
                  a

# 1378 "lib/read.ml"
# 1378 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and

# 349 "lib/read.mll"
                             
# 349 "lib/read.mll"
                             b

# 1383 "lib/read.ml"
# 1383 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and

# 349 "lib/read.mll"
                                        
# 349 "lib/read.mll"
                                        c

# 1388 "lib/read.ml"
# 1388 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4)
and

# 349 "lib/read.mll"
                                                   
# 349 "lib/read.mll"
                                                   d

# 1393 "lib/read.ml"
# 1393 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 5) in

# 350 "lib/read.mll"
         
# 350 "lib/read.mll"
         ( let y =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if y >= 0xDC00 && y <= 0xDFFF then
             utf8_of_surrogate_pair v.buf x y
           else
             long_error "Invalid low surrogate for code point beyond U+FFFF"
               v lexbuf
         )

# 1405 "lib/read.ml"

  
# 1406 "lib/read.ml"
  | 1 ->

# 359 "lib/read.mll"
         
# 359 "lib/read.mll"
         ( long_error "Missing escape sequence representing low surrogate \
                       for code point beyond U+FFFF" v lexbuf )

# 1411 "lib/read.ml"

  
# 1412 "lib/read.ml"
  | 2 ->

# 361 "lib/read.mll"
         
# 361 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1416 "lib/read.ml"

  
# 1417 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_surrogate_pair_rec v x lexbuf __ocaml_lex_state

and finish_stringlit v lexbuf =
   __ocaml_lex_finish_stringlit_rec v lexbuf 91
and __ocaml_lex_finish_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 366 "lib/read.mll"
         
# 366 "lib/read.mll"
         ( let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
           let s = Bytes.create (len+1) in
           Bytes.set s 0 '"';
           Bytes.blit lexbuf.lex_buffer lexbuf.lex_start_pos s 1 len;
           Bytes.to_string s
         )

# 1433 "lib/read.ml"

  
# 1434 "lib/read.ml"
  | 1 ->

# 372 "lib/read.mll"
         
# 372 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 1438 "lib/read.ml"

  
# 1439 "lib/read.ml"
  | 2 ->

# 373 "lib/read.mll"
         
# 373 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1443 "lib/read.ml"

  
# 1444 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_stringlit_rec v lexbuf __ocaml_lex_state

and finish_variant v lexbuf =
   __ocaml_lex_finish_variant_rec v lexbuf 102
and __ocaml_lex_finish_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 376 "lib/read.mll"
         
# 376 "lib/read.mll"
         ( let x = read_json v lexbuf in
           read_space v lexbuf;
           read_gt v lexbuf;
           Some x )

# 1458 "lib/read.ml"

  
# 1459 "lib/read.ml"
  | 1 ->

# 380 "lib/read.mll"
         
# 380 "lib/read.mll"
         ( None )

# 1463 "lib/read.ml"

  
# 1464 "lib/read.ml"
  | 2 ->

# 381 "lib/read.mll"
         
# 381 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 1468 "lib/read.ml"

  
# 1469 "lib/read.ml"
  | 3 ->

# 382 "lib/read.mll"
         
# 382 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1473 "lib/read.ml"

  
# 1474 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_variant_rec v lexbuf __ocaml_lex_state

and read_lt v lexbuf =
   __ocaml_lex_read_lt_rec v lexbuf 107
and __ocaml_lex_read_lt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 385 "lib/read.mll"
             
# 385 "lib/read.mll"
             ( () )

# 1485 "lib/read.ml"

  
# 1486 "lib/read.ml"
  | 1 ->

# 386 "lib/read.mll"
             
# 386 "lib/read.mll"
             ( long_error "Expected '<' but found" v lexbuf )

# 1490 "lib/read.ml"

  
# 1491 "lib/read.ml"
  | 2 ->

# 387 "lib/read.mll"
             
# 387 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1495 "lib/read.ml"

  
# 1496 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lt_rec v lexbuf __ocaml_lex_state

and read_gt v lexbuf =
   __ocaml_lex_read_gt_rec v lexbuf 111
and __ocaml_lex_read_gt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 390 "lib/read.mll"
         
# 390 "lib/read.mll"
         ( () )

# 1507 "lib/read.ml"

  
# 1508 "lib/read.ml"
  | 1 ->

# 391 "lib/read.mll"
         
# 391 "lib/read.mll"
         ( long_error "Expected '>' but found" v lexbuf )

# 1512 "lib/read.ml"

  
# 1513 "lib/read.ml"
  | 2 ->

# 392 "lib/read.mll"
         
# 392 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1517 "lib/read.ml"

  
# 1518 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_gt_rec v lexbuf __ocaml_lex_state

and read_comma v lexbuf =
   __ocaml_lex_read_comma_rec v lexbuf 115
and __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 395 "lib/read.mll"
         
# 395 "lib/read.mll"
         ( () )

# 1529 "lib/read.ml"

  
# 1530 "lib/read.ml"
  | 1 ->

# 396 "lib/read.mll"
         
# 396 "lib/read.mll"
         ( long_error "Expected ',' but found" v lexbuf )

# 1534 "lib/read.ml"

  
# 1535 "lib/read.ml"
  | 2 ->

# 397 "lib/read.mll"
         
# 397 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1539 "lib/read.ml"

  
# 1540 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state

and start_any_variant v lexbuf =
   __ocaml_lex_start_any_variant_rec v lexbuf 119
and __ocaml_lex_start_any_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 400 "lib/read.mll"
             
# 400 "lib/read.mll"
             ( `Edgy_bracket )

# 1551 "lib/read.ml"

  
# 1552 "lib/read.ml"
  | 1 ->

# 401 "lib/read.mll"
             
# 401 "lib/read.mll"
             ( Buffer.clear v.buf;
               `Double_quote )

# 1557 "lib/read.ml"

  
# 1558 "lib/read.ml"
  | 2 ->

# 403 "lib/read.mll"
             
# 403 "lib/read.mll"
             ( `Square_bracket )

# 1562 "lib/read.ml"

  
# 1563 "lib/read.ml"
  | 3 ->

# 404 "lib/read.mll"
             
# 404 "lib/read.mll"
             ( long_error "Expected '<', '\"' or '[' but found" v lexbuf )

# 1567 "lib/read.ml"

  
# 1568 "lib/read.ml"
  | 4 ->

# 405 "lib/read.mll"
             
# 405 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1572 "lib/read.ml"

  
# 1573 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_start_any_variant_rec v lexbuf __ocaml_lex_state

and finish_comment v lexbuf =
   __ocaml_lex_finish_comment_rec v lexbuf 125
and __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 408 "lib/read.mll"
         
# 408 "lib/read.mll"
         ( () )

# 1584 "lib/read.ml"

  
# 1585 "lib/read.ml"
  | 1 ->

# 409 "lib/read.mll"
         
# 409 "lib/read.mll"
         ( long_error "Unterminated comment" v lexbuf )

# 1589 "lib/read.ml"

  
# 1590 "lib/read.ml"
  | 2 ->

# 410 "lib/read.mll"
         
# 410 "lib/read.mll"
         ( newline v lexbuf; finish_comment v lexbuf )

# 1594 "lib/read.ml"

  
# 1595 "lib/read.ml"
  | 3 ->

# 411 "lib/read.mll"
         
# 411 "lib/read.mll"
         ( finish_comment v lexbuf )

# 1599 "lib/read.ml"

  
# 1600 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state

and read_eof lexbuf =
   __ocaml_lex_read_eof_rec lexbuf 131
and __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 419 "lib/read.mll"
              
# 419 "lib/read.mll"
              ( true )

# 1611 "lib/read.ml"

  
# 1612 "lib/read.ml"
  | 1 ->

# 420 "lib/read.mll"
              
# 420 "lib/read.mll"
              ( false )

# 1616 "lib/read.ml"

  
# 1617 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state

and read_space v lexbuf =
   __ocaml_lex_read_space_rec v lexbuf 133
and __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 423 "lib/read.mll"
                             
# 423 "lib/read.mll"
                             ( newline v lexbuf; read_space v lexbuf )

# 1628 "lib/read.ml"

  
# 1629 "lib/read.ml"
  | 1 ->

# 424 "lib/read.mll"
                             
# 424 "lib/read.mll"
                             ( finish_comment v lexbuf; read_space v lexbuf )

# 1633 "lib/read.ml"

  
# 1634 "lib/read.ml"
  | 2 ->

# 425 "lib/read.mll"
                             
# 425 "lib/read.mll"
                             ( newline v lexbuf; read_space v lexbuf )

# 1638 "lib/read.ml"

  
# 1639 "lib/read.ml"
  | 3 ->

# 426 "lib/read.mll"
                             
# 426 "lib/read.mll"
                             ( read_space v lexbuf )

# 1643 "lib/read.ml"

  
# 1644 "lib/read.ml"
  | 4 ->

# 427 "lib/read.mll"
                             
# 427 "lib/read.mll"
                             ( () )

# 1648 "lib/read.ml"

  
# 1649 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state

and read_null v lexbuf =
   __ocaml_lex_read_null_rec v lexbuf 140
and __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 430 "lib/read.mll"
              
# 430 "lib/read.mll"
              ( () )

# 1660 "lib/read.ml"

  
# 1661 "lib/read.ml"
  | 1 ->

# 431 "lib/read.mll"
              
# 431 "lib/read.mll"
              ( long_error "Expected 'null' but found" v lexbuf )

# 1665 "lib/read.ml"

  
# 1666 "lib/read.ml"
  | 2 ->

# 432 "lib/read.mll"
              
# 432 "lib/read.mll"
              ( custom_error "Unexpected end of input" v lexbuf )

# 1670 "lib/read.ml"

  
# 1671 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state

and read_null_if_possible v lexbuf =
   __ocaml_lex_read_null_if_possible_rec v lexbuf 147
and __ocaml_lex_read_null_if_possible_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 435 "lib/read.mll"
              
# 435 "lib/read.mll"
              ( true )

# 1682 "lib/read.ml"

  
# 1683 "lib/read.ml"
  | 1 ->

# 436 "lib/read.mll"
              
# 436 "lib/read.mll"
              ( false )

# 1687 "lib/read.ml"

  
# 1688 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_null_if_possible_rec v lexbuf __ocaml_lex_state

and read_bool v lexbuf =
   __ocaml_lex_read_bool_rec v lexbuf 152
and __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 439 "lib/read.mll"
                
# 439 "lib/read.mll"
                ( true )

# 1699 "lib/read.ml"

  
# 1700 "lib/read.ml"
  | 1 ->

# 440 "lib/read.mll"
                
# 440 "lib/read.mll"
                ( false )

# 1704 "lib/read.ml"

  
# 1705 "lib/read.ml"
  | 2 ->

# 443 "lib/read.mll"
                
# 443 "lib/read.mll"
                ( true )

# 1709 "lib/read.ml"

  
# 1710 "lib/read.ml"
  | 3 ->

# 444 "lib/read.mll"
                
# 444 "lib/read.mll"
                ( false )

# 1714 "lib/read.ml"

  
# 1715 "lib/read.ml"
  | 4 ->

# 446 "lib/read.mll"
                
# 446 "lib/read.mll"
                ( long_error "Expected 'true' or 'false' but found" v lexbuf )

# 1719 "lib/read.ml"

  
# 1720 "lib/read.ml"
  | 5 ->

# 447 "lib/read.mll"
                
# 447 "lib/read.mll"
                ( custom_error "Unexpected end of input" v lexbuf )

# 1724 "lib/read.ml"

  
# 1725 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state

and read_int v lexbuf =
   __ocaml_lex_read_int_rec v lexbuf 176
and __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 450 "lib/read.mll"
                         
# 450 "lib/read.mll"
                         ( try extract_positive_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf )

# 1738 "lib/read.ml"

  
# 1739 "lib/read.ml"
  | 1 ->

# 453 "lib/read.mll"
                         
# 453 "lib/read.mll"
                         ( try extract_negative_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf )

# 1745 "lib/read.ml"

  
# 1746 "lib/read.ml"
  | 2 ->

# 456 "lib/read.mll"
                         
# 456 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             int_of_string s
                           with _ ->
                             custom_error
                               "Expected an integer but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1763 "lib/read.ml"

  
# 1764 "lib/read.ml"
  | 3 ->

# 470 "lib/read.mll"
                         
# 470 "lib/read.mll"
                         ( long_error "Expected integer but found" v lexbuf )

# 1768 "lib/read.ml"

  
# 1769 "lib/read.ml"
  | 4 ->

# 471 "lib/read.mll"
                         
# 471 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1773 "lib/read.ml"

  
# 1774 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state

and read_int32 v lexbuf =
   __ocaml_lex_read_int32_rec v lexbuf 185
and __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 474 "lib/read.mll"
                         
# 474 "lib/read.mll"
                         ( try Int32.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf )

# 1787 "lib/read.ml"

  
# 1788 "lib/read.ml"
  | 1 ->

# 477 "lib/read.mll"
                         
# 477 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int32.of_string s
                           with _ ->
                             custom_error
                               "Expected an int32 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1805 "lib/read.ml"

  
# 1806 "lib/read.ml"
  | 2 ->

# 491 "lib/read.mll"
                         
# 491 "lib/read.mll"
                         ( long_error "Expected int32 but found" v lexbuf )

# 1810 "lib/read.ml"

  
# 1811 "lib/read.ml"
  | 3 ->

# 492 "lib/read.mll"
                         
# 492 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1815 "lib/read.ml"

  
# 1816 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state

and read_int64 v lexbuf =
   __ocaml_lex_read_int64_rec v lexbuf 192
and __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 495 "lib/read.mll"
                         
# 495 "lib/read.mll"
                         ( try Int64.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf )

# 1829 "lib/read.ml"

  
# 1830 "lib/read.ml"
  | 1 ->

# 498 "lib/read.mll"
                         
# 498 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int64.of_string s
                           with _ ->
                             custom_error
                               "Expected an int64 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1847 "lib/read.ml"

  
# 1848 "lib/read.ml"
  | 2 ->

# 512 "lib/read.mll"
                         
# 512 "lib/read.mll"
                         ( long_error "Expected int64 but found" v lexbuf )

# 1852 "lib/read.ml"

  
# 1853 "lib/read.ml"
  | 3 ->

# 513 "lib/read.mll"
                         
# 513 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1857 "lib/read.ml"

  
# 1858 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state

and read_number v lexbuf =
   __ocaml_lex_read_number_rec v lexbuf 199
and __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 516 "lib/read.mll"
                
# 516 "lib/read.mll"
                ( nan )

# 1869 "lib/read.ml"

  
# 1870 "lib/read.ml"
  | 1 ->

# 517 "lib/read.mll"
                
# 517 "lib/read.mll"
                ( infinity )

# 1874 "lib/read.ml"

  
# 1875 "lib/read.ml"
  | 2 ->

# 518 "lib/read.mll"
                
# 518 "lib/read.mll"
                ( neg_infinity )

# 1879 "lib/read.ml"

  
# 1880 "lib/read.ml"
  | 3 ->

# 519 "lib/read.mll"
                
# 519 "lib/read.mll"
                ( float_of_string (lexeme lexbuf) )

# 1884 "lib/read.ml"

  
# 1885 "lib/read.ml"
  | 4 ->

# 520 "lib/read.mll"
                
# 520 "lib/read.mll"
                ( Buffer.clear v.buf;
                  let s = finish_string v lexbuf in
                  try
                    (* Any OCaml-compliant float will pass,
                       including hexadecimal and octal notations,
                       and embedded underscores. *)
                    float_of_string s
                  with _ ->
                    match s with
                        "NaN" -> nan
                      | "Infinity" -> infinity
                      | "-Infinity" -> neg_infinity
                      | _ ->
                          custom_error
                            "Expected a number but found a string that \
                             doesn't even represent a number"
                            v lexbuf
                )

# 1906 "lib/read.ml"

  
# 1907 "lib/read.ml"
  | 5 ->

# 538 "lib/read.mll"
                
# 538 "lib/read.mll"
                ( long_error "Expected number but found" v lexbuf )

# 1911 "lib/read.ml"

  
# 1912 "lib/read.ml"
  | 6 ->

# 539 "lib/read.mll"
                
# 539 "lib/read.mll"
                ( custom_error "Unexpected end of input" v lexbuf )

# 1916 "lib/read.ml"

  
# 1917 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state

and read_string v lexbuf =
   __ocaml_lex_read_string_rec v lexbuf 233
and __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 542 "lib/read.mll"
             
# 542 "lib/read.mll"
             ( Buffer.clear v.buf;
               finish_string v lexbuf )

# 1929 "lib/read.ml"

  
# 1930 "lib/read.ml"
  | 1 ->

# 544 "lib/read.mll"
             
# 544 "lib/read.mll"
             ( long_error "Expected '\"' but found" v lexbuf )

# 1934 "lib/read.ml"

  
# 1935 "lib/read.ml"
  | 2 ->

# 545 "lib/read.mll"
             
# 545 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1939 "lib/read.ml"

  
# 1940 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state

and read_ident v lexbuf =
   __ocaml_lex_read_ident_rec v lexbuf 237
and __ocaml_lex_read_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 548 "lib/read.mll"
             
# 548 "lib/read.mll"
             ( Buffer.clear v.buf;
               finish_string v lexbuf )

# 1952 "lib/read.ml"

  
# 1953 "lib/read.ml"
  | 1 ->
let

# 550 "lib/read.mll"
             
# 550 "lib/read.mll"
             s

# 1958 "lib/read.ml"
# 1958 "lib/read.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in

# 551 "lib/read.mll"
             
# 551 "lib/read.mll"
             ( s )

# 1962 "lib/read.ml"

  
# 1963 "lib/read.ml"
  | 2 ->

# 552 "lib/read.mll"
             
# 552 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 1967 "lib/read.ml"

  
# 1968 "lib/read.ml"
  | 3 ->

# 553 "lib/read.mll"
             
# 553 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1972 "lib/read.ml"

  
# 1973 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_ident_rec v lexbuf __ocaml_lex_state

and map_ident v f lexbuf =
   __ocaml_lex_map_ident_rec v f lexbuf 242
and __ocaml_lex_map_ident_rec v f lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 556 "lib/read.mll"
             
# 556 "lib/read.mll"
             ( Buffer.clear v.buf;
               map_string v f lexbuf )

# 1985 "lib/read.ml"

  
# 1986 "lib/read.ml"
  | 1 ->

# 559 "lib/read.mll"
             
# 559 "lib/read.mll"
             ( map_lexeme f lexbuf )

# 1990 "lib/read.ml"

  
# 1991 "lib/read.ml"
  | 2 ->

# 560 "lib/read.mll"
             
# 560 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 1995 "lib/read.ml"

  
# 1996 "lib/read.ml"
  | 3 ->

# 561 "lib/read.mll"
             
# 561 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2000 "lib/read.ml"

  
# 2001 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_map_ident_rec v f lexbuf __ocaml_lex_state

and read_sequence read_cell init_acc v lexbuf =
   __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf 247
and __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 564 "lib/read.mll"
             
# 564 "lib/read.mll"
             ( let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell !acc v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell !acc v lexbuf;
                 done;
                 assert false
               with End_of_array ->
                 !acc
             )

# 2026 "lib/read.ml"

  
# 2027 "lib/read.ml"
  | 1 ->

# 579 "lib/read.mll"
             
# 579 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2031 "lib/read.ml"

  
# 2032 "lib/read.ml"
  | 2 ->

# 580 "lib/read.mll"
             
# 580 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2036 "lib/read.ml"

  
# 2037 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf __ocaml_lex_state

and read_list_rev read_cell v lexbuf =
   __ocaml_lex_read_list_rev_rec read_cell v lexbuf 251
and __ocaml_lex_read_list_rev_rec read_cell v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 583 "lib/read.mll"
             
# 583 "lib/read.mll"
             ( let acc = ref [] in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell v lexbuf :: !acc;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell v lexbuf :: !acc;
                 done;
                 assert false
               with End_of_array ->
                 !acc
             )

# 2062 "lib/read.ml"

  
# 2063 "lib/read.ml"
  | 1 ->

# 598 "lib/read.mll"
             
# 598 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2067 "lib/read.ml"

  
# 2068 "lib/read.ml"
  | 2 ->

# 599 "lib/read.mll"
             
# 599 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2072 "lib/read.ml"

  
# 2073 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_list_rev_rec read_cell v lexbuf __ocaml_lex_state

and read_array_end lexbuf =
   __ocaml_lex_read_array_end_rec lexbuf 255
and __ocaml_lex_read_array_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 602 "lib/read.mll"
             
# 602 "lib/read.mll"
             ( raise End_of_array )

# 2084 "lib/read.ml"

  
# 2085 "lib/read.ml"
  | 1 ->

# 603 "lib/read.mll"
             
# 603 "lib/read.mll"
             ( () )

# 2089 "lib/read.ml"

  
# 2090 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_array_end_rec lexbuf __ocaml_lex_state

and read_array_sep v lexbuf =
   __ocaml_lex_read_array_sep_rec v lexbuf 257
and __ocaml_lex_read_array_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 606 "lib/read.mll"
             
# 606 "lib/read.mll"
             ( () )

# 2101 "lib/read.ml"

  
# 2102 "lib/read.ml"
  | 1 ->

# 607 "lib/read.mll"
             
# 607 "lib/read.mll"
             ( raise End_of_array )

# 2106 "lib/read.ml"

  
# 2107 "lib/read.ml"
  | 2 ->

# 608 "lib/read.mll"
             
# 608 "lib/read.mll"
             ( long_error "Expected ',' or ']' but found" v lexbuf )

# 2111 "lib/read.ml"

  
# 2112 "lib/read.ml"
  | 3 ->

# 609 "lib/read.mll"
             
# 609 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2116 "lib/read.ml"

  
# 2117 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_array_sep_rec v lexbuf __ocaml_lex_state

and read_tuple read_cell init_acc v lexbuf =
   __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf 262
and __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 613 "lib/read.mll"
                 
# 613 "lib/read.mll"
                 (
                     
# 615 "lib/read.mll"
                     let pos = ref 0 in
                     let acc = ref init_acc in
                     try
                       read_space v lexbuf;
                       read_tuple_end lexbuf;
                       acc := read_cell !pos !acc v lexbuf;
                       incr pos;
                       while true do
                         read_space v lexbuf;
                         read_tuple_sep v lexbuf;
                         read_space v lexbuf;
                         acc := read_cell !pos !acc v lexbuf;
                         incr pos;
                       done;
                       assert false
                     with End_of_tuple ->
                       !acc
                 
# 635 "lib/read.mll"
                 )

# 2150 "lib/read.ml"

  
# 2151 "lib/read.ml"
  | 1 ->

# 636 "lib/read.mll"
             
# 636 "lib/read.mll"
             ( long_error "Expected ')' but found" v lexbuf )

# 2155 "lib/read.ml"

  
# 2156 "lib/read.ml"
  | 2 ->

# 637 "lib/read.mll"
             
# 637 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2160 "lib/read.ml"

  
# 2161 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf __ocaml_lex_state

and read_tuple_end lexbuf =
   __ocaml_lex_read_tuple_end_rec lexbuf 266
and __ocaml_lex_read_tuple_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 640 "lib/read.mll"
             
# 640 "lib/read.mll"
             ( raise End_of_tuple )

# 2172 "lib/read.ml"

  
# 2173 "lib/read.ml"
  | 1 ->

# 641 "lib/read.mll"
             
# 641 "lib/read.mll"
             ( () )

# 2177 "lib/read.ml"

  
# 2178 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_end_rec lexbuf __ocaml_lex_state

and read_tuple_end2 v std lexbuf =
   __ocaml_lex_read_tuple_end2_rec v std lexbuf 268
and __ocaml_lex_read_tuple_end2_rec v std lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 644 "lib/read.mll"
             
# 644 "lib/read.mll"
             ( if std then
                 long_error "Expected ')' or '' but found" v lexbuf
               else
                 raise End_of_tuple )

# 2192 "lib/read.ml"

  
# 2193 "lib/read.ml"
  | 1 ->

# 648 "lib/read.mll"
             
# 648 "lib/read.mll"
             ( if std then
                 raise End_of_tuple
               else
                 long_error "Expected ']' or '' but found" v lexbuf )

# 2200 "lib/read.ml"

  
# 2201 "lib/read.ml"
  | 2 ->

# 652 "lib/read.mll"
             
# 652 "lib/read.mll"
             ( () )

# 2205 "lib/read.ml"

  
# 2206 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_end2_rec v std lexbuf __ocaml_lex_state

and read_tuple_sep v lexbuf =
   __ocaml_lex_read_tuple_sep_rec v lexbuf 271
and __ocaml_lex_read_tuple_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 655 "lib/read.mll"
             
# 655 "lib/read.mll"
             ( () )

# 2217 "lib/read.ml"

  
# 2218 "lib/read.ml"
  | 1 ->

# 656 "lib/read.mll"
             
# 656 "lib/read.mll"
             ( raise End_of_tuple )

# 2222 "lib/read.ml"

  
# 2223 "lib/read.ml"
  | 2 ->

# 657 "lib/read.mll"
             
# 657 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 2227 "lib/read.ml"

  
# 2228 "lib/read.ml"
  | 3 ->

# 658 "lib/read.mll"
             
# 658 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2232 "lib/read.ml"

  
# 2233 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_sep_rec v lexbuf __ocaml_lex_state

and read_tuple_sep2 v std lexbuf =
   __ocaml_lex_read_tuple_sep2_rec v std lexbuf 276
and __ocaml_lex_read_tuple_sep2_rec v std lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 661 "lib/read.mll"
             
# 661 "lib/read.mll"
             ( () )

# 2244 "lib/read.ml"

  
# 2245 "lib/read.ml"
  | 1 ->

# 662 "lib/read.mll"
             
# 662 "lib/read.mll"
             ( if std then
                 long_error "Expected ',' or ']' but found" v lexbuf
               else
                 raise End_of_tuple )

# 2252 "lib/read.ml"

  
# 2253 "lib/read.ml"
  | 2 ->

# 666 "lib/read.mll"
             
# 666 "lib/read.mll"
             ( if std then
                 raise End_of_tuple
               else
                 long_error "Expected ',' or ')' but found" v lexbuf )

# 2260 "lib/read.ml"

  
# 2261 "lib/read.ml"
  | 3 ->

# 670 "lib/read.mll"
             
# 670 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 2265 "lib/read.ml"

  
# 2266 "lib/read.ml"
  | 4 ->

# 671 "lib/read.mll"
             
# 671 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2270 "lib/read.ml"

  
# 2271 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_sep2_rec v std lexbuf __ocaml_lex_state

and read_abstract_fields read_key read_field init_acc v lexbuf =
   __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf 282
and __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 675 "lib/read.mll"
             
# 675 "lib/read.mll"
             ( let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_object_end lexbuf;
                 let field_name = read_key v lexbuf in
                 read_space v lexbuf;
                 read_colon v lexbuf;
                 read_space v lexbuf;
                 acc := read_field !acc field_name v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_object_sep v lexbuf;
                   read_space v lexbuf;
                   let field_name = read_key v lexbuf in
                   read_space v lexbuf;
                   read_colon v lexbuf;
                   read_space v lexbuf;
                   acc := read_field !acc field_name v lexbuf;
                 done;
                 assert false
               with End_of_object ->
                 !acc
             )

# 2304 "lib/read.ml"

  
# 2305 "lib/read.ml"
  | 1 ->

# 698 "lib/read.mll"
             
# 698 "lib/read.mll"
             ( long_error "Expected '{' but found" v lexbuf )

# 2309 "lib/read.ml"

  
# 2310 "lib/read.ml"
  | 2 ->

# 699 "lib/read.mll"
             
# 699 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2314 "lib/read.ml"

  
# 2315 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf __ocaml_lex_state

and read_lcurl v lexbuf =
   __ocaml_lex_read_lcurl_rec v lexbuf 286
and __ocaml_lex_read_lcurl_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 702 "lib/read.mll"
             
# 702 "lib/read.mll"
             ( () )

# 2326 "lib/read.ml"

  
# 2327 "lib/read.ml"
  | 1 ->

# 703 "lib/read.mll"
             
# 703 "lib/read.mll"
             ( long_error "Expected '{' but found" v lexbuf )

# 2331 "lib/read.ml"

  
# 2332 "lib/read.ml"
  | 2 ->

# 704 "lib/read.mll"
             
# 704 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2336 "lib/read.ml"

  
# 2337 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lcurl_rec v lexbuf __ocaml_lex_state

and read_object_end lexbuf =
   __ocaml_lex_read_object_end_rec lexbuf 290
and __ocaml_lex_read_object_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 707 "lib/read.mll"
             
# 707 "lib/read.mll"
             ( raise End_of_object )

# 2348 "lib/read.ml"

  
# 2349 "lib/read.ml"
  | 1 ->

# 708 "lib/read.mll"
             
# 708 "lib/read.mll"
             ( () )

# 2353 "lib/read.ml"

  
# 2354 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_object_end_rec lexbuf __ocaml_lex_state

and read_object_sep v lexbuf =
   __ocaml_lex_read_object_sep_rec v lexbuf 292
and __ocaml_lex_read_object_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 711 "lib/read.mll"
             
# 711 "lib/read.mll"
             ( () )

# 2365 "lib/read.ml"

  
# 2366 "lib/read.ml"
  | 1 ->

# 712 "lib/read.mll"
             
# 712 "lib/read.mll"
             ( raise End_of_object )

# 2370 "lib/read.ml"

  
# 2371 "lib/read.ml"
  | 2 ->

# 713 "lib/read.mll"
             
# 713 "lib/read.mll"
             ( long_error "Expected ',' or '}' but found" v lexbuf )

# 2375 "lib/read.ml"

  
# 2376 "lib/read.ml"
  | 3 ->

# 714 "lib/read.mll"
             
# 714 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2380 "lib/read.ml"

  
# 2381 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_object_sep_rec v lexbuf __ocaml_lex_state

and read_colon v lexbuf =
   __ocaml_lex_read_colon_rec v lexbuf 297
and __ocaml_lex_read_colon_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 717 "lib/read.mll"
             
# 717 "lib/read.mll"
             ( () )

# 2392 "lib/read.ml"

  
# 2393 "lib/read.ml"
  | 1 ->

# 718 "lib/read.mll"
             
# 718 "lib/read.mll"
             ( long_error "Expected ':' but found" v lexbuf )

# 2397 "lib/read.ml"

  
# 2398 "lib/read.ml"
  | 2 ->

# 719 "lib/read.mll"
             
# 719 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2402 "lib/read.ml"

  
# 2403 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_colon_rec v lexbuf __ocaml_lex_state

and start_any_tuple v lexbuf =
   __ocaml_lex_start_any_tuple_rec v lexbuf 301
and __ocaml_lex_start_any_tuple_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 722 "lib/read.mll"
             
# 722 "lib/read.mll"
             ( false )

# 2414 "lib/read.ml"

  
# 2415 "lib/read.ml"
  | 1 ->

# 723 "lib/read.mll"
             
# 723 "lib/read.mll"
             ( true )

# 2419 "lib/read.ml"

  
# 2420 "lib/read.ml"
  | 2 ->

# 724 "lib/read.mll"
             
# 724 "lib/read.mll"
             ( long_error "Expected '(' or '[' but found" v lexbuf )

# 2424 "lib/read.ml"

  
# 2425 "lib/read.ml"
  | 3 ->

# 725 "lib/read.mll"
             
# 725 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2429 "lib/read.ml"

  
# 2430 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_start_any_tuple_rec v lexbuf __ocaml_lex_state

and read_lpar v lexbuf =
   __ocaml_lex_read_lpar_rec v lexbuf 306
and __ocaml_lex_read_lpar_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 728 "lib/read.mll"
             
# 728 "lib/read.mll"
             ( () )

# 2441 "lib/read.ml"

  
# 2442 "lib/read.ml"
  | 1 ->

# 729 "lib/read.mll"
             
# 729 "lib/read.mll"
             ( long_error "Expected '(' but found" v lexbuf )

# 2446 "lib/read.ml"

  
# 2447 "lib/read.ml"
  | 2 ->

# 730 "lib/read.mll"
             
# 730 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2451 "lib/read.ml"

  
# 2452 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lpar_rec v lexbuf __ocaml_lex_state

and read_rpar v lexbuf =
   __ocaml_lex_read_rpar_rec v lexbuf 310
and __ocaml_lex_read_rpar_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 733 "lib/read.mll"
             
# 733 "lib/read.mll"
             ( () )

# 2463 "lib/read.ml"

  
# 2464 "lib/read.ml"
  | 1 ->

# 734 "lib/read.mll"
             
# 734 "lib/read.mll"
             ( long_error "Expected ')' but found" v lexbuf )

# 2468 "lib/read.ml"

  
# 2469 "lib/read.ml"
  | 2 ->

# 735 "lib/read.mll"
             
# 735 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2473 "lib/read.ml"

  
# 2474 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rpar_rec v lexbuf __ocaml_lex_state

and read_lbr v lexbuf =
   __ocaml_lex_read_lbr_rec v lexbuf 314
and __ocaml_lex_read_lbr_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 738 "lib/read.mll"
             
# 738 "lib/read.mll"
             ( () )

# 2485 "lib/read.ml"

  
# 2486 "lib/read.ml"
  | 1 ->

# 739 "lib/read.mll"
             
# 739 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2490 "lib/read.ml"

  
# 2491 "lib/read.ml"
  | 2 ->

# 740 "lib/read.mll"
             
# 740 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2495 "lib/read.ml"

  
# 2496 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lbr_rec v lexbuf __ocaml_lex_state

and read_rbr v lexbuf =
   __ocaml_lex_read_rbr_rec v lexbuf 318
and __ocaml_lex_read_rbr_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 743 "lib/read.mll"
             
# 743 "lib/read.mll"
             ( () )

# 2507 "lib/read.ml"

  
# 2508 "lib/read.ml"
  | 1 ->

# 744 "lib/read.mll"
             
# 744 "lib/read.mll"
             ( long_error "Expected ']' but found" v lexbuf )

# 2512 "lib/read.ml"

  
# 2513 "lib/read.ml"
  | 2 ->

# 745 "lib/read.mll"
             
# 745 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2517 "lib/read.ml"

  
# 2518 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rbr_rec v lexbuf __ocaml_lex_state

and skip_json v lexbuf =
   __ocaml_lex_skip_json_rec v lexbuf 322
and __ocaml_lex_skip_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 751 "lib/read.mll"
                
# 751 "lib/read.mll"
                ( () )

# 2529 "lib/read.ml"

  
# 2530 "lib/read.ml"
  | 1 ->

# 752 "lib/read.mll"
                
# 752 "lib/read.mll"
                ( () )

# 2534 "lib/read.ml"

  
# 2535 "lib/read.ml"
  | 2 ->

# 753 "lib/read.mll"
                
# 753 "lib/read.mll"
                ( () )

# 2539 "lib/read.ml"

  
# 2540 "lib/read.ml"
  | 3 ->

# 754 "lib/read.mll"
                
# 754 "lib/read.mll"
                ( () )

# 2544 "lib/read.ml"

  
# 2545 "lib/read.ml"
  | 4 ->

# 755 "lib/read.mll"
                
# 755 "lib/read.mll"
                ( () )

# 2549 "lib/read.ml"

  
# 2550 "lib/read.ml"
  | 5 ->

# 756 "lib/read.mll"
                
# 756 "lib/read.mll"
                ( () )

# 2554 "lib/read.ml"

  
# 2555 "lib/read.ml"
  | 6 ->

# 757 "lib/read.mll"
                
# 757 "lib/read.mll"
                ( finish_skip_stringlit v lexbuf )

# 2559 "lib/read.ml"

  
# 2560 "lib/read.ml"
  | 7 ->

# 758 "lib/read.mll"
                          
# 758 "lib/read.mll"
                          ( () )

# 2564 "lib/read.ml"

  
# 2565 "lib/read.ml"
  | 8 ->

# 759 "lib/read.mll"
                
# 759 "lib/read.mll"
                ( () )

# 2569 "lib/read.ml"

  
# 2570 "lib/read.ml"
  | 9 ->

# 761 "lib/read.mll"
                 
# 761 "lib/read.mll"
                 ( try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     skip_ident v lexbuf;
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       skip_ident v lexbuf;
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with End_of_object ->
                     ()
                 )

# 2595 "lib/read.ml"

  
# 2596 "lib/read.ml"
  | 10 ->

# 784 "lib/read.mll"
                 
# 784 "lib/read.mll"
                 ( try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with End_of_array ->
                     ()
                 )

# 2613 "lib/read.ml"

  
# 2614 "lib/read.ml"
  | 11 ->

# 799 "lib/read.mll"
                 
# 799 "lib/read.mll"
                 (
                     
# 801 "lib/read.mll"
                     try
                       read_space v lexbuf;
                       read_tuple_end lexbuf;
                       skip_json v lexbuf;
                       while true do
                         read_space v lexbuf;
                         read_tuple_sep v lexbuf;
                         read_space v lexbuf;
                         skip_json v lexbuf;
                       done;
                       assert false
                     with End_of_tuple ->
                       ()
                 
# 817 "lib/read.mll"
                 )

# 2636 "lib/read.ml"

  
# 2637 "lib/read.ml"
  | 12 ->

# 819 "lib/read.mll"
                 
# 819 "lib/read.mll"
                 (
                     
# 821 "lib/read.mll"
                     read_space v lexbuf;
                     skip_ident v lexbuf;
                     read_space v lexbuf;
                     finish_skip_variant v lexbuf
                 
# 828 "lib/read.mll"
                 )

# 2650 "lib/read.ml"

  
# 2651 "lib/read.ml"
  | 13 ->

# 830 "lib/read.mll"
                 
# 830 "lib/read.mll"
                 ( skip_json v lexbuf )

# 2655 "lib/read.ml"

  
# 2656 "lib/read.ml"
  | 14 ->

# 831 "lib/read.mll"
                 
# 831 "lib/read.mll"
                 ( finish_comment v lexbuf; skip_json v lexbuf )

# 2660 "lib/read.ml"

  
# 2661 "lib/read.ml"
  | 15 ->

# 832 "lib/read.mll"
                 
# 832 "lib/read.mll"
                 ( newline v lexbuf; skip_json v lexbuf )

# 2665 "lib/read.ml"

  
# 2666 "lib/read.ml"
  | 16 ->

# 833 "lib/read.mll"
                 
# 833 "lib/read.mll"
                 ( skip_json v lexbuf )

# 2670 "lib/read.ml"

  
# 2671 "lib/read.ml"
  | 17 ->

# 834 "lib/read.mll"
                 
# 834 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 2675 "lib/read.ml"

  
# 2676 "lib/read.ml"
  | 18 ->

# 835 "lib/read.mll"
                 
# 835 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 2680 "lib/read.ml"

  
# 2681 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_json_rec v lexbuf __ocaml_lex_state

and finish_skip_stringlit v lexbuf =
   __ocaml_lex_finish_skip_stringlit_rec v lexbuf 378
and __ocaml_lex_finish_skip_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 841 "lib/read.mll"
         
# 841 "lib/read.mll"
         ( () )

# 2692 "lib/read.ml"

  
# 2693 "lib/read.ml"
  | 1 ->

# 842 "lib/read.mll"
         
# 842 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 2697 "lib/read.ml"

  
# 2698 "lib/read.ml"
  | 2 ->

# 843 "lib/read.mll"
         
# 843 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2702 "lib/read.ml"

  
# 2703 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_skip_stringlit_rec v lexbuf __ocaml_lex_state

and finish_skip_variant v lexbuf =
   __ocaml_lex_finish_skip_variant_rec v lexbuf 389
and __ocaml_lex_finish_skip_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 846 "lib/read.mll"
         
# 846 "lib/read.mll"
         ( skip_json v lexbuf;
           read_space v lexbuf;
           read_gt v lexbuf )

# 2716 "lib/read.ml"

  
# 2717 "lib/read.ml"
  | 1 ->

# 849 "lib/read.mll"
         
# 849 "lib/read.mll"
         ( () )

# 2721 "lib/read.ml"

  
# 2722 "lib/read.ml"
  | 2 ->

# 850 "lib/read.mll"
         
# 850 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 2726 "lib/read.ml"

  
# 2727 "lib/read.ml"
  | 3 ->

# 851 "lib/read.mll"
         
# 851 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2731 "lib/read.ml"

  
# 2732 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_skip_variant_rec v lexbuf __ocaml_lex_state

and skip_ident v lexbuf =
   __ocaml_lex_skip_ident_rec v lexbuf 394
and __ocaml_lex_skip_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 854 "lib/read.mll"
             
# 854 "lib/read.mll"
             ( finish_skip_stringlit v lexbuf )

# 2743 "lib/read.ml"

  
# 2744 "lib/read.ml"
  | 1 ->

# 855 "lib/read.mll"
             
# 855 "lib/read.mll"
             ( () )

# 2748 "lib/read.ml"

  
# 2749 "lib/read.ml"
  | 2 ->

# 856 "lib/read.mll"
             
# 856 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 2753 "lib/read.ml"

  
# 2754 "lib/read.ml"
  | 3 ->

# 857 "lib/read.mll"
             
# 857 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2758 "lib/read.ml"

  
# 2759 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_ident_rec v lexbuf __ocaml_lex_state

and buffer_json v lexbuf =
   __ocaml_lex_buffer_json_rec v lexbuf 399
and __ocaml_lex_buffer_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 870 "lib/read.mll"
                
# 870 "lib/read.mll"
                ( add_lexeme v.buf lexbuf )

# 2770 "lib/read.ml"

  
# 2771 "lib/read.ml"
  | 1 ->

# 872 "lib/read.mll"
                
# 872 "lib/read.mll"
                ( finish_buffer_stringlit v lexbuf )

# 2775 "lib/read.ml"

  
# 2776 "lib/read.ml"
  | 2 ->

# 873 "lib/read.mll"
                 
# 873 "lib/read.mll"
                 ( try
                     Buffer.add_char v.buf '{';
                     buffer_space v lexbuf;
                     buffer_object_end v lexbuf;
                     buffer_ident v lexbuf;
                     buffer_space v lexbuf;
                     buffer_colon v lexbuf;
                     buffer_space v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_object_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_ident v lexbuf;
                       buffer_space v lexbuf;
                       buffer_colon v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with End_of_object ->
                     ()
                 )

# 2802 "lib/read.ml"

  
# 2803 "lib/read.ml"
  | 3 ->

# 897 "lib/read.mll"
                 
# 897 "lib/read.mll"
                 ( try
                     Buffer.add_char v.buf '[';
                     buffer_space v lexbuf;
                     buffer_array_end v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_array_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with End_of_array ->
                     ()
                 )

# 2821 "lib/read.ml"

  
# 2822 "lib/read.ml"
  | 4 ->

# 913 "lib/read.mll"
                 
# 913 "lib/read.mll"
                 (
                     
# 915 "lib/read.mll"
                     try
                       Buffer.add_char v.buf '(';
                       buffer_space v lexbuf;
                       buffer_tuple_end v lexbuf;
                       buffer_json v lexbuf;
                       while true do
                         buffer_space v lexbuf;
                         buffer_tuple_sep v lexbuf;
                         buffer_space v lexbuf;
                         buffer_json v lexbuf;
                       done;
                       assert false
                     with End_of_tuple ->
                       ()
                 
# 932 "lib/read.mll"
                 )

# 2845 "lib/read.ml"

  
# 2846 "lib/read.ml"
  | 5 ->

# 934 "lib/read.mll"
                 
# 934 "lib/read.mll"
                 (
                     
# 936 "lib/read.mll"
                     Buffer.add_char v.buf '<';
                     buffer_space v lexbuf;
                     buffer_ident v lexbuf;
                     buffer_space v lexbuf;
                     finish_buffer_variant v lexbuf
                 
# 944 "lib/read.mll"
                 )

# 2860 "lib/read.ml"

  
# 2861 "lib/read.ml"
  | 6 ->

# 946 "lib/read.mll"
                 
# 946 "lib/read.mll"
                 ( add_lexeme v.buf lexbuf; buffer_json v lexbuf )

# 2865 "lib/read.ml"

  
# 2866 "lib/read.ml"
  | 7 ->

# 947 "lib/read.mll"
                 
# 947 "lib/read.mll"
                 ( Buffer.add_string v.buf "/*";
                   finish_buffer_comment v lexbuf;
                   buffer_json v lexbuf )

# 2872 "lib/read.ml"

  
# 2873 "lib/read.ml"
  | 8 ->

# 950 "lib/read.mll"
                 
# 950 "lib/read.mll"
                 ( Buffer.add_char v.buf '\n';
                   newline v lexbuf;
                   buffer_json v lexbuf )

# 2879 "lib/read.ml"

  
# 2880 "lib/read.ml"
  | 9 ->

# 953 "lib/read.mll"
                 
# 953 "lib/read.mll"
                 ( add_lexeme v.buf lexbuf; buffer_json v lexbuf )

# 2884 "lib/read.ml"

  
# 2885 "lib/read.ml"
  | 10 ->

# 954 "lib/read.mll"
                 
# 954 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 2889 "lib/read.ml"

  
# 2890 "lib/read.ml"
  | 11 ->

# 955 "lib/read.mll"
                 
# 955 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 2894 "lib/read.ml"

  
# 2895 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_json_rec v lexbuf __ocaml_lex_state

and finish_buffer_stringlit v lexbuf =
   __ocaml_lex_finish_buffer_stringlit_rec v lexbuf 450
and __ocaml_lex_finish_buffer_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 961 "lib/read.mll"
         
# 961 "lib/read.mll"
         ( Buffer.add_char v.buf '"';
           add_lexeme v.buf lexbuf
         )

# 2908 "lib/read.ml"

  
# 2909 "lib/read.ml"
  | 1 ->

# 964 "lib/read.mll"
         
# 964 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 2913 "lib/read.ml"

  
# 2914 "lib/read.ml"
  | 2 ->

# 965 "lib/read.mll"
         
# 965 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2918 "lib/read.ml"

  
# 2919 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_stringlit_rec v lexbuf __ocaml_lex_state

and finish_buffer_variant v lexbuf =
   __ocaml_lex_finish_buffer_variant_rec v lexbuf 461
and __ocaml_lex_finish_buffer_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 968 "lib/read.mll"
         
# 968 "lib/read.mll"
         ( Buffer.add_char v.buf ':';
           buffer_json v lexbuf;
           buffer_space v lexbuf;
           buffer_gt v lexbuf )

# 2933 "lib/read.ml"

  
# 2934 "lib/read.ml"
  | 1 ->

# 972 "lib/read.mll"
         
# 972 "lib/read.mll"
         ( Buffer.add_char v.buf '>' )

# 2938 "lib/read.ml"

  
# 2939 "lib/read.ml"
  | 2 ->

# 973 "lib/read.mll"
         
# 973 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 2943 "lib/read.ml"

  
# 2944 "lib/read.ml"
  | 3 ->

# 974 "lib/read.mll"
         
# 974 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2948 "lib/read.ml"

  
# 2949 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_variant_rec v lexbuf __ocaml_lex_state

and buffer_ident v lexbuf =
   __ocaml_lex_buffer_ident_rec v lexbuf 466
and __ocaml_lex_buffer_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 977 "lib/read.mll"
             
# 977 "lib/read.mll"
             ( finish_buffer_stringlit v lexbuf )

# 2960 "lib/read.ml"

  
# 2961 "lib/read.ml"
  | 1 ->

# 978 "lib/read.mll"
             
# 978 "lib/read.mll"
             ( add_lexeme v.buf lexbuf )

# 2965 "lib/read.ml"

  
# 2966 "lib/read.ml"
  | 2 ->

# 979 "lib/read.mll"
             
# 979 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 2970 "lib/read.ml"

  
# 2971 "lib/read.ml"
  | 3 ->

# 980 "lib/read.mll"
             
# 980 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2975 "lib/read.ml"

  
# 2976 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_ident_rec v lexbuf __ocaml_lex_state

and buffer_space v lexbuf =
   __ocaml_lex_buffer_space_rec v lexbuf 471
and __ocaml_lex_buffer_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 983 "lib/read.mll"
                             
# 983 "lib/read.mll"
                             (
    add_lexeme v.buf lexbuf;
    newline v lexbuf;
    buffer_space v lexbuf )

# 2990 "lib/read.ml"

  
# 2991 "lib/read.ml"
  | 1 ->

# 987 "lib/read.mll"
                             
# 987 "lib/read.mll"
                             (
    Buffer.add_string v.buf "/*";
    finish_buffer_comment v lexbuf;
    buffer_space v lexbuf )

# 2998 "lib/read.ml"

  
# 2999 "lib/read.ml"
  | 2 ->

# 991 "lib/read.mll"
                             
# 991 "lib/read.mll"
                             (
    Buffer.add_char v.buf '\n';
    newline v lexbuf;
    buffer_space v lexbuf )

# 3006 "lib/read.ml"

  
# 3007 "lib/read.ml"
  | 3 ->

# 995 "lib/read.mll"
                             
# 995 "lib/read.mll"
                             (
    add_lexeme v.buf lexbuf;
    buffer_space v lexbuf )

# 3013 "lib/read.ml"

  
# 3014 "lib/read.ml"
  | 4 ->

# 998 "lib/read.mll"
                             
# 998 "lib/read.mll"
                             ( () )

# 3018 "lib/read.ml"

  
# 3019 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_space_rec v lexbuf __ocaml_lex_state

and buffer_object_end v lexbuf =
   __ocaml_lex_buffer_object_end_rec v lexbuf 478
and __ocaml_lex_buffer_object_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1001 "lib/read.mll"
             
# 1001 "lib/read.mll"
             (
      Buffer.add_char v.buf '}';
      raise End_of_object )

# 3032 "lib/read.ml"

  
# 3033 "lib/read.ml"
  | 1 ->

# 1004 "lib/read.mll"
             
# 1004 "lib/read.mll"
             ( () )

# 3037 "lib/read.ml"

  
# 3038 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_object_end_rec v lexbuf __ocaml_lex_state

and buffer_object_sep v lexbuf =
   __ocaml_lex_buffer_object_sep_rec v lexbuf 480
and __ocaml_lex_buffer_object_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1007 "lib/read.mll"
             
# 1007 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3049 "lib/read.ml"

  
# 3050 "lib/read.ml"
  | 1 ->

# 1008 "lib/read.mll"
             
# 1008 "lib/read.mll"
             ( Buffer.add_char v.buf '}'; raise End_of_object )

# 3054 "lib/read.ml"

  
# 3055 "lib/read.ml"
  | 2 ->

# 1009 "lib/read.mll"
             
# 1009 "lib/read.mll"
             ( long_error "Expected ',' or '}' but found" v lexbuf )

# 3059 "lib/read.ml"

  
# 3060 "lib/read.ml"
  | 3 ->

# 1010 "lib/read.mll"
             
# 1010 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3064 "lib/read.ml"

  
# 3065 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_object_sep_rec v lexbuf __ocaml_lex_state

and buffer_array_end v lexbuf =
   __ocaml_lex_buffer_array_end_rec v lexbuf 485
and __ocaml_lex_buffer_array_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1013 "lib/read.mll"
             
# 1013 "lib/read.mll"
             ( Buffer.add_char v.buf ']'; raise End_of_array )

# 3076 "lib/read.ml"

  
# 3077 "lib/read.ml"
  | 1 ->

# 1014 "lib/read.mll"
             
# 1014 "lib/read.mll"
             ( () )

# 3081 "lib/read.ml"

  
# 3082 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_array_end_rec v lexbuf __ocaml_lex_state

and buffer_array_sep v lexbuf =
   __ocaml_lex_buffer_array_sep_rec v lexbuf 487
and __ocaml_lex_buffer_array_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1017 "lib/read.mll"
             
# 1017 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3093 "lib/read.ml"

  
# 3094 "lib/read.ml"
  | 1 ->

# 1018 "lib/read.mll"
             
# 1018 "lib/read.mll"
             ( Buffer.add_char v.buf ']'; raise End_of_array )

# 3098 "lib/read.ml"

  
# 3099 "lib/read.ml"
  | 2 ->

# 1019 "lib/read.mll"
             
# 1019 "lib/read.mll"
             ( long_error "Expected ',' or ']' but found" v lexbuf )

# 3103 "lib/read.ml"

  
# 3104 "lib/read.ml"
  | 3 ->

# 1020 "lib/read.mll"
             
# 1020 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3108 "lib/read.ml"

  
# 3109 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_array_sep_rec v lexbuf __ocaml_lex_state

and buffer_tuple_end v lexbuf =
   __ocaml_lex_buffer_tuple_end_rec v lexbuf 492
and __ocaml_lex_buffer_tuple_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1023 "lib/read.mll"
             
# 1023 "lib/read.mll"
             (
      Buffer.add_char v.buf ')';
      raise End_of_tuple )

# 3122 "lib/read.ml"

  
# 3123 "lib/read.ml"
  | 1 ->

# 1026 "lib/read.mll"
             
# 1026 "lib/read.mll"
             ( () )

# 3127 "lib/read.ml"

  
# 3128 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_tuple_end_rec v lexbuf __ocaml_lex_state

and buffer_tuple_sep v lexbuf =
   __ocaml_lex_buffer_tuple_sep_rec v lexbuf 494
and __ocaml_lex_buffer_tuple_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1029 "lib/read.mll"
             
# 1029 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3139 "lib/read.ml"

  
# 3140 "lib/read.ml"
  | 1 ->

# 1030 "lib/read.mll"
             
# 1030 "lib/read.mll"
             ( Buffer.add_char v.buf ')'; raise End_of_tuple )

# 3144 "lib/read.ml"

  
# 3145 "lib/read.ml"
  | 2 ->

# 1031 "lib/read.mll"
             
# 1031 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 3149 "lib/read.ml"

  
# 3150 "lib/read.ml"
  | 3 ->

# 1032 "lib/read.mll"
             
# 1032 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3154 "lib/read.ml"

  
# 3155 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_tuple_sep_rec v lexbuf __ocaml_lex_state

and buffer_colon v lexbuf =
   __ocaml_lex_buffer_colon_rec v lexbuf 499
and __ocaml_lex_buffer_colon_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1035 "lib/read.mll"
             
# 1035 "lib/read.mll"
             ( Buffer.add_char v.buf ':' )

# 3166 "lib/read.ml"

  
# 3167 "lib/read.ml"
  | 1 ->

# 1036 "lib/read.mll"
             
# 1036 "lib/read.mll"
             ( long_error "Expected ':' but found" v lexbuf )

# 3171 "lib/read.ml"

  
# 3172 "lib/read.ml"
  | 2 ->

# 1037 "lib/read.mll"
             
# 1037 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3176 "lib/read.ml"

  
# 3177 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_colon_rec v lexbuf __ocaml_lex_state

and buffer_gt v lexbuf =
   __ocaml_lex_buffer_gt_rec v lexbuf 503
and __ocaml_lex_buffer_gt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1040 "lib/read.mll"
         
# 1040 "lib/read.mll"
         ( Buffer.add_char v.buf '>' )

# 3188 "lib/read.ml"

  
# 3189 "lib/read.ml"
  | 1 ->

# 1041 "lib/read.mll"
         
# 1041 "lib/read.mll"
         ( long_error "Expected '>' but found" v lexbuf )

# 3193 "lib/read.ml"

  
# 3194 "lib/read.ml"
  | 2 ->

# 1042 "lib/read.mll"
         
# 1042 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 3198 "lib/read.ml"

  
# 3199 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_gt_rec v lexbuf __ocaml_lex_state

and finish_buffer_comment v lexbuf =
   __ocaml_lex_finish_buffer_comment_rec v lexbuf 507
and __ocaml_lex_finish_buffer_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1045 "lib/read.mll"
         
# 1045 "lib/read.mll"
         ( Buffer.add_string v.buf "*/" )

# 3210 "lib/read.ml"

  
# 3211 "lib/read.ml"
  | 1 ->

# 1046 "lib/read.mll"
         
# 1046 "lib/read.mll"
         ( long_error "Unterminated comment" v lexbuf )

# 3215 "lib/read.ml"

  
# 3216 "lib/read.ml"
  | 2 ->

# 1047 "lib/read.mll"
         
# 1047 "lib/read.mll"
         ( Buffer.add_char v.buf '\n';
           newline v lexbuf;
           finish_buffer_comment v lexbuf )

# 3222 "lib/read.ml"

  
# 3223 "lib/read.ml"
  | 3 ->

# 1050 "lib/read.mll"
         
# 1050 "lib/read.mll"
         ( add_lexeme v.buf lexbuf; finish_buffer_comment v lexbuf )

# 3227 "lib/read.ml"

  
# 3228 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_comment_rec v lexbuf __ocaml_lex_state

and junk lexbuf =
   __ocaml_lex_junk_rec lexbuf 513
and __ocaml_lex_junk_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1053 "lib/read.mll"
             
# 1053 "lib/read.mll"
             ( Lexing.lexeme lexbuf )

# 3239 "lib/read.ml"

  
# 3240 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_junk_rec lexbuf __ocaml_lex_state

;;


# 1055 "lib/read.mll"
 
  
# 1056 "lib/read.mll"
  let _ = (read_json : lexer_state -> Lexing.lexbuf -> t)

  let read_t = read_json

  let () =
    read_junk := junk

  let read_int8 v lexbuf =
    let n = read_int v lexbuf in
    if n < 0 || n > 255 then
      lexer_error "Int8 overflow" v lexbuf
    else
      char_of_int n

  let read_list read_cell v lexbuf =
    List.rev (read_list_rev read_cell v lexbuf)

  let array_of_rev_list l =
    match l with
        [] -> [| |]
      | x :: tl ->
          let len = List.length l in
          let a = Array.make len x in
          let r = ref tl in
          for i = len - 2 downto 0 do
            a.(i) <- List.hd !r;
            r := List.tl !r
          done;
          a

  let read_array read_cell v lexbuf =
    let l = read_list_rev read_cell v lexbuf in
    array_of_rev_list l

  (* Read a JSON object, reading the keys into OCaml strings
     (provided for backward compatibility) *)
  let read_fields read_field init_acc v =
    read_abstract_fields read_ident read_field init_acc v

  let finish v lexbuf =
    read_space v lexbuf;
    if not (read_eof lexbuf) then
      long_error "Junk after end of JSON value:" v lexbuf

  let init_lexer = init_lexer

  let from_lexbuf v ?(stream = false) lexbuf =
    read_space v lexbuf;

    let x =
      if read_eof lexbuf then
        raise End_of_input
      else
        read_json v lexbuf
    in

    if not stream then
      finish v lexbuf;

    x


  let from_string ?buf ?fname ?lnum s =
    try
      let lexbuf = Lexing.from_string s in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with End_of_input ->
      json_error "Blank input data"

  let from_channel ?buf ?fname ?lnum ic =
    try
      let lexbuf = Lexing.from_channel ic in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with End_of_input ->
      json_error "Blank input data"

  let from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    try
      let x = from_channel ?buf ?fname ?lnum ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  exception Finally of exn * exn

  let stream_from_lexbuf v ?(fin = fun () -> ()) lexbuf =
    let stream = Some true in
    let f i =
      try Some (from_lexbuf v ?stream lexbuf)
      with
          End_of_input ->
            fin ();
            None
        | e ->
            (try fin () with fin_e -> raise (Finally (e, fin_e)));
            raise e
    in
    Stream.from f

  let stream_from_string ?buf ?fname ?lnum s =
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v (Lexing.from_string s)

  let stream_from_channel ?buf ?fin ?fname ?lnum ic =
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v ?fin lexbuf

  let stream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v ~fin lexbuf

  type json_line = [ `Json of t | `Exn of exn ]

  let linestream_from_channel
      ?buf ?(fin = fun () -> ()) ?fname ?lnum:(lnum0 = 1) ic =
    let buf =
      match buf with
          None -> Some (Buffer.create 256)
        | Some _ -> buf
    in
    let f i =
      try
        let line = input_line ic in
        let lnum = lnum0 + i in
        Some (`Json (from_string ?buf ?fname ~lnum line))
      with
          End_of_file -> fin (); None
        | e -> Some (`Exn e)
    in
    Stream.from f

  let linestream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    linestream_from_channel ?buf ~fin ?fname ?lnum ic

  let prettify ?std s =
    pretty_to_string ?std (from_string s)

  let compact ?std s =
    to_string (from_string s)

  let validate_json _path _value = None


# 3411 "lib/read.ml"
# 62 "yojson.cppo.ml"
module Util =
struct
# 1 "util.ml"
exception Type_error of string * t

let typeof = function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `Int _ -> "int"
  | `List _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"
  | `Intlit _ -> "intlit"
  | `Tuple _ -> "tuple"
  | `Variant _ -> "variant"

let typerr msg js = raise (Type_error (msg ^ typeof js, js))

exception Undefined of string * t

let assoc name obj =
  try List.assoc name obj
  with Not_found -> `Null

let member name = function
  | `Assoc obj -> assoc name obj
  | js -> typerr ("Can't get member '" ^ name ^ "' of non-object type ") js

let index i = function
  | `List l as js ->
      let len = List.length l in
      let wrapped_index = if i < 0 then len + i else i in
      if wrapped_index < 0 || wrapped_index >= len then
        raise (Undefined ("Index " ^ string_of_int i ^ " out of bounds", js))
      else List.nth l wrapped_index
  | js -> typerr ("Can't get index " ^ string_of_int i
                 ^ " of non-array type ") js

let map f = function
  | `List l -> `List (List.map f l)
  | js -> typerr "Can't map function over non-array type " js

let to_assoc = function
  | `Assoc obj -> obj
  | js -> typerr "Expected object, got " js

let to_option f = function
  | `Null -> None
  | x -> Some (f x)

let to_bool = function
  | `Bool b -> b
  | js -> typerr "Expected bool, got " js

let to_bool_option = function
  | `Bool b -> Some b
  | `Null -> None
  | js -> typerr "Expected bool or null, got " js

let to_number = function
  | `Int i -> float i
  | `Float f -> f
  | js -> typerr "Expected number, got " js

let to_number_option = function
  | `Int i -> Some (float i)
  | `Float f -> Some f
  | `Null -> None
  | js -> typerr "Expected number or null, got " js

let to_float = function
  | `Float f -> f
  | js -> typerr "Expected float, got " js

let to_float_option = function
  | `Float f -> Some f
  | `Null -> None
  | js -> typerr "Expected float or null, got " js

let to_int = function
  | `Int i -> i
  | js -> typerr "Expected int, got " js

let to_int_option = function
  | `Int i -> Some i
  | `Null -> None
  | js -> typerr "Expected int or null, got " js

let to_list = function
  | `List l -> l
  | js -> typerr "Expected array, got " js

let to_string = function
  | `String s -> s
  | js -> typerr "Expected string, got " js

let to_string_option = function
  | `String s -> Some s
  | `Null -> None
  | js -> typerr "Expected string or null, got " js

let convert_each f = function
  | `List l -> List.map f l
  | js -> typerr "Can't convert each element of non-array type " js


let rec rev_filter_map f acc l =
  match l with
      [] -> acc
    | x :: tl ->
        match f x with
            None -> rev_filter_map f acc tl
          | Some y -> rev_filter_map f (y :: acc) tl

let filter_map f l =
  List.rev (rev_filter_map f [] l)

let rec rev_flatten acc l =
  match l with
      [] -> acc
    | x :: tl ->
        match x with
            `List l2 -> rev_flatten (List.rev_append l2 acc) tl
          | _ -> rev_flatten acc tl

let flatten l =
  List.rev (rev_flatten [] l)

let filter_index i l =
  filter_map (
    function
        `List l ->
          (try Some (List.nth l i)
           with _ -> None)
      | _ -> None
  ) l

let filter_list l =
  filter_map (
    function
        `List l -> Some l
      | _ -> None
  ) l

let filter_member k l =
  filter_map (
    function
        `Assoc l ->
          (try Some (List.assoc k l)
           with _ -> None)
      | _ -> None
  ) l

let filter_assoc l =
  filter_map (
    function
        `Assoc l -> Some l
      | _ -> None
  ) l

let filter_bool l =
  filter_map (
    function
        `Bool x -> Some x
      | _ -> None
  ) l

let filter_int l =
  filter_map (
    function
        `Int x -> Some x
      | _ -> None
  ) l

let filter_float l =
  filter_map (
    function
        `Float x -> Some x
      | _ -> None
  ) l

let filter_number l =
  filter_map (
    function
        `Int x -> Some (float x)
      | `Float x -> Some x
      | _ -> None
  ) l

let filter_string l =
  filter_map (
    function
        `String x -> Some x
      | _ -> None
  ) l

let keys o =
  to_assoc o |> List.map (fun (key, _) -> key)

let values o =
  to_assoc o |> List.map (fun (_, value) -> value)

let combine (first : t) (second : t) =
  match (first, second) with
  | (`Assoc a, `Assoc b) -> (`Assoc (a @ b) :  t)
  | (a, b) -> raise (Invalid_argument "Expected two objects, check inputs")
# 65 "yojson.cppo.ml"
end
# 72 "yojson.cppo.ml"
end

module Raw =
struct
# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 11 "type.ml"
    | `Intlit of string
    
# 17 "type.ml"
    | `Floatlit of string
    
# 23 "type.ml"
    | `Stringlit of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 28 "type.ml"
    | `Tuple of t list
    
# 31 "type.ml"
    | `Variant of (string * t option)
    
# 33 "type.ml"
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)
# 1 "write.ml"
(* included: type.ml *)

let hex n =
  Char.chr (
    if n < 10 then n + 48
    else n + 87
  )

let write_special src start stop ob str =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob str;
  start := stop + 1

let write_control_char src start stop ob c =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob "\\u00";
  Buffer.add_char ob (hex (Char.code c lsr 4));
  Buffer.add_char ob (hex (Char.code c land 0xf));
  start := stop + 1

let finish_string src start ob =
  try
    Buffer.add_substring ob src !start (String.length src - !start)
  with exc ->
    Printf.eprintf "src=%S start=%i len=%i\n%!"
      src !start (String.length src - !start);
    raise exc

let write_string_body ob s =
  let start = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '"' -> write_special s start i ob "\\\""
      | '\\' -> write_special s start i ob "\\\\"
      | '\b' -> write_special s start i ob "\\b"
      | '\012' -> write_special s start i ob "\\f"
      | '\n' -> write_special s start i ob "\\n"
      | '\r' -> write_special s start i ob "\\r"
      | '\t' -> write_special s start i ob "\\t"
      | '\x00'..'\x1F'
      | '\x7F' as c -> write_control_char s start i ob c
      | _ -> ()
  done;
  finish_string s start ob

let write_string ob s =
  Buffer.add_char ob '"';
  write_string_body ob s;
  Buffer.add_char ob '"'

let json_string_of_string s =
  let ob = Buffer.create 10 in
  write_string ob s;
  Buffer.contents ob

let test_string () =
  let s = Bytes.create 256 in
  for i = 0 to 255 do
    Bytes.set s i (Char.chr i)
  done;
  json_string_of_string (Bytes.to_string s)


let write_null ob () =
  Buffer.add_string ob "null"

let write_bool ob x =
  Buffer.add_string ob (if x then "true" else "false")


let max_digits =
  max
    (String.length (string_of_int max_int))
    (String.length (string_of_int min_int))

let dec n =
  Char.chr (n + 48)

let rec write_digits s x =
  if x = 0 then ()
  else
    let d = x mod 10 in
    write_digits s (x / 10);
    Buffer.add_char s (dec (abs d))

let write_int ob x =
  if x > 0 then
    write_digits ob x
  else if x < 0 then (
    Buffer.add_char ob '-';
    write_digits ob x
  )
  else
    Buffer.add_char ob '0'


let json_string_of_int i =
  string_of_int i


(*
  Ensure that the float is not printed as an int.
  This is not required by JSON, but useful in order to guarantee
  reversibility.
*)
let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with
          '0'..'9' | '-' -> ()
        | _ -> raise Exit
    done;
    true
  with Exit ->
    false

(*
  Both write_float_fast and write_float guarantee
  that a sufficient number of digits are printed in order to
  allow reversibility.

  The _fast version is faster but often produces unnecessarily long numbers.
*)
let write_float_fast ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s = Printf.sprintf "%.17g" x in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_float ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_normal_float_prec significant_figures ob x =
  let open Printf in
  let s =
    match significant_figures with
        1 -> sprintf "%.1g" x
      | 2 -> sprintf "%.2g" x
      | 3 -> sprintf "%.3g" x
      | 4 -> sprintf "%.4g" x
      | 5 -> sprintf "%.5g" x
      | 6 -> sprintf "%.6g" x
      | 7 -> sprintf "%.7g" x
      | 8 -> sprintf "%.8g" x
      | 9 -> sprintf "%.9g" x
      | 10 -> sprintf "%.10g" x
      | 11 -> sprintf "%.11g" x
      | 12 -> sprintf "%.12g" x
      | 13 -> sprintf "%.13g" x
      | 14 -> sprintf "%.14g" x
      | 15 -> sprintf "%.15g" x
      | 16 -> sprintf "%.16g" x
      | _ -> sprintf "%.17g" x
  in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

let write_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      write_normal_float_prec significant_figures ob x

let json_string_of_float x =
  let ob = Buffer.create 20 in
  write_float ob x;
  Buffer.contents ob


let write_std_float_fast ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s = Printf.sprintf "%.17g" x in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_std_float ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_std_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      write_normal_float_prec significant_figures ob x

let std_json_string_of_float x =
  let ob = Buffer.create 20 in
  write_std_float ob x;
  Buffer.contents ob


let test_float () =
  let l = [ 0.; 1.; -1. ] in
  let l = l @ List.map (fun x -> 2. *. x +. 1.) l in
  let l = l @ List.map (fun x -> x /. sqrt 2.) l in
  let l = l @ List.map (fun x -> x *. sqrt 3.) l in
  let l = l @ List.map cos l in
  let l = l @ List.map (fun x -> x *. 1.23e50) l in
  let l = l @ [ infinity; neg_infinity ] in
  List.iter (
    fun x ->
      let s = Printf.sprintf "%.17g" x in
      let y = float_of_string s in
      Printf.printf "%g %g %S %B\n" x y s (x = y)
  )
    l

(*
let () = test_float ()
*)

let write_intlit = Buffer.add_string
let write_floatlit = Buffer.add_string
let write_stringlit = Buffer.add_string

let rec iter2_aux f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_sep x;
      f_elt x y;
      iter2_aux f_elt f_sep x l

let iter2 f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_elt x y;
      iter2_aux f_elt f_sep x l

let f_sep ob =
  Buffer.add_char ob ','

let rec write_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    
# 296 "write.ml"
    | `Intlit s -> Buffer.add_string ob s
    
# 302 "write.ml"
    | `Floatlit s -> Buffer.add_string ob s
    
# 308 "write.ml"
    | `Stringlit s -> Buffer.add_string ob s
    
# 310 "write.ml"
    | `Assoc l -> write_assoc ob l
    | `List l -> write_list ob l
    
# 313 "write.ml"
    | `Tuple l -> write_tuple ob l
    
# 316 "write.ml"
    | `Variant (s, o) -> write_variant ob s o

# 319 "write.ml"
and write_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_list ob l =
  Buffer.add_char ob '[';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ']'

# 335 "write.ml"
and write_tuple ob l =
  Buffer.add_char ob '(';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ')'

# 342 "write.ml"
and write_variant ob s o =
  Buffer.add_char ob '<';
  write_string ob s;
  (match o with
       None -> ()
     | Some x ->
         Buffer.add_char ob ':';
         write_json ob x
  );
  Buffer.add_char ob '>'

# 354 "write.ml"
let write_t = write_json

let rec write_std_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    
# 364 "write.ml"
    | `Intlit s -> Buffer.add_string ob s
    
# 370 "write.ml"
    | `Floatlit s -> Buffer.add_string ob s
    
# 376 "write.ml"
    | `Stringlit s -> Buffer.add_string ob s
    
# 378 "write.ml"
    | `Assoc l -> write_std_assoc ob l
    | `List l -> write_std_list ob l
    
# 381 "write.ml"
    | `Tuple l -> write_std_tuple ob l
    
# 384 "write.ml"
    | `Variant (s, o) -> write_std_variant ob s o

# 387 "write.ml"
and write_std_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_std_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_std_list ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'

and write_std_tuple ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'

# 408 "write.ml"
and write_std_variant ob s o =
  match o with
      None -> write_string ob s
    | Some x ->
        Buffer.add_char ob '[';
        write_string ob s;
        Buffer.add_char ob ',';
        write_std_json ob x;
        Buffer.add_char ob ']'


# 420 "write.ml"
let to_buffer ?(std = false) ob x =
  if std then (
    if not (is_object_or_array x) then
      json_error "Root is not an object or array"
    else
      write_std_json ob x
  )
  else
    write_json ob x


let to_string ?buf ?(len = 256) ?std x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  to_buffer ?std ob x;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let to_channel ?buf ?(len=4096) ?std oc x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  to_buffer ?std ob x;
  Buffer.output_buffer oc ob

let to_output ?buf ?(len=4096) ?std out x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  to_buffer ?std ob x;
  out#output (Buffer.contents ob) 0 (Buffer.length ob);
  ()

let to_file ?len ?std file x =
  let oc = open_out file in
  try
    to_channel ?len ?std oc x;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let stream_to_buffer ?std ob st =
  Stream.iter (to_buffer ?std ob) st

let stream_to_string ?buf ?(len = 256) ?std st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  stream_to_buffer ?std ob st;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let stream_to_channel ?buf ?(len=2096) ?std oc st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> ob
  in
  stream_to_buffer ?std ob st

let stream_to_file ?len ?std file st =
  let oc = open_out file in
  try
    stream_to_channel ?len ?std oc st;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e


let rec sort = function
  | `Assoc l ->
      let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
      `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
  | `List l ->
      `List (List.rev (List.rev_map sort l))
  
# 513 "write.ml"
  | `Tuple l ->
      `Tuple (List.rev (List.rev_map sort l))
  
# 517 "write.ml"
  | `Variant (k, Some v) as x ->
      let v' = sort v in
      if v == v' then x
      else
        `Variant (k, Some v')
  
# 523 "write.ml"
  | x -> x
# 1 "monomorphic.ml"
let rec pp fmt =
  function
  | `Null -> Format.pp_print_string fmt "`Null"
  | `Bool x ->
    Format.fprintf fmt "`Bool (@[<hov>";
    Format.fprintf fmt "%B" x;
    Format.fprintf fmt "@])"
  
# 15 "monomorphic.ml"
  | `Intlit x ->
    Format.fprintf fmt "`Intlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 27 "monomorphic.ml"
  | `Floatlit x ->
    Format.fprintf fmt "`Floatlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 39 "monomorphic.ml"
  | `Stringlit x ->
    Format.fprintf fmt "`Stringlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 44 "monomorphic.ml"
  | `Assoc xs ->
    Format.fprintf fmt "`Assoc (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep (key, value) ->
        if sep then
          Format.fprintf fmt ";@ ";
          Format.fprintf fmt "(@[";
          Format.fprintf fmt "%S" key;
          Format.fprintf fmt ",@ ";
          pp fmt value;
          Format.fprintf fmt "@])";
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  | `List xs ->
    Format.fprintf fmt "`List (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep x ->
        if sep then
          Format.fprintf fmt ";@ ";
          pp fmt x;
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  
# 71 "monomorphic.ml"
  | `Tuple tup ->
    Format.fprintf fmt "`Tuple (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep e ->
         if sep then
           Format.fprintf fmt ";@ ";
           pp fmt e;
           true) false tup);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  
# 84 "monomorphic.ml"
  | `Variant (name, value) ->
    Format.fprintf fmt "`Variant (@[<hov>";
    Format.fprintf fmt "(@[";
    Format.fprintf fmt "%S" name;
    Format.fprintf fmt ",@ ";
    (match value with
      | None -> Format.pp_print_string fmt "None"
      | Some x ->
        Format.pp_print_string fmt "(Some ";
        pp fmt x;
        Format.pp_print_string fmt ")");
    Format.fprintf fmt "@])";
    Format.fprintf fmt "@])"

# 99 "monomorphic.ml"
let show x =
  Format.asprintf "%a" pp x

let rec equal a b =
  match a, b with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
    
# 110 "monomorphic.ml"
    | `Intlit a, `Intlit b -> a = b
    
# 116 "monomorphic.ml"
    | `Floatlit a, `Floatlit b -> a = b
    
# 122 "monomorphic.ml"
    | `Stringlit a, `Stringlit b -> a = b
    
# 124 "monomorphic.ml"
    | `Assoc xs, `Assoc ys ->
      let compare_keys = fun (key, _) (key', _) -> String.compare key key' in
      let xs = List.stable_sort compare_keys xs in
      let ys = List.stable_sort compare_keys ys in
      (match List.for_all2 (fun (key, value) (key', value') ->
        match key = key' with
        | false -> false
        | true -> equal value value') xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    
# 137 "monomorphic.ml"
    | `Tuple xs, `Tuple ys
    
# 139 "monomorphic.ml"
    | `List xs, `List ys ->
      (match List.for_all2 equal xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    
# 146 "monomorphic.ml"
    | `Variant (name, value), `Variant (name', value') ->
      (match name = name' with
      | false -> false
      | true ->
        match value, value' with
        | None, None -> true
        | Some x, Some y -> equal x y
        | _ -> false)
    
# 155 "monomorphic.ml"
    | _ -> false

# 2 "write2.ml"
let pretty_print ?std out (x : t) =
  Pretty.pp ?std out (x :> json_max)

let pretty_to_string ?std (x : t) =
  Pretty.to_string ?std (x :> json_max)

let pretty_to_channel ?std oc (x : t) =
  Pretty.to_channel ?std oc (x :> json_max)

# 1 "lib/read.mll"
 
  
# 2 "lib/read.mll"
  module Lexing =
    (*
      We override Lexing.engine in order to avoid creating a new position
      record each time a rule is matched.
      This reduces total parsing time by about 31%.
    *)
  struct
    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
        buf.lex_start_p <- buf.lex_curr_p;
        buf.lex_curr_p <- {buf.lex_curr_p
                           with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end

  open Printf
  open Lexing

  (* see description in common.mli *)
  type lexer_state = Lexer_state.t = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }

  let dec c =
    Char.code c - 48

  let hex c =
    match c with
        '0'..'9' -> int_of_char c - int_of_char '0'
      | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
      | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
      | _ -> assert false

  let custom_error descr v lexbuf =
    let offs = lexbuf.lex_abs_pos - 1 in
    let bol = v.bol in
    let pos1 = offs + lexbuf.lex_start_pos - bol - 1 in
    let pos2 = max pos1 (offs + lexbuf.lex_curr_pos - bol) in
    let file_line =
      match v.fname with
          None -> "Line"
        | Some s ->
            sprintf "File %s, line" s
    in
    let bytes =
      if pos1 = pos2 then
        sprintf "byte %i" (pos1+1)
      else
        sprintf "bytes %i-%i" (pos1+1) (pos2+1)
    in
    let msg = sprintf "%s %i, %s:\n%s" file_line v.lnum bytes descr in
    json_error msg


  let lexer_error descr v lexbuf =
    custom_error
      (sprintf "%s '%s'" descr (Lexing.lexeme lexbuf))
      v lexbuf

  let read_junk = ref (fun _ -> assert false)

  let long_error descr v lexbuf =
    let junk = Lexing.lexeme lexbuf in
    let extra_junk = !read_junk lexbuf in
    custom_error
      (sprintf "%s '%s%s'" descr junk extra_junk)
      v lexbuf

  let min10 = min_int / 10 - (if min_int mod 10 = 0 then 0 else 1)
  let max10 = max_int / 10 + (if max_int mod 10 = 0 then 0 else 1)

  exception Int_overflow

  let extract_positive_int lexbuf =
    let start = lexbuf.lex_start_pos in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n >= max10 then
        raise Int_overflow
      else
        n := 10 * !n + dec (Bytes.get s i)
    done;
    if !n < 0 then
      raise Int_overflow
    else
      !n

  let make_positive_int v lexbuf =
        
# 108 "lib/read.mll"
        `Intlit (lexeme lexbuf)

  
# 113 "lib/read.mll"
  let extract_negative_int lexbuf =
    let start = lexbuf.lex_start_pos + 1 in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n <= min10 then
        raise Int_overflow
      else
        n := 10 * !n - dec (Bytes.get s i)
    done;
    if !n > 0 then
      raise Int_overflow
    else
      !n

  let make_negative_int v lexbuf =
        
# 135 "lib/read.mll"
        `Intlit (lexeme lexbuf)


  
# 141 "lib/read.mll"
  let set_file_name v fname =
    v.fname <- fname

  let newline v lexbuf =
    v.lnum <- v.lnum + 1;
    v.bol <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos

  let add_lexeme buf lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_subbytes buf lexbuf.lex_buffer lexbuf.lex_start_pos len

  let map_lexeme f lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    f (Bytes.to_string lexbuf.lex_buffer) lexbuf.lex_start_pos len

  type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]
  type tuple_kind = [ `Parenthesis | `Square_bracket ]


# 161 "lib/read.ml"
# 161 "lib/read.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\236\255\237\255\003\000\239\255\016\000\242\255\243\255\
    \244\255\245\255\000\000\031\000\249\255\085\000\001\000\000\000\
    \000\000\001\000\000\000\001\000\002\000\255\255\000\000\000\000\
    \003\000\254\255\001\000\004\000\253\255\011\000\252\255\003\000\
    \001\000\003\000\002\000\003\000\000\000\251\255\021\000\097\000\
    \010\000\022\000\020\000\016\000\022\000\012\000\008\000\250\255\
    \119\000\129\000\139\000\161\000\171\000\181\000\193\000\209\000\
    \240\255\011\000\038\000\252\255\065\000\254\255\255\255\110\000\
    \252\255\163\000\254\255\255\255\234\000\247\255\248\255\048\001\
    \250\255\251\255\252\255\253\255\254\255\255\255\071\001\126\001\
    \149\001\249\255\039\000\253\255\254\255\038\000\187\001\210\001\
    \248\001\015\002\255\255\220\000\253\255\255\255\245\000\039\002\
    \109\002\014\001\088\002\164\002\187\002\225\002\013\000\252\255\
    \253\255\254\255\255\255\014\000\253\255\254\255\255\255\030\000\
    \253\255\254\255\255\255\015\000\253\255\254\255\255\255\017\001\
    \251\255\252\255\253\255\254\255\255\255\019\000\252\255\253\255\
    \254\255\015\000\255\255\016\000\255\255\008\001\005\000\253\255\
    \023\000\254\255\020\000\255\255\046\000\253\255\254\255\042\000\
    \052\000\053\000\255\255\053\000\048\000\091\000\092\000\255\255\
    \027\001\250\255\251\255\137\000\104\000\089\000\088\000\106\000\
    \255\255\143\000\137\000\177\000\254\255\183\000\168\000\166\000\
    \183\000\002\000\253\255\177\000\172\000\187\000\004\000\252\255\
    \053\002\251\255\252\255\253\255\103\001\255\255\248\002\254\255\
    \006\003\030\003\252\255\253\255\254\255\255\255\040\003\050\003\
    \074\003\252\255\253\255\254\255\255\255\061\003\084\003\108\003\
    \249\255\250\255\251\255\244\000\120\003\142\003\179\000\194\000\
    \015\000\255\255\190\000\188\000\187\000\193\000\183\000\179\000\
    \254\255\191\000\201\000\200\000\196\000\203\000\193\000\189\000\
    \253\255\157\003\095\003\174\003\196\003\206\003\216\003\228\003\
    \239\003\060\000\253\255\254\255\255\255\012\004\252\255\253\255\
    \087\004\255\255\145\004\252\255\253\255\221\004\255\255\229\000\
    \253\255\254\255\255\255\231\000\253\255\254\255\255\255\002\000\
    \255\255\018\001\252\255\253\255\254\255\255\255\034\001\253\255\
    \254\255\255\255\000\000\255\255\003\000\254\255\255\255\038\001\
    \252\255\253\255\254\255\255\255\120\001\251\255\252\255\253\255\
    \254\255\255\255\208\000\253\255\254\255\255\255\211\000\253\255\
    \254\255\255\255\189\000\255\255\143\001\252\255\253\255\254\255\
    \255\255\013\001\253\255\254\255\255\255\095\001\252\255\253\255\
    \254\255\255\255\050\001\253\255\254\255\255\255\026\001\253\255\
    \254\255\255\255\233\000\253\255\254\255\255\255\222\000\253\255\
    \254\255\255\255\079\005\237\255\238\255\010\000\240\255\044\001\
    \243\255\244\255\245\255\246\255\061\001\002\004\249\255\045\005\
    \209\000\228\000\211\000\232\000\225\000\223\000\240\000\255\255\
    \235\000\234\000\008\001\254\255\004\001\023\001\253\255\054\001\
    \252\255\031\001\029\001\032\001\039\001\049\001\045\001\251\255\
    \057\001\082\001\080\001\078\001\084\001\074\001\086\001\250\255\
    \110\005\012\004\123\005\155\005\165\005\177\005\187\005\197\005\
    \241\255\199\001\077\002\253\255\255\255\154\002\222\005\209\005\
    \155\002\239\005\053\006\076\006\114\006\016\002\252\255\253\255\
    \254\255\255\255\152\006\252\255\253\255\227\006\255\255\085\007\
    \244\255\245\255\011\000\247\255\076\002\250\255\251\255\252\255\
    \253\255\254\255\031\002\243\005\051\007\100\001\115\001\104\001\
    \133\001\118\001\154\001\171\001\255\255\173\001\176\001\191\001\
    \185\001\187\001\253\001\230\001\230\001\234\001\247\001\237\001\
    \234\001\009\002\019\002\019\002\015\002\021\002\011\002\007\002\
    \142\006\152\006\116\007\170\007\180\007\190\007\200\007\210\007\
    \248\255\120\002\167\002\253\255\255\255\216\002\082\007\220\007\
    \236\002\244\007\058\008\081\008\119\008\076\002\252\255\253\255\
    \254\255\255\255\157\008\252\255\253\255\232\008\255\255\135\002\
    \120\002\253\255\100\002\254\255\182\002\255\255\011\002\255\255\
    \204\002\252\255\253\255\254\255\255\255\046\002\255\255\178\002\
    \252\255\253\255\254\255\255\255\023\000\255\255\183\002\252\255\
    \253\255\254\255\255\255\187\002\253\255\254\255\255\255\121\002\
    \253\255\254\255\255\255\184\002\252\255\253\255\254\255\019\000\
    \255\255\140\001\146\001\255\255\150\001\151\001\154\001\168\001\
    \170\001\171\001\172\001\173\001\181\001\184\001\185\001\187\001\
    \191\001\193\001\195\001\196\001\197\001\200\001\203\001\223\001\
    \225\001\228\001\249\001\251\001\002\002\004\002\011\002\012\002\
    \013\002\000\000";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\017\000\255\255\019\000\255\255\255\255\
    \255\255\255\255\007\000\007\000\255\255\019\000\019\000\019\000\
    \019\000\019\000\019\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\008\000\008\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\009\000\255\255\009\000\255\255\009\000\255\255\
    \255\255\014\000\255\255\255\255\002\000\255\255\255\255\255\255\
    \255\255\002\000\255\255\255\255\255\255\255\255\255\255\007\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\001\000\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\255\255\001\000\255\255\004\000\003\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\
    \255\255\255\255\255\255\001\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\004\000\004\000\004\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\255\255\000\000\255\255\
    \001\000\255\255\255\255\255\255\255\255\255\255\000\000\002\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\002\000\255\255\
    \255\255\255\255\255\255\003\000\003\000\005\000\005\000\005\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\003\000\255\255\003\000\255\255\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \001\000\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\002\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\255\255\018\000\
    \255\255\255\255\255\255\255\255\007\000\007\000\255\255\018\000\
    \018\000\018\000\018\000\018\000\018\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\008\000\255\255\008\000\255\255\008\000\255\255\
    \255\255\013\000\255\255\255\255\255\255\001\000\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\009\000\255\255\011\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\255\255\000\000\255\255\
    \255\255\006\000\255\255\255\255\255\255\001\000\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\004\000\
    \003\000\255\255\255\255\255\255\255\255\255\255\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\001\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\000\000\255\255\000\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\057\000\060\000\000\000\060\000\000\000\000\000\065\000\
    \000\000\065\000\000\000\000\000\070\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\084\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\094\000\000\000\000\000\097\000\255\255\
    \255\255\097\000\255\255\255\255\255\255\255\255\104\000\000\000\
    \000\000\000\000\000\000\109\000\000\000\000\000\000\000\113\000\
    \000\000\000\000\000\000\117\000\000\000\000\000\000\000\121\000\
    \000\000\000\000\000\000\000\000\000\000\126\000\000\000\000\000\
    \000\000\255\255\000\000\255\255\000\000\255\255\255\255\000\000\
    \255\255\000\000\138\000\000\000\142\000\000\000\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \154\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \178\000\000\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \255\255\187\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \194\000\000\000\000\000\000\000\000\000\255\255\255\255\201\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\235\000\000\000\000\000\000\000\239\000\000\000\000\000\
    \255\255\000\000\244\000\000\000\000\000\255\255\000\000\249\000\
    \000\000\000\000\000\000\253\000\000\000\000\000\000\000\255\255\
    \000\000\003\001\000\000\000\000\000\000\000\000\008\001\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\017\001\
    \000\000\000\000\000\000\000\000\022\001\000\000\000\000\000\000\
    \000\000\000\000\028\001\000\000\000\000\000\000\032\001\000\000\
    \000\000\000\000\255\255\000\000\038\001\000\000\000\000\000\000\
    \000\000\043\001\000\000\000\000\000\000\047\001\000\000\000\000\
    \000\000\000\000\052\001\000\000\000\000\000\000\056\001\000\000\
    \000\000\000\000\060\001\000\000\000\000\000\000\064\001\000\000\
    \000\000\000\000\067\001\000\000\000\000\255\255\000\000\255\255\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\121\001\125\001\000\000\000\000\128\001\255\255\255\255\
    \128\001\255\255\255\255\255\255\255\255\135\001\000\000\000\000\
    \000\000\000\000\140\001\000\000\000\000\255\255\000\000\144\001\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\193\001\197\001\000\000\000\000\200\001\255\255\255\255\
    \200\001\255\255\255\255\255\255\255\255\207\001\000\000\000\000\
    \000\000\000\000\212\001\000\000\000\000\255\255\000\000\255\255\
    \255\255\000\000\255\255\000\000\220\001\000\000\255\255\000\000\
    \226\001\000\000\000\000\000\000\000\000\255\255\000\000\233\001\
    \000\000\000\000\000\000\000\000\255\255\000\000\240\001\000\000\
    \000\000\000\000\000\000\245\001\000\000\000\000\000\000\249\001\
    \000\000\000\000\000\000\252\001\000\000\000\000\000\000\255\255\
    \000\000\002\002\004\002\000\000\005\002\006\002\007\002\008\002\
    \009\002\010\002\011\002\012\002\013\002\014\002\015\002\016\002\
    \017\002\018\002\019\002\020\002\021\002\022\002\023\002\024\002\
    \025\002\026\002\027\002\028\002\029\002\030\002\031\002\032\002\
    \033\002\003\002";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\004\000\000\000\003\000\003\000\134\000\000\000\
    \003\000\000\000\134\000\069\001\146\001\255\255\000\000\069\001\
    \146\001\000\000\000\000\000\000\000\000\127\000\139\000\000\000\
    \003\000\000\000\012\000\003\000\170\000\134\000\175\000\000\000\
    \007\000\011\001\069\001\146\001\014\001\013\000\049\000\005\000\
    \010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\056\000\118\000\006\000\129\000\130\000\057\000\
    \237\001\137\000\000\002\049\000\000\000\048\000\138\000\106\000\
    \062\000\014\000\110\000\105\000\000\000\049\000\015\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\030\000\048\000\008\000\114\000\209\000\236\000\000\001\
    \013\001\029\000\022\000\255\255\048\000\048\000\017\000\021\000\
    \025\000\032\000\033\000\035\000\023\000\027\000\016\000\031\000\
    \028\000\034\000\019\000\024\000\018\000\026\000\020\000\036\000\
    \041\000\037\000\048\000\009\000\042\000\043\000\044\000\045\000\
    \046\000\047\000\061\000\085\000\048\000\038\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\049\000\
    \067\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\086\000\143\000\255\255\040\000\144\000\
    \145\000\146\000\055\000\148\000\055\000\149\000\048\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\255\255\048\000\150\000\
    \151\000\161\000\066\000\158\000\053\000\159\000\053\000\160\000\
    \051\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\165\000\
    \051\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\162\000\163\000\166\000\093\000\255\255\
    \002\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\255\255\077\000\103\000\108\000\116\000\
    \132\000\134\000\135\000\128\000\139\000\134\000\164\000\093\000\
    \171\000\077\000\167\000\168\000\169\000\172\000\112\000\173\000\
    \174\000\210\000\226\000\208\000\211\000\212\000\059\000\083\000\
    \134\000\213\000\214\000\215\000\216\000\218\000\141\000\219\000\
    \093\000\220\000\221\000\123\000\222\000\223\000\224\000\136\000\
    \095\000\225\000\035\001\065\001\234\000\155\000\005\001\097\001\
    \250\000\255\255\254\000\057\001\061\001\095\001\077\000\044\001\
    \092\001\088\001\009\001\029\001\076\000\124\000\033\001\018\001\
    \075\000\098\000\019\001\085\001\086\001\087\001\120\001\089\001\
    \074\000\225\000\053\001\121\001\073\000\090\001\072\000\071\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\098\000\113\001\122\000\091\001\064\000\004\001\
    \093\001\078\000\078\000\078\000\078\000\078\000\078\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\156\000\112\001\094\001\096\001\098\001\099\001\049\001\
    \079\000\079\000\079\000\079\000\079\000\079\000\100\001\157\000\
    \101\001\078\000\078\000\078\000\078\000\078\000\078\000\183\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\024\001\112\001\255\255\025\001\102\001\103\001\105\001\
    \079\000\079\000\079\000\079\000\079\000\079\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \106\001\107\001\048\001\040\001\108\001\109\001\110\001\080\000\
    \080\000\080\000\080\000\080\000\080\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\111\001\
    \027\001\255\255\171\001\031\001\170\001\023\001\081\000\081\000\
    \081\000\081\000\081\000\081\000\092\000\168\001\063\001\080\000\
    \080\000\080\000\080\000\080\000\080\000\248\000\165\001\252\000\
    \162\001\059\001\069\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\255\255\081\000\081\000\
    \081\000\081\000\081\000\081\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\039\001\042\001\255\255\163\001\
    \164\001\120\000\002\001\088\000\088\000\088\000\088\000\088\000\
    \088\000\166\001\055\001\153\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\007\001\167\001\164\001\169\001\016\001\164\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\051\001\088\000\088\000\088\000\088\000\088\000\
    \088\000\089\000\089\000\089\000\089\000\089\000\089\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\097\000\137\001\164\001\172\001\185\001\136\001\173\001\
    \090\000\090\000\090\000\090\000\090\000\090\000\097\000\179\000\
    \174\001\089\000\089\000\089\000\089\000\089\000\089\000\046\001\
    \175\001\176\001\180\000\164\001\184\001\181\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\124\001\
    \090\000\090\000\090\000\090\000\090\000\090\000\192\001\178\001\
    \021\001\179\001\097\000\193\001\180\001\181\001\182\001\183\001\
    \164\001\216\001\255\255\097\000\184\001\216\001\209\001\097\000\
    \223\001\097\000\208\001\230\001\003\002\097\000\219\001\037\001\
    \216\001\217\001\003\002\220\001\216\001\097\000\003\002\003\002\
    \216\001\097\000\003\002\097\000\096\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\216\001\
    \003\002\126\001\003\002\003\002\003\002\003\002\099\000\099\000\
    \099\000\099\000\099\000\099\000\097\000\003\002\218\001\250\001\
    \003\002\003\002\097\000\003\002\124\001\124\001\097\000\003\002\
    \221\001\003\002\253\001\003\002\003\002\003\002\097\000\255\255\
    \003\002\196\001\097\000\003\002\097\000\096\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\235\001\003\002\
    \241\001\003\002\255\001\242\001\003\002\100\000\100\000\100\000\
    \100\000\100\000\100\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\246\001\129\001\129\001\
    \228\001\003\002\196\001\003\002\101\000\101\000\101\000\101\000\
    \101\000\101\000\003\002\198\001\003\002\100\000\100\000\100\000\
    \100\000\100\000\100\000\003\002\003\002\003\002\196\001\234\001\
    \134\001\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\000\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\000\000\000\000\201\001\177\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \188\000\000\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \201\001\227\001\000\000\191\000\206\001\123\001\189\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\189\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\195\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\198\000\
    \255\255\248\001\196\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\196\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\202\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\205\000\255\255\255\255\203\000\204\000\204\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\226\000\195\001\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\232\001\000\000\000\000\206\000\221\001\239\001\
    \254\001\000\000\207\000\244\001\000\000\225\000\203\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \232\000\000\000\232\000\000\000\225\001\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\217\000\
    \255\255\000\000\000\000\000\000\000\000\225\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\230\000\
    \000\000\230\000\000\000\228\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\000\000\228\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\186\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\000\000\000\000\000\000\000\000\000\000\241\000\000\000\
    \113\001\000\000\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\114\001\000\000\112\001\
    \000\000\000\000\193\000\000\000\000\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\112\001\
    \000\000\000\000\000\000\240\000\200\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\000\000\246\000\000\000\000\000\240\000\000\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\000\000\000\000\000\000\000\000\
    \245\000\000\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\238\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \000\000\000\000\000\000\000\000\245\000\000\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \069\001\070\001\000\000\000\000\069\001\076\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\069\001\
    \000\000\078\001\000\000\000\000\000\000\000\000\104\001\073\001\
    \000\000\000\000\000\000\000\000\079\001\000\000\071\001\076\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\000\000\000\000\072\001\000\000\000\000\000\000\000\000\
    \000\000\243\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \080\001\119\001\000\000\119\001\000\000\081\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\118\001\
    \000\000\000\000\074\001\114\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\083\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\082\001\000\000\000\000\
    \115\001\000\000\000\000\084\001\000\000\000\000\117\001\000\000\
    \117\001\000\000\075\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\000\000\
    \115\001\116\001\116\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\000\000\
    \128\001\130\001\130\001\130\001\130\001\130\001\130\001\130\001\
    \130\001\130\001\130\001\000\000\000\000\128\001\000\000\000\000\
    \000\000\128\001\130\001\130\001\130\001\130\001\130\001\130\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\128\001\000\000\
    \000\000\185\001\000\000\155\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\000\000\000\000\000\000\
    \000\000\000\000\130\001\130\001\130\001\130\001\130\001\130\001\
    \184\001\000\000\128\001\000\000\000\000\000\000\000\000\000\000\
    \128\001\000\000\000\000\000\000\128\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\128\001\128\001\000\000\000\000\068\001\
    \128\001\128\001\128\001\127\001\000\000\128\001\000\000\000\000\
    \184\001\000\000\000\000\000\000\000\000\128\001\000\000\000\000\
    \000\000\128\001\000\000\128\001\127\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\131\001\131\001\
    \131\001\131\001\131\001\131\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\132\001\132\001\132\001\
    \132\001\132\001\132\001\000\000\000\000\000\000\131\001\131\001\
    \131\001\131\001\131\001\131\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\128\001\128\001\128\001\128\001\128\001\128\001\
    \128\001\128\001\128\001\128\001\000\000\132\001\132\001\132\001\
    \132\001\132\001\132\001\128\001\128\001\128\001\128\001\128\001\
    \128\001\191\001\142\001\191\001\000\000\000\000\190\001\190\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\000\000\128\001\128\001\128\001\128\001\128\001\
    \128\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\000\000\000\000\000\000\000\000\141\001\
    \000\000\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\000\000\000\000\
    \000\000\000\000\141\001\000\000\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\146\001\147\001\
    \000\000\000\000\146\001\154\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\200\001\146\001\000\000\153\001\
    \000\000\000\000\000\000\000\000\177\001\150\001\000\000\000\000\
    \000\000\200\001\156\001\000\000\148\001\154\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\155\001\155\001\000\000\
    \000\000\149\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \139\001\000\000\000\000\000\000\000\000\000\000\157\001\000\000\
    \000\000\000\000\000\000\158\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\200\001\000\000\
    \151\001\000\000\000\000\000\000\200\001\000\000\000\000\000\000\
    \200\001\187\001\000\000\160\001\000\000\000\000\000\000\000\000\
    \200\001\000\000\000\000\159\001\200\001\000\000\200\001\199\001\
    \000\000\161\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \152\001\000\000\000\000\000\000\000\000\189\001\000\000\189\001\
    \000\000\187\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\190\001\190\001\202\001\202\001\202\001\202\001\
    \202\001\202\001\202\001\202\001\202\001\202\001\200\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\202\001\202\001\202\001\
    \202\001\202\001\202\001\200\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\202\001\202\001\202\001\
    \202\001\202\001\202\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \200\001\000\000\000\000\000\000\000\000\145\001\200\001\000\000\
    \000\000\000\000\200\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\200\001\000\000\000\000\000\000\200\001\000\000\
    \200\001\199\001\203\001\203\001\203\001\203\001\203\001\203\001\
    \203\001\203\001\203\001\203\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\203\001\203\001\203\001\203\001\203\001\
    \203\001\204\001\204\001\204\001\204\001\204\001\204\001\204\001\
    \204\001\204\001\204\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\204\001\204\001\204\001\204\001\204\001\204\001\
    \000\000\000\000\000\000\203\001\203\001\203\001\203\001\203\001\
    \203\001\000\000\000\000\000\000\000\000\000\000\000\000\200\001\
    \200\001\200\001\200\001\200\001\200\001\200\001\200\001\200\001\
    \200\001\000\000\204\001\204\001\204\001\204\001\204\001\204\001\
    \200\001\200\001\200\001\200\001\200\001\200\001\000\000\214\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \200\001\200\001\200\001\200\001\200\001\200\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \000\000\000\000\000\000\000\000\213\001\000\000\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\000\000\000\000\000\000\000\000\213\001\
    \000\000\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\211\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\003\000\000\000\134\000\255\255\
    \003\000\255\255\134\000\069\001\146\001\057\000\255\255\069\001\
    \146\001\255\255\255\255\255\255\255\255\125\000\138\000\255\255\
    \000\000\255\255\000\000\003\000\169\000\134\000\174\000\255\255\
    \000\000\010\001\069\001\146\001\012\001\000\000\010\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\005\000\115\000\000\000\125\000\129\000\005\000\
    \236\001\136\000\255\001\038\000\255\255\010\000\136\000\102\000\
    \058\000\000\000\107\000\102\000\255\255\011\000\000\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\029\000\038\000\000\000\111\000\208\000\233\000\255\000\
    \012\001\015\000\017\000\060\000\011\000\010\000\000\000\020\000\
    \024\000\031\000\032\000\034\000\022\000\026\000\000\000\014\000\
    \027\000\033\000\018\000\023\000\000\000\016\000\019\000\035\000\
    \040\000\036\000\038\000\000\000\041\000\042\000\043\000\044\000\
    \045\000\046\000\058\000\082\000\011\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\039\000\
    \063\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\085\000\140\000\060\000\013\000\143\000\
    \144\000\145\000\048\000\147\000\048\000\148\000\039\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\065\000\039\000\149\000\
    \150\000\156\000\063\000\157\000\051\000\158\000\051\000\159\000\
    \050\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\155\000\
    \050\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\161\000\162\000\155\000\091\000\065\000\
    \000\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\057\000\068\000\102\000\107\000\115\000\
    \131\000\133\000\133\000\125\000\138\000\133\000\163\000\094\000\
    \165\000\068\000\166\000\167\000\168\000\171\000\111\000\172\000\
    \173\000\206\000\203\000\207\000\210\000\211\000\058\000\082\000\
    \133\000\212\000\213\000\214\000\215\000\217\000\140\000\218\000\
    \097\000\219\000\220\000\119\000\221\000\222\000\223\000\133\000\
    \091\000\203\000\034\001\062\001\233\000\152\000\001\001\080\001\
    \247\000\060\000\251\000\054\001\058\001\081\001\068\000\041\001\
    \082\001\083\001\006\001\026\001\068\000\119\000\030\001\015\001\
    \068\000\094\000\015\001\084\001\085\001\086\001\071\001\088\001\
    \068\000\203\000\050\001\071\001\068\000\089\001\068\000\068\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\097\000\076\001\119\000\090\001\063\000\001\001\
    \092\001\071\000\071\000\071\000\071\000\071\000\071\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\152\000\076\001\093\001\095\001\097\001\098\001\045\001\
    \078\000\078\000\078\000\078\000\078\000\078\000\099\001\152\000\
    \100\001\071\000\071\000\071\000\071\000\071\000\071\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\020\001\076\001\065\000\020\001\101\001\102\001\104\001\
    \078\000\078\000\078\000\078\000\078\000\078\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \105\001\106\001\045\001\036\001\107\001\108\001\109\001\079\000\
    \079\000\079\000\079\000\079\000\079\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\110\001\
    \026\001\121\001\157\001\030\001\158\001\020\001\080\000\080\000\
    \080\000\080\000\080\000\080\000\091\000\159\001\062\001\079\000\
    \079\000\079\000\079\000\079\000\079\000\247\000\160\001\251\000\
    \161\001\058\001\068\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\094\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\036\001\041\001\097\000\162\001\
    \163\001\119\000\001\001\087\000\087\000\087\000\087\000\087\000\
    \087\000\165\001\054\001\152\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\006\001\166\001\167\001\168\001\015\001\169\001\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\050\001\087\000\087\000\087\000\087\000\087\000\
    \087\000\088\000\088\000\088\000\088\000\088\000\088\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\095\000\133\001\170\001\171\001\154\001\133\001\172\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\095\000\176\000\
    \173\001\088\000\088\000\088\000\088\000\088\000\088\000\045\001\
    \174\001\175\001\176\000\176\001\154\001\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\122\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\148\001\177\001\
    \020\001\178\001\098\000\148\001\179\001\180\001\181\001\182\001\
    \183\001\216\001\193\001\095\000\154\001\216\001\205\001\098\000\
    \222\001\095\000\205\001\229\001\001\002\095\000\218\001\036\001\
    \215\001\215\001\002\002\218\001\215\001\095\000\004\002\005\002\
    \216\001\095\000\006\002\095\000\095\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\215\001\
    \007\002\122\001\008\002\009\002\010\002\011\002\096\000\096\000\
    \096\000\096\000\096\000\096\000\098\000\012\002\215\001\247\001\
    \013\002\014\002\098\000\015\002\125\001\128\001\098\000\016\002\
    \220\001\017\002\251\001\018\002\019\002\020\002\098\000\121\001\
    \021\002\194\001\098\000\022\002\098\000\098\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\231\001\023\002\
    \238\001\024\002\251\001\238\001\025\002\099\000\099\000\099\000\
    \099\000\099\000\099\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\243\001\125\001\128\001\
    \224\001\026\002\197\001\027\002\100\000\100\000\100\000\100\000\
    \100\000\100\000\028\002\194\001\029\002\099\000\099\000\099\000\
    \099\000\099\000\099\000\030\002\031\002\032\002\200\001\231\001\
    \133\001\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\255\255\100\000\100\000\100\000\100\000\
    \100\000\100\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\255\255\255\255\197\001\176\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \185\000\255\255\101\000\101\000\101\000\101\000\101\000\101\000\
    \200\001\224\001\255\255\185\000\205\001\122\001\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\192\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\192\000\
    \193\001\247\001\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\198\000\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\199\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\199\000\125\001\128\001\199\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\204\000\194\001\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\231\001\255\255\255\255\199\000\220\001\238\001\
    \251\001\255\255\199\000\243\001\255\255\204\000\205\000\205\000\
    \205\000\205\000\205\000\205\000\205\000\205\000\205\000\205\000\
    \225\000\255\255\225\000\255\255\224\001\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\225\000\225\000\225\000\205\000\
    \197\001\255\255\255\255\255\255\255\255\204\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \255\255\255\255\255\255\255\255\200\001\255\255\255\255\228\000\
    \255\255\228\000\255\255\227\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\255\255\227\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\185\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\255\255\255\255\255\255\255\255\255\255\237\000\255\255\
    \077\001\255\255\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\113\001\113\001\113\001\113\001\
    \113\001\113\001\113\001\113\001\113\001\113\001\255\255\077\001\
    \255\255\255\255\192\000\255\255\255\255\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\077\001\
    \255\255\255\255\255\255\237\000\199\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\255\255\242\000\255\255\255\255\240\000\255\255\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\255\255\255\255\255\255\255\255\
    \242\000\255\255\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\237\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \255\255\255\255\255\255\255\255\245\000\255\255\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \066\001\066\001\255\255\255\255\066\001\079\001\079\001\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\066\001\
    \255\255\066\001\255\255\255\255\255\255\255\255\079\001\066\001\
    \255\255\255\255\255\255\255\255\066\001\255\255\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\255\255\255\255\066\001\255\255\255\255\255\255\255\255\
    \255\255\242\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \066\001\112\001\255\255\112\001\255\255\066\001\112\001\112\001\
    \112\001\112\001\112\001\112\001\112\001\112\001\112\001\112\001\
    \255\255\255\255\066\001\114\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\066\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\066\001\255\255\255\255\
    \114\001\255\255\255\255\066\001\255\255\255\255\115\001\255\255\
    \115\001\255\255\066\001\115\001\115\001\115\001\115\001\115\001\
    \115\001\115\001\115\001\115\001\115\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\255\255\
    \114\001\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\117\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\119\001\119\001\119\001\
    \119\001\119\001\119\001\119\001\119\001\119\001\119\001\255\255\
    \126\001\127\001\127\001\127\001\127\001\127\001\127\001\127\001\
    \127\001\127\001\127\001\255\255\255\255\126\001\255\255\255\255\
    \255\255\129\001\127\001\127\001\127\001\127\001\127\001\127\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\129\001\255\255\
    \255\255\155\001\255\255\155\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\255\255\255\255\255\255\
    \255\255\255\255\127\001\127\001\127\001\127\001\127\001\127\001\
    \155\001\255\255\126\001\255\255\255\255\255\255\255\255\255\255\
    \126\001\255\255\255\255\255\255\126\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\129\001\126\001\255\255\255\255\066\001\
    \126\001\129\001\126\001\126\001\255\255\129\001\255\255\255\255\
    \155\001\255\255\255\255\255\255\255\255\129\001\255\255\255\255\
    \255\255\129\001\255\255\129\001\129\001\130\001\130\001\130\001\
    \130\001\130\001\130\001\130\001\130\001\130\001\130\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\130\001\130\001\
    \130\001\130\001\130\001\130\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\131\001\131\001\131\001\
    \131\001\131\001\131\001\255\255\255\255\255\255\130\001\130\001\
    \130\001\130\001\130\001\130\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\255\255\131\001\131\001\131\001\
    \131\001\131\001\131\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\184\001\138\001\184\001\255\255\255\255\184\001\184\001\
    \184\001\184\001\184\001\184\001\184\001\184\001\184\001\184\001\
    \185\001\185\001\185\001\185\001\185\001\185\001\185\001\185\001\
    \185\001\185\001\255\255\132\001\132\001\132\001\132\001\132\001\
    \132\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\255\255\255\255\255\255\255\255\138\001\
    \255\255\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\255\255\255\255\
    \255\255\255\255\141\001\255\255\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\143\001\143\001\
    \255\255\255\255\143\001\156\001\156\001\156\001\156\001\156\001\
    \156\001\156\001\156\001\156\001\156\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\198\001\143\001\255\255\143\001\
    \255\255\255\255\255\255\255\255\156\001\143\001\255\255\255\255\
    \255\255\198\001\143\001\255\255\143\001\143\001\143\001\143\001\
    \143\001\143\001\143\001\143\001\143\001\143\001\143\001\255\255\
    \255\255\143\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \138\001\255\255\255\255\255\255\255\255\255\255\143\001\255\255\
    \255\255\255\255\255\255\143\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\198\001\255\255\
    \143\001\255\255\255\255\255\255\198\001\255\255\255\255\255\255\
    \198\001\186\001\255\255\143\001\255\255\255\255\255\255\255\255\
    \198\001\255\255\255\255\143\001\198\001\255\255\198\001\198\001\
    \255\255\143\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \143\001\255\255\255\255\255\255\255\255\187\001\255\255\187\001\
    \255\255\186\001\187\001\187\001\187\001\187\001\187\001\187\001\
    \187\001\187\001\187\001\187\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\189\001\189\001\
    \189\001\189\001\189\001\189\001\189\001\189\001\189\001\189\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\191\001\191\001\191\001\191\001\191\001\191\001\
    \191\001\191\001\191\001\191\001\199\001\199\001\199\001\199\001\
    \199\001\199\001\199\001\199\001\199\001\199\001\201\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\199\001\199\001\199\001\
    \199\001\199\001\199\001\201\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\199\001\199\001\199\001\
    \199\001\199\001\199\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \201\001\255\255\255\255\255\255\255\255\143\001\201\001\255\255\
    \255\255\255\255\201\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\201\001\255\255\255\255\255\255\201\001\255\255\
    \201\001\201\001\202\001\202\001\202\001\202\001\202\001\202\001\
    \202\001\202\001\202\001\202\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\202\001\202\001\202\001\202\001\202\001\
    \202\001\203\001\203\001\203\001\203\001\203\001\203\001\203\001\
    \203\001\203\001\203\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\203\001\203\001\203\001\203\001\203\001\203\001\
    \255\255\255\255\255\255\202\001\202\001\202\001\202\001\202\001\
    \202\001\255\255\255\255\255\255\255\255\255\255\255\255\204\001\
    \204\001\204\001\204\001\204\001\204\001\204\001\204\001\204\001\
    \204\001\255\255\203\001\203\001\203\001\203\001\203\001\203\001\
    \204\001\204\001\204\001\204\001\204\001\204\001\255\255\210\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \204\001\204\001\204\001\204\001\204\001\204\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \255\255\255\255\255\255\255\255\210\001\255\255\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\255\255\255\255\255\255\255\255\213\001\
    \255\255\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\210\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read_json v lexbuf =
   __ocaml_lex_read_json_rec v lexbuf 0
and __ocaml_lex_read_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 188 "lib/read.mll"
                
# 188 "lib/read.mll"
                ( `Bool true )

# 1032 "lib/read.ml"

  
# 1033 "lib/read.ml"
  | 1 ->

# 189 "lib/read.mll"
                
# 189 "lib/read.mll"
                ( `Bool false )

# 1037 "lib/read.ml"

  
# 1038 "lib/read.ml"
  | 2 ->

# 190 "lib/read.mll"
                
# 190 "lib/read.mll"
                ( `Null )

# 1042 "lib/read.ml"

  
# 1043 "lib/read.ml"
  | 3 ->

# 191 "lib/read.mll"
                
# 191 "lib/read.mll"
                (
                    
# 195 "lib/read.mll"
                    `Floatlit "NaN"
                
# 197 "lib/read.mll"
                )

# 1053 "lib/read.ml"

  
# 1054 "lib/read.ml"
  | 4 ->

# 198 "lib/read.mll"
                
# 198 "lib/read.mll"
                (
                    
# 202 "lib/read.mll"
                    `Floatlit "Infinity"
                
# 204 "lib/read.mll"
                )

# 1064 "lib/read.ml"

  
# 1065 "lib/read.ml"
  | 5 ->

# 205 "lib/read.mll"
                
# 205 "lib/read.mll"
                (
                    
# 209 "lib/read.mll"
                    `Floatlit "-Infinity"
                
# 211 "lib/read.mll"
                )

# 1075 "lib/read.ml"

  
# 1076 "lib/read.ml"
  | 6 ->

# 212 "lib/read.mll"
                
# 212 "lib/read.mll"
                (
                    
# 217 "lib/read.mll"
                    `Stringlit (finish_stringlit v lexbuf)
                
# 219 "lib/read.mll"
                )

# 1087 "lib/read.ml"

  
# 1088 "lib/read.ml"
  | 7 ->

# 220 "lib/read.mll"
                         
# 220 "lib/read.mll"
                         ( make_positive_int v lexbuf )

# 1092 "lib/read.ml"

  
# 1093 "lib/read.ml"
  | 8 ->

# 221 "lib/read.mll"
                         
# 221 "lib/read.mll"
                         ( make_negative_int v lexbuf )

# 1097 "lib/read.ml"

  
# 1098 "lib/read.ml"
  | 9 ->

# 222 "lib/read.mll"
                
# 222 "lib/read.mll"
                (
                    
# 226 "lib/read.mll"
                    `Floatlit (lexeme lexbuf)
                 
# 228 "lib/read.mll"
                 )

# 1108 "lib/read.ml"

  
# 1109 "lib/read.ml"
  | 10 ->

# 230 "lib/read.mll"
                 
# 230 "lib/read.mll"
                 ( let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     let field_name = read_ident v lexbuf in
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     acc := (field_name, read_json v lexbuf) :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       let field_name = read_ident v lexbuf in
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       acc := (field_name, read_json v lexbuf) :: !acc;
                     done;
                     assert false
                   with End_of_object ->
                     `Assoc (List.rev !acc)
                 )

# 1135 "lib/read.ml"

  
# 1136 "lib/read.ml"
  | 11 ->

# 254 "lib/read.mll"
                 
# 254 "lib/read.mll"
                 ( let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     acc := read_json v lexbuf :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       acc := read_json v lexbuf :: !acc;
                     done;
                     assert false
                   with End_of_array ->
                     `List (List.rev !acc)
                 )

# 1154 "lib/read.ml"

  
# 1155 "lib/read.ml"
  | 12 ->

# 270 "lib/read.mll"
                 
# 270 "lib/read.mll"
                 (
                     
# 272 "lib/read.mll"
                     let acc = ref [] in
                     try
                       read_space v lexbuf;
                       read_tuple_end lexbuf;
                       acc := read_json v lexbuf :: !acc;
                       while true do
                         read_space v lexbuf;
                         read_tuple_sep v lexbuf;
                         read_space v lexbuf;
                         acc := read_json v lexbuf :: !acc;
                       done;
                       assert false
                     with End_of_tuple ->
                       `Tuple (List.rev !acc)
                 
# 289 "lib/read.mll"
                 )

# 1178 "lib/read.ml"

  
# 1179 "lib/read.ml"
  | 13 ->

# 291 "lib/read.mll"
                 
# 291 "lib/read.mll"
                 (
                     
# 293 "lib/read.mll"
                     read_space v lexbuf;
                     let cons = read_ident v lexbuf in
                     read_space v lexbuf;
                     `Variant (cons, finish_variant v lexbuf)
                 
# 300 "lib/read.mll"
                 )

# 1192 "lib/read.ml"

  
# 1193 "lib/read.ml"
  | 14 ->

# 302 "lib/read.mll"
                 
# 302 "lib/read.mll"
                 ( read_json v lexbuf )

# 1197 "lib/read.ml"

  
# 1198 "lib/read.ml"
  | 15 ->

# 303 "lib/read.mll"
                 
# 303 "lib/read.mll"
                 ( finish_comment v lexbuf; read_json v lexbuf )

# 1202 "lib/read.ml"

  
# 1203 "lib/read.ml"
  | 16 ->

# 304 "lib/read.mll"
                 
# 304 "lib/read.mll"
                 ( newline v lexbuf; read_json v lexbuf )

# 1207 "lib/read.ml"

  
# 1208 "lib/read.ml"
  | 17 ->

# 305 "lib/read.mll"
                 
# 305 "lib/read.mll"
                 ( read_json v lexbuf )

# 1212 "lib/read.ml"

  
# 1213 "lib/read.ml"
  | 18 ->

# 306 "lib/read.mll"
                 
# 306 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 1217 "lib/read.ml"

  
# 1218 "lib/read.ml"
  | 19 ->

# 307 "lib/read.mll"
                 
# 307 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 1222 "lib/read.ml"

  
# 1223 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_json_rec v lexbuf __ocaml_lex_state

and finish_string v lexbuf =
   __ocaml_lex_finish_string_rec v lexbuf 58
and __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 311 "lib/read.mll"
                  
# 311 "lib/read.mll"
                  ( Buffer.contents v.buf )

# 1234 "lib/read.ml"

  
# 1235 "lib/read.ml"
  | 1 ->

# 312 "lib/read.mll"
                  
# 312 "lib/read.mll"
                  ( finish_escaped_char v lexbuf;
                    finish_string v lexbuf )

# 1240 "lib/read.ml"

  
# 1241 "lib/read.ml"
  | 2 ->

# 314 "lib/read.mll"
                  
# 314 "lib/read.mll"
                  ( add_lexeme v.buf lexbuf;
                    finish_string v lexbuf )

# 1246 "lib/read.ml"

  
# 1247 "lib/read.ml"
  | 3 ->

# 316 "lib/read.mll"
                  
# 316 "lib/read.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )

# 1251 "lib/read.ml"

  
# 1252 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state

and map_string v f lexbuf =
   __ocaml_lex_map_string_rec v f lexbuf 63
and __ocaml_lex_map_string_rec v f lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 319 "lib/read.mll"
                  
# 319 "lib/read.mll"
                  ( let b = v.buf in
                    f (Buffer.contents b) 0 (Buffer.length b) )

# 1264 "lib/read.ml"

  
# 1265 "lib/read.ml"
  | 1 ->

# 321 "lib/read.mll"
                  
# 321 "lib/read.mll"
                  ( finish_escaped_char v lexbuf;
                    map_string v f lexbuf )

# 1270 "lib/read.ml"

  
# 1271 "lib/read.ml"
  | 2 ->

# 323 "lib/read.mll"
                  
# 323 "lib/read.mll"
                  ( add_lexeme v.buf lexbuf;
                    map_string v f lexbuf )

# 1276 "lib/read.ml"

  
# 1277 "lib/read.ml"
  | 3 ->

# 325 "lib/read.mll"
                  
# 325 "lib/read.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )

# 1281 "lib/read.ml"

  
# 1282 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_map_string_rec v f lexbuf __ocaml_lex_state

and finish_escaped_char v lexbuf =
   __ocaml_lex_finish_escaped_char_rec v lexbuf 68
and __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let

# 330 "lib/read.mll"
           
# 330 "lib/read.mll"
           c

# 1294 "lib/read.ml"
# 1294 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in

# 330 "lib/read.mll"
             
# 330 "lib/read.mll"
             ( Buffer.add_char v.buf c )

# 1298 "lib/read.ml"

  
# 1299 "lib/read.ml"
  | 1 ->

# 331 "lib/read.mll"
         
# 331 "lib/read.mll"
         ( Buffer.add_char v.buf '\b' )

# 1303 "lib/read.ml"

  
# 1304 "lib/read.ml"
  | 2 ->

# 332 "lib/read.mll"
         
# 332 "lib/read.mll"
         ( Buffer.add_char v.buf '\012' )

# 1308 "lib/read.ml"

  
# 1309 "lib/read.ml"
  | 3 ->

# 333 "lib/read.mll"
         
# 333 "lib/read.mll"
         ( Buffer.add_char v.buf '\n' )

# 1313 "lib/read.ml"

  
# 1314 "lib/read.ml"
  | 4 ->

# 334 "lib/read.mll"
         
# 334 "lib/read.mll"
         ( Buffer.add_char v.buf '\r' )

# 1318 "lib/read.ml"

  
# 1319 "lib/read.ml"
  | 5 ->

# 335 "lib/read.mll"
         
# 335 "lib/read.mll"
         ( Buffer.add_char v.buf '\t' )

# 1323 "lib/read.ml"

  
# 1324 "lib/read.ml"
  | 6 ->
let

# 336 "lib/read.mll"
                
# 336 "lib/read.mll"
                a

# 1329 "lib/read.ml"
# 1329 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and

# 336 "lib/read.mll"
                           
# 336 "lib/read.mll"
                           b

# 1334 "lib/read.ml"
# 1334 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and

# 336 "lib/read.mll"
                                      
# 336 "lib/read.mll"
                                      c

# 1339 "lib/read.ml"
# 1339 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and

# 336 "lib/read.mll"
                                                 
# 336 "lib/read.mll"
                                                 d

# 1344 "lib/read.ml"
# 1344 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4) in

# 337 "lib/read.mll"
         
# 337 "lib/read.mll"
         ( let x =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if x >= 0xD800 && x <= 0xDBFF then
             finish_surrogate_pair v x lexbuf
           else
             utf8_of_code v.buf x
         )

# 1355 "lib/read.ml"

  
# 1356 "lib/read.ml"
  | 7 ->

# 345 "lib/read.mll"
         
# 345 "lib/read.mll"
         ( long_error "Invalid escape sequence" v lexbuf )

# 1360 "lib/read.ml"

  
# 1361 "lib/read.ml"
  | 8 ->

# 346 "lib/read.mll"
         
# 346 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1365 "lib/read.ml"

  
# 1366 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state

and finish_surrogate_pair v x lexbuf =
   __ocaml_lex_finish_surrogate_pair_rec v x lexbuf 82
and __ocaml_lex_finish_surrogate_pair_rec v x lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let

# 349 "lib/read.mll"
                  
# 349 "lib/read.mll"
                  a

# 1378 "lib/read.ml"
# 1378 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and

# 349 "lib/read.mll"
                             
# 349 "lib/read.mll"
                             b

# 1383 "lib/read.ml"
# 1383 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and

# 349 "lib/read.mll"
                                        
# 349 "lib/read.mll"
                                        c

# 1388 "lib/read.ml"
# 1388 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4)
and

# 349 "lib/read.mll"
                                                   
# 349 "lib/read.mll"
                                                   d

# 1393 "lib/read.ml"
# 1393 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 5) in

# 350 "lib/read.mll"
         
# 350 "lib/read.mll"
         ( let y =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if y >= 0xDC00 && y <= 0xDFFF then
             utf8_of_surrogate_pair v.buf x y
           else
             long_error "Invalid low surrogate for code point beyond U+FFFF"
               v lexbuf
         )

# 1405 "lib/read.ml"

  
# 1406 "lib/read.ml"
  | 1 ->

# 359 "lib/read.mll"
         
# 359 "lib/read.mll"
         ( long_error "Missing escape sequence representing low surrogate \
                       for code point beyond U+FFFF" v lexbuf )

# 1411 "lib/read.ml"

  
# 1412 "lib/read.ml"
  | 2 ->

# 361 "lib/read.mll"
         
# 361 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1416 "lib/read.ml"

  
# 1417 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_surrogate_pair_rec v x lexbuf __ocaml_lex_state

and finish_stringlit v lexbuf =
   __ocaml_lex_finish_stringlit_rec v lexbuf 91
and __ocaml_lex_finish_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 366 "lib/read.mll"
         
# 366 "lib/read.mll"
         ( let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
           let s = Bytes.create (len+1) in
           Bytes.set s 0 '"';
           Bytes.blit lexbuf.lex_buffer lexbuf.lex_start_pos s 1 len;
           Bytes.to_string s
         )

# 1433 "lib/read.ml"

  
# 1434 "lib/read.ml"
  | 1 ->

# 372 "lib/read.mll"
         
# 372 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 1438 "lib/read.ml"

  
# 1439 "lib/read.ml"
  | 2 ->

# 373 "lib/read.mll"
         
# 373 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1443 "lib/read.ml"

  
# 1444 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_stringlit_rec v lexbuf __ocaml_lex_state

and finish_variant v lexbuf =
   __ocaml_lex_finish_variant_rec v lexbuf 102
and __ocaml_lex_finish_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 376 "lib/read.mll"
         
# 376 "lib/read.mll"
         ( let x = read_json v lexbuf in
           read_space v lexbuf;
           read_gt v lexbuf;
           Some x )

# 1458 "lib/read.ml"

  
# 1459 "lib/read.ml"
  | 1 ->

# 380 "lib/read.mll"
         
# 380 "lib/read.mll"
         ( None )

# 1463 "lib/read.ml"

  
# 1464 "lib/read.ml"
  | 2 ->

# 381 "lib/read.mll"
         
# 381 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 1468 "lib/read.ml"

  
# 1469 "lib/read.ml"
  | 3 ->

# 382 "lib/read.mll"
         
# 382 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1473 "lib/read.ml"

  
# 1474 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_variant_rec v lexbuf __ocaml_lex_state

and read_lt v lexbuf =
   __ocaml_lex_read_lt_rec v lexbuf 107
and __ocaml_lex_read_lt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 385 "lib/read.mll"
             
# 385 "lib/read.mll"
             ( () )

# 1485 "lib/read.ml"

  
# 1486 "lib/read.ml"
  | 1 ->

# 386 "lib/read.mll"
             
# 386 "lib/read.mll"
             ( long_error "Expected '<' but found" v lexbuf )

# 1490 "lib/read.ml"

  
# 1491 "lib/read.ml"
  | 2 ->

# 387 "lib/read.mll"
             
# 387 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1495 "lib/read.ml"

  
# 1496 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lt_rec v lexbuf __ocaml_lex_state

and read_gt v lexbuf =
   __ocaml_lex_read_gt_rec v lexbuf 111
and __ocaml_lex_read_gt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 390 "lib/read.mll"
         
# 390 "lib/read.mll"
         ( () )

# 1507 "lib/read.ml"

  
# 1508 "lib/read.ml"
  | 1 ->

# 391 "lib/read.mll"
         
# 391 "lib/read.mll"
         ( long_error "Expected '>' but found" v lexbuf )

# 1512 "lib/read.ml"

  
# 1513 "lib/read.ml"
  | 2 ->

# 392 "lib/read.mll"
         
# 392 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1517 "lib/read.ml"

  
# 1518 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_gt_rec v lexbuf __ocaml_lex_state

and read_comma v lexbuf =
   __ocaml_lex_read_comma_rec v lexbuf 115
and __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 395 "lib/read.mll"
         
# 395 "lib/read.mll"
         ( () )

# 1529 "lib/read.ml"

  
# 1530 "lib/read.ml"
  | 1 ->

# 396 "lib/read.mll"
         
# 396 "lib/read.mll"
         ( long_error "Expected ',' but found" v lexbuf )

# 1534 "lib/read.ml"

  
# 1535 "lib/read.ml"
  | 2 ->

# 397 "lib/read.mll"
         
# 397 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1539 "lib/read.ml"

  
# 1540 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state

and start_any_variant v lexbuf =
   __ocaml_lex_start_any_variant_rec v lexbuf 119
and __ocaml_lex_start_any_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 400 "lib/read.mll"
             
# 400 "lib/read.mll"
             ( `Edgy_bracket )

# 1551 "lib/read.ml"

  
# 1552 "lib/read.ml"
  | 1 ->

# 401 "lib/read.mll"
             
# 401 "lib/read.mll"
             ( Buffer.clear v.buf;
               `Double_quote )

# 1557 "lib/read.ml"

  
# 1558 "lib/read.ml"
  | 2 ->

# 403 "lib/read.mll"
             
# 403 "lib/read.mll"
             ( `Square_bracket )

# 1562 "lib/read.ml"

  
# 1563 "lib/read.ml"
  | 3 ->

# 404 "lib/read.mll"
             
# 404 "lib/read.mll"
             ( long_error "Expected '<', '\"' or '[' but found" v lexbuf )

# 1567 "lib/read.ml"

  
# 1568 "lib/read.ml"
  | 4 ->

# 405 "lib/read.mll"
             
# 405 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1572 "lib/read.ml"

  
# 1573 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_start_any_variant_rec v lexbuf __ocaml_lex_state

and finish_comment v lexbuf =
   __ocaml_lex_finish_comment_rec v lexbuf 125
and __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 408 "lib/read.mll"
         
# 408 "lib/read.mll"
         ( () )

# 1584 "lib/read.ml"

  
# 1585 "lib/read.ml"
  | 1 ->

# 409 "lib/read.mll"
         
# 409 "lib/read.mll"
         ( long_error "Unterminated comment" v lexbuf )

# 1589 "lib/read.ml"

  
# 1590 "lib/read.ml"
  | 2 ->

# 410 "lib/read.mll"
         
# 410 "lib/read.mll"
         ( newline v lexbuf; finish_comment v lexbuf )

# 1594 "lib/read.ml"

  
# 1595 "lib/read.ml"
  | 3 ->

# 411 "lib/read.mll"
         
# 411 "lib/read.mll"
         ( finish_comment v lexbuf )

# 1599 "lib/read.ml"

  
# 1600 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state

and read_eof lexbuf =
   __ocaml_lex_read_eof_rec lexbuf 131
and __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 419 "lib/read.mll"
              
# 419 "lib/read.mll"
              ( true )

# 1611 "lib/read.ml"

  
# 1612 "lib/read.ml"
  | 1 ->

# 420 "lib/read.mll"
              
# 420 "lib/read.mll"
              ( false )

# 1616 "lib/read.ml"

  
# 1617 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state

and read_space v lexbuf =
   __ocaml_lex_read_space_rec v lexbuf 133
and __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 423 "lib/read.mll"
                             
# 423 "lib/read.mll"
                             ( newline v lexbuf; read_space v lexbuf )

# 1628 "lib/read.ml"

  
# 1629 "lib/read.ml"
  | 1 ->

# 424 "lib/read.mll"
                             
# 424 "lib/read.mll"
                             ( finish_comment v lexbuf; read_space v lexbuf )

# 1633 "lib/read.ml"

  
# 1634 "lib/read.ml"
  | 2 ->

# 425 "lib/read.mll"
                             
# 425 "lib/read.mll"
                             ( newline v lexbuf; read_space v lexbuf )

# 1638 "lib/read.ml"

  
# 1639 "lib/read.ml"
  | 3 ->

# 426 "lib/read.mll"
                             
# 426 "lib/read.mll"
                             ( read_space v lexbuf )

# 1643 "lib/read.ml"

  
# 1644 "lib/read.ml"
  | 4 ->

# 427 "lib/read.mll"
                             
# 427 "lib/read.mll"
                             ( () )

# 1648 "lib/read.ml"

  
# 1649 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state

and read_null v lexbuf =
   __ocaml_lex_read_null_rec v lexbuf 140
and __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 430 "lib/read.mll"
              
# 430 "lib/read.mll"
              ( () )

# 1660 "lib/read.ml"

  
# 1661 "lib/read.ml"
  | 1 ->

# 431 "lib/read.mll"
              
# 431 "lib/read.mll"
              ( long_error "Expected 'null' but found" v lexbuf )

# 1665 "lib/read.ml"

  
# 1666 "lib/read.ml"
  | 2 ->

# 432 "lib/read.mll"
              
# 432 "lib/read.mll"
              ( custom_error "Unexpected end of input" v lexbuf )

# 1670 "lib/read.ml"

  
# 1671 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state

and read_null_if_possible v lexbuf =
   __ocaml_lex_read_null_if_possible_rec v lexbuf 147
and __ocaml_lex_read_null_if_possible_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 435 "lib/read.mll"
              
# 435 "lib/read.mll"
              ( true )

# 1682 "lib/read.ml"

  
# 1683 "lib/read.ml"
  | 1 ->

# 436 "lib/read.mll"
              
# 436 "lib/read.mll"
              ( false )

# 1687 "lib/read.ml"

  
# 1688 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_null_if_possible_rec v lexbuf __ocaml_lex_state

and read_bool v lexbuf =
   __ocaml_lex_read_bool_rec v lexbuf 152
and __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 439 "lib/read.mll"
                
# 439 "lib/read.mll"
                ( true )

# 1699 "lib/read.ml"

  
# 1700 "lib/read.ml"
  | 1 ->

# 440 "lib/read.mll"
                
# 440 "lib/read.mll"
                ( false )

# 1704 "lib/read.ml"

  
# 1705 "lib/read.ml"
  | 2 ->

# 443 "lib/read.mll"
                
# 443 "lib/read.mll"
                ( true )

# 1709 "lib/read.ml"

  
# 1710 "lib/read.ml"
  | 3 ->

# 444 "lib/read.mll"
                
# 444 "lib/read.mll"
                ( false )

# 1714 "lib/read.ml"

  
# 1715 "lib/read.ml"
  | 4 ->

# 446 "lib/read.mll"
                
# 446 "lib/read.mll"
                ( long_error "Expected 'true' or 'false' but found" v lexbuf )

# 1719 "lib/read.ml"

  
# 1720 "lib/read.ml"
  | 5 ->

# 447 "lib/read.mll"
                
# 447 "lib/read.mll"
                ( custom_error "Unexpected end of input" v lexbuf )

# 1724 "lib/read.ml"

  
# 1725 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state

and read_int v lexbuf =
   __ocaml_lex_read_int_rec v lexbuf 176
and __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 450 "lib/read.mll"
                         
# 450 "lib/read.mll"
                         ( try extract_positive_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf )

# 1738 "lib/read.ml"

  
# 1739 "lib/read.ml"
  | 1 ->

# 453 "lib/read.mll"
                         
# 453 "lib/read.mll"
                         ( try extract_negative_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf )

# 1745 "lib/read.ml"

  
# 1746 "lib/read.ml"
  | 2 ->

# 456 "lib/read.mll"
                         
# 456 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             int_of_string s
                           with _ ->
                             custom_error
                               "Expected an integer but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1763 "lib/read.ml"

  
# 1764 "lib/read.ml"
  | 3 ->

# 470 "lib/read.mll"
                         
# 470 "lib/read.mll"
                         ( long_error "Expected integer but found" v lexbuf )

# 1768 "lib/read.ml"

  
# 1769 "lib/read.ml"
  | 4 ->

# 471 "lib/read.mll"
                         
# 471 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1773 "lib/read.ml"

  
# 1774 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state

and read_int32 v lexbuf =
   __ocaml_lex_read_int32_rec v lexbuf 185
and __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 474 "lib/read.mll"
                         
# 474 "lib/read.mll"
                         ( try Int32.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf )

# 1787 "lib/read.ml"

  
# 1788 "lib/read.ml"
  | 1 ->

# 477 "lib/read.mll"
                         
# 477 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int32.of_string s
                           with _ ->
                             custom_error
                               "Expected an int32 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1805 "lib/read.ml"

  
# 1806 "lib/read.ml"
  | 2 ->

# 491 "lib/read.mll"
                         
# 491 "lib/read.mll"
                         ( long_error "Expected int32 but found" v lexbuf )

# 1810 "lib/read.ml"

  
# 1811 "lib/read.ml"
  | 3 ->

# 492 "lib/read.mll"
                         
# 492 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1815 "lib/read.ml"

  
# 1816 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state

and read_int64 v lexbuf =
   __ocaml_lex_read_int64_rec v lexbuf 192
and __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 495 "lib/read.mll"
                         
# 495 "lib/read.mll"
                         ( try Int64.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf )

# 1829 "lib/read.ml"

  
# 1830 "lib/read.ml"
  | 1 ->

# 498 "lib/read.mll"
                         
# 498 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int64.of_string s
                           with _ ->
                             custom_error
                               "Expected an int64 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1847 "lib/read.ml"

  
# 1848 "lib/read.ml"
  | 2 ->

# 512 "lib/read.mll"
                         
# 512 "lib/read.mll"
                         ( long_error "Expected int64 but found" v lexbuf )

# 1852 "lib/read.ml"

  
# 1853 "lib/read.ml"
  | 3 ->

# 513 "lib/read.mll"
                         
# 513 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1857 "lib/read.ml"

  
# 1858 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state

and read_number v lexbuf =
   __ocaml_lex_read_number_rec v lexbuf 199
and __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 516 "lib/read.mll"
                
# 516 "lib/read.mll"
                ( nan )

# 1869 "lib/read.ml"

  
# 1870 "lib/read.ml"
  | 1 ->

# 517 "lib/read.mll"
                
# 517 "lib/read.mll"
                ( infinity )

# 1874 "lib/read.ml"

  
# 1875 "lib/read.ml"
  | 2 ->

# 518 "lib/read.mll"
                
# 518 "lib/read.mll"
                ( neg_infinity )

# 1879 "lib/read.ml"

  
# 1880 "lib/read.ml"
  | 3 ->

# 519 "lib/read.mll"
                
# 519 "lib/read.mll"
                ( float_of_string (lexeme lexbuf) )

# 1884 "lib/read.ml"

  
# 1885 "lib/read.ml"
  | 4 ->

# 520 "lib/read.mll"
                
# 520 "lib/read.mll"
                ( Buffer.clear v.buf;
                  let s = finish_string v lexbuf in
                  try
                    (* Any OCaml-compliant float will pass,
                       including hexadecimal and octal notations,
                       and embedded underscores. *)
                    float_of_string s
                  with _ ->
                    match s with
                        "NaN" -> nan
                      | "Infinity" -> infinity
                      | "-Infinity" -> neg_infinity
                      | _ ->
                          custom_error
                            "Expected a number but found a string that \
                             doesn't even represent a number"
                            v lexbuf
                )

# 1906 "lib/read.ml"

  
# 1907 "lib/read.ml"
  | 5 ->

# 538 "lib/read.mll"
                
# 538 "lib/read.mll"
                ( long_error "Expected number but found" v lexbuf )

# 1911 "lib/read.ml"

  
# 1912 "lib/read.ml"
  | 6 ->

# 539 "lib/read.mll"
                
# 539 "lib/read.mll"
                ( custom_error "Unexpected end of input" v lexbuf )

# 1916 "lib/read.ml"

  
# 1917 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state

and read_string v lexbuf =
   __ocaml_lex_read_string_rec v lexbuf 233
and __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 542 "lib/read.mll"
             
# 542 "lib/read.mll"
             ( Buffer.clear v.buf;
               finish_string v lexbuf )

# 1929 "lib/read.ml"

  
# 1930 "lib/read.ml"
  | 1 ->

# 544 "lib/read.mll"
             
# 544 "lib/read.mll"
             ( long_error "Expected '\"' but found" v lexbuf )

# 1934 "lib/read.ml"

  
# 1935 "lib/read.ml"
  | 2 ->

# 545 "lib/read.mll"
             
# 545 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1939 "lib/read.ml"

  
# 1940 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state

and read_ident v lexbuf =
   __ocaml_lex_read_ident_rec v lexbuf 237
and __ocaml_lex_read_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 548 "lib/read.mll"
             
# 548 "lib/read.mll"
             ( Buffer.clear v.buf;
               finish_string v lexbuf )

# 1952 "lib/read.ml"

  
# 1953 "lib/read.ml"
  | 1 ->
let

# 550 "lib/read.mll"
             
# 550 "lib/read.mll"
             s

# 1958 "lib/read.ml"
# 1958 "lib/read.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in

# 551 "lib/read.mll"
             
# 551 "lib/read.mll"
             ( s )

# 1962 "lib/read.ml"

  
# 1963 "lib/read.ml"
  | 2 ->

# 552 "lib/read.mll"
             
# 552 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 1967 "lib/read.ml"

  
# 1968 "lib/read.ml"
  | 3 ->

# 553 "lib/read.mll"
             
# 553 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1972 "lib/read.ml"

  
# 1973 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_ident_rec v lexbuf __ocaml_lex_state

and map_ident v f lexbuf =
   __ocaml_lex_map_ident_rec v f lexbuf 242
and __ocaml_lex_map_ident_rec v f lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 556 "lib/read.mll"
             
# 556 "lib/read.mll"
             ( Buffer.clear v.buf;
               map_string v f lexbuf )

# 1985 "lib/read.ml"

  
# 1986 "lib/read.ml"
  | 1 ->

# 559 "lib/read.mll"
             
# 559 "lib/read.mll"
             ( map_lexeme f lexbuf )

# 1990 "lib/read.ml"

  
# 1991 "lib/read.ml"
  | 2 ->

# 560 "lib/read.mll"
             
# 560 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 1995 "lib/read.ml"

  
# 1996 "lib/read.ml"
  | 3 ->

# 561 "lib/read.mll"
             
# 561 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2000 "lib/read.ml"

  
# 2001 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_map_ident_rec v f lexbuf __ocaml_lex_state

and read_sequence read_cell init_acc v lexbuf =
   __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf 247
and __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 564 "lib/read.mll"
             
# 564 "lib/read.mll"
             ( let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell !acc v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell !acc v lexbuf;
                 done;
                 assert false
               with End_of_array ->
                 !acc
             )

# 2026 "lib/read.ml"

  
# 2027 "lib/read.ml"
  | 1 ->

# 579 "lib/read.mll"
             
# 579 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2031 "lib/read.ml"

  
# 2032 "lib/read.ml"
  | 2 ->

# 580 "lib/read.mll"
             
# 580 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2036 "lib/read.ml"

  
# 2037 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf __ocaml_lex_state

and read_list_rev read_cell v lexbuf =
   __ocaml_lex_read_list_rev_rec read_cell v lexbuf 251
and __ocaml_lex_read_list_rev_rec read_cell v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 583 "lib/read.mll"
             
# 583 "lib/read.mll"
             ( let acc = ref [] in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell v lexbuf :: !acc;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell v lexbuf :: !acc;
                 done;
                 assert false
               with End_of_array ->
                 !acc
             )

# 2062 "lib/read.ml"

  
# 2063 "lib/read.ml"
  | 1 ->

# 598 "lib/read.mll"
             
# 598 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2067 "lib/read.ml"

  
# 2068 "lib/read.ml"
  | 2 ->

# 599 "lib/read.mll"
             
# 599 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2072 "lib/read.ml"

  
# 2073 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_list_rev_rec read_cell v lexbuf __ocaml_lex_state

and read_array_end lexbuf =
   __ocaml_lex_read_array_end_rec lexbuf 255
and __ocaml_lex_read_array_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 602 "lib/read.mll"
             
# 602 "lib/read.mll"
             ( raise End_of_array )

# 2084 "lib/read.ml"

  
# 2085 "lib/read.ml"
  | 1 ->

# 603 "lib/read.mll"
             
# 603 "lib/read.mll"
             ( () )

# 2089 "lib/read.ml"

  
# 2090 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_array_end_rec lexbuf __ocaml_lex_state

and read_array_sep v lexbuf =
   __ocaml_lex_read_array_sep_rec v lexbuf 257
and __ocaml_lex_read_array_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 606 "lib/read.mll"
             
# 606 "lib/read.mll"
             ( () )

# 2101 "lib/read.ml"

  
# 2102 "lib/read.ml"
  | 1 ->

# 607 "lib/read.mll"
             
# 607 "lib/read.mll"
             ( raise End_of_array )

# 2106 "lib/read.ml"

  
# 2107 "lib/read.ml"
  | 2 ->

# 608 "lib/read.mll"
             
# 608 "lib/read.mll"
             ( long_error "Expected ',' or ']' but found" v lexbuf )

# 2111 "lib/read.ml"

  
# 2112 "lib/read.ml"
  | 3 ->

# 609 "lib/read.mll"
             
# 609 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2116 "lib/read.ml"

  
# 2117 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_array_sep_rec v lexbuf __ocaml_lex_state

and read_tuple read_cell init_acc v lexbuf =
   __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf 262
and __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 613 "lib/read.mll"
                 
# 613 "lib/read.mll"
                 (
                     
# 615 "lib/read.mll"
                     let pos = ref 0 in
                     let acc = ref init_acc in
                     try
                       read_space v lexbuf;
                       read_tuple_end lexbuf;
                       acc := read_cell !pos !acc v lexbuf;
                       incr pos;
                       while true do
                         read_space v lexbuf;
                         read_tuple_sep v lexbuf;
                         read_space v lexbuf;
                         acc := read_cell !pos !acc v lexbuf;
                         incr pos;
                       done;
                       assert false
                     with End_of_tuple ->
                       !acc
                 
# 635 "lib/read.mll"
                 )

# 2150 "lib/read.ml"

  
# 2151 "lib/read.ml"
  | 1 ->

# 636 "lib/read.mll"
             
# 636 "lib/read.mll"
             ( long_error "Expected ')' but found" v lexbuf )

# 2155 "lib/read.ml"

  
# 2156 "lib/read.ml"
  | 2 ->

# 637 "lib/read.mll"
             
# 637 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2160 "lib/read.ml"

  
# 2161 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf __ocaml_lex_state

and read_tuple_end lexbuf =
   __ocaml_lex_read_tuple_end_rec lexbuf 266
and __ocaml_lex_read_tuple_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 640 "lib/read.mll"
             
# 640 "lib/read.mll"
             ( raise End_of_tuple )

# 2172 "lib/read.ml"

  
# 2173 "lib/read.ml"
  | 1 ->

# 641 "lib/read.mll"
             
# 641 "lib/read.mll"
             ( () )

# 2177 "lib/read.ml"

  
# 2178 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_end_rec lexbuf __ocaml_lex_state

and read_tuple_end2 v std lexbuf =
   __ocaml_lex_read_tuple_end2_rec v std lexbuf 268
and __ocaml_lex_read_tuple_end2_rec v std lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 644 "lib/read.mll"
             
# 644 "lib/read.mll"
             ( if std then
                 long_error "Expected ')' or '' but found" v lexbuf
               else
                 raise End_of_tuple )

# 2192 "lib/read.ml"

  
# 2193 "lib/read.ml"
  | 1 ->

# 648 "lib/read.mll"
             
# 648 "lib/read.mll"
             ( if std then
                 raise End_of_tuple
               else
                 long_error "Expected ']' or '' but found" v lexbuf )

# 2200 "lib/read.ml"

  
# 2201 "lib/read.ml"
  | 2 ->

# 652 "lib/read.mll"
             
# 652 "lib/read.mll"
             ( () )

# 2205 "lib/read.ml"

  
# 2206 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_end2_rec v std lexbuf __ocaml_lex_state

and read_tuple_sep v lexbuf =
   __ocaml_lex_read_tuple_sep_rec v lexbuf 271
and __ocaml_lex_read_tuple_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 655 "lib/read.mll"
             
# 655 "lib/read.mll"
             ( () )

# 2217 "lib/read.ml"

  
# 2218 "lib/read.ml"
  | 1 ->

# 656 "lib/read.mll"
             
# 656 "lib/read.mll"
             ( raise End_of_tuple )

# 2222 "lib/read.ml"

  
# 2223 "lib/read.ml"
  | 2 ->

# 657 "lib/read.mll"
             
# 657 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 2227 "lib/read.ml"

  
# 2228 "lib/read.ml"
  | 3 ->

# 658 "lib/read.mll"
             
# 658 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2232 "lib/read.ml"

  
# 2233 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_sep_rec v lexbuf __ocaml_lex_state

and read_tuple_sep2 v std lexbuf =
   __ocaml_lex_read_tuple_sep2_rec v std lexbuf 276
and __ocaml_lex_read_tuple_sep2_rec v std lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 661 "lib/read.mll"
             
# 661 "lib/read.mll"
             ( () )

# 2244 "lib/read.ml"

  
# 2245 "lib/read.ml"
  | 1 ->

# 662 "lib/read.mll"
             
# 662 "lib/read.mll"
             ( if std then
                 long_error "Expected ',' or ']' but found" v lexbuf
               else
                 raise End_of_tuple )

# 2252 "lib/read.ml"

  
# 2253 "lib/read.ml"
  | 2 ->

# 666 "lib/read.mll"
             
# 666 "lib/read.mll"
             ( if std then
                 raise End_of_tuple
               else
                 long_error "Expected ',' or ')' but found" v lexbuf )

# 2260 "lib/read.ml"

  
# 2261 "lib/read.ml"
  | 3 ->

# 670 "lib/read.mll"
             
# 670 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 2265 "lib/read.ml"

  
# 2266 "lib/read.ml"
  | 4 ->

# 671 "lib/read.mll"
             
# 671 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2270 "lib/read.ml"

  
# 2271 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_sep2_rec v std lexbuf __ocaml_lex_state

and read_abstract_fields read_key read_field init_acc v lexbuf =
   __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf 282
and __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 675 "lib/read.mll"
             
# 675 "lib/read.mll"
             ( let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_object_end lexbuf;
                 let field_name = read_key v lexbuf in
                 read_space v lexbuf;
                 read_colon v lexbuf;
                 read_space v lexbuf;
                 acc := read_field !acc field_name v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_object_sep v lexbuf;
                   read_space v lexbuf;
                   let field_name = read_key v lexbuf in
                   read_space v lexbuf;
                   read_colon v lexbuf;
                   read_space v lexbuf;
                   acc := read_field !acc field_name v lexbuf;
                 done;
                 assert false
               with End_of_object ->
                 !acc
             )

# 2304 "lib/read.ml"

  
# 2305 "lib/read.ml"
  | 1 ->

# 698 "lib/read.mll"
             
# 698 "lib/read.mll"
             ( long_error "Expected '{' but found" v lexbuf )

# 2309 "lib/read.ml"

  
# 2310 "lib/read.ml"
  | 2 ->

# 699 "lib/read.mll"
             
# 699 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2314 "lib/read.ml"

  
# 2315 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf __ocaml_lex_state

and read_lcurl v lexbuf =
   __ocaml_lex_read_lcurl_rec v lexbuf 286
and __ocaml_lex_read_lcurl_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 702 "lib/read.mll"
             
# 702 "lib/read.mll"
             ( () )

# 2326 "lib/read.ml"

  
# 2327 "lib/read.ml"
  | 1 ->

# 703 "lib/read.mll"
             
# 703 "lib/read.mll"
             ( long_error "Expected '{' but found" v lexbuf )

# 2331 "lib/read.ml"

  
# 2332 "lib/read.ml"
  | 2 ->

# 704 "lib/read.mll"
             
# 704 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2336 "lib/read.ml"

  
# 2337 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lcurl_rec v lexbuf __ocaml_lex_state

and read_object_end lexbuf =
   __ocaml_lex_read_object_end_rec lexbuf 290
and __ocaml_lex_read_object_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 707 "lib/read.mll"
             
# 707 "lib/read.mll"
             ( raise End_of_object )

# 2348 "lib/read.ml"

  
# 2349 "lib/read.ml"
  | 1 ->

# 708 "lib/read.mll"
             
# 708 "lib/read.mll"
             ( () )

# 2353 "lib/read.ml"

  
# 2354 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_object_end_rec lexbuf __ocaml_lex_state

and read_object_sep v lexbuf =
   __ocaml_lex_read_object_sep_rec v lexbuf 292
and __ocaml_lex_read_object_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 711 "lib/read.mll"
             
# 711 "lib/read.mll"
             ( () )

# 2365 "lib/read.ml"

  
# 2366 "lib/read.ml"
  | 1 ->

# 712 "lib/read.mll"
             
# 712 "lib/read.mll"
             ( raise End_of_object )

# 2370 "lib/read.ml"

  
# 2371 "lib/read.ml"
  | 2 ->

# 713 "lib/read.mll"
             
# 713 "lib/read.mll"
             ( long_error "Expected ',' or '}' but found" v lexbuf )

# 2375 "lib/read.ml"

  
# 2376 "lib/read.ml"
  | 3 ->

# 714 "lib/read.mll"
             
# 714 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2380 "lib/read.ml"

  
# 2381 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_object_sep_rec v lexbuf __ocaml_lex_state

and read_colon v lexbuf =
   __ocaml_lex_read_colon_rec v lexbuf 297
and __ocaml_lex_read_colon_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 717 "lib/read.mll"
             
# 717 "lib/read.mll"
             ( () )

# 2392 "lib/read.ml"

  
# 2393 "lib/read.ml"
  | 1 ->

# 718 "lib/read.mll"
             
# 718 "lib/read.mll"
             ( long_error "Expected ':' but found" v lexbuf )

# 2397 "lib/read.ml"

  
# 2398 "lib/read.ml"
  | 2 ->

# 719 "lib/read.mll"
             
# 719 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2402 "lib/read.ml"

  
# 2403 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_colon_rec v lexbuf __ocaml_lex_state

and start_any_tuple v lexbuf =
   __ocaml_lex_start_any_tuple_rec v lexbuf 301
and __ocaml_lex_start_any_tuple_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 722 "lib/read.mll"
             
# 722 "lib/read.mll"
             ( false )

# 2414 "lib/read.ml"

  
# 2415 "lib/read.ml"
  | 1 ->

# 723 "lib/read.mll"
             
# 723 "lib/read.mll"
             ( true )

# 2419 "lib/read.ml"

  
# 2420 "lib/read.ml"
  | 2 ->

# 724 "lib/read.mll"
             
# 724 "lib/read.mll"
             ( long_error "Expected '(' or '[' but found" v lexbuf )

# 2424 "lib/read.ml"

  
# 2425 "lib/read.ml"
  | 3 ->

# 725 "lib/read.mll"
             
# 725 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2429 "lib/read.ml"

  
# 2430 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_start_any_tuple_rec v lexbuf __ocaml_lex_state

and read_lpar v lexbuf =
   __ocaml_lex_read_lpar_rec v lexbuf 306
and __ocaml_lex_read_lpar_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 728 "lib/read.mll"
             
# 728 "lib/read.mll"
             ( () )

# 2441 "lib/read.ml"

  
# 2442 "lib/read.ml"
  | 1 ->

# 729 "lib/read.mll"
             
# 729 "lib/read.mll"
             ( long_error "Expected '(' but found" v lexbuf )

# 2446 "lib/read.ml"

  
# 2447 "lib/read.ml"
  | 2 ->

# 730 "lib/read.mll"
             
# 730 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2451 "lib/read.ml"

  
# 2452 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lpar_rec v lexbuf __ocaml_lex_state

and read_rpar v lexbuf =
   __ocaml_lex_read_rpar_rec v lexbuf 310
and __ocaml_lex_read_rpar_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 733 "lib/read.mll"
             
# 733 "lib/read.mll"
             ( () )

# 2463 "lib/read.ml"

  
# 2464 "lib/read.ml"
  | 1 ->

# 734 "lib/read.mll"
             
# 734 "lib/read.mll"
             ( long_error "Expected ')' but found" v lexbuf )

# 2468 "lib/read.ml"

  
# 2469 "lib/read.ml"
  | 2 ->

# 735 "lib/read.mll"
             
# 735 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2473 "lib/read.ml"

  
# 2474 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rpar_rec v lexbuf __ocaml_lex_state

and read_lbr v lexbuf =
   __ocaml_lex_read_lbr_rec v lexbuf 314
and __ocaml_lex_read_lbr_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 738 "lib/read.mll"
             
# 738 "lib/read.mll"
             ( () )

# 2485 "lib/read.ml"

  
# 2486 "lib/read.ml"
  | 1 ->

# 739 "lib/read.mll"
             
# 739 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2490 "lib/read.ml"

  
# 2491 "lib/read.ml"
  | 2 ->

# 740 "lib/read.mll"
             
# 740 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2495 "lib/read.ml"

  
# 2496 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lbr_rec v lexbuf __ocaml_lex_state

and read_rbr v lexbuf =
   __ocaml_lex_read_rbr_rec v lexbuf 318
and __ocaml_lex_read_rbr_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 743 "lib/read.mll"
             
# 743 "lib/read.mll"
             ( () )

# 2507 "lib/read.ml"

  
# 2508 "lib/read.ml"
  | 1 ->

# 744 "lib/read.mll"
             
# 744 "lib/read.mll"
             ( long_error "Expected ']' but found" v lexbuf )

# 2512 "lib/read.ml"

  
# 2513 "lib/read.ml"
  | 2 ->

# 745 "lib/read.mll"
             
# 745 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2517 "lib/read.ml"

  
# 2518 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rbr_rec v lexbuf __ocaml_lex_state

and skip_json v lexbuf =
   __ocaml_lex_skip_json_rec v lexbuf 322
and __ocaml_lex_skip_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 751 "lib/read.mll"
                
# 751 "lib/read.mll"
                ( () )

# 2529 "lib/read.ml"

  
# 2530 "lib/read.ml"
  | 1 ->

# 752 "lib/read.mll"
                
# 752 "lib/read.mll"
                ( () )

# 2534 "lib/read.ml"

  
# 2535 "lib/read.ml"
  | 2 ->

# 753 "lib/read.mll"
                
# 753 "lib/read.mll"
                ( () )

# 2539 "lib/read.ml"

  
# 2540 "lib/read.ml"
  | 3 ->

# 754 "lib/read.mll"
                
# 754 "lib/read.mll"
                ( () )

# 2544 "lib/read.ml"

  
# 2545 "lib/read.ml"
  | 4 ->

# 755 "lib/read.mll"
                
# 755 "lib/read.mll"
                ( () )

# 2549 "lib/read.ml"

  
# 2550 "lib/read.ml"
  | 5 ->

# 756 "lib/read.mll"
                
# 756 "lib/read.mll"
                ( () )

# 2554 "lib/read.ml"

  
# 2555 "lib/read.ml"
  | 6 ->

# 757 "lib/read.mll"
                
# 757 "lib/read.mll"
                ( finish_skip_stringlit v lexbuf )

# 2559 "lib/read.ml"

  
# 2560 "lib/read.ml"
  | 7 ->

# 758 "lib/read.mll"
                          
# 758 "lib/read.mll"
                          ( () )

# 2564 "lib/read.ml"

  
# 2565 "lib/read.ml"
  | 8 ->

# 759 "lib/read.mll"
                
# 759 "lib/read.mll"
                ( () )

# 2569 "lib/read.ml"

  
# 2570 "lib/read.ml"
  | 9 ->

# 761 "lib/read.mll"
                 
# 761 "lib/read.mll"
                 ( try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     skip_ident v lexbuf;
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       skip_ident v lexbuf;
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with End_of_object ->
                     ()
                 )

# 2595 "lib/read.ml"

  
# 2596 "lib/read.ml"
  | 10 ->

# 784 "lib/read.mll"
                 
# 784 "lib/read.mll"
                 ( try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with End_of_array ->
                     ()
                 )

# 2613 "lib/read.ml"

  
# 2614 "lib/read.ml"
  | 11 ->

# 799 "lib/read.mll"
                 
# 799 "lib/read.mll"
                 (
                     
# 801 "lib/read.mll"
                     try
                       read_space v lexbuf;
                       read_tuple_end lexbuf;
                       skip_json v lexbuf;
                       while true do
                         read_space v lexbuf;
                         read_tuple_sep v lexbuf;
                         read_space v lexbuf;
                         skip_json v lexbuf;
                       done;
                       assert false
                     with End_of_tuple ->
                       ()
                 
# 817 "lib/read.mll"
                 )

# 2636 "lib/read.ml"

  
# 2637 "lib/read.ml"
  | 12 ->

# 819 "lib/read.mll"
                 
# 819 "lib/read.mll"
                 (
                     
# 821 "lib/read.mll"
                     read_space v lexbuf;
                     skip_ident v lexbuf;
                     read_space v lexbuf;
                     finish_skip_variant v lexbuf
                 
# 828 "lib/read.mll"
                 )

# 2650 "lib/read.ml"

  
# 2651 "lib/read.ml"
  | 13 ->

# 830 "lib/read.mll"
                 
# 830 "lib/read.mll"
                 ( skip_json v lexbuf )

# 2655 "lib/read.ml"

  
# 2656 "lib/read.ml"
  | 14 ->

# 831 "lib/read.mll"
                 
# 831 "lib/read.mll"
                 ( finish_comment v lexbuf; skip_json v lexbuf )

# 2660 "lib/read.ml"

  
# 2661 "lib/read.ml"
  | 15 ->

# 832 "lib/read.mll"
                 
# 832 "lib/read.mll"
                 ( newline v lexbuf; skip_json v lexbuf )

# 2665 "lib/read.ml"

  
# 2666 "lib/read.ml"
  | 16 ->

# 833 "lib/read.mll"
                 
# 833 "lib/read.mll"
                 ( skip_json v lexbuf )

# 2670 "lib/read.ml"

  
# 2671 "lib/read.ml"
  | 17 ->

# 834 "lib/read.mll"
                 
# 834 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 2675 "lib/read.ml"

  
# 2676 "lib/read.ml"
  | 18 ->

# 835 "lib/read.mll"
                 
# 835 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 2680 "lib/read.ml"

  
# 2681 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_json_rec v lexbuf __ocaml_lex_state

and finish_skip_stringlit v lexbuf =
   __ocaml_lex_finish_skip_stringlit_rec v lexbuf 378
and __ocaml_lex_finish_skip_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 841 "lib/read.mll"
         
# 841 "lib/read.mll"
         ( () )

# 2692 "lib/read.ml"

  
# 2693 "lib/read.ml"
  | 1 ->

# 842 "lib/read.mll"
         
# 842 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 2697 "lib/read.ml"

  
# 2698 "lib/read.ml"
  | 2 ->

# 843 "lib/read.mll"
         
# 843 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2702 "lib/read.ml"

  
# 2703 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_skip_stringlit_rec v lexbuf __ocaml_lex_state

and finish_skip_variant v lexbuf =
   __ocaml_lex_finish_skip_variant_rec v lexbuf 389
and __ocaml_lex_finish_skip_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 846 "lib/read.mll"
         
# 846 "lib/read.mll"
         ( skip_json v lexbuf;
           read_space v lexbuf;
           read_gt v lexbuf )

# 2716 "lib/read.ml"

  
# 2717 "lib/read.ml"
  | 1 ->

# 849 "lib/read.mll"
         
# 849 "lib/read.mll"
         ( () )

# 2721 "lib/read.ml"

  
# 2722 "lib/read.ml"
  | 2 ->

# 850 "lib/read.mll"
         
# 850 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 2726 "lib/read.ml"

  
# 2727 "lib/read.ml"
  | 3 ->

# 851 "lib/read.mll"
         
# 851 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2731 "lib/read.ml"

  
# 2732 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_skip_variant_rec v lexbuf __ocaml_lex_state

and skip_ident v lexbuf =
   __ocaml_lex_skip_ident_rec v lexbuf 394
and __ocaml_lex_skip_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 854 "lib/read.mll"
             
# 854 "lib/read.mll"
             ( finish_skip_stringlit v lexbuf )

# 2743 "lib/read.ml"

  
# 2744 "lib/read.ml"
  | 1 ->

# 855 "lib/read.mll"
             
# 855 "lib/read.mll"
             ( () )

# 2748 "lib/read.ml"

  
# 2749 "lib/read.ml"
  | 2 ->

# 856 "lib/read.mll"
             
# 856 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 2753 "lib/read.ml"

  
# 2754 "lib/read.ml"
  | 3 ->

# 857 "lib/read.mll"
             
# 857 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2758 "lib/read.ml"

  
# 2759 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_ident_rec v lexbuf __ocaml_lex_state

and buffer_json v lexbuf =
   __ocaml_lex_buffer_json_rec v lexbuf 399
and __ocaml_lex_buffer_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 870 "lib/read.mll"
                
# 870 "lib/read.mll"
                ( add_lexeme v.buf lexbuf )

# 2770 "lib/read.ml"

  
# 2771 "lib/read.ml"
  | 1 ->

# 872 "lib/read.mll"
                
# 872 "lib/read.mll"
                ( finish_buffer_stringlit v lexbuf )

# 2775 "lib/read.ml"

  
# 2776 "lib/read.ml"
  | 2 ->

# 873 "lib/read.mll"
                 
# 873 "lib/read.mll"
                 ( try
                     Buffer.add_char v.buf '{';
                     buffer_space v lexbuf;
                     buffer_object_end v lexbuf;
                     buffer_ident v lexbuf;
                     buffer_space v lexbuf;
                     buffer_colon v lexbuf;
                     buffer_space v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_object_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_ident v lexbuf;
                       buffer_space v lexbuf;
                       buffer_colon v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with End_of_object ->
                     ()
                 )

# 2802 "lib/read.ml"

  
# 2803 "lib/read.ml"
  | 3 ->

# 897 "lib/read.mll"
                 
# 897 "lib/read.mll"
                 ( try
                     Buffer.add_char v.buf '[';
                     buffer_space v lexbuf;
                     buffer_array_end v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_array_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with End_of_array ->
                     ()
                 )

# 2821 "lib/read.ml"

  
# 2822 "lib/read.ml"
  | 4 ->

# 913 "lib/read.mll"
                 
# 913 "lib/read.mll"
                 (
                     
# 915 "lib/read.mll"
                     try
                       Buffer.add_char v.buf '(';
                       buffer_space v lexbuf;
                       buffer_tuple_end v lexbuf;
                       buffer_json v lexbuf;
                       while true do
                         buffer_space v lexbuf;
                         buffer_tuple_sep v lexbuf;
                         buffer_space v lexbuf;
                         buffer_json v lexbuf;
                       done;
                       assert false
                     with End_of_tuple ->
                       ()
                 
# 932 "lib/read.mll"
                 )

# 2845 "lib/read.ml"

  
# 2846 "lib/read.ml"
  | 5 ->

# 934 "lib/read.mll"
                 
# 934 "lib/read.mll"
                 (
                     
# 936 "lib/read.mll"
                     Buffer.add_char v.buf '<';
                     buffer_space v lexbuf;
                     buffer_ident v lexbuf;
                     buffer_space v lexbuf;
                     finish_buffer_variant v lexbuf
                 
# 944 "lib/read.mll"
                 )

# 2860 "lib/read.ml"

  
# 2861 "lib/read.ml"
  | 6 ->

# 946 "lib/read.mll"
                 
# 946 "lib/read.mll"
                 ( add_lexeme v.buf lexbuf; buffer_json v lexbuf )

# 2865 "lib/read.ml"

  
# 2866 "lib/read.ml"
  | 7 ->

# 947 "lib/read.mll"
                 
# 947 "lib/read.mll"
                 ( Buffer.add_string v.buf "/*";
                   finish_buffer_comment v lexbuf;
                   buffer_json v lexbuf )

# 2872 "lib/read.ml"

  
# 2873 "lib/read.ml"
  | 8 ->

# 950 "lib/read.mll"
                 
# 950 "lib/read.mll"
                 ( Buffer.add_char v.buf '\n';
                   newline v lexbuf;
                   buffer_json v lexbuf )

# 2879 "lib/read.ml"

  
# 2880 "lib/read.ml"
  | 9 ->

# 953 "lib/read.mll"
                 
# 953 "lib/read.mll"
                 ( add_lexeme v.buf lexbuf; buffer_json v lexbuf )

# 2884 "lib/read.ml"

  
# 2885 "lib/read.ml"
  | 10 ->

# 954 "lib/read.mll"
                 
# 954 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 2889 "lib/read.ml"

  
# 2890 "lib/read.ml"
  | 11 ->

# 955 "lib/read.mll"
                 
# 955 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 2894 "lib/read.ml"

  
# 2895 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_json_rec v lexbuf __ocaml_lex_state

and finish_buffer_stringlit v lexbuf =
   __ocaml_lex_finish_buffer_stringlit_rec v lexbuf 450
and __ocaml_lex_finish_buffer_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 961 "lib/read.mll"
         
# 961 "lib/read.mll"
         ( Buffer.add_char v.buf '"';
           add_lexeme v.buf lexbuf
         )

# 2908 "lib/read.ml"

  
# 2909 "lib/read.ml"
  | 1 ->

# 964 "lib/read.mll"
         
# 964 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 2913 "lib/read.ml"

  
# 2914 "lib/read.ml"
  | 2 ->

# 965 "lib/read.mll"
         
# 965 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2918 "lib/read.ml"

  
# 2919 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_stringlit_rec v lexbuf __ocaml_lex_state

and finish_buffer_variant v lexbuf =
   __ocaml_lex_finish_buffer_variant_rec v lexbuf 461
and __ocaml_lex_finish_buffer_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 968 "lib/read.mll"
         
# 968 "lib/read.mll"
         ( Buffer.add_char v.buf ':';
           buffer_json v lexbuf;
           buffer_space v lexbuf;
           buffer_gt v lexbuf )

# 2933 "lib/read.ml"

  
# 2934 "lib/read.ml"
  | 1 ->

# 972 "lib/read.mll"
         
# 972 "lib/read.mll"
         ( Buffer.add_char v.buf '>' )

# 2938 "lib/read.ml"

  
# 2939 "lib/read.ml"
  | 2 ->

# 973 "lib/read.mll"
         
# 973 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 2943 "lib/read.ml"

  
# 2944 "lib/read.ml"
  | 3 ->

# 974 "lib/read.mll"
         
# 974 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2948 "lib/read.ml"

  
# 2949 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_variant_rec v lexbuf __ocaml_lex_state

and buffer_ident v lexbuf =
   __ocaml_lex_buffer_ident_rec v lexbuf 466
and __ocaml_lex_buffer_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 977 "lib/read.mll"
             
# 977 "lib/read.mll"
             ( finish_buffer_stringlit v lexbuf )

# 2960 "lib/read.ml"

  
# 2961 "lib/read.ml"
  | 1 ->

# 978 "lib/read.mll"
             
# 978 "lib/read.mll"
             ( add_lexeme v.buf lexbuf )

# 2965 "lib/read.ml"

  
# 2966 "lib/read.ml"
  | 2 ->

# 979 "lib/read.mll"
             
# 979 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 2970 "lib/read.ml"

  
# 2971 "lib/read.ml"
  | 3 ->

# 980 "lib/read.mll"
             
# 980 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2975 "lib/read.ml"

  
# 2976 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_ident_rec v lexbuf __ocaml_lex_state

and buffer_space v lexbuf =
   __ocaml_lex_buffer_space_rec v lexbuf 471
and __ocaml_lex_buffer_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 983 "lib/read.mll"
                             
# 983 "lib/read.mll"
                             (
    add_lexeme v.buf lexbuf;
    newline v lexbuf;
    buffer_space v lexbuf )

# 2990 "lib/read.ml"

  
# 2991 "lib/read.ml"
  | 1 ->

# 987 "lib/read.mll"
                             
# 987 "lib/read.mll"
                             (
    Buffer.add_string v.buf "/*";
    finish_buffer_comment v lexbuf;
    buffer_space v lexbuf )

# 2998 "lib/read.ml"

  
# 2999 "lib/read.ml"
  | 2 ->

# 991 "lib/read.mll"
                             
# 991 "lib/read.mll"
                             (
    Buffer.add_char v.buf '\n';
    newline v lexbuf;
    buffer_space v lexbuf )

# 3006 "lib/read.ml"

  
# 3007 "lib/read.ml"
  | 3 ->

# 995 "lib/read.mll"
                             
# 995 "lib/read.mll"
                             (
    add_lexeme v.buf lexbuf;
    buffer_space v lexbuf )

# 3013 "lib/read.ml"

  
# 3014 "lib/read.ml"
  | 4 ->

# 998 "lib/read.mll"
                             
# 998 "lib/read.mll"
                             ( () )

# 3018 "lib/read.ml"

  
# 3019 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_space_rec v lexbuf __ocaml_lex_state

and buffer_object_end v lexbuf =
   __ocaml_lex_buffer_object_end_rec v lexbuf 478
and __ocaml_lex_buffer_object_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1001 "lib/read.mll"
             
# 1001 "lib/read.mll"
             (
      Buffer.add_char v.buf '}';
      raise End_of_object )

# 3032 "lib/read.ml"

  
# 3033 "lib/read.ml"
  | 1 ->

# 1004 "lib/read.mll"
             
# 1004 "lib/read.mll"
             ( () )

# 3037 "lib/read.ml"

  
# 3038 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_object_end_rec v lexbuf __ocaml_lex_state

and buffer_object_sep v lexbuf =
   __ocaml_lex_buffer_object_sep_rec v lexbuf 480
and __ocaml_lex_buffer_object_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1007 "lib/read.mll"
             
# 1007 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3049 "lib/read.ml"

  
# 3050 "lib/read.ml"
  | 1 ->

# 1008 "lib/read.mll"
             
# 1008 "lib/read.mll"
             ( Buffer.add_char v.buf '}'; raise End_of_object )

# 3054 "lib/read.ml"

  
# 3055 "lib/read.ml"
  | 2 ->

# 1009 "lib/read.mll"
             
# 1009 "lib/read.mll"
             ( long_error "Expected ',' or '}' but found" v lexbuf )

# 3059 "lib/read.ml"

  
# 3060 "lib/read.ml"
  | 3 ->

# 1010 "lib/read.mll"
             
# 1010 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3064 "lib/read.ml"

  
# 3065 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_object_sep_rec v lexbuf __ocaml_lex_state

and buffer_array_end v lexbuf =
   __ocaml_lex_buffer_array_end_rec v lexbuf 485
and __ocaml_lex_buffer_array_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1013 "lib/read.mll"
             
# 1013 "lib/read.mll"
             ( Buffer.add_char v.buf ']'; raise End_of_array )

# 3076 "lib/read.ml"

  
# 3077 "lib/read.ml"
  | 1 ->

# 1014 "lib/read.mll"
             
# 1014 "lib/read.mll"
             ( () )

# 3081 "lib/read.ml"

  
# 3082 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_array_end_rec v lexbuf __ocaml_lex_state

and buffer_array_sep v lexbuf =
   __ocaml_lex_buffer_array_sep_rec v lexbuf 487
and __ocaml_lex_buffer_array_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1017 "lib/read.mll"
             
# 1017 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3093 "lib/read.ml"

  
# 3094 "lib/read.ml"
  | 1 ->

# 1018 "lib/read.mll"
             
# 1018 "lib/read.mll"
             ( Buffer.add_char v.buf ']'; raise End_of_array )

# 3098 "lib/read.ml"

  
# 3099 "lib/read.ml"
  | 2 ->

# 1019 "lib/read.mll"
             
# 1019 "lib/read.mll"
             ( long_error "Expected ',' or ']' but found" v lexbuf )

# 3103 "lib/read.ml"

  
# 3104 "lib/read.ml"
  | 3 ->

# 1020 "lib/read.mll"
             
# 1020 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3108 "lib/read.ml"

  
# 3109 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_array_sep_rec v lexbuf __ocaml_lex_state

and buffer_tuple_end v lexbuf =
   __ocaml_lex_buffer_tuple_end_rec v lexbuf 492
and __ocaml_lex_buffer_tuple_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1023 "lib/read.mll"
             
# 1023 "lib/read.mll"
             (
      Buffer.add_char v.buf ')';
      raise End_of_tuple )

# 3122 "lib/read.ml"

  
# 3123 "lib/read.ml"
  | 1 ->

# 1026 "lib/read.mll"
             
# 1026 "lib/read.mll"
             ( () )

# 3127 "lib/read.ml"

  
# 3128 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_tuple_end_rec v lexbuf __ocaml_lex_state

and buffer_tuple_sep v lexbuf =
   __ocaml_lex_buffer_tuple_sep_rec v lexbuf 494
and __ocaml_lex_buffer_tuple_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1029 "lib/read.mll"
             
# 1029 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3139 "lib/read.ml"

  
# 3140 "lib/read.ml"
  | 1 ->

# 1030 "lib/read.mll"
             
# 1030 "lib/read.mll"
             ( Buffer.add_char v.buf ')'; raise End_of_tuple )

# 3144 "lib/read.ml"

  
# 3145 "lib/read.ml"
  | 2 ->

# 1031 "lib/read.mll"
             
# 1031 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 3149 "lib/read.ml"

  
# 3150 "lib/read.ml"
  | 3 ->

# 1032 "lib/read.mll"
             
# 1032 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3154 "lib/read.ml"

  
# 3155 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_tuple_sep_rec v lexbuf __ocaml_lex_state

and buffer_colon v lexbuf =
   __ocaml_lex_buffer_colon_rec v lexbuf 499
and __ocaml_lex_buffer_colon_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1035 "lib/read.mll"
             
# 1035 "lib/read.mll"
             ( Buffer.add_char v.buf ':' )

# 3166 "lib/read.ml"

  
# 3167 "lib/read.ml"
  | 1 ->

# 1036 "lib/read.mll"
             
# 1036 "lib/read.mll"
             ( long_error "Expected ':' but found" v lexbuf )

# 3171 "lib/read.ml"

  
# 3172 "lib/read.ml"
  | 2 ->

# 1037 "lib/read.mll"
             
# 1037 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3176 "lib/read.ml"

  
# 3177 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_colon_rec v lexbuf __ocaml_lex_state

and buffer_gt v lexbuf =
   __ocaml_lex_buffer_gt_rec v lexbuf 503
and __ocaml_lex_buffer_gt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1040 "lib/read.mll"
         
# 1040 "lib/read.mll"
         ( Buffer.add_char v.buf '>' )

# 3188 "lib/read.ml"

  
# 3189 "lib/read.ml"
  | 1 ->

# 1041 "lib/read.mll"
         
# 1041 "lib/read.mll"
         ( long_error "Expected '>' but found" v lexbuf )

# 3193 "lib/read.ml"

  
# 3194 "lib/read.ml"
  | 2 ->

# 1042 "lib/read.mll"
         
# 1042 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 3198 "lib/read.ml"

  
# 3199 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_gt_rec v lexbuf __ocaml_lex_state

and finish_buffer_comment v lexbuf =
   __ocaml_lex_finish_buffer_comment_rec v lexbuf 507
and __ocaml_lex_finish_buffer_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1045 "lib/read.mll"
         
# 1045 "lib/read.mll"
         ( Buffer.add_string v.buf "*/" )

# 3210 "lib/read.ml"

  
# 3211 "lib/read.ml"
  | 1 ->

# 1046 "lib/read.mll"
         
# 1046 "lib/read.mll"
         ( long_error "Unterminated comment" v lexbuf )

# 3215 "lib/read.ml"

  
# 3216 "lib/read.ml"
  | 2 ->

# 1047 "lib/read.mll"
         
# 1047 "lib/read.mll"
         ( Buffer.add_char v.buf '\n';
           newline v lexbuf;
           finish_buffer_comment v lexbuf )

# 3222 "lib/read.ml"

  
# 3223 "lib/read.ml"
  | 3 ->

# 1050 "lib/read.mll"
         
# 1050 "lib/read.mll"
         ( add_lexeme v.buf lexbuf; finish_buffer_comment v lexbuf )

# 3227 "lib/read.ml"

  
# 3228 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_comment_rec v lexbuf __ocaml_lex_state

and junk lexbuf =
   __ocaml_lex_junk_rec lexbuf 513
and __ocaml_lex_junk_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1053 "lib/read.mll"
             
# 1053 "lib/read.mll"
             ( Lexing.lexeme lexbuf )

# 3239 "lib/read.ml"

  
# 3240 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_junk_rec lexbuf __ocaml_lex_state

;;


# 1055 "lib/read.mll"
 
  
# 1056 "lib/read.mll"
  let _ = (read_json : lexer_state -> Lexing.lexbuf -> t)

  let read_t = read_json

  let () =
    read_junk := junk

  let read_int8 v lexbuf =
    let n = read_int v lexbuf in
    if n < 0 || n > 255 then
      lexer_error "Int8 overflow" v lexbuf
    else
      char_of_int n

  let read_list read_cell v lexbuf =
    List.rev (read_list_rev read_cell v lexbuf)

  let array_of_rev_list l =
    match l with
        [] -> [| |]
      | x :: tl ->
          let len = List.length l in
          let a = Array.make len x in
          let r = ref tl in
          for i = len - 2 downto 0 do
            a.(i) <- List.hd !r;
            r := List.tl !r
          done;
          a

  let read_array read_cell v lexbuf =
    let l = read_list_rev read_cell v lexbuf in
    array_of_rev_list l

  (* Read a JSON object, reading the keys into OCaml strings
     (provided for backward compatibility) *)
  let read_fields read_field init_acc v =
    read_abstract_fields read_ident read_field init_acc v

  let finish v lexbuf =
    read_space v lexbuf;
    if not (read_eof lexbuf) then
      long_error "Junk after end of JSON value:" v lexbuf

  let init_lexer = init_lexer

  let from_lexbuf v ?(stream = false) lexbuf =
    read_space v lexbuf;

    let x =
      if read_eof lexbuf then
        raise End_of_input
      else
        read_json v lexbuf
    in

    if not stream then
      finish v lexbuf;

    x


  let from_string ?buf ?fname ?lnum s =
    try
      let lexbuf = Lexing.from_string s in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with End_of_input ->
      json_error "Blank input data"

  let from_channel ?buf ?fname ?lnum ic =
    try
      let lexbuf = Lexing.from_channel ic in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with End_of_input ->
      json_error "Blank input data"

  let from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    try
      let x = from_channel ?buf ?fname ?lnum ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  exception Finally of exn * exn

  let stream_from_lexbuf v ?(fin = fun () -> ()) lexbuf =
    let stream = Some true in
    let f i =
      try Some (from_lexbuf v ?stream lexbuf)
      with
          End_of_input ->
            fin ();
            None
        | e ->
            (try fin () with fin_e -> raise (Finally (e, fin_e)));
            raise e
    in
    Stream.from f

  let stream_from_string ?buf ?fname ?lnum s =
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v (Lexing.from_string s)

  let stream_from_channel ?buf ?fin ?fname ?lnum ic =
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v ?fin lexbuf

  let stream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v ~fin lexbuf

  type json_line = [ `Json of t | `Exn of exn ]

  let linestream_from_channel
      ?buf ?(fin = fun () -> ()) ?fname ?lnum:(lnum0 = 1) ic =
    let buf =
      match buf with
          None -> Some (Buffer.create 256)
        | Some _ -> buf
    in
    let f i =
      try
        let line = input_line ic in
        let lnum = lnum0 + i in
        Some (`Json (from_string ?buf ?fname ~lnum line))
      with
          End_of_file -> fin (); None
        | e -> Some (`Exn e)
    in
    Stream.from f

  let linestream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    linestream_from_channel ?buf ~fin ?fname ?lnum ic

  let prettify ?std s =
    pretty_to_string ?std (from_string s)

  let compact ?std s =
    to_string (from_string s)

  let validate_json _path _value = None


# 3411 "lib/read.ml"
# 91 "yojson.cppo.ml"
end
