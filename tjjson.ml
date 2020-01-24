(* With regard to tjjson.ml(i), tjllist.ml(i), tjbase64.ml(i),
 * tjparserMonad.ml(i), tjutil.ml(i), tjutf16.ml(i): *)

(* Copyright (c) 2011 Yoshihiro Imai

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.*)

open Tjutil
open TjparserMonad
module P = TjparserMonad

type t =
  | String of string
  | Number of string (* float is not appropriate for decoding 64bit int *)
  | Object of obj
  | Array of t list
  | Bool of bool
  | Null
and obj = (string * t) list

exception JSON_NotObject of t
exception JSON_InvalidField of (string)
exception JSON_CastErr of string
exception JSON_UnknownErr of string

(* CR jfuruse: it uses string concat. Very bad efficiency. *)
let show =
  let rec show_aux depth = function
    | String s -> "str(" ^s^ ")"
    | Number x -> !%"num(%s)" x
    | Object fs ->
	let indent d = String.make d '\t' in
	"{\n"
	^indent depth^ slist (",\n"^ indent depth) (fun (k,v) -> k^":"^ (show_aux (depth+1)) v) fs
	^"\n"^indent(depth-1)^"}"
    | Array xs -> "[" ^slist "," (show_aux depth) xs ^ "]"
    | Bool true -> "TRUE"
    | Bool false -> "FALSE"
    | Null -> "NULL"
  in
  show_aux 1

  let rec format_list sep f ppf = function
    | [] -> ()
    | [x] -> f ppf x
    | x::xs -> f ppf x; Format.fprintf ppf sep; format_list sep f ppf xs

  (* CR jfuruse: Need test! *)
  let rec format ppf = 
    let open Format in
    function
      | String s -> 
          let buf = Buffer.create (String.length s * 2) in
          Buffer.add_char buf '"';
          for i = 0 to String.length s - 1 do
            let c = String.unsafe_get s i in
            match c with  
            | '"' -> Buffer.add_string buf "\\\""
            | '\\' -> Buffer.add_string buf "\\\\"
            | '\b' -> Buffer.add_string buf "\\b"
            | '\012' -> Buffer.add_string buf "\\f"
            | '\n' -> Buffer.add_string buf "\\n"
            | '\r' -> Buffer.add_string buf "\\r"
            | '\t' -> Buffer.add_string buf "\\t"
            | _ when Char.code c <= 32 && c <> ' ' -> 
                Printf.ksprintf (Buffer.add_string buf) "\\u%04X" (Char.code c)
            | _ -> Buffer.add_char buf c
          done;
          Buffer.add_char buf '"';
          pp_print_string ppf (Buffer.contents buf)
      | Number s -> fprintf ppf "%s" s
      | Object o -> 
          fprintf ppf "{ @[%a }@]"
            (format_list ",@ " (fun ppf (s,v) -> fprintf ppf "@[\"%s\": @[<2>%a@]@]" s format v)) o
      | Array ts -> 
          fprintf ppf "[ @[%a ]@]"
            (format_list ",@ " format) ts
      | Bool b -> fprintf ppf "%b" b
      | Null -> fprintf ppf "null"

let getf field t =
  match t with
  | Object o ->
      begin try List.assoc field o with
      | _ -> raise (JSON_InvalidField (field))
      end
  | _ -> raise (JSON_NotObject t)

let getf_opt field t =
  match t with
  | Object o ->
      begin try Some (List.assoc field o) with
      | _ -> None
      end
  | _ -> None
      
let as_bool = function
  | Bool true -> true
  | Bool false -> false
  | v -> raise (JSON_CastErr ("as_bool:" ^ show v))

let as_object = function
  | Object obj -> obj
  | v -> raise (JSON_CastErr ("as_object:" ^ show v))

let as_float = function
  | Number s -> float_of_string s (* may fail, or returns wrong result *)
  | v -> raise (JSON_CastErr ("as_float:" ^ show v))

let as_string = function
  | String s -> s
  | v -> raise (JSON_CastErr ("as_string:" ^ show v))

let as_list = function
  | Array l -> l
  | v -> raise (JSON_CastErr ("as_list:" ^ show v))

let as_int = function
  | Number s -> int_of_string s (* may fail, or returns wrong result *)
  | v -> raise (JSON_CastErr ("as_int:" ^ show v))


(*parser*)

let whitespace = many (char '\n' <|> char ' ' <|> char '\t' <|> char '\r')

let string s =
  let rec iter i =
    if i < String.length s then
      char s.[i] >> iter (i+1)
    else return s
  in
  iter 0

(*
let alp =
  char1 >>= fun c -> if c<>' ' && c<>'\n' && c<>'\t' && c<>'\r' then return c else error""

let alps0 = many alp
let alps = alp >>= fun c -> many alp >>= fun cs -> return (string_of_chars (c::cs))
*)

type token =
  | ObjOpen
  | ObjClose
  | ListOpen
  | ListClose
  | Comma
  | Colon

  | TTrue
  | TFalse
  | TNull
  | TString of string
  | TNumber of string (* we keep the string repr. *)

let lit_string =
  let four_hex_digits =
    let hex = char1 >>= function
      | '0'..'9' | 'A'..'F' | 'a'..'f' as c -> return c
      | _ -> error ""
    in
    hex >>= fun d1 -> hex >>= fun d2 -> hex >>= fun d3 -> hex >>= fun d4 ->
      let s = string_of_chars [d1;d2;d3;d4] in
      let n = int_of_string ("0x" ^ Tjutf16.utf16c_to_utf8c s) in
      let m, n1 = n / (16*16), n mod (16*16) in
      let n3,n2 = m / (16*16), m mod (16*16) in
      let cs = List.map char_of_int
	begin match [n3;n2;n1] with
	| [0; 0; _] ->         [n1]
	| [0; _; _] ->     [n2; n1]
	| _         -> [n3; n2; n1]
	end
      in
      return (string_of_chars cs)
  in
  let lit_char =
    char1 >>= function
      | '\"' -> error ""
      | '\\' -> char1 >>=
	  begin function
	  | '\"' | '\\' | '/' as c -> return (string1 c)
	  | 'b' -> return "\b"
(*	  | 'f' -> return "\f"*)
	  | 'n' -> return "\n"
	  | 'r' -> return "\r"
	  | 't' -> return "\t"
	  | 'u' -> four_hex_digits
	  | _ -> error ""
	  end
      | c -> return (string1 c)
  in
  char '\"' >> many lit_char >>= fun ss -> char '\"' >> return (TString (slist "" id ss))

let digits =
  let digit =
    char1 >>= function
      | '0'..'9' | '-' | '.' | 'e' | 'E' | '+' as c -> return c
      | _ -> error "digit"
  in
  many1 digit >>= (return $ string_of_chars)

let lit_number = (* TODO *)
  (* We cannot simply use [float_of_string] here, if we want to handle int64.
     int64 and double are both 64bits, which means double cannot express all the int64!!! 
  *)
  digits >>= fun x -> return (TNumber x)

let token1 =
  let aux =
  (char '{' >> return ObjOpen)
    <|>
  (char '}' >> return ObjClose)
    <|>
  (char '[' >> return ListOpen)
    <|>
  (char ']' >> return ListClose)
    <|>
  (char ',' >> return Comma)
    <|>
  (char ':' >> return Colon)
    <|>
  (string "true" >> return TTrue)
    <|>
  (string "false" >> return TFalse)
    <|>
  (string "null" >> return TNull)
    <|>
  lit_string
    <|>
  lit_number
  in
  whitespace >> aux

let token t =
  token1 >>= fun x -> if t = x then return t else error "token"

let json_string =
  token1 >>= function TString s -> return s | _ -> error "json_string"

let json_number =
  token1 >>= function TNumber x -> return x | _ -> error "json_number"

let rec json (): t P.t =
  begin
  let field =
    json_string >>= fun key -> token Colon >> json () >>= fun v -> return (key, v)
  in
  (token ObjOpen >> sep (token Comma) field >>= fun fields -> token ObjClose >>
    return @@ Object fields)
    <|>
  (token ListOpen >>= (fun _ -> sep (token Comma) (json()) >>= fun vs -> opt (token Comma) >> token ListClose >>
    return @@ Array vs))
    <|>
  (token TTrue >> return (Bool true))
    <|>
  (token TFalse >> return (Bool false))
    <|>
  (token TNull >> return Null)
    <|>
  (json_string >>= fun s -> return @@ String s)
    <|>
  (json_number >>= fun x -> return @@ Number x)
  end 
  

let parse_ch ch = run_ch (json()) ch

let parse s = run_string (json()) s

let parse_function f = run_function (json()) f
