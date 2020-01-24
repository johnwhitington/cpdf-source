external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let ($) g f = fun x -> g (f x)
let id x = x
let tee f x = ignore @@ f x; x

let (!%) = Printf.sprintf
let (!$) x = Lazy.force x

let slist delim show l =
  String.concat delim @@ List.map show l

let string_of_chars = slist "" (String.make 1)

let string1 c = String.make 1 c

type ('l, 'r) either = Inl of 'l | Inr of 'r

let maybe f x =
  try Inl (f x) with e -> Inr e
let value = function
    Inl v -> v | Inr e -> raise e

let open_with (opn, close) filepath f =
  let ch = opn filepath in
  value @@ tee (fun _ -> close ch) (maybe f ch)

let open_in_with filepath f = open_with (open_in, close_in) filepath f

let to_hex n =
  let to_char = function
    | x when 0<=x && x<=9 -> (string_of_int x).[0]
    | x when 10<=x && x<=15 -> char_of_int (int_of_char 'A'+(x-10))
    | _ -> failwith"tohex MNH"
  in
  let rec iter store n =
    if n < 16 then
      to_char n :: store
    else
      let r,q = n / 16, n mod 16 in
      iter (to_char q :: store) r
  in
  if n < 0 then raise (Invalid_argument (!%"to_hex: (%d)" n))
  else string_of_chars @@ iter [] n

