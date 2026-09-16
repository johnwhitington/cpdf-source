(*---------------------------------------------------------------------------
   Copyright (c) 2008 The uuidm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Bits *)

type bits62 = int64
type bits4 = int
type bits12 = int

(* Hashing *)

let md5 = Digest.string
let sha_1 s =
  (* Based on pseudo-code of RFC 3174. Slow and ugly but does the job. *)
  let sha_1_pad s =
    let len = String.length s in
    let blen = 8 * len in
    let rem = len mod 64 in
    let mlen = if rem > 55 then len + 128 - rem else len + 64 - rem in
    let m = Bytes.create mlen in
    Bytes.blit_string s 0 m 0 len;
    Bytes.fill m len (mlen - len) '\x00';
    Bytes.set m len '\x80';
    if Sys.word_size > 32 then begin
      Bytes.set_uint8 m (mlen - 8) (blen lsr 56 land 0xFF);
      Bytes.set_uint8 m (mlen - 7) (blen lsr 48 land 0xFF);
      Bytes.set_uint8 m (mlen - 6) (blen lsr 40 land 0xFF);
      Bytes.set_uint8 m (mlen - 5) (blen lsr 32 land 0xFF);
    end;
    Bytes.set_uint8 m (mlen - 4) (blen lsr 24 land 0xFF);
    Bytes.set_uint8 m (mlen - 3) (blen lsr 16 land 0xFF);
    Bytes.set_uint8 m (mlen - 2) (blen lsr 8 land 0xFF);
    Bytes.set_uint8 m (mlen - 1) (blen land 0xFF);
    m
  in
  (* Operations on int32 *)
  let ( &&& ) = ( land ) in
  let ( lor ) = Int32.logor in
  let ( lxor ) = Int32.logxor in
  let ( land ) = Int32.logand in
  let ( ++ ) = Int32.add in
  let lnot = Int32.lognot in
  let sl = Int32.shift_left in
  let cls n x = (sl x n) lor (Int32.shift_right_logical x (32 - n)) in
  (* Start *)
  let m = sha_1_pad s in
  let w = Array.make 16 0l in
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in
  let a = ref 0l in
  let b = ref 0l in
  let c = ref 0l in
  let d = ref 0l in
  let e = ref 0l in
  for i = 0 to ((Bytes.length m) / 64) - 1 do              (* For each block *)
    (* Fill w *)
    let base = i * 64 in
    for j = 0 to 15 do w.(j) <- Bytes.get_int32_be m (base + (j * 4)); done;
    (* Loop *)
    a := !h0; b := !h1; c := !h2; d := !h3; e := !h4;
    for t = 0 to 79 do
      let f, k =
        if t <= 19 then (!b land !c) lor ((lnot !b) land !d), 0x5A827999l else
        if t <= 39 then !b lxor !c lxor !d, 0x6ED9EBA1l else
        if t <= 59 then
          (!b land !c) lor (!b land !d) lor (!c land !d), 0x8F1BBCDCl
        else
        !b lxor !c lxor !d, 0xCA62C1D6l
      in
      let s = t &&& 0xF in
      if (t >= 16) then begin
        w.(s) <- cls 1 begin
            w.((s + 13) &&& 0xF) lxor
            w.((s + 8) &&& 0xF) lxor
            w.((s + 2) &&& 0xF) lxor
            w.(s)
          end
      end;
      let temp = (cls 5 !a) ++ f ++ !e ++ w.(s) ++ k in
      e := !d;
      d := !c;
      c := cls 30 !b;
      b := !a;
      a := temp;
    done;
    (* Update *)
    h0 := !h0 ++ !a;
    h1 := !h1 ++ !b;
    h2 := !h2 ++ !c;
    h3 := !h3 ++ !d;
    h4 := !h4 ++ !e
  done;
  let h = Bytes.create 20 in
  let i2s h k i = Bytes.set_int32_be h k i in
  i2s h 0 !h0;
  i2s h 4 !h1;
  i2s h 8 !h2;
  i2s h 12 !h3;
  i2s h 16 !h4;
  Bytes.unsafe_to_string h

(* Uuids *)

type t = string (* 16 bytes *)

let make u ~version =
  let b6 = (version lsl 4) lor ((Bytes.get_uint8 u 6) land 0b0000_1111) in
  let b8 = 0b1000_0000 lor ((Bytes.get_uint8 u 8) land 0b0011_1111) in
  Bytes.set_uint8 u 6 b6;
  Bytes.set_uint8 u 8 b8;
  Bytes.unsafe_to_string u

let make_named ~version digest ns n =
  let hash = Bytes.unsafe_of_string (digest (ns ^ n)) in
  make (Bytes.sub hash 0 16) ~version

let v3 ns n = make_named ~version:3 md5 ns n
let v5 ns n = make_named ~version:5 sha_1 ns n
let v4 b = make (Bytes.sub b 0 16) ~version:4
let v7 ~time_ms ~rand_a ~rand_b =
  let u = Bytes.create 16 in
  Bytes.set_int64_be u 0 (Int64.shift_left time_ms 16);
  Bytes.set_int16_be u 6 rand_a;
  Bytes.set_int64_be u 8 rand_b;
  make u ~version:7

let v7_ns ~time_ns ~rand_b =
  let ns_in_ms = 1_000_000L in
  let sub_ms_frac_multiplier = Int64.unsigned_div Int64.minus_one ns_in_ms in
  let u = Bytes.create 16 in
  (* RFC 9562 requires we use 48 bits for a timestamp in milliseconds, and
     allows for 12 bits to store a sub-millisecond fraction. We get the
     latter by multiplying to put the fraction in a 64-bit range, then
     shifting into 12 bits. *)
  let ms = Int64.unsigned_div time_ns ns_in_ms in
  let ns = Int64.unsigned_rem time_ns ns_in_ms in
  let sub_ms_frac =
    Int64.shift_right_logical (Int64.mul ns sub_ms_frac_multiplier) 52
  in
  Bytes.set_int64_be u 0 (Int64.shift_left ms 16);
  Bytes.set_int16_be u 6 (Int64.to_int sub_ms_frac);
  Bytes.set_int64_be u 8 rand_b;
  make u ~version:7

let v8 s =
  let l = String.length s in
  if l = 16 then make (Bytes.of_string s) ~version:8 else
  invalid_arg (Printf.sprintf "expected 16 bytes but found: %d" l)

(* Generators *)

type posix_ms_clock = unit -> int64

let v4_random rstate =
  let r0 = Random.State.bits64 rstate in
  let r1 = Random.State.bits64 rstate in
  let u = Bytes.create 16 in
  Bytes.set_int64_be u 0 r0;
  Bytes.set_int64_be u 8 r1;
  make u ~version:4

let v4_gen rstate = function () -> v4_random rstate

let v7_non_monotonic_gen ~now_ms rstate =
  fun () ->
  let time_ms = now_ms () in
  let rand_a = Random.State.bits (* 30 bits *) rstate  in
  let rand_b = Random.State.bits64 rstate in
  v7 ~time_ms ~rand_a ~rand_b

let v7_monotonic_gen ~now_ms rstate =
  let last_ms = ref 0L in
  let count = ref 0 in
  fun () ->
    let time_ms = now_ms () in
    let rand_b = Random.State.bits64 rstate in
    if Int64.equal time_ms !last_ms then
      let rand_a = incr count; !count in
      if rand_a >= 4096 then None else Some (v7 ~time_ms ~rand_a ~rand_b)
    else
      (count := 0; last_ms := time_ms; Some (v7 ~time_ms ~rand_a:0 ~rand_b))

(* Constants *)

let nil = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
let max = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
let ns_dns = "\x6b\xa7\xb8\x10\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"
let ns_url = "\x6b\xa7\xb8\x11\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"
let ns_oid = "\x6b\xa7\xb8\x12\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"
let ns_X500 ="\x6b\xa7\xb8\x14\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"

(* Properties *)

let variant u = (String.get_uint8 u 8) lsr 4
let version u = (String.get_uint8 u 6) lsr 4
let time_ms u =
  let variant = variant u in
  if not (0x8 <= variant && variant <= 0xB && version u = 7) then None else
  Some (Int64.shift_right_logical (String.get_int64_be u 0) 16)

(* Predicates and comparisons *)

let equal = String.equal
let compare = String.compare

(* Standard binary format *)

let to_binary_string s = s
let of_binary_string ?(pos = 0) s =
  let len = String.length s in
  if pos + 16 > len then None else
  if pos = 0 && len = 16 then Some s else
  Some (String.sub s pos 16)

(* Mixed endian binary format *)

let mixed_swaps s =
  let swap b i j =
    let t = Bytes.get b i in
    Bytes.set b i (Bytes.get b j);
    Bytes.set b j t
  in
  let b = Bytes.of_string s in
  swap b 0 3; swap b 1 2;
  swap b 4 5; swap b 6 7;
  Bytes.unsafe_to_string b

let to_mixed_endian_binary_string s = mixed_swaps s
let of_mixed_endian_binary_string ?pos s =
  Option.map mixed_swaps (of_binary_string ?pos s)

(* Unsafe conversions *)

let unsafe_of_binary_string u = u
let unsafe_to_binary_string u = u

(* US-ASCII format *)

let of_string ?(pos = 0) s =
  let len = String.length s in
  if
    pos + 36 > len || s.[pos + 8] <> '-' || s.[pos + 13] <> '-' ||
    s.[pos + 18] <> '-' || s.[pos + 23] <> '-'
  then
    None
  else try
    let u = Bytes.create 16 in
    let i = ref 0 in
    let j = ref pos in
    let ihex c =
      let i = Char.code c in
      if i < 0x30 then raise Exit else
      if i <= 0x39 then i - 0x30 else
      if i < 0x41 then raise Exit else
      if i <= 0x46 then i - 0x37 else
      if i < 0x61 then raise Exit else
      if i <= 0x66 then i - 0x57 else
      raise Exit
    in
    let byte s j = Char.unsafe_chr (ihex s.[j] lsl 4 lor ihex s.[j + 1]) in
    while (!i < 4) do Bytes.set u !i (byte s !j); j := !j + 2; incr i done;
    incr j;
    while (!i < 6) do Bytes.set u !i (byte s !j); j := !j + 2; incr i done;
    incr j;
    while (!i < 8) do Bytes.set u !i (byte s !j); j := !j + 2; incr i done;
    incr j;
    while (!i < 10) do Bytes.set u !i (byte s !j); j := !j + 2; incr i done;
    incr j;
    while (!i < 16) do Bytes.set u !i (byte s !j); j := !j + 2; incr i done;
    Some (Bytes.unsafe_to_string u)
  with Exit -> None

let to_string ?(upper = false) u =
  let hbase = if upper then 0x37 else 0x57 in
  let hex hbase i = Char.unsafe_chr (if i < 10 then 0x30 + i else hbase + i) in
  let s = Bytes.of_string "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX" in
  let i = ref 0 in
  let j = ref 0 in
  let byte s i c =
    Bytes.set s i @@ hex hbase (c lsr 4);
    Bytes.set s (i + 1) @@ hex hbase (c land 0x0F)
  in
  while (!j < 4) do byte s !i (Char.code u.[!j]); i := !i + 2; incr j; done;
  incr i;
  while (!j < 6) do byte s !i (Char.code u.[!j]); i := !i + 2; incr j; done;
  incr i;
  while (!j < 8) do byte s !i (Char.code u.[!j]); i := !i + 2; incr j; done;
  incr i;
  while (!j < 10) do byte s !i (Char.code u.[!j]); i := !i + 2; incr j; done;
  incr i;
  while (!j < 16) do byte s !i (Char.code u.[!j]); i := !i + 2; incr j; done;
  Bytes.unsafe_to_string s

(* Pretty-printing *)

let pp ppf u = Format.pp_print_string ppf (to_string u)
let pp' ~upper ppf u = Format.pp_print_string ppf (to_string ~upper u)

(* Deprecated *)

let default_seed = lazy (Random.State.make_self_init ())

type version = [ `V3 of t * string | `V4 | `V5 of t * string ]
let v = function
| `V4 -> v4_random (Lazy.force default_seed)
| `V3 (ns, n) -> v3 ns n
| `V5 (ns, n) -> v5 ns n

let create = v (* deprecated *)
let pp_string ?upper ppf u = Format.pp_print_string ppf (to_string ?upper u)
let print = pp_string (* deprecated *)
let to_bytes = to_binary_string
let of_bytes = of_binary_string
let to_mixed_endian_bytes = to_mixed_endian_binary_string
let of_mixed_endian_bytes = of_mixed_endian_binary_string
let unsafe_of_bytes = unsafe_of_binary_string
let unsafe_to_bytes = unsafe_to_binary_string
