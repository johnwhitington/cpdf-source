(* C-Style strftime *)
open Pdfutil

let strf_A t =
  match t.Unix.tm_wday with
  | 0 -> "Sunday" | 1 -> "Monday" | 2 -> "Tuesday"
  | 3 -> "Wednesday" | 4 -> "Thursday" | 5 -> "Friday"
  | 6 -> "Saturday"
  | _ -> "strf_AFail"

let strf_a t =
  String.sub (strf_A t) 0 3

let strf_B t =
  match t.Unix.tm_mon with
  | 0 -> "January" | 1 -> "February" | 2 -> "March" | 3 -> "April"
  | 4 -> "May" | 5 -> "June" | 6 -> "July" | 7 -> "August"
  | 8 -> "September" | 9 -> "October" | 10 -> "November"
  | 11 -> "December" | _ -> "strf_Bfail"

let strf_b t =
  String.sub (strf_B t) 0 3

let strf_d t =
  let s = string_of_int t.Unix.tm_mday in 
    if String.length s = 1 then "0" ^ s else s

let strf_e t =
  let s = string_of_int t.Unix.tm_mday in
    if String.length s = 1 then " " ^ s else s

let strf_H t =
  let s = string_of_int t.Unix.tm_hour in
    if String.length s = 1 then "0" ^ s else s

let strf_I t =
  let s = string_of_int (t.Unix.tm_hour mod 12) in
    if String.length s = 1 then "0" ^ s else s

let strf_j t =
  let s = string_of_int t.Unix.tm_yday in
    match String.length s with
    | 1 -> "00" ^ s
    | 2 -> "0" ^ s
    | _ -> s

let strf_m t =
  let s = string_of_int (t.Unix.tm_mon + 1) in
    if String.length s = 1 then "0" ^ s else s

let strf_M t =
  let s = string_of_int t.Unix.tm_min in
    if String.length s = 1 then "0" ^ s else s

let strf_p t =
  if t.Unix.tm_hour >= 12 then "p.m" else "a.m"

let strf_S t =
  let s = string_of_int t.Unix.tm_sec in
    if String.length s = 1 then "0" ^ s else s

let strf_T t =
  strf_H t ^ ":" ^ strf_M t ^ ":" ^ strf_S t

let strf_u t =
  match t.Unix.tm_wday with
  | 0 -> "7"
  | n -> string_of_int (n + 1)

let strf_w t =
  string_of_int t.Unix.tm_wday

let strf_Y t =
  string_of_int (t.Unix.tm_year + 1900)

let strf_percent _ = "%"

let strftime_pairs =
  ["%a", strf_a; "%A", strf_A; "%b", strf_b; "%B", strf_B;
   "%d", strf_d; "%e", strf_e; "%H", strf_H;
   "%I", strf_I; "%j", strf_j; "%m", strf_m; "%M", strf_M;
   "%p", strf_p; "%S", strf_S; "%T", strf_T; "%u", strf_u;
   "%w", strf_w; "%Y", strf_Y; "%%", strf_percent]

let strftime text =
  let time = Unix.localtime (Unix.gettimeofday ()) in
    let text = ref text in
      iter
        (fun (search, replace_fun) ->
           text := string_replace_all search (replace_fun time) !text)
        strftime_pairs;
      !text

