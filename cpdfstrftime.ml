(* C-Style strftime *)
open Pdfutil

type t =
  {_tm_sec : int;
   _tm_min : int;
   _tm_hour : int;
   _tm_mday : int;
   _tm_mon : int;
   _tm_year : int;
   _tm_wday : int;
   _tm_yday : int;
   _tm_isdst : bool}

let dummy =
  {_tm_sec = 0;
   _tm_min = 0;
   _tm_hour = 0;
   _tm_mday = 1;
   _tm_mon = 0;
   _tm_year = 2000;
   _tm_wday = 0;
   _tm_yday = 0;
   _tm_isdst = false}

let strf_A t =
  match t._tm_wday with
  | 0 -> "Sunday" | 1 -> "Monday" | 2 -> "Tuesday"
  | 3 -> "Wednesday" | 4 -> "Thursday" | 5 -> "Friday"
  | 6 -> "Saturday"
  | _ -> "strf_AFail"

let strf_a t =
  String.sub (strf_A t) 0 3

let strf_B t =
  match t._tm_mon with
  | 0 -> "January" | 1 -> "February" | 2 -> "March" | 3 -> "April"
  | 4 -> "May" | 5 -> "June" | 6 -> "July" | 7 -> "August"
  | 8 -> "September" | 9 -> "October" | 10 -> "November"
  | 11 -> "December" | _ -> "strf_Bfail"

let strf_b t =
  String.sub (strf_B t) 0 3

let strf_d t =
  let s = string_of_int t._tm_mday in 
    if String.length s = 1 then "0" ^ s else s

let strf_e t =
  let s = string_of_int t._tm_mday in
    if String.length s = 1 then " " ^ s else s

let strf_H t =
  let s = string_of_int t._tm_hour in
    if String.length s = 1 then "0" ^ s else s

let strf_I t =
  let s = string_of_int (t._tm_hour mod 12) in
    if String.length s = 1 then "0" ^ s else s

let strf_j t =
  let s = string_of_int t._tm_yday in
    match String.length s with
    | 1 -> "00" ^ s
    | 2 -> "0" ^ s
    | _ -> s

let strf_m t =
  let s = string_of_int (t._tm_mon + 1) in
    if String.length s = 1 then "0" ^ s else s

let strf_M t =
  let s = string_of_int t._tm_min in
    if String.length s = 1 then "0" ^ s else s

let strf_p t =
  if t._tm_hour >= 12 then "p.m" else "a.m"

let strf_S t =
  let s = string_of_int t._tm_sec in
    if String.length s = 1 then "0" ^ s else s

let strf_T t =
  strf_H t ^ ":" ^ strf_M t ^ ":" ^ strf_S t

let strf_u t =
  match t._tm_wday with
  | 0 -> "7"
  | n -> string_of_int (n + 1)

let strf_w t =
  string_of_int t._tm_wday

let strf_Y t =
  string_of_int (t._tm_year + 1900)

let strf_percent _ = "%"

let strftime_pairs =
  ["%a", strf_a; "%A", strf_A; "%b", strf_b; "%B", strf_B;
   "%d", strf_d; "%e", strf_e; "%H", strf_H;
   "%I", strf_I; "%j", strf_j; "%m", strf_m; "%M", strf_M;
   "%p", strf_p; "%S", strf_S; "%T", strf_T; "%u", strf_u;
   "%w", strf_w; "%Y", strf_Y; "%%", strf_percent]

let contents_of_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

(* Platform-independent current time and date with no Unix module *)
let utf8_of_utf16le s =
  implode (drop_evens (tl (tl (explode s))))

let year_day d m y =
  let n1 = 275 * m / 9 in
  let n2 = (m + 9) / 12 in
  let n3 = 1 + (y - 4 * (y / 4) + 2) / 3 in
    n1 - n2 * n3 + d - 30

(* OCAML *)
let js_date () = dummy

(* JS *)
(*
let js_date () =
  let d = Unix.localtime (Unix.time ()) in
  {_tm_sec = d.tm_sec;
   _tm_min = d.tm_min;
   _tm_hour = d.tm_hour;
   _tm_mday = d.tm_mday;
   _tm_mon = d.tm_mon;
   _tm_year = d.tm_year;
   _tm_wday = d.tm_wday;
   _tm_yday = d.tm_yday;
   _tm_isdst = d.tm_isdst}
*)

let return_date () =
  match Sys.backend_type with Sys.Other "js_of_ocaml" -> js_date () | _ ->
  match Sys.os_type with
    "Unix" ->
      (* Call the POSIX 'date' program, redirected to a temp file, and parse. *)
      let tempfile = Filename.temp_file "cpdf" "strftime" in
      let command = Filename.quote_command "date" ~stdout:tempfile ["+%S-%M-%H-%d-%m-%Y-%w-%j"] in
      let outcode = Sys.command command in
      if outcode > 0 then raise (Failure "Date command returned non-zero exit code") else
        let r = contents_of_file tempfile in
        let get_int o l = int_of_string (String.sub r o l) in
          Sys.remove tempfile;
          {_tm_sec = get_int 0 2;
           _tm_min = get_int 3 2;
           _tm_hour = get_int 6 2;
           _tm_mday = get_int 9 2;
           _tm_mon = get_int 12 2 - 1;
           _tm_year = get_int 15 4 - 1900;
           _tm_wday = get_int 20 1;
           _tm_yday = get_int 22 3 - 1;
           _tm_isdst = false}
   | "Win32" | "Cygwin" ->
      (* Run 'wmic os get LocalDateTime' (exists on XP Pro or later, Vista or later). *)
      let get_int r o l = int_of_string (String.sub r o l) in
      let tempfile = Filename.temp_file "cpdf" "strftime" in
      let command = Filename.quote_command "wmic.exe" ~stdout:tempfile ["os"; "get"; "LocalDateTime"] in
      let outcode = Sys.command command in
      if outcode > 0 then raise (Failure "wmic.exe os get LocalDateTime command returned non-zero exit code") else
        let r = contents_of_file tempfile in
        Sys.remove tempfile;
        let r = utf8_of_utf16le r in
      (* Run 'wmic path win32_localtime get dayofweek' (exists on XP Pro or later, Vista or later). *)
      let tempfile = Filename.temp_file "cpdf" "strftime" in
      let command = Filename.quote_command "wmic.exe" ~stdout:tempfile ["path"; "win32_localtime"; "get"; "dayofweek"] in
      let outcode = Sys.command command in
      if outcode > 0 then raise (Failure "wmic.exe path win32_localtime get dayofweek returned non-zero exit code") else
        let r2 = contents_of_file tempfile in
        Sys.remove tempfile;
        let r2 = utf8_of_utf16le r2 in
        let day = get_int r 35 2 in
        let month = get_int r 33 2 in
        let year = get_int r 29 4 in
        {_tm_sec = get_int r 41 2;
         _tm_min = get_int r 39 2;
         _tm_hour = get_int r 37 2;
         _tm_mday = day;
         _tm_mon = month - 1;
         _tm_year = year - 1900;
         _tm_wday = get_int r2 13 1;
         _tm_yday = year_day day month year - 1;
         _tm_isdst = false}
  | _ -> failwith "Unknown Sys.os_type in Cpdfstrftime.return_date"

let current_time () =
  try return_date () with
    e ->
      Pdfe.log (Printf.sprintf "Failed to retrieve time due to %s\n" (Printexc.to_string e));
      {_tm_sec = 0;
       _tm_min = 0;
       _tm_hour = 0;
       _tm_mday = 1;
       _tm_mon = 0;
       _tm_year = 0;
       _tm_wday = 0;
       _tm_yday = 0;
       _tm_isdst = false}

let strftime ?time text =
  let time =
    match time with None -> current_time () | Some t -> t
  in
    let text = ref text in
      iter
        (fun (search, replace_fun) ->
           text := string_replace_all search (replace_fun time) !text)
        strftime_pairs;
      !text
