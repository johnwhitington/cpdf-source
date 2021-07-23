(** C-style strftime *)

(** Supports the following format specifiers:
    %a %A %b %B %s %e %H %I %j %m %M %p %S %T %u %w %Y %% *)

(** Our version of Unix's tm, so Unix not required *)
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

(** Get the time now *)
val current_time : unit -> t

(** A dummy time value *)
val dummy : t

(** Strftime. If time omitted, the current time is used. *)
val strftime : ?time:t -> string -> string
