(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

type tm =
  { tm_sec : int;               (** Seconds 0..60 *)
    tm_min : int;               (** Minutes 0..59 *)
    tm_hour : int;              (** Hours 0..23 *)
    tm_mday : int;              (** Day of month 1..31 *)
    tm_mon : int;               (** Month of year 0..11 *)
    tm_year : int;              (** Year - 1900 *)
    tm_wday : int;              (** Day of week (Sunday is 0) *)
    tm_yday : int;              (** Day of year 0..365 *)
    tm_isdst : bool;            (** Daylight time savings in effect *)
  }

val gettimeofday : unit -> float

val localtime : float -> tm

