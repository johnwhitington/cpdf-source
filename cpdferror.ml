(** Two exceptions recommended for use with the library. *)
exception SoftError of string

exception HardError of string

let error s = raise (SoftError s)
