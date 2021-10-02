(** Two exceptions recommended for use with the library, though currently not
raised by any function in this module. Cpdfcommand uses them extensively. *)
exception SoftError of string

exception HardError of string

let error s = raise (SoftError s)


