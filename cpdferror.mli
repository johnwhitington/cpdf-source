(** Error handling *)

(** Soft error in CPDF. *)
exception SoftError of string

(** Hard error in CPDF *)
exception HardError of string

(** Raise SoftError with the given string. *)
val error : string -> 'a
