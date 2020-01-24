type t =
    String of string
  | Number of string (* We keep its string repr. *)
  | Object of obj
  | Array of t list
  | Bool of bool
  | Null
and obj = (string * t) list

exception JSON_NotObject of t
exception JSON_InvalidField of string
exception JSON_CastErr of string
exception JSON_UnknownErr of string

(** {6 Printer and formatter} *)
val show : t -> string
val format : Format.formatter -> t -> unit

(** {6 Object field access} *)

val getf : string -> t -> t
(** Get field from an object. Failure raises an exception. *)

val getf_opt : string -> t -> t option
(** Get field from an object. Failure is reported as [None] *)

(** {6 Coercions. They may fail and raise JSON_CastErr.} *)
val as_bool   : t -> bool
val as_object : t -> obj
val as_float  : t -> float
val as_string : t -> string
val as_list   : t -> t list
val as_int    : t -> int

(** {6 Parsers} *)
val parse_ch : in_channel -> t
val parse : string -> t
val parse_function : (bytes -> int -> int) -> t
