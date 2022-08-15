# 1 "yojson.cppo.mli"
(**
   The Yojson library provides several types for representing JSON values, with different use cases.

   - The {{!basic}Basic} JSON type,
   - The {{!safe}Safe} JSON type, a superset of JSON with safer support for integers,
   - The {{!raw}Raw} JSON type, a superset of JSON, safer but less integrated with OCaml types.

Each of these different types have their own module.

*)

(** {1 Shared types and functions} *)

# 1 "common.mli"
val version : string

exception Json_error of string
(** Exception used:
    - in JSON readers, if parsing fails;
    - in JSON writers and pretty printing, if [float] value is not allowed in standard JSON. *)

val json_error : string -> 'a
(** @raise Json_error *)

type lexer_state = {
  buf : Buffer.t;
    (** Buffer used to accumulate substrings *)

  mutable lnum : int;
    (** Current line number (counting from 1) *)

  mutable bol : int;
    (** Absolute position of the first character of the current line
        (counting from 0) *)

  mutable fname : string option;
    (** Name referencing the input file in error messages *)
}

module Lexer_state :
sig
  type t = lexer_state = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }
end

val init_lexer :
  ?buf: Buffer.t ->
  ?fname: string ->
  ?lnum: int ->
  unit -> lexer_state
  (** Create a fresh lexer_state record. *)


(**/**)
(* begin undocumented section *)

exception End_of_array
exception End_of_object
exception End_of_tuple
exception End_of_input

(* end undocumented section *)
(**/**)

# 16 "yojson.cppo.mli"
(** {1:basic Basic JSON tree type} *)

module Basic :
sig
(**
   This module supports standard JSON nodes only, i.e. no special syntax
   for variants or tuples as supported by {!Yojson.Safe}.
   Arbitrary integers are not supported as they must all fit within the
   standard OCaml int type (31 or 63 bits depending on the platform).

   The main advantage of this module is its simplicity.
*)

# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 8 "type.ml"
    | `Int of int
    
# 14 "type.ml"
    | `Float of float
    
# 20 "type.ml"
    | `String of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 33 "type.ml"
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)
# 1 "write.mli"
(** {2 JSON writers} *)

val to_string :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  t -> string
  (** Write a compact JSON value to a string.
      @param buf allows to reuse an existing buffer created with
      [Buffer.create]. The buffer is cleared of all contents
      before starting and right before returning.
      @param len initial length of the output buffer.
      @param suf appended to the output as a suffix,
      defaults to empty string.
      @param std use only standard JSON syntax,
      i.e. convert tuples and variants into standard JSON (if applicable),
      refuse to print NaN and infinities,
      require the root node to be either an object or an array.
      Default is [false].
      @raise Json_error if [float] value is not allowed in standard JSON.
  *)

val to_channel :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  out_channel -> t -> unit
  (** Write a compact JSON value to a channel.
      Note: the [out_channel] is not flushed by this function.

      See [to_string] for the role of the optional arguments and raised exceptions. *)

val to_output :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  < output : string -> int -> int -> int; .. > -> t -> unit
  (** Write a compact JSON value to an OO channel.

      See [to_string] for the role of the optional arguments and raised exceptions. *)

val to_file :
  ?len:int ->
  ?std:bool ->
  ?suf:string ->
  string -> t -> unit
  (** Write a compact JSON value to a file.
      See [to_string] for the role of the optional arguments and raised exceptions.
      @param suf is a suffix appended to the output Newline by default
      for POSIX compliance. *)

val to_buffer :
  ?suf:string ->
  ?std:bool ->
  Buffer.t -> t -> unit
  (** Write a compact JSON value to an existing buffer.
      See [to_string] for the role of the optional argument and raised exceptions. *)

val seq_to_string :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  t Seq.t -> string
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a string.
      @param suf is the suffix ouf each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val seq_to_channel :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  out_channel -> t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a channel.
      @param suf is the suffix of each value written. Newline by default.
      See [to_channel] for the role of the optional arguments and raised exceptions. *)

val seq_to_file :
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  string -> t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a file.
      @param suf is the suffix of each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val seq_to_buffer :
  ?suf:string ->
  ?std:bool ->
  Buffer.t ->
  t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      an existing buffer.
      @param suf is the suffix of each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val write_t : Buffer.t -> t -> unit
(** Write the given JSON value to the given buffer.
    Provided as a writer function for atdgen.
*)

(** {2 Miscellaneous} *)

val sort : t -> t
  (** Sort object fields (stable sort, comparing field names
      and treating them as byte sequences) *)


(**/**)
(* begin undocumented section *)

val write_null : Buffer.t -> unit -> unit
val write_bool : Buffer.t -> bool -> unit
# 122 "write.mli"
val write_int : Buffer.t -> int -> unit
# 125 "write.mli"
val write_float : Buffer.t -> float -> unit
val write_std_float : Buffer.t -> float -> unit
val write_float_prec : int -> Buffer.t -> float -> unit
val write_std_float_prec : int -> Buffer.t -> float -> unit
# 131 "write.mli"
val write_string : Buffer.t -> string -> unit


# 144 "write.mli"
val write_assoc : Buffer.t -> (string * t) list -> unit
val write_list : Buffer.t -> t list -> unit

# 155 "write.mli"
val write_json : Buffer.t -> t -> unit
val write_std_json : Buffer.t -> t -> unit

(* end undocumented section *)
(**/**)
# 1 "monomorphic.mli"
val pp : Format.formatter -> t -> unit
  (** Pretty printer, useful for debugging *)

val show : t -> string
  (** Convert value to string, useful for debugging *)

val equal : t -> t -> bool
  (** [equal a b] is the monomorphic equality.
      Determines whether two JSON values are considered equal. In the case of
      JSON objects, the order of the keys does not matter, except for
      duplicate keys which will be considered equal as long as they are in the
      same input order.
    *)
# 1 "write2.mli"
(** {2 JSON pretty-printing} *)

val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  (** Pretty-print into a {!Format.formatter}.
      See [to_string] for the role of the optional [std] argument.
      @raise Json_error if [float] value is not allowed in standard JSON.

      @since 1.3.1 *)

val pretty_to_string : ?std:bool -> t -> string
  (** Pretty-print into a string.
      See [to_string] for the role of the optional [std] argument.
      See [pretty_print] for raised exceptions.
  *)

val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  (** Pretty-print to a channel.
      See [to_string] for the role of the optional [std] argument.
      See [pretty_print] for raised exceptions.
  *)
# 1 "read.mli"
val prettify : ?std:bool -> string -> string
  (** Combined parser and pretty-printer.
      See [to_string] for the role of the optional [std] argument and raised exceptions. *)

val compact : ?std:bool -> string -> string
  (** Combined parser and printer.
      See [to_string] for the role of the optional [std] argument and raised exceptions. *)

(** {2 JSON readers} *)

exception Finally of exn * exn
(** Exception describing a failure in both finalizer and parsing. *)

val from_string :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a string.
      @param buf use this buffer at will during parsing instead of creating
      a new one.
      @param fname data file name to be used in error messages. It does
      not have to be a real file.
      @param lnum number of the first line of input. Default is 1.
      @raise Json_error if parsing fails.
  *)

val from_channel :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t
  (** Read a JSON value from a channel.
      See [from_string] for the meaning of the optional arguments and raised exceptions. *)

val from_file :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a file.
      See [from_string] for the meaning of the optional arguments and raised exceptions. *)


type lexer_state = Lexer_state.t = {
  buf : Buffer.t;
  mutable lnum : int;
  mutable bol : int;
  mutable fname : string option;
}
    (** This alias is provided for backward compatibility.
        New code should refer to {!Yojson.lexer_state} directly.
    *)

val init_lexer :
  ?buf: Buffer.t ->
  ?fname: string ->
  ?lnum: int ->
  unit -> lexer_state
  (** This alias is provided for backward compatibility.
      New code should use {!Yojson.init_lexer} directly. *)

val from_lexbuf :
  lexer_state ->
  ?stream:bool ->
  Lexing.lexbuf -> t
  (** Read a JSON value from a lexbuf.
      A valid initial [lexer_state] can be created with [init_lexer].
      See [from_string] for the meaning of the optional arguments and raised exceptions.

      @param stream indicates whether more data may follow. The default value
      is false and indicates that only JSON whitespace can be found between
      the end of the JSON value and the end of the input. *)

val seq_from_string :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t Seq.t
  (** Input a sequence of JSON values from a string.
      Whitespace between JSON values is fine but not required.
      See [from_string] for the meaning of the optional arguments and raised exceptions. *)

val seq_from_channel :
  ?buf:Buffer.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t Seq.t
  (** Input a sequence of JSON values from a channel.
      Whitespace between JSON values is fine but not required.
      @param fin finalization function executed once when the end of the
      sequence is reached either because there is no more input or because
      the input could not be parsed, raising an exception.
      @raise Finally When the parsing and the finalizer both raised, [Finally (exn, fin_exn)]
      is raised, [exn] being the parsing exception and [fin_exn] the finalizer one.

      See [from_string] for the meaning of the other optional arguments and other raised exceptions. *)

val seq_from_file :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t Seq.t
  (** Input a sequence of JSON values from a file.
      Whitespace between JSON values is fine but not required.

      See [from_string] for the meaning of the optional arguments and raised exceptions. *)

val seq_from_lexbuf :
  lexer_state ->
  ?fin:(unit -> unit) ->
  Lexing.lexbuf -> t Seq.t
  (** Input a sequence of JSON values from a lexbuf.
      A valid initial [lexer_state] can be created with [init_lexer].
      Whitespace between JSON values is fine but not required.
      @raise Finally When the parsing and the finalizer both raised, [Finally (exn, fin_exn)]
      is raised, [exn] being the parsing exception and [fin_exn] the finalizer one.

      See [seq_from_channel] for the meaning of the optional [fin]
      argument and other raised exceptions. *)


type json_line = [ `Json of t | `Exn of exn ]
    (** The type of values resulting from a parsing attempt of a JSON value. *)

val lineseq_from_channel :
  ?buf:Buffer.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> json_line Seq.t
  (** Input a sequence of JSON values, one per line, from a channel.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [seq_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments and raised exceptions. *)

val lineseq_from_file :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> json_line Seq.t
  (** Input a sequence of JSON values, one per line, from a file.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [seq_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments and raised exceptions. *)

val read_t : lexer_state -> Lexing.lexbuf -> t
(** Read a JSON value from the given lexer_state and lexing buffer and return it.
    Provided as a reader function for atdgen.
*)


(**/**)
(* begin undocumented section *)

val finish_string : lexer_state -> Lexing.lexbuf -> string

val read_string : lexer_state -> Lexing.lexbuf -> string
val read_ident : lexer_state -> Lexing.lexbuf -> string

val map_string :
  lexer_state -> (string -> int -> int -> 'a) -> Lexing.lexbuf -> 'a
  (* equivalent to finish_string *)

val map_ident :
  lexer_state -> (string -> int -> int -> 'a) -> Lexing.lexbuf -> 'a
  (* equivalent to read_ident *)


type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]
val start_any_variant : lexer_state -> Lexing.lexbuf -> variant_kind
val finish_variant : lexer_state -> Lexing.lexbuf -> t option
val finish_skip_variant : lexer_state -> Lexing.lexbuf -> unit
val read_lt : lexer_state -> Lexing.lexbuf -> unit
val read_gt : lexer_state -> Lexing.lexbuf -> unit
val read_comma : lexer_state -> Lexing.lexbuf -> unit

val finish_stringlit : lexer_state -> Lexing.lexbuf -> string
val finish_skip_stringlit : lexer_state -> Lexing.lexbuf -> unit
val finish_escaped_char : lexer_state -> Lexing.lexbuf -> unit
val finish_comment : lexer_state -> Lexing.lexbuf -> unit


val read_space : lexer_state -> Lexing.lexbuf -> unit
val read_eof : Lexing.lexbuf -> bool
val read_null : lexer_state -> Lexing.lexbuf -> unit
val read_null_if_possible : lexer_state -> Lexing.lexbuf -> bool
val read_bool : lexer_state -> Lexing.lexbuf -> bool
val read_int : lexer_state -> Lexing.lexbuf -> int
val read_int8 : lexer_state -> Lexing.lexbuf -> char
val read_int32 : lexer_state -> Lexing.lexbuf -> int32
val read_int64 : lexer_state -> Lexing.lexbuf -> int64
val read_number : lexer_state -> Lexing.lexbuf -> float
val skip_ident : lexer_state -> Lexing.lexbuf -> unit

val read_sequence :
  ('a -> lexer_state -> Lexing.lexbuf -> 'a) ->
  'a ->
  lexer_state ->
  Lexing.lexbuf -> 'a

val read_list :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a list

val read_list_rev :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a list

val read_array_end : Lexing.lexbuf -> unit
val read_array_sep : lexer_state -> Lexing.lexbuf -> unit

val read_array :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a array

val read_tuple :
  (int -> 'a -> lexer_state -> Lexing.lexbuf -> 'a) ->
  'a ->
  lexer_state ->
  Lexing.lexbuf -> 'a

val start_any_tuple : lexer_state -> Lexing.lexbuf -> bool
val read_lpar : lexer_state -> Lexing.lexbuf -> unit
val read_rpar : lexer_state -> Lexing.lexbuf -> unit
val read_tuple_end : Lexing.lexbuf -> unit
val read_tuple_end2 : lexer_state -> bool -> Lexing.lexbuf -> unit
val read_tuple_sep : lexer_state -> Lexing.lexbuf -> unit
val read_tuple_sep2 : lexer_state -> bool -> Lexing.lexbuf -> unit
val read_lbr : lexer_state -> Lexing.lexbuf -> unit
val read_rbr : lexer_state -> Lexing.lexbuf -> unit

val read_fields :
  ('acc -> string -> lexer_state -> Lexing.lexbuf -> 'acc) ->
  'acc ->
  lexer_state ->
  Lexing.lexbuf -> 'acc

val read_abstract_fields :
  (lexer_state -> Lexing.lexbuf -> 'key) ->
  ('acc -> 'key -> lexer_state -> Lexing.lexbuf -> 'acc) ->
  'acc ->
  lexer_state ->
  Lexing.lexbuf -> 'acc

val read_lcurl : lexer_state -> Lexing.lexbuf -> unit
val read_object_end : Lexing.lexbuf -> unit
val read_object_sep : lexer_state -> Lexing.lexbuf -> unit
val read_colon : lexer_state -> Lexing.lexbuf -> unit

val read_json : lexer_state -> Lexing.lexbuf -> t
val skip_json : lexer_state -> Lexing.lexbuf -> unit
val buffer_json : lexer_state -> Lexing.lexbuf -> unit

(* end undocumented section *)
(**/**)
# 37 "yojson.cppo.mli"
(** This module provides combinators for extracting fields from JSON values. *)
module Util :
sig
# 1 "util.mli"
(**
   This module provides combinators for extracting fields from JSON
   values. This approach is recommended for reading a few fields
   from data returned by public APIs. However for more complex applications
   we recommend {{:https://github.com/ahrefs/atd}Atdgen}.

   Here is some sample JSON data:
{v
{
  "id": "398eb027",
  "name": "John Doe",
  "pages": [
    {
      "id": 1,
      "title": "The Art of Flipping Coins",
      "url": "http://example.com/398eb027/1"
    },
    {
      "id": 2,
      "deleted": true
    },
    {
      "id": 3,
      "title": "Artichoke Salad",
      "url": "http://example.com/398eb027/3"
    },
    {
      "id": 4,
      "title": "Flying Bananas",
      "url": "http://example.com/398eb027/4"
    }
  ]
}
v}

   In order to extract the "id" field, assuming it is mandatory,
   we would use the following OCaml code that operates on single JSON
   nodes:
{v
open Yojson.Basic.Util
...
  let id = json |> member "id" |> to_string in
  ...
v}

   In order to extract all the "title" fields, we would write the following
   OCaml code that operates on lists of JSON nodes, skipping
   undefined nodes and nodes of unexpected type:
{v
open Yojson.Basic.Util

let extract_titles (json : Yojson.Basic.t) : string list =
  [json]
    |> filter_member "pages"
    |> flatten
    |> filter_member "title"
    |> filter_string
v}
*)

exception Type_error of string * t
  (** Raised when the JSON value is not of the correct type to support an
      operation, e.g. [member] on an [`Int]. The string message explains the
      mismatch. *)

exception Undefined of string * t
  (** Raised when the equivalent JavaScript operation on the JSON value would
      return undefined. Currently this only happens when an array index is out
      of bounds. *)

val keys : t -> string list
  (** Returns all the key names in the given JSON object.
      @raise Type_error if argument is not a JSON object. *)

val values : t -> t list
  (** Return all the value in the given JSON object.
      @raise Type_error if argument is not a JSON object. *)

val combine : t -> t -> t
  (** Combine two JSON objects together.
      @raise Invalid_argument if either argument is not a JSON object. *)

val member : string -> t -> t
  (** [member k obj] returns the value associated with the key [k] in the JSON
      object [obj], or [`Null] if [k] is not present in [obj].
      @raise Type_error if [obj] is not a JSON object. *)

val index : int -> t -> t
  (** [index i arr] returns the value at index [i] in the JSON array [arr].
      Negative indices count from the end of the list (so -1 is the last
      element).
      @raise Type_error if [arr] is not a JSON array.
      @raise Undefined if index is out of bounds. *)

val map : (t -> t) -> t -> t
  (** [map f arr] calls the function [f] on each element of the JSON array
      [arr], and returns a JSON array containing the results.
      @raise Type_error if [arr] is not an JSON array. *)

val to_assoc : t -> (string * t) list
  (** Extract the items of a JSON object.
      @raise Type_error if argument is not a JSON object. *)

val to_option : (t -> 'a) -> t -> 'a option
  (** Return [None] if the JSON value is null or map the JSON value
      to [Some] value using the provided function. *)

val to_bool : t -> bool
  (** Extract a boolean value.
      @raise Type_error if argument is not a JSON boolean. *)

val to_bool_option : t -> bool option
  (** Extract [Some] boolean value,
      return [None] if the value is null.
      @raise Type_error if argument is neither. *)

val to_number : t -> float
  (** Extract a number.
      @raise Type_error if argument is not a JSON number. *)

val to_number_option : t -> float option
  (** Extract [Some] number,
      return [None] if the value is null.
      @raise Type_error if argument is neither. *)

val to_float : t -> float
  (** Extract a float value.
      [to_number] is generally preferred as it also works with int literals.
      @raise Type_error if argument is not a JSON float. *)

val to_float_option : t -> float option
  (** Extract [Some] float value,
      return [None] if the value is null.
      [to_number_option] is generally preferred as it also works
      with int literals.
      @raise Type_error if argument is neither. *)

val to_int : t -> int
  (** Extract an int from a JSON int.
      @raise Type_error if argument is not a JSON int. *)

val to_int_option : t -> int option
  (** Extract [Some] int from a JSON int,
      return [None] if the value is null.
      @raise Type_error if argument is neither. *)

val to_list : t -> t list
  (** Extract a list from JSON array.
      @raise Type_error if argument is not a JSON array. *)

val to_string : t -> string
  (** Extract a string from a JSON string.
      @raise Type_error if argument is not a JSON string. *)

val to_string_option : t -> string option
  (** Extract [Some] string from a JSON string,
      return [None] if the value is null.
      @raise Type_error if argument is neither. *)

val convert_each : (t -> 'a) -> t -> 'a list
  (** The conversion functions above cannot be used with [map], because they do
      not return JSON values. This convenience function [convert_each to_f arr]
      is equivalent to [List.map to_f (to_list arr)].
      @raise Type_error if [arr] is not a JSON array. *)


(** {3 Exception-free filters} *)

(**
   The following functions operate on lists of JSON nodes.
   None of them raises an exception when a certain kind of node is expected
   but no node or the wrong kind of node is found.
   Instead of raising an exception, nodes that are not as expected
   are simply ignored.
*)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] maps each element of the list [l] to an optional value
      using function [f] and unwraps the resulting values. *)

val flatten : t list -> t list
  (** Expects JSON arrays and returns all their elements as a single
      list. [flatten l] is equivalent to [List.flatten (filter_list l)]. *)

val filter_index : int -> t list -> t list
  (** Expects JSON arrays and returns all their elements existing at the given
      position. *)

val filter_list : t list -> t list list
  (** Expects JSON arrays and unwraps them. *)

val filter_member : string -> t list -> t list
  (** Expects JSON objects and returns all the fields of the given name
      (at most one field per object). *)

val filter_assoc : t list -> (string * t) list list
  (** Expects JSON objects and unwraps them. *)

val filter_bool : t list -> bool list
  (** Expects JSON booleans and unwraps them. *)

val filter_int : t list -> int list
  (** Expects JSON integers ([`Int] nodes) and unwraps them. *)

val filter_float : t list -> float list
  (** Expects JSON floats ([`Float] nodes) and unwraps them. *)

val filter_number : t list -> float list
  (** Expects JSON numbers ([`Int] or [`Float]) and unwraps them.
      Ints are converted to floats. *)

val filter_string : t list -> string list
  (** Expects JSON strings and unwraps them. *)
# 41 "yojson.cppo.mli"
end
# 45 "yojson.cppo.mli"
end

(** {1:safe Multipurpose JSON tree type} *)

module Safe :
sig
(**
   This module supports a specific syntax for variants and tuples
   in addition to the standard JSON nodes.
   Arbitrary integers are supported and represented as a decimal string 
   using [`Intlit] when they cannot be represented using OCaml's int type
   (31 or 63 bits depending on the platform).

   This module is recommended for intensive use 
   or OCaml-friendly use of JSON.
*)

# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 8 "type.ml"
    | `Int of int
    
# 11 "type.ml"
    | `Intlit of string
    
# 14 "type.ml"
    | `Float of float
    
# 20 "type.ml"
    | `String of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 28 "type.ml"
    | `Tuple of t list
    
# 31 "type.ml"
    | `Variant of (string * t option)
    
# 33 "type.ml"
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)
# 1 "monomorphic.mli"
val pp : Format.formatter -> t -> unit
  (** Pretty printer, useful for debugging *)

val show : t -> string
  (** Convert value to string, useful for debugging *)

val equal : t -> t -> bool
  (** [equal a b] is the monomorphic equality.
      Determines whether two JSON values are considered equal. In the case of
      JSON objects, the order of the keys does not matter, except for
      duplicate keys which will be considered equal as long as they are in the
      same input order.
    *)
# 1 "safe.mli"
val to_basic : t -> Basic.t
  (**
     Tuples are converted to JSON arrays,
     Variants are converted to JSON strings or arrays of a string (constructor)
     and a json value (argument).
     Long integers are converted to JSON strings.

     Examples:
{v
`Tuple [ `Int 1; `Float 2.3 ]   ->    `List [ `Int 1; `Float 2.3 ]
`Variant ("A", None)            ->    `String "A"
`Variant ("B", Some x)          ->    `List [ `String "B", x ]
`Intlit "12345678901234567890"  ->    `String "12345678901234567890"
v}
  *)
# 1 "write.mli"
(** {2 JSON writers} *)

val to_string :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  t -> string
  (** Write a compact JSON value to a string.
      @param buf allows to reuse an existing buffer created with
      [Buffer.create]. The buffer is cleared of all contents
      before starting and right before returning.
      @param len initial length of the output buffer.
      @param suf appended to the output as a suffix,
      defaults to empty string.
      @param std use only standard JSON syntax,
      i.e. convert tuples and variants into standard JSON (if applicable),
      refuse to print NaN and infinities,
      require the root node to be either an object or an array.
      Default is [false].
      @raise Json_error if [float] value is not allowed in standard JSON.
  *)

val to_channel :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  out_channel -> t -> unit
  (** Write a compact JSON value to a channel.
      Note: the [out_channel] is not flushed by this function.

      See [to_string] for the role of the optional arguments and raised exceptions. *)

val to_output :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  < output : string -> int -> int -> int; .. > -> t -> unit
  (** Write a compact JSON value to an OO channel.

      See [to_string] for the role of the optional arguments and raised exceptions. *)

val to_file :
  ?len:int ->
  ?std:bool ->
  ?suf:string ->
  string -> t -> unit
  (** Write a compact JSON value to a file.
      See [to_string] for the role of the optional arguments and raised exceptions.
      @param suf is a suffix appended to the output Newline by default
      for POSIX compliance. *)

val to_buffer :
  ?suf:string ->
  ?std:bool ->
  Buffer.t -> t -> unit
  (** Write a compact JSON value to an existing buffer.
      See [to_string] for the role of the optional argument and raised exceptions. *)

val seq_to_string :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  t Seq.t -> string
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a string.
      @param suf is the suffix ouf each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val seq_to_channel :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  out_channel -> t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a channel.
      @param suf is the suffix of each value written. Newline by default.
      See [to_channel] for the role of the optional arguments and raised exceptions. *)

val seq_to_file :
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  string -> t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a file.
      @param suf is the suffix of each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val seq_to_buffer :
  ?suf:string ->
  ?std:bool ->
  Buffer.t ->
  t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      an existing buffer.
      @param suf is the suffix of each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val write_t : Buffer.t -> t -> unit
(** Write the given JSON value to the given buffer.
    Provided as a writer function for atdgen.
*)

(** {2 Miscellaneous} *)

val sort : t -> t
  (** Sort object fields (stable sort, comparing field names
      and treating them as byte sequences) *)


(**/**)
(* begin undocumented section *)

val write_null : Buffer.t -> unit -> unit
val write_bool : Buffer.t -> bool -> unit
# 122 "write.mli"
val write_int : Buffer.t -> int -> unit
# 125 "write.mli"
val write_float : Buffer.t -> float -> unit
val write_std_float : Buffer.t -> float -> unit
val write_float_prec : int -> Buffer.t -> float -> unit
val write_std_float_prec : int -> Buffer.t -> float -> unit
# 131 "write.mli"
val write_string : Buffer.t -> string -> unit

# 135 "write.mli"
val write_intlit : Buffer.t -> string -> unit

# 144 "write.mli"
val write_assoc : Buffer.t -> (string * t) list -> unit
val write_list : Buffer.t -> t list -> unit
# 147 "write.mli"
val write_tuple : Buffer.t -> t list -> unit
val write_std_tuple : Buffer.t -> t list -> unit
# 151 "write.mli"
val write_variant : Buffer.t -> string -> t option -> unit
val write_std_variant : Buffer.t -> string -> t option -> unit

# 155 "write.mli"
val write_json : Buffer.t -> t -> unit
val write_std_json : Buffer.t -> t -> unit

(* end undocumented section *)
(**/**)
# 1 "write2.mli"
(** {2 JSON pretty-printing} *)

val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  (** Pretty-print into a {!Format.formatter}.
      See [to_string] for the role of the optional [std] argument.
      @raise Json_error if [float] value is not allowed in standard JSON.

      @since 1.3.1 *)

val pretty_to_string : ?std:bool -> t -> string
  (** Pretty-print into a string.
      See [to_string] for the role of the optional [std] argument.
      See [pretty_print] for raised exceptions.
  *)

val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  (** Pretty-print to a channel.
      See [to_string] for the role of the optional [std] argument.
      See [pretty_print] for raised exceptions.
  *)
# 1 "read.mli"
val prettify : ?std:bool -> string -> string
  (** Combined parser and pretty-printer.
      See [to_string] for the role of the optional [std] argument and raised exceptions. *)

val compact : ?std:bool -> string -> string
  (** Combined parser and printer.
      See [to_string] for the role of the optional [std] argument and raised exceptions. *)

(** {2 JSON readers} *)

exception Finally of exn * exn
(** Exception describing a failure in both finalizer and parsing. *)

val from_string :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a string.
      @param buf use this buffer at will during parsing instead of creating
      a new one.
      @param fname data file name to be used in error messages. It does
      not have to be a real file.
      @param lnum number of the first line of input. Default is 1.
      @raise Json_error if parsing fails.
  *)

val from_channel :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t
  (** Read a JSON value from a channel.
      See [from_string] for the meaning of the optional arguments and raised exceptions. *)

val from_file :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a file.
      See [from_string] for the meaning of the optional arguments and raised exceptions. *)


type lexer_state = Lexer_state.t = {
  buf : Buffer.t;
  mutable lnum : int;
  mutable bol : int;
  mutable fname : string option;
}
    (** This alias is provided for backward compatibility.
        New code should refer to {!Yojson.lexer_state} directly.
    *)

val init_lexer :
  ?buf: Buffer.t ->
  ?fname: string ->
  ?lnum: int ->
  unit -> lexer_state
  (** This alias is provided for backward compatibility.
      New code should use {!Yojson.init_lexer} directly. *)

val from_lexbuf :
  lexer_state ->
  ?stream:bool ->
  Lexing.lexbuf -> t
  (** Read a JSON value from a lexbuf.
      A valid initial [lexer_state] can be created with [init_lexer].
      See [from_string] for the meaning of the optional arguments and raised exceptions.

      @param stream indicates whether more data may follow. The default value
      is false and indicates that only JSON whitespace can be found between
      the end of the JSON value and the end of the input. *)

val seq_from_string :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t Seq.t
  (** Input a sequence of JSON values from a string.
      Whitespace between JSON values is fine but not required.
      See [from_string] for the meaning of the optional arguments and raised exceptions. *)

val seq_from_channel :
  ?buf:Buffer.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t Seq.t
  (** Input a sequence of JSON values from a channel.
      Whitespace between JSON values is fine but not required.
      @param fin finalization function executed once when the end of the
      sequence is reached either because there is no more input or because
      the input could not be parsed, raising an exception.
      @raise Finally When the parsing and the finalizer both raised, [Finally (exn, fin_exn)]
      is raised, [exn] being the parsing exception and [fin_exn] the finalizer one.

      See [from_string] for the meaning of the other optional arguments and other raised exceptions. *)

val seq_from_file :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t Seq.t
  (** Input a sequence of JSON values from a file.
      Whitespace between JSON values is fine but not required.

      See [from_string] for the meaning of the optional arguments and raised exceptions. *)

val seq_from_lexbuf :
  lexer_state ->
  ?fin:(unit -> unit) ->
  Lexing.lexbuf -> t Seq.t
  (** Input a sequence of JSON values from a lexbuf.
      A valid initial [lexer_state] can be created with [init_lexer].
      Whitespace between JSON values is fine but not required.
      @raise Finally When the parsing and the finalizer both raised, [Finally (exn, fin_exn)]
      is raised, [exn] being the parsing exception and [fin_exn] the finalizer one.

      See [seq_from_channel] for the meaning of the optional [fin]
      argument and other raised exceptions. *)


type json_line = [ `Json of t | `Exn of exn ]
    (** The type of values resulting from a parsing attempt of a JSON value. *)

val lineseq_from_channel :
  ?buf:Buffer.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> json_line Seq.t
  (** Input a sequence of JSON values, one per line, from a channel.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [seq_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments and raised exceptions. *)

val lineseq_from_file :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> json_line Seq.t
  (** Input a sequence of JSON values, one per line, from a file.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [seq_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments and raised exceptions. *)

val read_t : lexer_state -> Lexing.lexbuf -> t
(** Read a JSON value from the given lexer_state and lexing buffer and return it.
    Provided as a reader function for atdgen.
*)


(**/**)
(* begin undocumented section *)

val finish_string : lexer_state -> Lexing.lexbuf -> string

val read_string : lexer_state -> Lexing.lexbuf -> string
val read_ident : lexer_state -> Lexing.lexbuf -> string

val map_string :
  lexer_state -> (string -> int -> int -> 'a) -> Lexing.lexbuf -> 'a
  (* equivalent to finish_string *)

val map_ident :
  lexer_state -> (string -> int -> int -> 'a) -> Lexing.lexbuf -> 'a
  (* equivalent to read_ident *)


type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]
val start_any_variant : lexer_state -> Lexing.lexbuf -> variant_kind
val finish_variant : lexer_state -> Lexing.lexbuf -> t option
val finish_skip_variant : lexer_state -> Lexing.lexbuf -> unit
val read_lt : lexer_state -> Lexing.lexbuf -> unit
val read_gt : lexer_state -> Lexing.lexbuf -> unit
val read_comma : lexer_state -> Lexing.lexbuf -> unit

val finish_stringlit : lexer_state -> Lexing.lexbuf -> string
val finish_skip_stringlit : lexer_state -> Lexing.lexbuf -> unit
val finish_escaped_char : lexer_state -> Lexing.lexbuf -> unit
val finish_comment : lexer_state -> Lexing.lexbuf -> unit


val read_space : lexer_state -> Lexing.lexbuf -> unit
val read_eof : Lexing.lexbuf -> bool
val read_null : lexer_state -> Lexing.lexbuf -> unit
val read_null_if_possible : lexer_state -> Lexing.lexbuf -> bool
val read_bool : lexer_state -> Lexing.lexbuf -> bool
val read_int : lexer_state -> Lexing.lexbuf -> int
val read_int8 : lexer_state -> Lexing.lexbuf -> char
val read_int32 : lexer_state -> Lexing.lexbuf -> int32
val read_int64 : lexer_state -> Lexing.lexbuf -> int64
val read_number : lexer_state -> Lexing.lexbuf -> float
val skip_ident : lexer_state -> Lexing.lexbuf -> unit

val read_sequence :
  ('a -> lexer_state -> Lexing.lexbuf -> 'a) ->
  'a ->
  lexer_state ->
  Lexing.lexbuf -> 'a

val read_list :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a list

val read_list_rev :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a list

val read_array_end : Lexing.lexbuf -> unit
val read_array_sep : lexer_state -> Lexing.lexbuf -> unit

val read_array :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a array

val read_tuple :
  (int -> 'a -> lexer_state -> Lexing.lexbuf -> 'a) ->
  'a ->
  lexer_state ->
  Lexing.lexbuf -> 'a

val start_any_tuple : lexer_state -> Lexing.lexbuf -> bool
val read_lpar : lexer_state -> Lexing.lexbuf -> unit
val read_rpar : lexer_state -> Lexing.lexbuf -> unit
val read_tuple_end : Lexing.lexbuf -> unit
val read_tuple_end2 : lexer_state -> bool -> Lexing.lexbuf -> unit
val read_tuple_sep : lexer_state -> Lexing.lexbuf -> unit
val read_tuple_sep2 : lexer_state -> bool -> Lexing.lexbuf -> unit
val read_lbr : lexer_state -> Lexing.lexbuf -> unit
val read_rbr : lexer_state -> Lexing.lexbuf -> unit

val read_fields :
  ('acc -> string -> lexer_state -> Lexing.lexbuf -> 'acc) ->
  'acc ->
  lexer_state ->
  Lexing.lexbuf -> 'acc

val read_abstract_fields :
  (lexer_state -> Lexing.lexbuf -> 'key) ->
  ('acc -> 'key -> lexer_state -> Lexing.lexbuf -> 'acc) ->
  'acc ->
  lexer_state ->
  Lexing.lexbuf -> 'acc

val read_lcurl : lexer_state -> Lexing.lexbuf -> unit
val read_object_end : Lexing.lexbuf -> unit
val read_object_sep : lexer_state -> Lexing.lexbuf -> unit
val read_colon : lexer_state -> Lexing.lexbuf -> unit

val read_json : lexer_state -> Lexing.lexbuf -> t
val skip_json : lexer_state -> Lexing.lexbuf -> unit
val buffer_json : lexer_state -> Lexing.lexbuf -> unit

(* end undocumented section *)
(**/**)
# 74 "yojson.cppo.mli"
(** This module provides combinators for extracting fields from JSON values. *)
module Util :
sig
# 1 "util.mli"
(**
   This module provides combinators for extracting fields from JSON
   values. This approach is recommended for reading a few fields
   from data returned by public APIs. However for more complex applications
   we recommend {{:https://github.com/ahrefs/atd}Atdgen}.

   Here is some sample JSON data:
{v
{
  "id": "398eb027",
  "name": "John Doe",
  "pages": [
    {
      "id": 1,
      "title": "The Art of Flipping Coins",
      "url": "http://example.com/398eb027/1"
    },
    {
      "id": 2,
      "deleted": true
    },
    {
      "id": 3,
      "title": "Artichoke Salad",
      "url": "http://example.com/398eb027/3"
    },
    {
      "id": 4,
      "title": "Flying Bananas",
      "url": "http://example.com/398eb027/4"
    }
  ]
}
v}

   In order to extract the "id" field, assuming it is mandatory,
   we would use the following OCaml code that operates on single JSON
   nodes:
{v
open Yojson.Basic.Util
...
  let id = json |> member "id" |> to_string in
  ...
v}

   In order to extract all the "title" fields, we would write the following
   OCaml code that operates on lists of JSON nodes, skipping
   undefined nodes and nodes of unexpected type:
{v
open Yojson.Basic.Util

let extract_titles (json : Yojson.Basic.t) : string list =
  [json]
    |> filter_member "pages"
    |> flatten
    |> filter_member "title"
    |> filter_string
v}
*)

exception Type_error of string * t
  (** Raised when the JSON value is not of the correct type to support an
      operation, e.g. [member] on an [`Int]. The string message explains the
      mismatch. *)

exception Undefined of string * t
  (** Raised when the equivalent JavaScript operation on the JSON value would
      return undefined. Currently this only happens when an array index is out
      of bounds. *)

val keys : t -> string list
  (** Returns all the key names in the given JSON object.
      @raise Type_error if argument is not a JSON object. *)

val values : t -> t list
  (** Return all the value in the given JSON object.
      @raise Type_error if argument is not a JSON object. *)

val combine : t -> t -> t
  (** Combine two JSON objects together.
      @raise Invalid_argument if either argument is not a JSON object. *)

val member : string -> t -> t
  (** [member k obj] returns the value associated with the key [k] in the JSON
      object [obj], or [`Null] if [k] is not present in [obj].
      @raise Type_error if [obj] is not a JSON object. *)

val index : int -> t -> t
  (** [index i arr] returns the value at index [i] in the JSON array [arr].
      Negative indices count from the end of the list (so -1 is the last
      element).
      @raise Type_error if [arr] is not a JSON array.
      @raise Undefined if index is out of bounds. *)

val map : (t -> t) -> t -> t
  (** [map f arr] calls the function [f] on each element of the JSON array
      [arr], and returns a JSON array containing the results.
      @raise Type_error if [arr] is not an JSON array. *)

val to_assoc : t -> (string * t) list
  (** Extract the items of a JSON object.
      @raise Type_error if argument is not a JSON object. *)

val to_option : (t -> 'a) -> t -> 'a option
  (** Return [None] if the JSON value is null or map the JSON value
      to [Some] value using the provided function. *)

val to_bool : t -> bool
  (** Extract a boolean value.
      @raise Type_error if argument is not a JSON boolean. *)

val to_bool_option : t -> bool option
  (** Extract [Some] boolean value,
      return [None] if the value is null.
      @raise Type_error if argument is neither. *)

val to_number : t -> float
  (** Extract a number.
      @raise Type_error if argument is not a JSON number. *)

val to_number_option : t -> float option
  (** Extract [Some] number,
      return [None] if the value is null.
      @raise Type_error if argument is neither. *)

val to_float : t -> float
  (** Extract a float value.
      [to_number] is generally preferred as it also works with int literals.
      @raise Type_error if argument is not a JSON float. *)

val to_float_option : t -> float option
  (** Extract [Some] float value,
      return [None] if the value is null.
      [to_number_option] is generally preferred as it also works
      with int literals.
      @raise Type_error if argument is neither. *)

val to_int : t -> int
  (** Extract an int from a JSON int.
      @raise Type_error if argument is not a JSON int. *)

val to_int_option : t -> int option
  (** Extract [Some] int from a JSON int,
      return [None] if the value is null.
      @raise Type_error if argument is neither. *)

val to_list : t -> t list
  (** Extract a list from JSON array.
      @raise Type_error if argument is not a JSON array. *)

val to_string : t -> string
  (** Extract a string from a JSON string.
      @raise Type_error if argument is not a JSON string. *)

val to_string_option : t -> string option
  (** Extract [Some] string from a JSON string,
      return [None] if the value is null.
      @raise Type_error if argument is neither. *)

val convert_each : (t -> 'a) -> t -> 'a list
  (** The conversion functions above cannot be used with [map], because they do
      not return JSON values. This convenience function [convert_each to_f arr]
      is equivalent to [List.map to_f (to_list arr)].
      @raise Type_error if [arr] is not a JSON array. *)


(** {3 Exception-free filters} *)

(**
   The following functions operate on lists of JSON nodes.
   None of them raises an exception when a certain kind of node is expected
   but no node or the wrong kind of node is found.
   Instead of raising an exception, nodes that are not as expected
   are simply ignored.
*)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] maps each element of the list [l] to an optional value
      using function [f] and unwraps the resulting values. *)

val flatten : t list -> t list
  (** Expects JSON arrays and returns all their elements as a single
      list. [flatten l] is equivalent to [List.flatten (filter_list l)]. *)

val filter_index : int -> t list -> t list
  (** Expects JSON arrays and returns all their elements existing at the given
      position. *)

val filter_list : t list -> t list list
  (** Expects JSON arrays and unwraps them. *)

val filter_member : string -> t list -> t list
  (** Expects JSON objects and returns all the fields of the given name
      (at most one field per object). *)

val filter_assoc : t list -> (string * t) list list
  (** Expects JSON objects and unwraps them. *)

val filter_bool : t list -> bool list
  (** Expects JSON booleans and unwraps them. *)

val filter_int : t list -> int list
  (** Expects JSON integers ([`Int] nodes) and unwraps them. *)

val filter_float : t list -> float list
  (** Expects JSON floats ([`Float] nodes) and unwraps them. *)

val filter_number : t list -> float list
  (** Expects JSON numbers ([`Int] or [`Float]) and unwraps them.
      Ints are converted to floats. *)

val filter_string : t list -> string list
  (** Expects JSON strings and unwraps them. *)
# 78 "yojson.cppo.mli"
end
# 85 "yojson.cppo.mli"
end

(** {1 JSON tree type with literal int/float/string leaves} *)

module Raw :
sig
(**
   Ints, floats and strings literals are systematically preserved using
   [`Intlit], [`Floatlit] and [`Stringlit].
   This module also supports the specific syntax for variants and tuples
   supported by {!Yojson.Safe}.
*)

# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 11 "type.ml"
    | `Intlit of string
    
# 17 "type.ml"
    | `Floatlit of string
    
# 23 "type.ml"
    | `Stringlit of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 28 "type.ml"
    | `Tuple of t list
    
# 31 "type.ml"
    | `Variant of (string * t option)
    
# 33 "type.ml"
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)
# 1 "monomorphic.mli"
val pp : Format.formatter -> t -> unit
  (** Pretty printer, useful for debugging *)

val show : t -> string
  (** Convert value to string, useful for debugging *)

val equal : t -> t -> bool
  (** [equal a b] is the monomorphic equality.
      Determines whether two JSON values are considered equal. In the case of
      JSON objects, the order of the keys does not matter, except for
      duplicate keys which will be considered equal as long as they are in the
      same input order.
    *)
# 1 "write.mli"
(** {2 JSON writers} *)

val to_string :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  t -> string
  (** Write a compact JSON value to a string.
      @param buf allows to reuse an existing buffer created with
      [Buffer.create]. The buffer is cleared of all contents
      before starting and right before returning.
      @param len initial length of the output buffer.
      @param suf appended to the output as a suffix,
      defaults to empty string.
      @param std use only standard JSON syntax,
      i.e. convert tuples and variants into standard JSON (if applicable),
      refuse to print NaN and infinities,
      require the root node to be either an object or an array.
      Default is [false].
      @raise Json_error if [float] value is not allowed in standard JSON.
  *)

val to_channel :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  out_channel -> t -> unit
  (** Write a compact JSON value to a channel.
      Note: the [out_channel] is not flushed by this function.

      See [to_string] for the role of the optional arguments and raised exceptions. *)

val to_output :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  < output : string -> int -> int -> int; .. > -> t -> unit
  (** Write a compact JSON value to an OO channel.

      See [to_string] for the role of the optional arguments and raised exceptions. *)

val to_file :
  ?len:int ->
  ?std:bool ->
  ?suf:string ->
  string -> t -> unit
  (** Write a compact JSON value to a file.
      See [to_string] for the role of the optional arguments and raised exceptions.
      @param suf is a suffix appended to the output Newline by default
      for POSIX compliance. *)

val to_buffer :
  ?suf:string ->
  ?std:bool ->
  Buffer.t -> t -> unit
  (** Write a compact JSON value to an existing buffer.
      See [to_string] for the role of the optional argument and raised exceptions. *)

val seq_to_string :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  t Seq.t -> string
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a string.
      @param suf is the suffix ouf each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val seq_to_channel :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  out_channel -> t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a channel.
      @param suf is the suffix of each value written. Newline by default.
      See [to_channel] for the role of the optional arguments and raised exceptions. *)

val seq_to_file :
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  string -> t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a file.
      @param suf is the suffix of each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val seq_to_buffer :
  ?suf:string ->
  ?std:bool ->
  Buffer.t ->
  t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      an existing buffer.
      @param suf is the suffix of each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val write_t : Buffer.t -> t -> unit
(** Write the given JSON value to the given buffer.
    Provided as a writer function for atdgen.
*)

(** {2 Miscellaneous} *)

val sort : t -> t
  (** Sort object fields (stable sort, comparing field names
      and treating them as byte sequences) *)


(**/**)
(* begin undocumented section *)

val write_null : Buffer.t -> unit -> unit
val write_bool : Buffer.t -> bool -> unit

# 135 "write.mli"
val write_intlit : Buffer.t -> string -> unit
# 138 "write.mli"
val write_floatlit : Buffer.t -> string -> unit
# 141 "write.mli"
val write_stringlit : Buffer.t -> string -> unit

# 144 "write.mli"
val write_assoc : Buffer.t -> (string * t) list -> unit
val write_list : Buffer.t -> t list -> unit
# 147 "write.mli"
val write_tuple : Buffer.t -> t list -> unit
val write_std_tuple : Buffer.t -> t list -> unit
# 151 "write.mli"
val write_variant : Buffer.t -> string -> t option -> unit
val write_std_variant : Buffer.t -> string -> t option -> unit

# 155 "write.mli"
val write_json : Buffer.t -> t -> unit
val write_std_json : Buffer.t -> t -> unit

(* end undocumented section *)
(**/**)
# 1 "write2.mli"
(** {2 JSON pretty-printing} *)

val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  (** Pretty-print into a {!Format.formatter}.
      See [to_string] for the role of the optional [std] argument.
      @raise Json_error if [float] value is not allowed in standard JSON.

      @since 1.3.1 *)

val pretty_to_string : ?std:bool -> t -> string
  (** Pretty-print into a string.
      See [to_string] for the role of the optional [std] argument.
      See [pretty_print] for raised exceptions.
  *)

val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  (** Pretty-print to a channel.
      See [to_string] for the role of the optional [std] argument.
      See [pretty_print] for raised exceptions.
  *)
# 1 "read.mli"
val prettify : ?std:bool -> string -> string
  (** Combined parser and pretty-printer.
      See [to_string] for the role of the optional [std] argument and raised exceptions. *)

val compact : ?std:bool -> string -> string
  (** Combined parser and printer.
      See [to_string] for the role of the optional [std] argument and raised exceptions. *)

(** {2 JSON readers} *)

exception Finally of exn * exn
(** Exception describing a failure in both finalizer and parsing. *)

val from_string :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a string.
      @param buf use this buffer at will during parsing instead of creating
      a new one.
      @param fname data file name to be used in error messages. It does
      not have to be a real file.
      @param lnum number of the first line of input. Default is 1.
      @raise Json_error if parsing fails.
  *)

val from_channel :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t
  (** Read a JSON value from a channel.
      See [from_string] for the meaning of the optional arguments and raised exceptions. *)

val from_file :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a file.
      See [from_string] for the meaning of the optional arguments and raised exceptions. *)


type lexer_state = Lexer_state.t = {
  buf : Buffer.t;
  mutable lnum : int;
  mutable bol : int;
  mutable fname : string option;
}
    (** This alias is provided for backward compatibility.
        New code should refer to {!Yojson.lexer_state} directly.
    *)

val init_lexer :
  ?buf: Buffer.t ->
  ?fname: string ->
  ?lnum: int ->
  unit -> lexer_state
  (** This alias is provided for backward compatibility.
      New code should use {!Yojson.init_lexer} directly. *)

val from_lexbuf :
  lexer_state ->
  ?stream:bool ->
  Lexing.lexbuf -> t
  (** Read a JSON value from a lexbuf.
      A valid initial [lexer_state] can be created with [init_lexer].
      See [from_string] for the meaning of the optional arguments and raised exceptions.

      @param stream indicates whether more data may follow. The default value
      is false and indicates that only JSON whitespace can be found between
      the end of the JSON value and the end of the input. *)

val seq_from_string :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t Seq.t
  (** Input a sequence of JSON values from a string.
      Whitespace between JSON values is fine but not required.
      See [from_string] for the meaning of the optional arguments and raised exceptions. *)

val seq_from_channel :
  ?buf:Buffer.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t Seq.t
  (** Input a sequence of JSON values from a channel.
      Whitespace between JSON values is fine but not required.
      @param fin finalization function executed once when the end of the
      sequence is reached either because there is no more input or because
      the input could not be parsed, raising an exception.
      @raise Finally When the parsing and the finalizer both raised, [Finally (exn, fin_exn)]
      is raised, [exn] being the parsing exception and [fin_exn] the finalizer one.

      See [from_string] for the meaning of the other optional arguments and other raised exceptions. *)

val seq_from_file :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t Seq.t
  (** Input a sequence of JSON values from a file.
      Whitespace between JSON values is fine but not required.

      See [from_string] for the meaning of the optional arguments and raised exceptions. *)

val seq_from_lexbuf :
  lexer_state ->
  ?fin:(unit -> unit) ->
  Lexing.lexbuf -> t Seq.t
  (** Input a sequence of JSON values from a lexbuf.
      A valid initial [lexer_state] can be created with [init_lexer].
      Whitespace between JSON values is fine but not required.
      @raise Finally When the parsing and the finalizer both raised, [Finally (exn, fin_exn)]
      is raised, [exn] being the parsing exception and [fin_exn] the finalizer one.

      See [seq_from_channel] for the meaning of the optional [fin]
      argument and other raised exceptions. *)


type json_line = [ `Json of t | `Exn of exn ]
    (** The type of values resulting from a parsing attempt of a JSON value. *)

val lineseq_from_channel :
  ?buf:Buffer.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> json_line Seq.t
  (** Input a sequence of JSON values, one per line, from a channel.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [seq_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments and raised exceptions. *)

val lineseq_from_file :
  ?buf:Buffer.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> json_line Seq.t
  (** Input a sequence of JSON values, one per line, from a file.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [seq_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments and raised exceptions. *)

val read_t : lexer_state -> Lexing.lexbuf -> t
(** Read a JSON value from the given lexer_state and lexing buffer and return it.
    Provided as a reader function for atdgen.
*)


(**/**)
(* begin undocumented section *)

val finish_string : lexer_state -> Lexing.lexbuf -> string

val read_string : lexer_state -> Lexing.lexbuf -> string
val read_ident : lexer_state -> Lexing.lexbuf -> string

val map_string :
  lexer_state -> (string -> int -> int -> 'a) -> Lexing.lexbuf -> 'a
  (* equivalent to finish_string *)

val map_ident :
  lexer_state -> (string -> int -> int -> 'a) -> Lexing.lexbuf -> 'a
  (* equivalent to read_ident *)


type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]
val start_any_variant : lexer_state -> Lexing.lexbuf -> variant_kind
val finish_variant : lexer_state -> Lexing.lexbuf -> t option
val finish_skip_variant : lexer_state -> Lexing.lexbuf -> unit
val read_lt : lexer_state -> Lexing.lexbuf -> unit
val read_gt : lexer_state -> Lexing.lexbuf -> unit
val read_comma : lexer_state -> Lexing.lexbuf -> unit

val finish_stringlit : lexer_state -> Lexing.lexbuf -> string
val finish_skip_stringlit : lexer_state -> Lexing.lexbuf -> unit
val finish_escaped_char : lexer_state -> Lexing.lexbuf -> unit
val finish_comment : lexer_state -> Lexing.lexbuf -> unit


val read_space : lexer_state -> Lexing.lexbuf -> unit
val read_eof : Lexing.lexbuf -> bool
val read_null : lexer_state -> Lexing.lexbuf -> unit
val read_null_if_possible : lexer_state -> Lexing.lexbuf -> bool
val read_bool : lexer_state -> Lexing.lexbuf -> bool
val read_int : lexer_state -> Lexing.lexbuf -> int
val read_int8 : lexer_state -> Lexing.lexbuf -> char
val read_int32 : lexer_state -> Lexing.lexbuf -> int32
val read_int64 : lexer_state -> Lexing.lexbuf -> int64
val read_number : lexer_state -> Lexing.lexbuf -> float
val skip_ident : lexer_state -> Lexing.lexbuf -> unit

val read_sequence :
  ('a -> lexer_state -> Lexing.lexbuf -> 'a) ->
  'a ->
  lexer_state ->
  Lexing.lexbuf -> 'a

val read_list :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a list

val read_list_rev :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a list

val read_array_end : Lexing.lexbuf -> unit
val read_array_sep : lexer_state -> Lexing.lexbuf -> unit

val read_array :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a array

val read_tuple :
  (int -> 'a -> lexer_state -> Lexing.lexbuf -> 'a) ->
  'a ->
  lexer_state ->
  Lexing.lexbuf -> 'a

val start_any_tuple : lexer_state -> Lexing.lexbuf -> bool
val read_lpar : lexer_state -> Lexing.lexbuf -> unit
val read_rpar : lexer_state -> Lexing.lexbuf -> unit
val read_tuple_end : Lexing.lexbuf -> unit
val read_tuple_end2 : lexer_state -> bool -> Lexing.lexbuf -> unit
val read_tuple_sep : lexer_state -> Lexing.lexbuf -> unit
val read_tuple_sep2 : lexer_state -> bool -> Lexing.lexbuf -> unit
val read_lbr : lexer_state -> Lexing.lexbuf -> unit
val read_rbr : lexer_state -> Lexing.lexbuf -> unit

val read_fields :
  ('acc -> string -> lexer_state -> Lexing.lexbuf -> 'acc) ->
  'acc ->
  lexer_state ->
  Lexing.lexbuf -> 'acc

val read_abstract_fields :
  (lexer_state -> Lexing.lexbuf -> 'key) ->
  ('acc -> 'key -> lexer_state -> Lexing.lexbuf -> 'acc) ->
  'acc ->
  lexer_state ->
  Lexing.lexbuf -> 'acc

val read_lcurl : lexer_state -> Lexing.lexbuf -> unit
val read_object_end : Lexing.lexbuf -> unit
val read_object_sep : lexer_state -> Lexing.lexbuf -> unit
val read_colon : lexer_state -> Lexing.lexbuf -> unit

val read_json : lexer_state -> Lexing.lexbuf -> t
val skip_json : lexer_state -> Lexing.lexbuf -> unit
val buffer_json : lexer_state -> Lexing.lexbuf -> unit

(* end undocumented section *)
(**/**)
# 113 "yojson.cppo.mli"
end

(** {1:raw Supertype of all JSON tree types} *)

# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 8 "type.ml"
    | `Int of int
    
# 11 "type.ml"
    | `Intlit of string
    
# 14 "type.ml"
    | `Float of float
    
# 17 "type.ml"
    | `Floatlit of string
    
# 20 "type.ml"
    | `String of string
    
# 23 "type.ml"
    | `Stringlit of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 28 "type.ml"
    | `Tuple of t list
    
# 31 "type.ml"
    | `Variant of (string * t option)
    
# 33 "type.ml"
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)
# 1 "monomorphic.mli"
val pp : Format.formatter -> t -> unit
  (** Pretty printer, useful for debugging *)

val show : t -> string
  (** Convert value to string, useful for debugging *)

val equal : t -> t -> bool
  (** [equal a b] is the monomorphic equality.
      Determines whether two JSON values are considered equal. In the case of
      JSON objects, the order of the keys does not matter, except for
      duplicate keys which will be considered equal as long as they are in the
      same input order.
    *)
# 1 "write.mli"
(** {2 JSON writers} *)

val to_string :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  t -> string
  (** Write a compact JSON value to a string.
      @param buf allows to reuse an existing buffer created with
      [Buffer.create]. The buffer is cleared of all contents
      before starting and right before returning.
      @param len initial length of the output buffer.
      @param suf appended to the output as a suffix,
      defaults to empty string.
      @param std use only standard JSON syntax,
      i.e. convert tuples and variants into standard JSON (if applicable),
      refuse to print NaN and infinities,
      require the root node to be either an object or an array.
      Default is [false].
      @raise Json_error if [float] value is not allowed in standard JSON.
  *)

val to_channel :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  out_channel -> t -> unit
  (** Write a compact JSON value to a channel.
      Note: the [out_channel] is not flushed by this function.

      See [to_string] for the role of the optional arguments and raised exceptions. *)

val to_output :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  < output : string -> int -> int -> int; .. > -> t -> unit
  (** Write a compact JSON value to an OO channel.

      See [to_string] for the role of the optional arguments and raised exceptions. *)

val to_file :
  ?len:int ->
  ?std:bool ->
  ?suf:string ->
  string -> t -> unit
  (** Write a compact JSON value to a file.
      See [to_string] for the role of the optional arguments and raised exceptions.
      @param suf is a suffix appended to the output Newline by default
      for POSIX compliance. *)

val to_buffer :
  ?suf:string ->
  ?std:bool ->
  Buffer.t -> t -> unit
  (** Write a compact JSON value to an existing buffer.
      See [to_string] for the role of the optional argument and raised exceptions. *)

val seq_to_string :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  t Seq.t -> string
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a string.
      @param suf is the suffix ouf each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val seq_to_channel :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  out_channel -> t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a channel.
      @param suf is the suffix of each value written. Newline by default.
      See [to_channel] for the role of the optional arguments and raised exceptions. *)

val seq_to_file :
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  string -> t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a file.
      @param suf is the suffix of each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val seq_to_buffer :
  ?suf:string ->
  ?std:bool ->
  Buffer.t ->
  t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      an existing buffer.
      @param suf is the suffix of each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val write_t : Buffer.t -> t -> unit
(** Write the given JSON value to the given buffer.
    Provided as a writer function for atdgen.
*)

(** {2 Miscellaneous} *)

val sort : t -> t
  (** Sort object fields (stable sort, comparing field names
      and treating them as byte sequences) *)


(**/**)
(* begin undocumented section *)

val write_null : Buffer.t -> unit -> unit
val write_bool : Buffer.t -> bool -> unit
# 122 "write.mli"
val write_int : Buffer.t -> int -> unit
# 125 "write.mli"
val write_float : Buffer.t -> float -> unit
val write_std_float : Buffer.t -> float -> unit
val write_float_prec : int -> Buffer.t -> float -> unit
val write_std_float_prec : int -> Buffer.t -> float -> unit
# 131 "write.mli"
val write_string : Buffer.t -> string -> unit

# 135 "write.mli"
val write_intlit : Buffer.t -> string -> unit
# 138 "write.mli"
val write_floatlit : Buffer.t -> string -> unit
# 141 "write.mli"
val write_stringlit : Buffer.t -> string -> unit

# 144 "write.mli"
val write_assoc : Buffer.t -> (string * t) list -> unit
val write_list : Buffer.t -> t list -> unit
# 147 "write.mli"
val write_tuple : Buffer.t -> t list -> unit
val write_std_tuple : Buffer.t -> t list -> unit
# 151 "write.mli"
val write_variant : Buffer.t -> string -> t option -> unit
val write_std_variant : Buffer.t -> string -> t option -> unit

# 155 "write.mli"
val write_json : Buffer.t -> t -> unit
val write_std_json : Buffer.t -> t -> unit

(* end undocumented section *)
(**/**)
# 1 "write2.mli"
(** {2 JSON pretty-printing} *)

val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  (** Pretty-print into a {!Format.formatter}.
      See [to_string] for the role of the optional [std] argument.
      @raise Json_error if [float] value is not allowed in standard JSON.

      @since 1.3.1 *)

val pretty_to_string : ?std:bool -> t -> string
  (** Pretty-print into a string.
      See [to_string] for the role of the optional [std] argument.
      See [pretty_print] for raised exceptions.
  *)

val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  (** Pretty-print to a channel.
      See [to_string] for the role of the optional [std] argument.
      See [pretty_print] for raised exceptions.
  *)
