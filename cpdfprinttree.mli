(** Print trees *)

(** Print a tree to a buffer. *)
val to_buffer : ?line_prefix: string -> get_name: ('a -> string) -> get_children: ('a -> 'a list) -> Buffer.t -> 'a -> unit

(** Print a tree to a string. *)
val to_string : ?line_prefix: string -> get_name: ('a -> string) -> get_children: ('a -> 'a list) -> 'a -> string
