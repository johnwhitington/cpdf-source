(** Print trees *)

val to_buffer : ?line_prefix: string -> get_name: ('a -> string) -> get_children: ('a -> 'a list) -> Buffer.t -> 'a -> unit

val to_string : ?line_prefix: string -> get_name: ('a -> string) -> get_children: ('a -> 'a list) -> 'a -> string
