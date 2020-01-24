external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
val ( $ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val id : 'a -> 'a
val ( !% ) : ('a, unit, string) format -> 'a
val ( !$ ) : 'a Lazy.t -> 'a
val slist : string -> ('a -> string) -> 'a list -> string
val string_of_chars : char list -> string
val string1 : char -> string
type ('a, 'b) either = Inl of 'a | Inr of 'b
val open_in_with : string -> (in_channel -> 'a) -> 'a
val to_hex : int -> string
