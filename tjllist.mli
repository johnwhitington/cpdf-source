type 'a llist = Nil | Cons of 'a * 'a llist Lazy.t
(** It is a lazy list, but the first element is already forced *)

type 'a t = 'a llist Lazy.t

val hd : 'a llist -> 'a
val tl : 'a llist -> 'a llist
val take : int -> 'a llist -> 'a list
val map : ('a -> 'b) -> 'a llist -> 'b llist
val repeat : 'a -> 'a llist
val app : 'a llist -> 'a llist -> 'a llist
val combine : 'a llist -> 'b llist -> ('a * 'b) llist
val filter : ('a -> bool) -> 'a llist -> 'a llist
val concat : 'a llist llist -> 'a llist
val unfoldr : ('b -> ('a * 'b) option) -> 'b -> 'a llist
val continually : (unit -> 'a) -> 'a llist

val from : int -> int llist

val of_stream : 'a Cpdfstream.t -> 'a llist
val sllist : ?items:int -> string -> ('a -> string) -> 'a llist -> string
val of_string : string -> char llist

val of_function : (bytes -> int -> int) -> char llist
(** [of_function f]: [f buf len] is a filler, a function to fill [buf]
    with at most [len] chars. If it reaches the end of the input it returns [0].
*)

val to_list : 'a llist -> 'a list
