(** PDF Command Line Tools in library form. *)

(** Call the command line tools with the contents of [Sys.argv] *)
val go : unit -> unit

(** Call the command line tools with a specialised [argv] of our own *)
val go_withargv : string array -> unit

(**/**)
val demo : bool

exception StayOnError
