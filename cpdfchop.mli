(** Chop *)

(** Chop a page into pieces. *)
val chop : x:int -> y:int -> columns:bool -> btt:bool -> rtl:bool -> Pdf.t -> int list -> Pdf.t
