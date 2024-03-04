(** Chop *)

(** Chop pages in the given range into pieces. *)
val chop : x:int -> y:int -> columns:bool -> btt:bool -> rtl:bool -> Pdf.t -> int list -> Pdf.t

(** Chop a page in the given range horizontally or vertically. *)
val chop_hv : is_h:bool -> line:float -> columns:bool -> Pdf.t -> int list -> Pdf.t
