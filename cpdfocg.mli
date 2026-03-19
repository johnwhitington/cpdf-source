(** Optional content groups *)

(** Return list of OCG names. *)
val ocg_get_list : Pdf.t -> string list

(** Print OCG list to Standard Output, optioanally in JSON format. *)
val ocg_list : bool -> Pdf.t -> unit

(** Replace OCG from JSON. *)
val ocg_replace : string -> Pdf.t -> unit

(** Coalesce same-named OCGs resulting from merge. *)
val ocg_coalesce : Pdf.t -> unit

(** Rename an OCG. *)
val ocg_rename : string -> string -> Pdf.t -> unit

(** Make sure every OCG is in the /Order. *) 
val ocg_order_all : Pdf.t -> unit
