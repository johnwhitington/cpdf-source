(** PDF/UA *)
val test_matterhorn_print : Pdf.t -> string -> unit

val test_matterhorn_json : Pdf.t -> string -> Cpdfyojson.Safe.t

val mark : Pdf.t -> unit

val remove_mark : Pdf.t -> unit

val extract_struct_tree : Pdf.t -> Cpdfyojson.Safe.t

val replace_struct_tree : Pdf.t -> Cpdfyojson.Safe.t -> unit
