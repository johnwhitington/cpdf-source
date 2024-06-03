(** PDF/UA *)
val test_matterhorn_print : Pdf.t -> unit

val test_matterhorn_json : Pdf.t -> Cpdfyojson.Safe.t

val mark : Pdf.t -> unit

val extract_struct_tree : Pdf.t -> Cpdfyojson.Safe.t
