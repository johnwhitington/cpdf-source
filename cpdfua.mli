(** PDF/UA *)

type subformat =
  | PDFUA1
  | PDFUA2

(** Parse PDF/UA-1 and PDF/UA-2 to the subformat type. *)
val subformat_of_string : string -> subformat

(** Print matterhorn test results ("" = all tests) *)
val test_matterhorn_print : Pdf.t -> string -> unit

(** Return JSON results of a matterhorn test ("" = all tests *) 
val test_matterhorn_json : Pdf.t -> string -> Cpdfyojson.Safe.t

(** Mark PDF as PDF/UA-1 compliant. *)
val mark : Pdf.t -> unit

(** Mark PDF as PDF/UA-2 compliant. *)
val mark2 : int -> Pdf.t -> unit

(** Remove any PDF/UA-* marker *)
val remove_mark : Pdf.t -> unit

(** Print the structure tree to standard output, graphically. *)
val print_struct_tree : Pdf.t -> unit

(** Extract structure tree to JSON *)
val extract_struct_tree : Pdf.t -> Cpdfyojson.Safe.t

(** Reapply an edited JSON structure tree to its PDF. *)
val replace_struct_tree : Pdf.t -> Cpdfyojson.Safe.t -> unit

(* Make a blank PDF/UA-1 PDF given a title, paper size, and number of pages. *)
val create_pdfua1 : string -> Pdfpaper.t -> int -> Pdf.t

(* Make a blank PDF/UA-2 PDF given a title, paper size, and number of pages. *)
val create_pdfua2 : string -> Pdfpaper.t -> int -> Pdf.t
