(** {2 Presentations} *)

(** [presentation range t d horizontal inward direction effect_duration pdf]
adds a presentation on the pages in [range]. See cpdfmanual.pdf for details.
*)
val presentation : int list -> string option ->
    float option -> bool -> bool -> int -> float -> Pdf.t -> Pdf.t

