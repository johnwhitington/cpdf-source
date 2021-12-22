(** {2 Imposition} *)

val impose : x:float -> y:float -> fit:bool -> columns:bool -> rtl:bool -> btt:bool -> center:bool -> margin:float -> spacing:float -> linewidth:float -> fast:bool -> Pdf.t -> Pdf.t

(** The twoup_stack operation puts two logical pages on each physical page,
rotating them 90 degrees to do so. The new mediabox is thus larger. Bool true
(fast) if assume well-formed ISO content streams. *)
val twoup_stack : bool -> Pdf.t -> Pdf.t

(** The twoup operation does the same, but scales the new sides down so that
the media box is unchanged. Bool true (fast) if assume well-formed ISO content streams. *)
val twoup : bool -> Pdf.t -> Pdf.t
