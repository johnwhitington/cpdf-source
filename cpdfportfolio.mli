(** Portfolios. *)

(** A single entry in the portfolio. *)
type t =
  {filename : string;
   relationship : string option;
   description : string option}

(** Build a PDF portfolio from a given base PDF and a list of other files. *)
val portfolio : Pdf.t -> t list -> unit
