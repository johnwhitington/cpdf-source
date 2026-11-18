val rasterize :
  string list ref -> string -> bool -> string -> bool -> ?subformat:Cpdfua.subformat -> ?title:string -> bool -> bool -> string -> float -> bool -> int -> Pdf.t -> int list -> Pdf.t

val write_images :
  string list ref -> string -> bool -> string -> float -> int -> string option -> bool -> bool -> bool -> string -> Pdf.t -> int list -> unit
