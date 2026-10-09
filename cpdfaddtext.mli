(** Adding text *)

(** Rotation *)
type rotation = Rot0 | Rot90 | Rot180 | Rot270

(** Call [add_texts linewidth outline fast fontname font bates batespad colour
position linespacing fontsize underneath text range box opacity justification
midline topline rotation filename shift raw url_border pdf]. For details see
cpdfmanual.pdf *)
val addtexts :
    float -> (*linewidth*)
    bool -> (*outline*)
    bool -> (*fast*)
    string -> (*fontname*)
    Cpdfembed.cpdffont -> (*font*)
    int -> (* bates number *)
    int option -> (* bates padding width *)
    Cpdfutil.colour -> (*colour*)
    Cpdfposition.position -> (*position*)
    float -> (*linespacing*)
    float -> (*fontsize*)
    bool -> (*underneath*)
    string ->(*text*)
    int list ->(*page range*)
    string ->(*relative to box*)
    float ->(*opacity*)
    Cpdfutil.justification ->(*justification*)
    bool ->(*midline adjust?*)
    bool ->(*topline adjust?*)
    rotation -> (* rotation *)
    string ->(*filename*)
    string -> (* shift *)
    ?raw:bool -> (* raw *)
    bool -> (* URL border *)
    Pdf.t ->(*pdf*)
    Pdf.t

(** Add a rectangle to the given pages. [addrectangle fast coordinate colour outline linewidth opacity position relative_to_cropbox underneath range pdf]. *) 
val addrectangle :
    bool ->
    string ->
    Cpdfutil.colour ->
    bool ->
    float ->
    float ->
    Cpdfposition.position ->
    string -> bool -> int list -> Pdf.t -> Pdf.t

(**/**)
val replace_pairs :
  Pdfmarks.t list ->
  (int, int) Hashtbl.t ->
  Pdf.t ->
  int ->
  string ->
  int ->
  int option -> int -> Pdfpage.t -> (string * (unit -> string)) list

val process_text :
  Cpdfstrftime.t -> string -> (string * (unit -> string)) list -> string

