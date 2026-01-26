(** Adding text *)

(** Colours *)
type colour =
  Grey of float
| RGB of float * float * float
| CYMK of float * float * float * float

(** Build a colour operation for filling with the given colour. *)
val colour_op : colour -> Pdfops.t

(** Build a colour operation for filing with the given colour *)
val colour_op_stroke : colour -> Pdfops.t

(** Justification of multiline text *)
type justification =
  | LeftJustify
  | CentreJustify
  | RightJustify

(** Call [add_texts linewidth outline fast fontname font bates batespad colour
position linespacing fontsize underneath text pages orientation
relative_to_cropbox midline_adjust topline filename pdf]. For details see cpdfmanual.pdf *)
val addtexts :
    float -> (*linewidth*)
    bool -> (*outline*)
    bool -> (*fast*)
    string -> (*fontname*)
    Cpdfembed.cpdffont -> (*font*)
    int -> (* bates number *)
    int option -> (* bates padding width *)
    colour -> (*colour*)
    Cpdfposition.position -> (*position*)
    float -> (*linespacing*)
    float -> (*fontsize*)
    bool -> (*underneath*)
    string ->(*text*)
    int list ->(*page range*)
    string ->(*relative to box*)
    float ->(*opacity*)
    justification ->(*justification*)
    bool ->(*midline adjust?*)
    bool ->(*topline adjust?*)
    string ->(*filename*)
    float option -> (*extract_text_font_size*)
    string -> (* shift *)
    ?raw:bool -> (* raw *)
    Pdf.t ->(*pdf*)
    Pdf.t

(** Add a rectangle to the given pages. [addrectangle fast coordinate colour outline linewidth opacity position relative_to_cropbox underneath range pdf]. *) 
val addrectangle :
    bool ->
    string ->
    colour ->
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
  float option ->
  string ->
  int ->
  int option -> int -> Pdfpage.t -> (string * (unit -> string)) list

val process_text :
  Cpdfstrftime.t -> string -> (string * (unit -> string)) list -> string

