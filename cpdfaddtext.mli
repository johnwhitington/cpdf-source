(** Adding text *)

type color =
  Grey of float
| RGB of float * float * float
| CYMK of float * float * float * float

val colour_op : color -> Pdfops.t

val colour_op_stroke : color -> Pdfops.t

(** Justification of multiline text *)
type justification =
  | LeftJustify
  | CentreJustify
  | RightJustify

(** Call [add_texts metrics linewidth outline fast fontname font bates batespad colour
position linespacing fontsize underneath text pages orientation
relative_to_cropbox midline_adjust topline filename pdf]. For details see cpdfmanual.pdf *)
val addtexts :
    bool -> (*metrics*)
    float -> (*linewidth*)
    bool -> (*outline*)
    bool -> (*fast*)
    string -> (*fontname*)
    Pdftext.standard_font option -> (*font*)
    bool -> (* embed font *)
    int -> (* bates number *)
    int option -> (* bates padding width *)
    color -> (*colour*)
    Cpdfposition.position -> (*position*)
    float -> (*linespacing*)
    float -> (*fontsize*)
    bool -> (*underneath*)
    string ->(*text*)
    int list ->(*page range*)
    Cpdfposition.orientation ->(*orientation*)
    bool ->(*relative to cropbox?*)
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

(* Add a rectangle to the page *) 
val addrectangle :
    bool ->
    float * float ->
    color ->
    bool ->
    float ->
    float ->
    Cpdfposition.position ->
    bool -> bool -> int list -> Pdf.t -> Pdf.t

val metrics_howmany : unit -> int
val metrics_text : int -> string
val metrics_x : int -> float
val metrics_y : int -> float
val metrics_rot : int -> float
val metrics_baseline_adjustment : unit -> float
(** These functions returns some details about the text if [addtexts] is called with [metrics] true. The integer arguments are  1 for the first one, 2 for the second etc. Call [metrics_howmany] first to find out how many. *)

(** Remove text from the given pages. *)
val removetext : int list -> Pdf.t -> Pdf.t

(** Extract text *)
val extract_text : float option -> Pdf.t -> int list -> string 

(** Remove text *)
val remove_all_text : int list -> Pdf.t -> Pdf.t 
