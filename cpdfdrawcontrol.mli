(** Cpdfcommand draw control *)

val embed_font : (unit -> Cpdfembed.cpdffont) ref
val getfontname : (unit -> string) ref
val getfontsize : (unit -> float) ref
val getindent : (unit -> float option) ref
val setfontname : (string -> unit) ref
val setfontsize : (float -> unit) ref
val setdrawing : (unit -> unit) ref
val setembedstd14 : (bool -> string -> unit) ref
val ttfs : (string, (string * Cpdfembed.cpdffont)) Hashtbl.t
val loadttf : (string -> unit) ref
val loadttfseparate : string -> string -> unit
val fontpack_initialised : bool ref
val drawops : (string * Cpdfdraw.drawops list) list ref
val addop : Cpdfdraw.drawops -> unit
val parse_colour : string -> Cpdfaddtext.colour
val addtag : string -> unit
val addstag : string -> unit
val endtag : unit -> unit
val endstag : unit -> unit
val autotags : bool -> unit
val eltinfo : string -> Pdf.pdfobject -> unit
val endeltinfo : string -> unit
val addnamespace : string -> unit
val setrolemap : string -> unit
val artifact : unit -> unit
val endartifact : unit -> unit
val autoartifacts : bool -> unit
val addrect : string -> unit
val addto : string -> unit
val addline : string -> unit
val addbezier : string -> unit
val addbezier23 : string -> unit
val addbezier13 : string -> unit
val addcircle : string -> unit
val setstroke : string -> unit
val setfill : string -> unit
val stroke : unit -> unit
val fill : unit -> unit
val fillevenodd : unit -> unit
val strokefill : unit -> unit
val strokefillevenodd : unit -> unit
val clip : unit -> unit
val clipevenodd : unit -> unit
val closepath : unit -> unit
val setthickness : string -> unit
val setcap : string -> unit
val setjoin : string -> unit
val setmiter : string -> unit
val setdash : string -> unit
val push : unit -> unit
val pop : unit -> unit
val setmatrix : string -> unit
val setmtranslate : string -> unit
val setmrotate : string -> unit
val setmscale : string -> unit
val setmshearx : string -> unit
val setmsheary : string -> unit
val xobjbbox : string -> unit
val startxobj : string -> unit
val endxobj : unit -> unit
val usexobj : string -> unit
val addjpeg : ?data:Pdfio.rawbytes -> string -> unit
val addpng : ?data:Pdfio.rawbytes -> string -> unit
val addimage : string -> unit
val addopacity : float -> unit
val addsopacity : float -> unit
val addbt : unit -> unit
val addet : unit -> unit
val addnewpage : unit -> unit
val addleading : float -> unit
val addcharspace : float -> unit
val addwordspace : float -> unit
val addtextscale : float -> unit
val addrendermode : int -> unit
val addrise : float -> unit
val addnewline : unit -> unit
val add_default_fontpack : string -> unit
val addtext : string -> unit
val addspecialtext : string -> unit
val addpara : string -> unit
val addparas : string -> unit

(** This the beginnings of separation between cpdfcommand and cpdfdraw when
    drawing, for use in cpdflib. It is presently undocumented. *)
