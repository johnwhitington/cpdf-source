(** Representing page content as objects without loss. *)

(* We run through the ops, doing all the work to process the page w.r.t graphics and text state.

   Initial aim: get bounding box of objects so we can redact them, outputting the stream with objects redacted.

   Final aim: page contents as objects free of graphics state, but without blow-ups (i.e keep xobjects) and fully round-trippable.

*)
