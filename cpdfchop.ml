open Pdfutil
open Cpdferror

(* Prevent duplication of content by sharing content streams *)
(* Resources - patterns etc. - pattern matrices *)
(* Bookmarks: merge/duplicate them *)
(* Annotations: merge/duplicate them *)
(* Page boxes - mediabox or cropbox used? Output modifies all boxes? *)
(* fast/slow *)

(* Chop pages into pieces *)
let chop ~x ~y pdf range = pdf
