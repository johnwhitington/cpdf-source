open Pdfutil

type drawops_colspec =
   NoCol
 | RGB of float * float * float
 | Grey of float
 | CYMK of float * float * float * float

type drawops =
  | Rect of float * float * float * float (* x, y, w, h *)
  | To of float * float
  | Line of float * float
  | Fill of drawops_colspec
  | Stroke of drawops_colspec
  | SetLineThickness of float
  | SetLineCap of int
  | SetLineJoin of int
  | SetMiterLimit of float
  | SetDashPattern of float list * float
  | Matrix of Pdftransform.transform_matrix
  | Push
  | Pop
  | EndPath

type state =
  {mutable fill : drawops_colspec;
   mutable stroke : drawops_colspec;
   mutable linewidth : float;
   mutable linecap : int;
   mutable linejoin : int;
   mutable miterlimit : float;
   mutable dashpattern : float list * float}

let initial_state () =
  {fill = NoCol;
   stroke = RGB (0., 0., 0.);
   linewidth = 1.;
   linecap = 0;
   linejoin = 0;
   miterlimit = 10.;
   dashpattern = ([], 0.)}

let state =
  ref [initial_state ()]

let currstate () =
  match !state with s::_ -> s | [] -> assert false

let pushstate () =
  match !state with s::t -> state := {s with fill = s.fill}::s::t | [] -> assert false

let popstate () =
  match !state with [s] -> () | s::t -> state := t | [] -> assert false

let cleanstate () =
  state := [initial_state ()]

let ops_of_drawop = function
  | Push -> pushstate (); [Pdfops.Op_q]
  | Pop -> popstate (); [Pdfops.Op_Q]
  | Matrix m -> [Pdfops.Op_cm m] 
  | Rect (x, y, w, h) -> [Pdfops.Op_re (x, y, w, h)]
  | To (x, y) -> [Pdfops.Op_m (x, y)]
  | Line (x, y) -> [Pdfops.Op_l (x, y)]
  | Fill x ->
      (currstate ()).fill <- x;
      begin match x with
      | RGB (r, g, b) -> [Op_rg (r, g, b)]
      | Grey g -> [Op_g g]
      | CYMK (c, y, m, k) -> [Op_k (c, y, m, k)]
      | NoCol -> []
      end
  | Stroke x ->
      (currstate ()).stroke <- x;
      begin match x with
      | RGB (r, g, b) -> [Op_RG (r, g, b)]
      | Grey g -> [Op_G g]
      | CYMK (c, y, m, k) -> [Op_K (c, y, m, k)]
      | NoCol -> []
      end
  | EndPath ->
      begin match (currstate ()).fill, (currstate ()).stroke with
      | NoCol, NoCol -> []
      | NoCol, _ -> [Pdfops.Op_S]
      | _, NoCol -> [Pdfops.Op_f]
      | _, _ -> [Pdfops.Op_B']
      end
  | SetLineThickness t ->
      (currstate ()).linewidth <- t;
      [Pdfops.Op_w t]
  | SetLineCap c ->
      (currstate ()).linecap <- c;
      [Pdfops.Op_J c]
  | SetLineJoin j ->
      (currstate ()).linejoin <- j;
      [Pdfops.Op_j j]
  | SetMiterLimit m ->
      (currstate ()).miterlimit <- m;
      [Pdfops.Op_M m]
  | SetDashPattern (x, y) ->
      (currstate ()).dashpattern <- (x, y);
      [Pdfops.Op_d (x, y)]

let ops_of_drawops drawops = flatten (map ops_of_drawop drawops)

(* Draw all the accumulated operators *)
let draw fast range pdf drawops =
  let s = Pdfops.string_of_ops (ops_of_drawops drawops) in
    cleanstate ();
    Cpdftweak.append_page_content s false fast range pdf
