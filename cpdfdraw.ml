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
  | EndPath

type state =
  {mutable fill : drawops_colspec;
   mutable stroke : drawops_colspec;
   mutable linewidth : float;
   mutable linecap : int;
   mutable linejoin : int;
   mutable miterlimit : float;
   mutable dashpattern : float list * float}

let state =
  {fill = NoCol;
   stroke = RGB (0., 0., 0.);
   linewidth = 1.;
   linecap = 0;
   linejoin = 0;
   miterlimit = 10.;
   dashpattern = ([], 0.)}

let ops_of_drawop = function
  | Rect (x, y, w, h) -> [Pdfops.Op_re (x, y, w, h)]
  | To (x, y) -> [Pdfops.Op_m (x, y)]
  | Line (x, y) -> [Pdfops.Op_l (x, y)]
  | Fill x ->
      state.fill <- x;
      begin match x with
      | RGB (r, g, b) -> [Op_rg (r, g, b)]
      | Grey g -> [Op_g g]
      | CYMK (c, y, m, k) -> [Op_k (c, y, m, k)]
      | NoCol -> []
      end
  | Stroke x ->
      state.stroke <- x;
      begin match x with
      | RGB (r, g, b) -> [Op_RG (r, g, b)]
      | Grey g -> [Op_G g]
      | CYMK (c, y, m, k) -> [Op_K (c, y, m, k)]
      | NoCol -> []
      end
  | EndPath ->
      begin match state.fill, state.stroke with
      | NoCol, NoCol -> []
      | NoCol, _ -> [Pdfops.Op_S]
      | _, NoCol -> [Pdfops.Op_f]
      | _, _ -> [Pdfops.Op_B']
      end

let ops_of_drawops drawops = flatten (map ops_of_drawop drawops)

(* Draw all the accumulated operators *)
let draw fast range pdf drawops =
  let s = Pdfops.string_of_ops (ops_of_drawops drawops) in
    Cpdftweak.append_page_content s false fast range pdf
