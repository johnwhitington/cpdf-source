open Pdfutil

type colspec =
   NoCol
 | RGB of float * float * float
 | Grey of float
 | CYMK of float * float * float * float

type image =
  JPEG

type drawops =
  | Rect of float * float * float * float
  | Bezier of float * float * float * float * float * float
  | To of float * float
  | Line of float * float
  | ClosePath
  | SetFill of colspec
  | SetStroke of colspec
  | SetLineThickness of float
  | SetLineCap of int
  | SetLineJoin of int
  | SetMiterLimit of float
  | SetDashPattern of float list * float
  | Matrix of Pdftransform.transform_matrix
  | Push
  | Pop
  | Fill
  | FillEvenOdd
  | Stroke
  | FillStroke
  | FillStrokeEvenOdd
  | Clip
  | ClipEvenOdd
  | SoftXObject of drawops list
  | HardXObject of drawops list
  | Image of string
  | ImageXObject of string * Pdf.pdfobject
  | NewPage
  | Opacity of float
  | SOpacity of float
  | Text of string
  | Block of unit (* to fix *)
  | URL of string
  | EndURL

(* Hash table of (human name, (resources name, object)) for image xobjects *)
let images = null_hash ()
let gss = null_hash ()

(* Fresh XObject names. If we are stamping over another page, manage clashes later. *)
let fresh_xobj_name () = "/Img0"

let gsnum = ref ~-1

let fresh_gs_name () =
  gsnum += 1;
  "/gs" ^ string_of_int !gsnum

let rec ops_of_drawop pdf = function
  | Push -> [Pdfops.Op_q]
  | Pop -> [Pdfops.Op_Q]
  | Matrix m -> [Pdfops.Op_cm m] 
  | Rect (x, y, w, h) -> [Pdfops.Op_re (x, y, w, h)]
  | Bezier (a, b, c, d, e, f) -> [Pdfops.Op_c (a, b, c, d, e, f)]
  | To (x, y) -> [Pdfops.Op_m (x, y)]
  | Line (x, y) -> [Pdfops.Op_l (x, y)]
  | SetFill x ->
      begin match x with
      | RGB (r, g, b) -> [Op_rg (r, g, b)]
      | Grey g -> [Op_g g]
      | CYMK (c, y, m, k) -> [Op_k (c, y, m, k)]
      | NoCol -> []
      end
  | SetStroke x ->
      begin match x with
      | RGB (r, g, b) -> [Op_RG (r, g, b)]
      | Grey g -> [Op_G g]
      | CYMK (c, y, m, k) -> [Op_K (c, y, m, k)]
      | NoCol -> []
      end
  | ClosePath
  | Fill -> [Pdfops.Op_f]
  | FillEvenOdd -> [Pdfops.Op_f']
  | Stroke -> [Pdfops.Op_S]
  | FillStroke -> [Pdfops.Op_B]
  | FillStrokeEvenOdd -> [Pdfops.Op_B']
  | Clip -> [Pdfops.Op_W; Pdfops.Op_n]
  | ClipEvenOdd -> [Pdfops.Op_W']
  | SetLineThickness t -> [Pdfops.Op_w t; Pdfops.Op_n]
  | SetLineCap c -> [Pdfops.Op_J c]
  | SetLineJoin j -> [Pdfops.Op_j j]
  | SetMiterLimit m -> [Pdfops.Op_M m]
  | SetDashPattern (x, y) -> [Pdfops.Op_d (x, y)]
  | SoftXObject l | HardXObject l ->
      [Pdfops.Op_q] @ ops_of_drawops pdf l @ [Pdfops.Op_Q]
  | Image s -> [Pdfops.Op_Do (try fst (Hashtbl.find images s) with _ -> Cpdferror.error ("Image not found: " ^ s))]
  | ImageXObject (s, obj) ->
      Hashtbl.add images s (fresh_xobj_name (), Pdf.addobj pdf obj); 
      []
  | NewPage -> Pdfe.log ("NewPage remaining in graphic stream"); assert false
  | Opacity v ->
      let n = fresh_gs_name () in
        Hashtbl.add gss n (Pdf.Dictionary [("/ca", Pdf.Real v)]);
        [Pdfops.Op_gs n]
  | SOpacity v ->
      let n = fresh_gs_name () in
        Hashtbl.add gss n (Pdf.Dictionary [("/CA", Pdf.Real v)]);
        [Pdfops.Op_gs n]

and ops_of_drawops pdf drawops =
  flatten (map (ops_of_drawop pdf) drawops)

(* Draw all the accumulated operators. FIXME: Manage name clashes in Xobjects etc. *)
let draw fast range pdf drawops =
  let s = Pdfops.string_of_ops (ops_of_drawops pdf drawops) in
  let pdf = Cpdftweak.append_page_content s false fast range pdf in
  let images = list_of_hashtbl images in
  let image_resources = map (fun (_, (n, o)) -> (n, Pdf.Indirect o)) images in
  let gss_resources = list_of_hashtbl gss in
  Printf.printf "%i gss_resources\n" (length gss_resources);
    match images, gss_resources with [], [] -> pdf | _ ->
      let pages = Pdfpage.pages_of_pagetree pdf in
      let pages =
        map
          (fun p ->
            let new_resources =
              let existing_xobjects =
                match Pdf.lookup_direct pdf "/XObject" p.Pdfpage.resources with
                | Some (Pdf.Dictionary d) -> d
                | _ -> []
              in
              let existing_gss =
                match Pdf.lookup_direct pdf "/ExtGState" p.Pdfpage.resources with
                | Some (Pdf.Dictionary d) -> d
                | _ -> []
              in
                let new_xobjects = fold_right (fun (k, v) d -> add k v d) image_resources existing_xobjects in
                let new_gss = fold_right (fun (k, v) d -> add k v d) gss_resources existing_gss in
                let r =
                  Pdf.add_dict_entry (Pdf.add_dict_entry p.Pdfpage.resources "/XObject" (Pdf.Dictionary new_xobjects)) "/ExtGState" (Pdf.Dictionary new_gss)
                in
                  Printf.printf "final: %s\n" (Pdfwrite.string_of_pdf r);
                  r
            in
             {p with resources = new_resources})
          pages
      in
      Pdfpage.change_pages true pdf pages
