open Pdfutil

type position =
  | PosCentre of float * float
  | PosLeft of float * float
  | PosRight of float * float
  | Top of float
  | TopLeft of float
  | TopRight of float
  | Left of float
  | BottomLeft of float
  | Bottom of float
  | BottomRight of float
  | Right of float
  | Diagonal
  | ReverseDiagonal
  | Centre

let string_of_position = function
  | PosCentre (a, b) -> Printf.sprintf "PosCentre %f %f" a b
  | PosLeft (a, b) -> Printf.sprintf "PosLeft %f %f" a b
  | PosRight (a, b) -> Printf.sprintf "PosRight %f %f" a b
  | Top a -> Printf.sprintf "Top %f" a
  | TopLeft a -> Printf.sprintf "TopLeft %f" a
  | TopRight a -> Printf.sprintf "TopRight %f" a
  | Left a -> Printf.sprintf "Left %f" a
  | BottomLeft a -> Printf.sprintf "BottomLeft %f" a
  | Bottom a -> Printf.sprintf "Bottom %f" a
  | BottomRight a -> Printf.sprintf "BottomRight %f" a
  | Right a -> Printf.sprintf "Right %f" a
  | Diagonal -> "Diagonal"
  | ReverseDiagonal -> "Reverse Diagonal"
  | Centre -> "Centre"

type orientation =
  | Horizontal
  | Vertical
  | VerticalDown

(* Given the mediabox, calculate an absolute position for the text. *)
let calculate_position ignore_d w (xmin, ymin, xmax, ymax) orientation pos =
  let rot = if orientation = VerticalDown then rad_of_deg 270. else 0. in
    match pos with
    | Centre ->
        (xmin +. xmax) /. 2. -. w /. 2.,
        (ymin +. ymax) /. 2.,
        rot
    | Diagonal ->
        let angle = atan ((ymax -. ymin) /. (xmax -. xmin))
        in let cx, cy = (xmax +. xmin) /. 2., (ymax +. ymin) /. 2. in
          let dl = w /. 2. in
            let dx = dl *. cos angle
            in let dy = dl *. sin angle in
              cx -. dx, cy -. dy, angle
    | ReverseDiagonal ->
        let angle = atan ((ymax -. ymin) /. (xmax -. xmin))
        in let cx, cy = (xmax +. xmin) /. 2., (ymax +. ymin) /. 2. in
          let dl = w /. 2. in
            let dx = dl *. cos angle
            in let dy = dl *. sin angle in
              cx -. dx, (ymax +. ymin) -. (cy -. dy), angle -. ((2. *. pi) -. ((pi -. (2. *. angle)) *. 2.) /. 2.) +. pi
    | PosLeft (x, y) -> xmin +. x, ymin +. y, rot
    | PosCentre (x, y) -> xmin +. x -. (w /. 2.), ymin +. y, rot
    | PosRight (x, y) -> xmin +. x -. w, ymin +. y, rot
    | Top d ->
        let d = if ignore_d then 0. else d in
          (xmin +. xmax) /. 2. -. w /. 2., ymax -. d, rot
    | TopLeft d ->
        let d = if ignore_d then 0. else d in
          xmin +. d, ymax -. d, rot
    | TopRight d ->
        let d = if ignore_d then 0. else d in
        xmax -. d -. w, ymax -. d, rot
    | Left d ->
        let d = if ignore_d then 0. else d in
          xmin +. d, (ymax +. ymin) /. 2., rot
    | BottomLeft d ->
        let d = if ignore_d then 0. else d in
          xmin +. d, ymin +. d, rot
    | Bottom d ->
        let d = if ignore_d then 0. else d in
          (xmin +. xmax) /. 2. -. w /. 2., ymin +. d, rot
    | BottomRight d ->
        let d = if ignore_d then 0. else d in
          xmax -. d -. w, ymin +. d, rot
    | Right d ->
        let d = if ignore_d then 0. else d in
          xmax -. d -. w, (ymax +. ymin) /. 2., rot
