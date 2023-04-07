open Pdfutil

type position =
  | PosCentre of float * float
  | PosLeft of float * float
  | PosRight of float * float
  | Top of float
  | TopLeft of float * float
  | TopRight of float * float
  | Left of float
  | BottomLeft of float * float
  | Bottom of float
  | BottomRight of float * float
  | Right of float
  | Diagonal
  | ReverseDiagonal
  | Centre

let string_of_position = function
  | PosCentre (a, b) -> Printf.sprintf "PosCentre %f %f" a b
  | PosLeft (a, b) -> Printf.sprintf "PosLeft %f %f" a b
  | PosRight (a, b) -> Printf.sprintf "PosRight %f %f" a b
  | Top a -> Printf.sprintf "Top %f" a
  | TopLeft (a, b) -> Printf.sprintf "TopLeft %f %f" a b
  | TopRight (a, b) -> Printf.sprintf "TopRight %f %f" a b
  | Left a -> Printf.sprintf "Left %f" a
  | BottomLeft (a, b) -> Printf.sprintf "BottomLeft %f %f" a b
  | Bottom a -> Printf.sprintf "Bottom %f" a
  | BottomRight (a, b) -> Printf.sprintf "BottomRight %f %f" a b
  | Right a -> Printf.sprintf "Right %f" a
  | Diagonal -> "Diagonal"
  | ReverseDiagonal -> "Reverse Diagonal"
  | Centre -> "Centre"

(* Given the mediabox, calculate an absolute position for the text. *)
let calculate_position ignore_d w (xmin, ymin, xmax, ymax) pos =
  let rot = 0. in
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
    | TopLeft (a, b) ->
        let a = if ignore_d then 0. else a in
        let b = if ignore_d then 0. else b in
          xmin +. a, ymax -. b, rot
    | TopRight (a, b) ->
        let a = if ignore_d then 0. else a in
        let b = if ignore_d then 0. else b in
        xmax -. a -. w, ymax -. b, rot
    | Left d ->
        let d = if ignore_d then 0. else d in
          xmin +. d, (ymax +. ymin) /. 2., rot
    | BottomLeft (a, b) ->
        let a = if ignore_d then 0. else a in
        let b = if ignore_d then 0. else b in
          xmin +. a, ymin +. b, rot
    | Bottom d ->
        let d = if ignore_d then 0. else d in
          (xmin +. xmax) /. 2. -. w /. 2., ymin +. d, rot
    | BottomRight (a, b) ->
        let a = if ignore_d then 0. else a in
        let b = if ignore_d then 0. else b in
          xmax -. a -. w, ymin +. b, rot
    | Right d ->
        let d = if ignore_d then 0. else d in
          xmax -. d -. w, (ymax +. ymin) /. 2., rot
