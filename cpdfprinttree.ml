(* Courtesy Martin Jambon, in the public domain. *)
let rec titer f = function
  | [] -> ()
  | [x] -> f true x
  | x :: tl -> f false x; titer f tl

let to_buffer ?(line_prefix = "") ~get_name ~get_children buf x =
  let rec print_root indent x =
    Printf.bprintf buf "%s\n" (get_name x);
    let children = get_children x in
      titer (print_child indent) children
  and print_child indent is_last x =
    let line = if is_last then "└── " else "├── " in
      Printf.bprintf buf "%s%s" indent line;
      let extra_indent = if is_last then "    " else "│   " in
        print_root (indent ^ extra_indent) x
  in
    Buffer.add_string buf line_prefix;
    print_root line_prefix x

let to_string ?line_prefix ~get_name ~get_children x =
  let buf = Buffer.create 1000 in
    to_buffer ?line_prefix ~get_name ~get_children buf x;
    Buffer.contents buf
