let dir_count_regex = Str.regexp "\\([RLDU]\\) \\([0-9]+\\)"

let fold_each_match fn regex str init =
  let rec aux acc col =
    try
      let match_col = Str.search_forward regex str col in
      let str = Str.matched_string str in
      let new_acc = fn acc (str, match_col) in
      aux new_acc @@ match_col + String.length str
    with _ ->
      acc
  in
  aux init 0

let lines =
  let ic = open_in "inputs/day18_ex.txt" in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with _ ->
      acc
  in
  loop [] |> List.rev

type dig_instr = {
  dir: string;
  count: int;
}

let dig_instrs =
  List.fold_left
    (fun acc line ->
      let _ = Str.search_forward dir_count_regex line 0 in
      let instr = { dir = Str.matched_group 1 line; count = int_of_string @@ Str.matched_group 2 line; } in
      instr :: acc)
    []
   lines
  |> List.rev

let dug =
  let dug = Hashtbl.create 2048 in
  let rec loop row col instrs = match instrs with
    | [] -> ()
    | { count = 0; _ } :: rest -> loop row col rest
    | inst :: rest ->
      Hashtbl.replace dug (row, col) 1;
      let next_instrs = { dir = inst.dir; count = inst.count - 1 } :: rest in
      match inst with
        | { dir = "R"; _ } -> loop row (col + 1) next_instrs
        | { dir = "L"; _ } -> loop row (col - 1) next_instrs
        | { dir = "U"; _ } -> loop (row - 1) col next_instrs
        | { dir = "D"; _ } -> loop (row + 1) col next_instrs
        | _ -> failwith "Invalid dig instruction"
  in
  loop 0 0 dig_instrs;
  dug

let dug_extents =
  let fold (row, col) _ (left, top, right, bottom) =
    let next_left = min left col in
    let next_top = min top row in
    let next_right = max right col in
    let next_bottom = max bottom row in
    (next_left, next_top, next_right, next_bottom)
  in
  Hashtbl.fold fold dug (Int.max_int, Int.max_int, Int.min_int, Int.min_int)

let dug_array =
  let (left, top, right, bottom) = dug_extents in
  let w = right - left + 1 in
  let h = bottom - top + 1 in
  (* 1 = dug, 0 = not dug *)
  let dug_array = Array.make_matrix h w 0 in
  Hashtbl.iter
    (fun (row, col) _ ->
      dug_array.(row - top).(col - left) <- 1)
    dug;
  dug_array

let filled_array =
  let filled_array = Array.map Array.copy dug_array in
  let h = Array.length filled_array in
  let w = Array.length filled_array.(0) in
  for row = 0 to (h - 1) do
    let inside_ref = ref false in
    for col = 0 to (w - 1) do
      let inside = !inside_ref in
      let dug_value = dug_array.(row).(col) in
      let was_prev_dug = col > 0 && dug_array.(row).(col - 1) = 1 in
      let next_inside = if dug_value = 1 && (not was_prev_dug) then not inside else inside in
      if dug_value = 1 || inside then
        filled_array.(row).(col) <- 1
      else
        ();
      inside_ref := next_inside;
    done
  done;
  filled_array

let filled_area =
  Array.fold_left
    (fun acc elem ->
      acc + (Array.fold_left (+) 0 elem))
    0
    filled_array

let () = Array.iter 
  (fun elem -> 
    Array.iter print_int elem; 
    print_newline ()) 
  filled_array

(*let () = Hashtbl.iter (fun (row, col) _ -> Printf.printf "%d,%d\n" row col) dug*)