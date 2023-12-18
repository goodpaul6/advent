let dir_count_regex = Str.regexp "\\(#[0-9a-f]+\\)"

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
      let hex = Str.matched_string line in
      let dist_hex = "0x" ^ String.sub hex 1 5 in
      let col_hex = String.get hex 6 in
      let dir = match col_hex with
        | '0' -> "R"
        | '1' -> "D"
        | '2' -> "L"
        | '3' -> "U"
        | _ -> failwith "Invalid direction hex"
      in
      let count = int_of_string dist_hex in
      let instr = { dir; count; } in
      instr :: acc)
    []
   lines
  |> List.rev

type point = {
  x: int;
  y: int;
}

let dig_points =
  let rec loop points x y instrs = match instrs with
  | [] -> points
  | { dir; count } :: rest ->
    let next_points = { x; y; } :: points in
    match dir with
    | "R" -> loop next_points (x + count) y rest
    | "D" -> loop next_points x (y + count) rest
    | "L" -> loop next_points (x - count) y rest
    | "U" -> loop next_points x (y - count) rest
    | _ -> failwith "Invalid dir"
  in
  loop [] 0 0 dig_instrs
  |> List.rev

(*
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

exception FoundStartPos of int * int

let filled_array =
  let filled_array = Array.map Array.copy dug_array in
  let h = Array.length filled_array in
  let w = Array.length filled_array.(0) in
  try
    for col = 0 to (w - 1) do
      if dug_array.(1).(col) = 0 && dug_array.(0).(col) = 1 then
        raise @@ FoundStartPos (1, col)
      else
        ();
    done;
    filled_array
  with FoundStartPos (row, col) ->
    let queue = Queue.create () in
    Queue.push (row, col) queue;
    while not @@ Queue.is_empty queue do
      let (row, col) = Queue.pop queue in
      if row < 0 || row >= h || col < 0 || col >= w || filled_array.(row).(col) = 1 then
        ()
      else begin
          filled_array.(row).(col) <- 1;
          Queue.push (row - 1, col) queue;
          Queue.push (row + 1, col) queue;
          Queue.push (row, col - 1) queue;
          Queue.push (row, col + 1) queue;
      end
    done;
    filled_array

let filled_area =
  Array.fold_left
    (fun acc elem ->
      acc + (Array.fold_left (+) 0 elem))
    0
    filled_array

(*
42859 too high 
*)

(*let () = Array.iter 
  (fun elem -> 
    Array.iter print_int elem; 
    print_newline ()) 
  filled_array*)

(*let () = Hashtbl.iter (fun (row, col) _ -> Printf.printf "%d,%d\n" row col) dug*)
*)