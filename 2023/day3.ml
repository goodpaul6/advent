type pos = {
  row: int;
  col: int;
}

type part_pos = {
  str: string;
  pos: pos;
}

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

let digits_regex = Str.regexp "[0-9]+"

let find_all_part_nums row line = 
  fold_each_match 
    (fun acc (str, match_col) -> { str; pos = {row; col=match_col} } :: acc)
    digits_regex
    line
    []

(*let symbol_regex = Str.regexp "[^0-9.]"

let find_all_symbol_pos row line =
  fold_each_match
    (fun acc (_, match_col) -> {row; col=match_col} :: acc)
    symbol_regex
    line
    []*)
  
let gear_regex = Str.regexp "\\*"

let find_all_gear_pos row line =
  fold_each_match
    (fun acc (_, match_col) -> {row; col=match_col} :: acc)
    gear_regex
    line
    []

let is_pos_inside_part_pos pos part_pos =
  pos.row == part_pos.pos.row &&
  pos.col >= part_pos.pos.col &&
  pos.col < part_pos.pos.col + String.length part_pos.str

let is_symbol_pos_adjacent_to_part_pos symbol_pos part_pos =
  is_pos_inside_part_pos {row=symbol_pos.row; col=symbol_pos.col - 1} part_pos ||
  is_pos_inside_part_pos {row=symbol_pos.row; col=symbol_pos.col + 1} part_pos ||
  is_pos_inside_part_pos {row=symbol_pos.row - 1; col=symbol_pos.col} part_pos ||
  is_pos_inside_part_pos {row=symbol_pos.row + 1; col=symbol_pos.col} part_pos ||
  is_pos_inside_part_pos {row=symbol_pos.row - 1; col=symbol_pos.col - 1} part_pos ||
  is_pos_inside_part_pos {row=symbol_pos.row - 1; col=symbol_pos.col + 1} part_pos ||
  is_pos_inside_part_pos {row=symbol_pos.row + 1; col=symbol_pos.col - 1} part_pos ||
  is_pos_inside_part_pos {row=symbol_pos.row + 1; col=symbol_pos.col + 1} part_pos

(*let is_part_adjacent_to_symbol part_pos symbol_posns =
  List.fold_left
    (fun acc symbol_pos -> acc || is_symbol_pos_adjacent_to_part_pos symbol_pos part_pos)
    false
    symbol_posns*)
  
let get_adjacent_parts symbol_pos part_posns =
  List.fold_left
    (fun acc part_pos ->
      if is_symbol_pos_adjacent_to_part_pos symbol_pos part_pos then
        part_pos :: acc
      else
        acc)
    []
    part_posns

let find_gear_ratios gear_posns part_posns =
  List.filter_map
    (fun gear_pos ->
      let adjacent_parts = get_adjacent_parts gear_pos part_posns in
      match adjacent_parts with
      | [a; b] -> Some ((int_of_string a.str) * (int_of_string b.str))
      | _ -> None)
    gear_posns

let () =
  let ic = open_in "inputs/day3.txt" in
  let rec aux part_posns gear_posns row =
    try
      let line = input_line ic in
      let parts = find_all_part_nums row line in
      let gears = find_all_gear_pos row line in
      aux (part_posns @ parts) (gear_posns @ gears) (row + 1)
    with _ ->
      (part_posns, gear_posns)
  in
  let (part_posns, gear_posns) = aux [] [] 0 in
  (* Part 1 soln
  let sum = List.fold_left
    (fun sum part -> 
      if is_part_adjacent_to_symbol part symbol_posns then
        (print_endline part.str; 
        int_of_string part.str + sum)
      else
        sum)
    0
    part_posns*)
  let gear_ratios = find_gear_ratios gear_posns part_posns in
  let sum = List.fold_left (+) 0 gear_ratios in
  print_endline @@ string_of_int sum