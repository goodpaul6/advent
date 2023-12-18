type pos = {
  row: int;
  col: int;
}

let lines =
  let ic = open_in "inputs/day11.txt" in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with _ ->
      acc
  in
  loop [] |> List.rev

let expanded_rows =
  let rec loop rows row lines =
    if List.is_empty lines then
      rows
    else
      let line = List.hd lines in
      if String.contains line '#' then
        loop rows (row + 1) (List.tl lines)
      else
        loop (row :: rows) (row + 1) (List.tl lines)
  in
  loop [] 0 lines |> List.rev
let expanded_cols =
  let rec loop cols lines col =
    if col >= String.length @@ List.hd lines then
      cols
    else
      if List.exists (fun line -> String.get line col = '#') lines then
        loop cols lines (col + 1)
      else
        loop (col :: cols) lines (col + 1)
  in
  loop [] lines 0
let expand_count = 1_000_000

let galaxy_posns =
  (* Go along and keep track of current row and col, and if we traverse an expanded row/col count that
     for 10M rows and cols *)
  let rec loop posns row col line_row line_col lines =
    if List.is_empty lines then
      posns
    else if List.exists (fun v -> line_row = v) expanded_rows then
      loop posns (row + expand_count) 0 (line_row + 1) 0 (List.tl lines)
    else if line_col >= String.length (List.hd lines) then
      loop posns (row + 1) 0 (line_row + 1) 0 (List.tl lines)
    else
      let line = List.hd lines in
      let next_posns = if String.get line line_col = '#' then
        ({row; col} :: posns)
      else
        posns
      in
      let next_col = if List.exists (fun v -> line_col = v) expanded_cols then
        col + expand_count
      else
        col + 1
      in
      loop next_posns row next_col line_row (line_col + 1) lines
  in
  loop [] 0 0 0 0 lines |> List.rev

let posn_pairs =
  let posns = Array.of_list galaxy_posns in
  let pair_up a rest = Array.map (fun p -> (a, p)) rest in
  Array.mapi 
    (fun i a ->
      (pair_up a (Array.sub posns (i + 1) (Array.length posns - i - 1))) |>
      Array.to_list)
    (Array.sub posns 0 (Array.length posns - 1)) |>
  Array.to_list |>
  List.flatten
 
let sum_shortest_paths =
  let rec loop sum = function
    | [] -> sum
    | (a, b) :: rest ->
      loop (sum + (abs (a.row - b.row)) + (abs (a.col - b.col))) rest
  in
  loop 0 posn_pairs

let () =
  print_endline @@ string_of_int sum_shortest_paths