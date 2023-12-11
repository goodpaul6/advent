type pos = {
  row: int;
  col: int;
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
  let rec loop new_lines lines =
    if List.is_empty lines then
      new_lines
    else
      let line = List.hd lines in
      if String.contains line '#' then
        loop (line :: new_lines) (List.tl lines)
      else
        loop (line :: line :: new_lines) (List.tl lines)
  in
  loop [] lines |> List.rev

let expanded =
  let rec loop lines col =
    if col >= String.length @@ List.hd lines then
      lines
    else
      if List.exists (fun line -> String.get line col = '#') lines then
        loop lines (col + 1)
      else
        let expanded_col_lines = List.map
          (fun line -> Printf.sprintf "%s.%s" 
            (String.sub line 0 col)
            (String.sub line col (String.length line - col)))
          lines
        in
        loop expanded_col_lines (col + 2)
  in
  loop expanded_rows 0

let galaxy_regex = Str.regexp "#"

let galaxy_posns =
  let posns = List.mapi
    (fun row line -> 
      fold_each_match 
        (fun acc (_, col) -> 
          { row; col } :: acc)
        galaxy_regex
        line
        [])
    expanded
  in
  List.flatten posns

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
  List.iter print_endline expanded;