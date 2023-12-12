let lines =
  let ic = open_in "inputs/day5.txt" in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with _ ->
      acc
  in
  loop [] |> List.rev

let digits_regex = Str.regexp "[0-9]+"

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

let parse_nums line =
  fold_each_match
    (fun acc (num_str, _) ->
      (int_of_string num_str) :: acc)
    digits_regex
    line
    []
  |> List.rev

type range = {
  dest_start: int;
  src_start: int;
  len: int;
}

type map = {
  name: string;
  ranges: range list;
}

let seeds =
  parse_nums (List.hd lines)

let map_regex = Str.regexp "\\(.*\\) map:"

let range_from_nums = function
  | dest_start :: src_start :: len :: _ -> { dest_start; src_start; len }
  | _ -> failwith "Invalid range"

let maps =
  let rec loop acc = function
  | [] -> acc
  | line :: rest ->
    if Str.string_match map_regex line 0 then
      let new_acc = { name = Str.matched_group 1 line; ranges = [] } :: acc in
      loop new_acc rest
    else if not (List.is_empty acc) && Str.string_match digits_regex line 0 then
      let nums = parse_nums line in
      let range = range_from_nums nums in
      let map = List.hd acc in
      let new_map = { name = map.name; ranges = range :: map.ranges } in
      loop (new_map :: (List.tl acc)) rest
    else
      loop acc rest
  in
  loop [] lines |> List.rev

let loc_for_seed seed =
  let rec loop loc = function
    | [] -> loc
    | map :: rest ->
      let new_loc = List.find_map
        (fun range ->
          if loc >= range.src_start && loc < range.src_start + range.len then
            Some ((loc - range.src_start) + range.dest_start)
          else
            None)
        map.ranges
      in
      match new_loc with
      | Some value -> loop value rest
      | None -> loop loc rest
  in
  loop seed maps

let lowest_loc =
  List.fold_left
    (fun min_value seed ->
      min min_value (loc_for_seed seed))
    Int.max_int
    seeds

