type pos = {
  row: int;
  col: int;
}

type tile = {
  s: string;
  pos: pos;
}

let tile_regex = Str.regexp "[-|LJ7FS\\.]"

let tiles row line =
  let rec aux acc col =
    try
      let match_col = Str.search_forward tile_regex line col in
      let s = Str.matched_string line in
      aux ({ s; pos = {row; col = match_col}} :: acc) (match_col + String.length s)
    with _ ->
      acc
  in
  aux [] 0

let all_tiles_in_input =
  let ic = open_in "inputs/day10.txt" in
  let rec aux acc row =
    try
      let line = input_line ic in
      let tiles = tiles row line in
      aux (acc @ tiles) (row + 1)
    with _ ->
      acc
  in
  aux [] 0

let tile_at_pos pos all_tiles =
  List.find_opt (fun tile -> tile.pos = pos) all_tiles

let get_pipe_opt pos cand_str all_pipes =
  let pos_pipe = tile_at_pos pos all_pipes in
  match pos_pipe with
  | Some p ->
    let pipe_ch = String.get p.s 0 in
    if String.contains cand_str pipe_ch then
      Some p
    else
      None
  | _ -> None


let cands_for_pipe (pipe: tile) all_pipes =
  let row = pipe.pos.row in
  let col = pipe.pos.col in
  let n_pos = {row = row - 1; col} in
  let s_pos = {row = row + 1; col} in
  let w_pos = {row; col = col - 1} in
  let e_pos = {row; col = col + 1} in
  let n_cand_str = "|7F" in
  let s_cand_str = "|JL" in
  let w_cand_str = "-LF" in
  let e_cand_str = "-J7" in
  let opt_cands = match pipe.s with
  | "S" -> 
    [
      get_pipe_opt n_pos n_cand_str all_pipes;
      get_pipe_opt s_pos s_cand_str all_pipes;
      get_pipe_opt w_pos w_cand_str all_pipes;
      get_pipe_opt e_pos e_cand_str all_pipes;
    ]
  | "|" ->
    [
      get_pipe_opt n_pos n_cand_str all_pipes;
      get_pipe_opt s_pos s_cand_str all_pipes;
    ]
  | "-" ->
    [
      get_pipe_opt w_pos w_cand_str all_pipes;
      get_pipe_opt e_pos e_cand_str all_pipes;
    ]
  | "L" ->
    [
      get_pipe_opt n_pos n_cand_str all_pipes;
      get_pipe_opt e_pos e_cand_str all_pipes;
    ]
  | "J" ->
    [
      get_pipe_opt n_pos n_cand_str all_pipes;
      get_pipe_opt w_pos w_cand_str all_pipes;
    ]
  | "7" ->
    [
      get_pipe_opt s_pos s_cand_str all_pipes;
      get_pipe_opt w_pos w_cand_str all_pipes;
    ]
  | "F" ->
    [
      get_pipe_opt s_pos s_cand_str all_pipes;
      get_pipe_opt e_pos e_cand_str all_pipes;
    ]
  | _ -> raise @@ Invalid_argument pipe.s
  in
  List.filter_map (Fun.id) opt_cands

let loop_tiles all_pipes =
  let start_pipe = List.find (fun p -> p.s = "S") all_pipes in
  let rec aux cur_dist visited cands =
    let cur_pipe = List.hd cands in
    let reachable = cands_for_pipe cur_pipe all_pipes in
    let new_cands = reachable @ List.tl cands in
    let new_cands_without_visited =
      List.filter
        (fun (pipe: tile) ->
          not @@ List.exists (fun dpos -> pipe.pos = dpos.pos) visited)
        new_cands
    in
    if List.length new_cands_without_visited = 0 then
      visited
    else
      aux 
        (cur_dist + 1) 
        (cur_pipe :: visited)
        new_cands_without_visited
  in
  let tiles = aux 0 [] [start_pipe] in
  tiles 

let adjacent_tiles tile all_tiles =
  let row = tile.pos.row in
  let col = tile.pos.col in
  let n_pos = {row = row - 1; col} in
  let s_pos = {row = row + 1; col} in
  let w_pos = {row; col = col - 1} in
  let e_pos = {row; col = col + 1} in
  List.filter_map (Fun.id) [
    tile_at_pos n_pos all_tiles;
    tile_at_pos s_pos all_tiles;
    tile_at_pos w_pos all_tiles;
    tile_at_pos e_pos all_tiles
  ]

let same_tile a b =
  a.pos = b.pos

let has_tile tile tiles =
  List.exists (fun t -> same_tile tile t) tiles

let rec fill_till_loop_tile all_tiles loop_tiles visited cand_tiles =
  if List.length cand_tiles = 0 then
    visited
  else
    let cand = List.hd cand_tiles in
    if has_tile cand loop_tiles || has_tile cand visited then
      fill_till_loop_tile all_tiles loop_tiles visited (List.tl cand_tiles)
    else
      let new_visited = cand :: visited in
      let next_cands = (adjacent_tiles cand all_tiles) @ (List.tl cand_tiles) in
      let next_cands_without_stops = List.filter 
        (fun tile ->
          (not @@ has_tile tile new_visited) &&
          (not @@ has_tile tile loop_tiles))
        next_cands
      in
      fill_till_loop_tile all_tiles loop_tiles new_visited next_cands_without_stops

type direction = North | South | East | West

let string_of_direction d =
  match d with
    | North -> "North"
    | South -> "South"
    | East -> "East"
    | West -> "West"

let rec follow_loop_and_fill travel_dir all_tiles inside loop_tiles rem_loop_tiles =
  match rem_loop_tiles with
  | [] -> inside
  | l_tile :: rest ->
    let adj_tile_opt = match travel_dir with
      | North -> tile_at_pos {row=l_tile.pos.row; col=l_tile.pos.col-1} all_tiles
      | South -> tile_at_pos {row=l_tile.pos.row; col=l_tile.pos.col+1} all_tiles
      | East -> tile_at_pos {row=l_tile.pos.row - 1; col=l_tile.pos.col} all_tiles
      | West -> tile_at_pos {row=l_tile.pos.row + 1; col=l_tile.pos.col} all_tiles
    in
    let new_inside = fill_till_loop_tile all_tiles loop_tiles inside
      (List.filter_map (Fun.id) [adj_tile_opt])
    in
    let new_travel_dir = match (travel_dir, l_tile.s) with
      | (North, "7") -> West
      | (North, "F") -> East
      | (South, "J") -> West
      | (South, "L") -> East
      | (East, "7") -> South
      | (East, "J") -> North
      | (West, "F") -> South
      | (West, "L") -> North
      | _ -> travel_dir
    in
    Printf.printf "%s %s->%s %d (%d,%d)\n" l_tile.s (string_of_direction travel_dir) (string_of_direction new_travel_dir) (List.length inside) l_tile.pos.row l_tile.pos.col;    
    follow_loop_and_fill new_travel_dir all_tiles new_inside loop_tiles rest  

let count_inner_tiles loop_tiles all_tiles =
  let st_pos = (List.hd loop_tiles).pos in
  let nx_pos = (List.nth loop_tiles 1).pos in
  let travel_dir = if nx_pos.row > st_pos.row then
    South
  else if nx_pos.row < st_pos.row then
    North
  else if nx_pos.col < st_pos.col then
    West
  else
    East
  in
  let inside = follow_loop_and_fill travel_dir all_tiles [] loop_tiles (List.tl loop_tiles) in
  List.length inside
    
let () =
  let lt = List.rev (loop_tiles all_tiles_in_input) in
  let inside_count = count_inner_tiles lt all_tiles_in_input in
  print_endline @@ string_of_int inside_count