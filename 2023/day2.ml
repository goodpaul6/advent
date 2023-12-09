let game_num_regex = Str.regexp "Game \\([0-9]+\\)"
let digit_color_regex = Str.regexp "\\([0-9]+\\) \\([a-z]+\\)"

type color_count = {
    color: string;
    count: int;
}

type game_set = color_count list
type game = {
    sets: game_set list;
}

let parse_color color_str =
    let _ = Str.search_forward digit_color_regex color_str 0 in
    let count_str = Str.matched_group 1 color_str in
    let color_str = Str.matched_group 2 color_str in
    { color = color_str; count = int_of_string count_str }

let parse_set set_str =
    let colors = String.split_on_char ',' set_str in
    let color_counts = List.map parse_color colors in
    color_counts

let parse_sets sets_str =
    let sets_strs = String.split_on_char ';' sets_str in
    let rec aux acc = function
        | [] -> acc
        | set_str :: rest -> aux (parse_set set_str :: acc) rest
    in
    aux [] sets_strs

let parse_game line =
    let split_strs = String.split_on_char ':' line in
    match split_strs with
    | [] -> { sets = [] }
    | game_str :: rest ->
      let sets_str = List.hd rest in
      let _ = Str.search_forward game_num_regex game_str 0 in
      let sets = parse_sets sets_str in
      { sets = sets }

let replace_if_greater_color_count orig_cc new_cc =
    if new_cc.color = orig_cc.color && new_cc.count > orig_cc.count then
        new_cc
    else
        orig_cc

let rec min_required_color_counts_for_set acc = function
    | [] -> acc
    | { color; _ } as cc :: rest ->
        match List.find_opt (fun v -> v.color = color) acc with
        | None -> min_required_color_counts_for_set (cc :: acc) rest
        | _ -> min_required_color_counts_for_set
                (List.map
                    (fun occ -> replace_if_greater_color_count occ cc)
                    acc)
                rest

let rec min_required_color_counts_for_sets acc = function
    | [] -> acc
    | set :: rest ->
        min_required_color_counts_for_sets
            (min_required_color_counts_for_set acc set)
            rest

let game_power game =
    let min_counts = min_required_color_counts_for_sets [] game.sets in
    List.fold_left (fun acc cc -> acc * cc.count) 1 min_counts
        
let () =
    let ic = open_in "input.txt" in
    let rec aux sum =
        try
            let line = input_line ic in
            let game = parse_game line in
            aux @@ sum + game_power game
        with e ->
            print_endline @@ Printexc.to_string e;
            sum
    in
    print_endline @@ string_of_int @@ aux 0