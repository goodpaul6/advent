type card = {
  winning_nums: int list;
  your_nums: int list;
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

let parse_nums nums_str =
  fold_each_match
    (fun acc (s, _) -> int_of_string s :: acc)
    digits_regex
    nums_str
    []

let parse_card line =
  match String.split_on_char ':' line with
    | [] | [_] -> raise @@ Invalid_argument line
    | _ :: all_nums :: _ ->
      print_endline all_nums;
      match String.split_on_char '|' all_nums with
        | [w_nums_str; y_nums_str] ->
          let w_nums = parse_nums w_nums_str in
          let y_nums = parse_nums y_nums_str in
          { winning_nums = w_nums; your_nums = y_nums }
        | _ -> raise @@ Invalid_argument line

let card_points card =
  List.fold_left
    (fun pts y_num ->
      if List.exists (fun v -> v = y_num) card.winning_nums then
        if pts = 0 then 1 else pts * 2
      else
        pts)
    0
    card.your_nums

let () =
  let ic = open_in "inputs/day4.txt" in
  let rec aux sum =
    try
      let line = input_line ic in
      let card = parse_card line in
      aux (sum + card_points card)
    with _ ->
      sum
  in
  print_endline @@ string_of_int @@ aux 0