type card = {
  wins: int;
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

let win_count win_nums your_nums =
  List.fold_left
    (fun count y_num ->
      if List.exists (fun v -> v = y_num) win_nums then
        count + 1
      else
        count)
    0
    your_nums

let parse_card line =
  match String.split_on_char ':' line with
    | [] | [_] -> raise @@ Invalid_argument line
    | _ :: all_nums :: _ ->
      match String.split_on_char '|' all_nums with
        | [w_nums_str; y_nums_str] ->
          let w_nums = parse_nums w_nums_str in
          let y_nums = parse_nums y_nums_str in
          {wins = win_count w_nums y_nums}
        | _ -> raise @@ Invalid_argument line

(*let first n list =
  let rec aux acc n list =
    if n == 0 then
      acc
    else
      aux (List.hd list :: acc) (n - 1) (List.tl list)
  in
  aux [] n list |> List.rev*)
  
(*let print_cards cards =
  List.map (fun card -> card.id) cards |>
  String.concat ", " |>
  print_endline*)

(*let next_n_cards orig_cards cur_card n =
  let card_index = Array.find_index (fun card -> cur_card.id = card.id) orig_cards in
  match card_index with
  | Some index ->
    Array.sub orig_cards (index + 1) n
  | None -> raise @@ Invalid_argument "cur_card"*)

let rec count_cards_mut orig_cards count cards_left =
  let card_idx = Array.find_index 
    (fun (_, count) -> count > 0)
    cards_left
  in
  match card_idx with
  | Some index ->
    let (card, _) = cards_left.(index) in
    let win_count = card.wins in
    Array.mapi_inplace
      (fun i (left_card, count) ->
        if i = index then
          (left_card, count - 1)
        else if i > index && i <= index + win_count then
          (left_card, count + 1)
        else
          (left_card, count))
      cards_left;
    count_cards_mut orig_cards (count + 1) cards_left
  | None -> count

let () =
  let ic = open_in "inputs/day4.txt" in
  let rec aux cards =
    try
      let line = input_line ic in
      let card = parse_card line in
      aux (card :: cards)
    with _ ->
      cards
  in
  let orig_cards = aux [] |> List.rev in
  let count = count_cards_mut
    (Array.of_list orig_cards)
    0 
    (Array.of_list @@ List.map (fun card -> (card, 1)) orig_cards)
  in
  print_endline @@ string_of_int count
  