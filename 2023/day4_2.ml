type card = {
  id: string;
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
    | card_id :: all_nums :: _ ->
      match String.split_on_char '|' all_nums with
        | [w_nums_str; y_nums_str] ->
          let w_nums = parse_nums w_nums_str in
          let y_nums = parse_nums y_nums_str in
          { id = card_id; winning_nums = w_nums; your_nums = y_nums }
        | _ -> raise @@ Invalid_argument line

let card_win_count card =
  List.fold_left
    (fun count y_num ->
      if List.exists (fun v -> v = y_num) card.winning_nums then
        count + 1
      else
        count)
    0
    card.your_nums

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

let next_n_cards orig_cards cur_card n =
  let card_index = List.find_index (fun card -> cur_card.id = card.id) orig_cards in
  match card_index with
  | Some index ->
    List.filteri (fun i _ -> i > index && i <= index + n) orig_cards
  | None -> raise @@ Invalid_argument "cur_card"

let rec count_cards orig_cards count cards_left =
  let card_count = List.find_opt 
    (fun (_, count) -> count > 0)
    cards_left
  in
  match card_count with
  | Some (card, _) ->
    let win_count = card_win_count card in
    let copy_cards = next_n_cards orig_cards card win_count in
    let new_cards_left = List.map
      (fun (left_card, count) ->
        if left_card.id = card.id then
          (left_card, count - 1)
        else if List.exists (fun copy_card -> copy_card.id = left_card.id) copy_cards then
          (left_card, count + 1)
        else
          (left_card, count))
      cards_left
    in
    count_cards orig_cards (count + 1) new_cards_left
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
  let count = count_cards 
    orig_cards 
    0 
    (List.map (fun card -> (card, 1)) orig_cards) 
  in
  print_endline @@ string_of_int count
  