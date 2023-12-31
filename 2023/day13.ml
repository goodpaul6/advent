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
  let ic = open_in "inputs/day13.txt" in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with _ ->
      acc
  in
  loop [] |> List.rev

let patterns =
  let rec loop pattern patterns = function
  | [] -> (List.rev pattern) :: patterns
  | line :: rest ->
    let (pattern, patterns) = if String.length line = 0 then
      ([], (List.rev pattern) :: patterns)
    else
      (line :: pattern, patterns)
    in
    loop pattern patterns rest
  in
  loop [] [] lines |>
  List.rev

let either_opt a b =
  match a with
  | None -> b
  | _ -> a

let either_lazy a b =
  match a with
  | Some value -> value
  | None -> Option.get (b ())

let horiz_mirror_line pattern =
  let orig_pattern = pattern in
  let rec aux start_row end_row =
    let patlen = end_row - start_row in
    if patlen < 2 then
      None
    else
      if patlen mod 2 = 0 then
        let pattern = Array.of_list pattern in
        let pattern = Array.sub pattern start_row (end_row - start_row) in
        let patlen = Array.length pattern in
        let first_half = Array.sub pattern 0 (patlen / 2) in
        let second_half = Array.sub pattern (patlen / 2) (patlen / 2) in
        let fhl = Array.to_list first_half |> List.rev in
        let shl = Array.to_list second_half in
        if fhl = shl then
          Some (patlen / 2 + start_row)
        else if start_row > 0 then
          aux (start_row + 2) end_row
        else
          aux start_row (end_row - 2)
      else
        let fdrop = aux (start_row + 1) (List.length orig_pattern) in
        let ldrop = aux 0 (end_row - 1) in
        either_opt fdrop ldrop
  in
  aux 0 (List.length pattern)


let is_palindrome a =
  let alen = String.length a in
  let rec aux i =
    if i == alen / 2 then
      true
    else
      let m = String.get a i = String.get a (alen - i - 1) in
      m && aux (i + 1)
  in
  aux 0

let vert_mirror_line pattern =
  let rec aux start_col end_col =
    let fpatlen = end_col - start_col in
    if fpatlen < 2 then
      None
    else
      if fpatlen mod 2 = 0 then
        let mirr = List.for_all
          (fun s ->
            let s = String.sub s start_col fpatlen in
            is_palindrome s)
          pattern
        in
        if mirr then
          (Printf.printf "start_col=%d, end_col=%d\n" start_col end_col;
          List.iter (fun s -> print_endline (String.sub s start_col fpatlen)) pattern;
          Some ((fpatlen / 2) + start_col))
        else if start_col > 0 then
          aux (start_col + 2) end_col
        else
          aux start_col (end_col - 2)
      else
        let fdrop = aux (start_col + 1) (String.length @@ List.hd pattern) in
        let ldrop = aux 0 (end_col - 1) in
        either_opt fdrop ldrop
  in
  aux 0 (String.length @@ List.hd pattern)
    
let answer =
  let rec loop sum = function
  | [] -> sum
  | pattern :: rest ->
    let hline = horiz_mirror_line pattern in
    match hline with
    | Some value -> loop (sum + value * 100) rest
    | None ->
      try
        let vline = vert_mirror_line pattern in
        let vline = Option.get vline in
        loop (sum + vline) rest
      with e ->
        print_endline "Could not find mirror line for pattern";
        List.iter print_endline pattern;
        raise e
  in
  loop 0 patterns