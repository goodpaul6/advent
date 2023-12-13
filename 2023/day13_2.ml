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
          let res = Some (patlen / 2 + start_row) in
          res
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
          let res = Some ((fpatlen / 2) + start_col) in
          res
          (*Printf.printf "start_col=%d, end_col=%d\n" start_col end_col;
          List.iter (fun s -> print_endline (String.sub s start_col fpatlen)) pattern;
          *)
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

type mirror_line = MirrorCol of int | MirrorRow of int
let get_mirror_line pattern =
  let hline = horiz_mirror_line pattern in
  match hline with
  | Some value -> MirrorRow value
  | None ->
    let vline = vert_mirror_line pattern in
    MirrorCol (Option.get vline)


let line_value line =
  match line with
  | MirrorCol col -> col
  | MirrorRow row -> row * 100

exception NewMirrorLine of mirror_line

let mirror_line_without_smudge pattern =
  let orig_line = get_mirror_line pattern in
  let pattern = Array.of_list pattern in
  let w = String.length @@ pattern.(0) in
  let h = Array.length pattern in
  try
    for i = 0 to h - 1 do
      let prev_str = pattern.(i) in
      for j = 0 to w - 1 do
        let ch = String.get prev_str j in
        let new_ch = if ch = '#' then '.' else '#' in
        let new_str = String.mapi (fun ci ch -> if ci == j then new_ch else ch) prev_str in

        pattern.(i) <- new_str;

        let lpattern = Array.to_list pattern in
        let new_hline = horiz_mirror_line lpattern in
        let new_vline = vert_mirror_line lpattern in

        match (orig_line, new_hline) with
        | (MirrorRow orig_row, Some row) ->
          if orig_row <> row then
            raise_notrace (NewMirrorLine (MirrorRow row))
          else
            ();
        | (MirrorCol _, Some row) ->
            raise_notrace (NewMirrorLine (MirrorRow row))
        | _ -> ();

        match (orig_line, new_vline) with
        | (MirrorCol orig_col, Some col) ->
          if orig_col <> col then
            raise_notrace (NewMirrorLine (MirrorCol col))
          else
            ();
        | (MirrorRow _, Some col) ->
            raise_notrace (NewMirrorLine (MirrorCol col))
        | _ -> ();

        pattern.(i) <- prev_str;
      done;
      pattern.(i) <- prev_str;
    done;
    Array.iter print_endline pattern;
    failwith "Found orig line again for this pattern"
  with NewMirrorLine line ->
    line




let answer =
  let rec loop sum = function
  | [] -> sum
  | pattern :: rest ->
    try
      let line = mirror_line_without_smudge pattern in
      loop (sum + line_value line) rest
    with e ->
      raise e
  in
  loop 0 patterns