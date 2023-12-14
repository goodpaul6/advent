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

module IntSet = Set.Make(Int)

let horiz_mirror_lines pattern =
  let rec aux acc start_row end_row =
    let patlen = end_row - start_row in
    if patlen < 2 then 
      acc
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
          let res = (patlen / 2 + start_row) in
          (*Printf.printf "p_row=%d\n" res;*)
          IntSet.add res acc
        else if start_row > 0 then
          aux acc (start_row + 2) end_row
        else
          aux acc start_row (end_row - 2)
      else
        let fdrop = aux acc (start_row + 1) end_row in
        aux fdrop start_row (end_row - 1)
  in
  aux IntSet.empty 0 (List.length pattern) |> IntSet.to_list

let horiz_mirror_line pattern =
  let lines = horiz_mirror_lines pattern in
  if List.is_empty lines then
    None
  else
    Some (List.hd lines)

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

let vert_mirror_lines pattern =
  let rec aux acc start_col end_col =
    let fpatlen = end_col - start_col in
    if fpatlen < 2 then
      acc
    else
      if fpatlen mod 2 = 0 then
        let mirr = List.for_all
          (fun s ->
            let s = String.sub s start_col fpatlen in
            is_palindrome s)
          pattern
        in
        if mirr then
          let res = ((fpatlen / 2) + start_col) in
          (*Printf.printf "p_col=%d\n" res;*)
          IntSet.add res acc
          (*Printf.printf "start_col=%d, end_col=%d\n" start_col end_col;
          List.iter (fun s -> print_endline (String.sub s start_col fpatlen)) pattern;
          *)
        else if start_col > 0 then
          aux acc (start_col + 2) end_col
        else
          aux acc start_col (end_col - 2)
      else
        let fdrop = aux acc (start_col + 1) end_col in
        aux fdrop start_col (end_col - 1)
  in
  aux IntSet.empty 0 (String.length @@ List.hd pattern) |> IntSet.to_list

let vert_mirror_line pattern =
  let lines = vert_mirror_lines pattern in
  if List.is_empty lines then
    None
  else
    Some (List.hd lines)

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

let pattern_without_smudge pattern row col =
  Array.mapi
    (fun lrow line -> 
      if lrow = row then
        String.mapi 
          (fun ci ch ->
            if ci = col then
              let new_ch = if ch = '#' then '.' else '#' in
              (*Printf.printf "%d,%d %c->%c\n" row col ch new_ch;*)
              new_ch
            else
              ch)
          line
      else
        line)
    pattern

let print_mirror_line line =
  let (s, d) = match line with
  | MirrorCol d -> ("col", d)
  | MirrorRow d -> ("row", d) 
  in
  Printf.printf "%s=%d" s d

let mirror_line_without_smudge pattern =
  let orig_line = get_mirror_line pattern in
  print_mirror_line orig_line;
  print_endline "";
  let pattern = Array.of_list pattern in
  let w = String.length @@ pattern.(0) in
  let h = Array.length pattern in
  try
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do
        (*Printf.printf "%d,%d\n" i j;*)
        let pattern = pattern_without_smudge pattern i j in
        let pattern = Array.to_list pattern in

        let vlines = vert_mirror_lines pattern in
        let hlines = horiz_mirror_lines pattern in

        begin
          match (orig_line, vlines) with
          | (MirrorCol orig_col, list) ->
            let found = List.find_opt (fun c -> c <> orig_col) list in
            begin 
                match found with
              | Some a -> raise_notrace (NewMirrorLine (MirrorCol a))
              | _ -> ();
            end
          | (MirrorRow _, a :: _) ->
              raise_notrace (NewMirrorLine (MirrorCol a))
          | _ -> ();
        end;

        begin
          match (orig_line, hlines) with
          | (MirrorRow orig_row, list) ->
            let found = List.find_opt (fun c -> c <> orig_row) list in
            begin 
                match found with
              | Some a -> raise_notrace (NewMirrorLine (MirrorRow a))
              | _ -> ();
            end
          | (MirrorCol _, a :: _) ->
              raise_notrace (NewMirrorLine (MirrorRow a))
          | _ -> ();
        end;
      done;
    done;
    print_endline "";
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