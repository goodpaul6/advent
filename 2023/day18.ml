let lines =
  let ic = open_in "inputs/day18_ex.txt" in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with _ ->
      acc
  in
  loop [] |> List.rev

let () = List.iter print_endline lines
