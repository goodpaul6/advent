let rec pairs = function
    | [] -> []
    | _ :: [] -> []
    | a :: ((b :: _) as rest)  -> (a,b) :: pairs rest

let rec diffs = function
    | [] -> []
    | (a, b) :: rest -> (b - a) :: diffs rest

let pairs_diffs list =
    pairs list |> diffs

let all_zero =
    List.for_all (fun x -> x = 0)

(*let last list = List.nth list (List.length list - 1)

let rec next_value list =
    if all_zero list then
        0
    else
        let diffs = pairs_diffs list in
        (last list) + next_value diffs*)

let rec prev_value list =
    if all_zero list then
        0
    else
        let diffs = pairs_diffs list in
        (List.hd list) - prev_value diffs

let parse_int_list line =
    String.split_on_char ' ' line |>
    List.map (fun x -> int_of_string x)

let () =
    let ic = open_in "input.txt" in
    let rec loop sum =
        try
            let line = input_line ic in
            let list = parse_int_list line in
            loop @@ sum + prev_value list
        with _ ->
            sum
    in
    print_endline @@ string_of_int @@ loop 0
