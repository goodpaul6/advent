let digit_regex = Str.regexp "[0-9]\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine"

let normalize_digit s =
    match s with
    | "one" -> "1"
    | "two" -> "2"
    | "three" -> "3"
    | "four" -> "4"
    | "five" -> "5"
    | "six" -> "6"
    | "seven" -> "7"
    | "eight" -> "8"
    | "nine" -> "9"
    | _ -> s 

let first_digit_str line =
    let _ = Str.search_forward digit_regex line 0 in
    normalize_digit @@ Str.matched_string line

let last_digit_str line =
    let len = String.length line in
    let _ = Str.search_backward digit_regex line (len - 1) in
    normalize_digit @@ Str.matched_string line

let () =
    let file = open_in "input.txt" in
    let rec read_line sum_so_far =
        try 
            let line = input_line file in
            let first = first_digit_str line in
            let last = last_digit_str line in
            let str = Printf.sprintf "%s%s" first last in
            read_line @@ sum_so_far + int_of_string str
        with _ ->
            sum_so_far
    in
    let sum = read_line 0 in
    print_endline @@ string_of_int sum;
    close_in_noerr file