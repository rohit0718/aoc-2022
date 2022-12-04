open Core

let parse_assg assg =
  match String.split assg ~on:'-' with
  | [first; second] -> int_of_string first, int_of_string second
  | _ -> failwith "invalid assg"

let parse_line line =
  match String.split line ~on:',' with
  | [first; second] -> parse_assg first, parse_assg second
  | _ -> failwith "invalid line"

let input = In_channel.(input_lines stdin)
  |> List.map ~f:parse_line

let fully_contains ((l1, r1), (l2, r2)) =
  (l1 >= l2 && r1 <= r2) || (l2 >= l1 && r2 <= r1)

let has_overlap ((l1, r1), (l2, r2)) =
  (l1 >= l2 && l1 <= r2) || (l2 >= l1 && l2 <= r1)

let part1 lines =
  List.filter lines ~f:fully_contains |> List.length

let part2 lines =
  List.filter lines ~f:has_overlap |> List.length

let _ =
  Printf.printf "part 1: %d\n" (part1 input);
  Printf.printf "part 2: %d\n" (part2 input);

