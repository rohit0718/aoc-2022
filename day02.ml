open Core

let input = In_channel.(input_lines stdin)

let get_score opp my =
  match (opp, my) with
  | ("A", "X") -> 4
  | ("A", "Y") -> 8
  | ("A", "Z") -> 3
  | ("B", "X") -> 1
  | ("B", "Y") -> 5
  | ("B", "Z") -> 9
  | ("C", "X") -> 7
  | ("C", "Y") -> 2
  | ("C", "Z") -> 6
  | _ -> failwith "invalid input"

let part1 lines =
  List.fold lines ~init:0 ~f:(fun acc line ->
    match String.split line ~on:' ' with
    | [opp; my] -> acc + get_score opp my
    | _ -> failwith "invalid input"
  )

let get_score_part2 opp my =
  match (opp, my) with
  | ("A", "X") -> 3
  | ("A", "Y") -> 4
  | ("A", "Z") -> 8
  | ("B", "X") -> 1
  | ("B", "Y") -> 5
  | ("B", "Z") -> 9
  | ("C", "X") -> 2
  | ("C", "Y") -> 6
  | ("C", "Z") -> 7
  | _ -> failwith "invalid input"

let part2 lines =
  List.fold lines ~init:0 ~f:(fun acc line ->
    match String.split line ~on:' ' with
    | [opp; my] -> acc + get_score_part2 opp my
    | _ -> failwith "invalid input"
  )

let _ = 
  Printf.printf "part 1: %d\n" (part1 input);
  Printf.printf "part 2: %d\n" (part2 input);

