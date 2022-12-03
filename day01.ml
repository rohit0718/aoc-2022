open Core

let input = In_channel.(input_lines stdin)

let rec sum list acc =
  match list with
  | [] -> acc
  | x::l -> sum l acc + x

let rec part1 (lines: string list) (cur_list: int list) (max_num: int) =
  match lines with
  | [] -> max_num
  | ""::tl ->
      let list_sum = sum cur_list 0 in
      let new_max_num = begin match list_sum with
      | x when x > max_num -> x
      | _ -> max_num
      end in
      part1 tl [] new_max_num
  | h::tl -> part1 tl (int_of_string h::cur_list) max_num

let part2 lines =
  let rec inner lines cur_list calories =
    match lines with
    | [] -> calories
    | ""::tl -> inner tl [] (sum cur_list 0::calories)
    | h::tl -> inner tl (int_of_string h::cur_list) calories
  in
  let calories_unsorted = inner lines [] [] in
  let calories_sorted = Core.List.sort
    calories_unsorted
    ~compare:(fun x y -> Int.compare y x)
  in
  sum (Core.List.slice calories_sorted 0 3) 0

let _ = 
  Printf.printf "part 1: %d\n" (part1 input [] 0);
  Printf.printf "part 2: %d\n" (part2 input);

