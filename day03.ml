open Core

let input = In_channel.(input_lines stdin)

let find_same_letter first second =
  let same_letter_opt =
    List.find first ~f:(fun c1 -> List.exists
      second
      ~f:(fun c2 -> Char.(c1 = c2))
    )
  in
  match same_letter_opt with
  | Some letter -> letter
  | None -> failwith "no same letters found"

let get_priority letter =
  match letter with
  | l when Char.('a' <= l && l <= 'z') -> Char.(to_int l - to_int 'a') + 1
  | l when Char.('A' <= l && l <= 'Z') -> Char.(to_int l - to_int 'A') + 27
  | _ -> failwith "non alpha letter found"

let part1 comps =
  List.fold comps ~init:0 ~f:(fun acc comp ->
    let comp_sz = String.length comp in
    let first, second = List.split_n (String.to_list comp) (comp_sz / 2) in
    let same_letter = find_same_letter first second in
    acc + get_priority same_letter
  )

let find_same_letter_part2 comps =
  let first = List.hd_exn comps in
  let first_letters = String.to_list first in
  let same_letters = List.fold comps ~init:first_letters ~f:(fun acc comp ->
    let comp_letters = String.to_list comp in
    List.filter acc ~f:(fun c1 ->
      List.exists comp_letters ~f:(fun c2 -> Char.(c1 = c2))
    )
  ) in
  List.hd_exn same_letters

let part2 comps =
  let comps_threes = List.chunks_of comps ~length:3 in
  List.fold comps_threes ~init:0 ~f:(fun acc comps ->
    let same_letter = find_same_letter_part2 comps in
    acc + get_priority same_letter
  )

let _ = 
  Printf.printf "part 1: %d\n" (part1 input);
  Printf.printf "part 2: %d\n" (part2 input);

