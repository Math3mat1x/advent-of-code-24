let parse_line (l : string) = List.map (fun a -> int_of_string a) (String.split_on_char ' ' l)

let parse_file (filename : string) =
  let ic = open_in filename in
  let rec aux () =
    try
      let l = input_line ic in
      (parse_line l) :: aux ()
    with  End_of_file -> []
  in
  aux ()

let nxor a b = (a && b) || (not a && not b)

let is_safe (l : int list) : bool =
  (* Complexity : O(n) *)
  let rec aux (l : int list) (incr : bool) : bool =
  match l with
  | [] -> true
  | [a] -> true
  | a::b::t when nxor (a > b) incr ->
      let dist = abs(a-b) in
      if (dist >= 1) && (dist <= 3) then (aux (b::t) incr)
      else false
  | a::b::t -> false
  in
  match l with
  | a::b::t -> aux l (a > b)
  | _ -> assert false

let is_safe_tolerant (l : int list) : bool =
  (* Dirty solution in O(n**3) *)
  let rec aux (h : int list) (t : int list) : bool =
    match t with
    | [] -> is_safe h
    | a::q -> is_safe (h @ q) || aux (h @ [a]) q
  in
  aux [] l

let () =
  (* Part 1 *)
  let filename = "day_two_input.txt" in
  let answer1 = List.fold_left (fun acc l -> if (is_safe l) then acc + 1 else acc) 0 (parse_file filename) in
  print_int answer1;
  print_char '\n';
  (* Part 2 *)
  let answer2 = List.fold_left (fun acc l -> if (is_safe_tolerant l) then acc + 1 else acc) 0 (parse_file filename) in
  print_int answer2;
