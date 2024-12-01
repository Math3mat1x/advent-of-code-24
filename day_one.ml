let parse_line (l : string) = (int_of_string @@ String.sub l 0 5, int_of_string @@ String.sub l 8 5)

let parse_file (filename : string) =
  let ic = open_in filename in
  let rec aux () =
    try
      let l = input_line ic in
      (parse_line l) :: aux ()
    with  End_of_file -> []
  in
  aux ()

let similarity_list (n1 : (int * int) list) (n2 : (int * int) list) : (int list) =
  (* input lists must be sorted *)
  let rec compute_occ (l : (int * int) list) (e : int) : (int * int) list * int =
    match l with
    | [] -> [],0
    | (_,h)::t when h < e -> compute_occ t e
    | (_,h)::t when h = e -> let l_next, occ = compute_occ t e in l_next, (occ+1)
    | (_,h)::t when h > e -> l,0
    | (_,h)::t -> assert false
  in
  let rec aux (l1 : (int * int) list) (l2 : (int * int) list) : int list =
    match l1 with
    | [] -> [0]
    | (h,_)::t -> let l2_next, occ = compute_occ l2 h in
    (h * occ)::(aux t l2_next)
  in
  aux n1 n2

let () =
  (* Part 1 *)
  let numbers = parse_file "day_one_input.txt" in
  let n1,n2 = List.sort (fun a b -> let e1,_=a in let e2,_=b in e1 - e2) numbers,
              List.sort (fun a b -> let _,e1=a in let _,e2=b in e1 - e2) numbers
  in
  let n = List.map2 (fun a b -> let e1,_=a in let _,e2=b in e1,e2) n1 n2 in
  let total_distance = List.fold_left (fun acc a -> let e1,e2 = a in acc + abs(e1 - e2)) 0 n in
  print_int total_distance;
  print_char '\n';
  (* Part 2 *)
  let similarity = List.fold_left (fun acc a -> acc + a) 0 (similarity_list n1 n2) in
  print_int similarity;
