#load "str.cma"

let parse_mul (s : string) : int =
  let pos_comma = String.index_from s 0 ',' in
  let pos_paren = String.index_from s pos_comma ')' in
  let n1, n2 = String.sub s 4 (pos_comma - 4), String.sub s (pos_comma + 1) (pos_paren-pos_comma-1) in
  (int_of_string n1) * (int_of_string n2)

let parse_file (filename : string) : string =
  let ic = open_in filename in
  let rec aux () =
    try
      let l = input_line ic in
      l :: aux ()
    with End_of_file -> []
  in
  String.concat "" (aux ())

let part_one (s : string) : int =
  let expr = Str.regexp "mul(\\([0-9]+,[0-9]+\\))" in
  let res = ref 0 in
  let pos = ref 0 in
  try
    while true do
      let next_pos = Str.search_forward expr s !pos in
      pos := next_pos + 1;
      res := !res + parse_mul (Str.matched_string s);
    done;
    !res
  with Not_found -> !res

let part_two (s : string) : int =
  (* mul instructions are enabled in the beginning *)
  let s = String.concat "" ["do()";s] in
  let expr_start = Str.regexp "do()" in
  let expr_avoid = Str.regexp "(do\\(\\))|(don\\'t\\(\\))" in
  let expr_mul   = Str.regexp "mul(\\([0-9]+,[0-9]+\\))" in
  let pos = ref 0 in
  let res = ref 0 in
  try
    while true do
      (* Find the next "do()" *)
      let next_pos = Str.search_forward expr_start s !pos in
      pos := next_pos + 4; (* Move past "do()" *)

      (* Check if "don()" follows; skip it if found *)
      if
        try
          let tmp =  Str.search_forward expr_avoid s !pos in
          pos := !pos + tmp;
          true
        with Not_found -> false
      then ()
      else begin
        (* Otherwise, look for "mul(x, y)" *)
        let tmp = Str.search_forward expr_mul s !pos in
        let current_string = Str.matched_string s in
        res := !res + (parse_mul current_string);
        pos := tmp + (String.length current_string);
      end
    done;
    !res (* never returned *)
  with Not_found -> !res

let () = 
  (* Part 1 *)
  let input = parse_file "day_three_input.txt" in
  let solution1 = part_one input in
  print_int solution1;
  print_char '\n';
  (* doesn’t work *)
  let solution2 = part_two input in
  print_int solution2;
