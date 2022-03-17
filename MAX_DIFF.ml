(* N >= S -> S is the answer, N < S -> S - N = T1, N = T2 *)
#load "str.cma";;

let solve [n; s] =
  if n >= s then s
  else abs (s - 2*n)

let parse s = 
  let rec to_int_list = function
    | [] -> []
    | h::t -> (int_of_string h) :: (to_int_list t) in
  let parse_pair s = to_int_list (Str.split (Str.regexp " ") s) in
parse_pair s

let read_lines () =
  let rec loop acc =
      try
        loop ((parse (read_line ())) :: acc)
      with End_of_file -> acc
  in
  List.rev (loop [])


let rec solutions = function
  | [] -> []
  | h::t -> solve h :: solutions t;;

read_int ();;

List.iter (Printf.printf "%d\n") (solutions (read_lines ()));;