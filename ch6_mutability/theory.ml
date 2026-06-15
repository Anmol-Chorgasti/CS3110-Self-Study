
(* ARRAYS *)

let a = [|1;2;3|]
(* arrays are mutable and fixed length *)

let b = a.(0) + a.(1)
(* arrays can be indexed using .(n) syntax and throw out of bound errors when necessary *)
let () = a.(0) <- 6

type vector = float array

let print_vector xs =
  for i = 0 to (Array.length xs)-1 do
    print_float xs.(i); print_newline ()
  done 

let print_vec xs =
  let print_elt n = print_float n; print_newline () in
  Array.iter print_elt xs
(* arrays, like in FP, has higher order functions. Iter takes in a function and applies it to each array element *)

let print_v xs = Array.iter (Printf.printf "%F, ") xs

let test_print_vec1 = [|1.0; 2.5; 3.5|]

let vec_add v1 v2 = 
  Array.map2 (+.) v1 v2
(** [vec_add v1 v2] is the vector sum of [v1] and [v2]. 
    Example: [vec_add [|1.0; 2.0|] [|3.0; 4.0|]] is [[|4.0; 6.0|]].
    Requires: [v1] and [v2] have the same length. *)
