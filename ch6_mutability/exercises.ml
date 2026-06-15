
(* My solutions to two starred, three starred, four starred problems *)

(* Mutable fields *)
type student = { name : string ; mutable value : float}
let alice = {name = "Alice"; value = 3.7}
let update_gpa x gpa = x.value <- gpa
let () = update_gpa alice 4.0

(* Inc fun *)
let inc = ref (fun x -> x + 1)
let dummy = !inc 3109

(* addition assignment. Introduce += using refs *)
let ( += ) x y =
  x := !x + !y; !x

(* norm *)
type vector = float array

let norm v =
     Array.map (fun x -> x *. x) v 
  |> Array.fold_left (+.) 0. 
  |> sqrt

let normalize v =
  let norm_v = norm v in
  if norm_v <> 0.0 then (
    let elt pos value = 
    v.(pos) <- value /. norm_v in
    Array.iteri elt v
  ) else failwith "Division by zero error"
  

(* INIT Matrix *)
let init_matrix dimx dimy f =
  Array.init dimx (fun x -> Array.init dimy (fun y -> f x y))