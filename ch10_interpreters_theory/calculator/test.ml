open OUnit2
open Ast
open Main

(**[make_i n i s] makes an Ounit test named [n] that expects [s]
   to evaluate to [i]. *)
let make_i n i s =
  n >:: (fun _ -> assert_equal (string_of_int i) (interp s))

let tests = [
  make_i "int" 22 "22"
]

