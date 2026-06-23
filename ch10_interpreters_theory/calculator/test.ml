open OUnit2
open Calc

let _ = Ast.Int 5


(**[make_i n i s] makes an Ounit test named [n] that expects [s]
   to evaluate to [i]. *)
let make_i n i s =
  n >:: (fun _ -> assert_equal (string_of_int i) (Main.interp s))

let tests = [
  make_i "int" 22 "22";
  make_i "add" 22 "11+11";
  make_i "add three" 22 "11+5+6";
  make_i "mult two" 22 "2x11";
  make_i "mult three" 22 "1x2x11";
  make_i "mult negatives" 22 "-1x-22";
  make_i "mult on right of add" 22 "2+2x10";
  make_i "mult on left of add" 22 "2x1+20";
  make_i "nested mult on right of add" 64 "4+2x(15x2)"
]

let _ = run_test_tt_main ("suite" >::: tests)

