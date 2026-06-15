open OUnit2
open Exercises

let make_helper eo ei f =
  fun _ -> assert_equal eo (f ei)

let tests = "test suite for chapter 6 exercises" >::: [
  "addition assignment" >:: make_helper 5 (ref 3) (fun x -> x += ref 2);
  "addition assignment" >:: make_helper 0 (ref 3) (fun x -> x += ref (-3));
  "empty vector norm" >:: make_helper 0.0 [||] norm;
  "only 1s vector norm" >:: make_helper 2.0 (Array.make 4 1.) norm
]

let _ = run_test_tt_main tests

