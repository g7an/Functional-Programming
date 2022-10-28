(*
  Put the tests for lib.ml functions here
*)

open Core
open OUnit2
open Lib

let test_get_n_items _ =
  assert_equal [ 1; 2; 3 ] @@ get_n_items 3 [ 1; 2; 3; 4; 5 ];
  assert_equal [ 1 ] @@ get_n_items 1 [ 1; 2; 3 ];
  assert_equal [ 1; 2; 3 ] @@ get_n_items 4 [ 1; 2; 3 ]

let part1_tests = "Part 1" >: test_list [ "get_n_items" >:: test_get_n_items ]
let series = "Assignment1 Tests" >::: [ part1_tests ]
let () = run_test_tt_main series
