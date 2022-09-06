(* open Core *)
open OUnit2
open Submission

[@@@ocaml.warning "-32"]

(* This file contains a few tests but not necessarily complete coverage.  You are
   encouraged to think of more tests for the corner cases.
   We will cover the details of the test syntax later, but with simple copy/paste it should
   not be hard to add new tests of your own without knowing the details.
   1) Write a new let which performs the test, e.g. let test_fibonacci_2 _ = ...
   2) Add that let-named entity to one of the test suite lists such as section1_tests
      by adding e.g.
       "Fibonacci 2"       >:: test_fibonacci_2;
   Thats it!

   Recall that you need to type "dune test" to your shell to run the test suite.
*)

let test_summate _ =
  assert_equal (summate 3) 6;
  assert_equal (summate 10) 55 (* note a semicolon is illegal here - OCaml oddity *)

let test_lcm _ =
  assert_equal (lcm 3 9) 9;
  assert_equal (lcm 9 12) 36

let test_fibonacci _ =
  assert_equal (fibonacci 0) 0;
  assert_equal (fibonacci 10) 55

let part1_section1_tests =
  "Part 1 Section 1"
  >: test_list
       [
         "Summate" >:: test_summate;
         "LCM" >:: test_lcm;
         "Fibonacci" >:: test_fibonacci;
       ]

let test_iota1 _ = assert_equal (iota1 5) [ 5; 4; 3; 2; 1 ]
let test_iota2 _ = assert_equal (iota2 6) [ 1; 2; 3; 4; 5; 6 ]

let test_factors _ =
  assert_equal (factors 10) [ 1; 2; 5; 10 ];
  assert_equal (factors 12) [ 1; 2; 3; 4; 6; 12 ]

let test_insert_string _ =
  assert_equal (insert_string "FPSE" [ "I"; "Love" ]) [ "FPSE"; "I"; "Love" ];
  assert_equal
    (insert_string "baa" [ "0"; "111"; "abb"; "abc"; "bcd" ])
    [ "0"; "111"; "abb"; "abc"; "baa"; "bcd" ]

let test_insert_string_exn _ =
  let f _ = insert_string_exn "Oops" [ "Not"; "In"; "Order"; "!" ] in
  assert_raises (Invalid_argument "List not sorted") f

let test_insertion_sort _ =
  assert_equal
    (insertion_sort [ "bus"; "cat"; "apple"; "green"; "email" ])
    [ "apple"; "bus"; "cat"; "email"; "green" ]

let test_remove_max _ =
  assert_equal (remove_max [ "hi"; "remove me!"; "don't remove me" ])
  @@ Ok ("remove me!", [ "hi"; "don't remove me" ])

let test_max_sort _ =
  assert_equal
    (max_sort [ "bus"; "cat"; "apple"; "green"; "email" ])
    [ "apple"; "bus"; "cat"; "email"; "green" ]

let part1_section2_tests =
  "Part 1 Section 2"
  >: test_list
       [
         "Iota1" >:: test_iota1;
         "Iota2" >:: test_iota2;
         "Factors" >:: test_factors;
         "Insert String" >:: test_insert_string;
         "Insert String Exn" >:: test_insert_string_exn;
         "Insertion Sort" >:: test_insertion_sort;
         "Remove Max" >:: test_remove_max;
         "Max Sort" >:: test_max_sort;
       ]

(* Part II Section 1: we leave this section to you *)
let test_iota1' _ = 
  assert_equal (iota1' 3) [ 3; 2; 1 ];
  assert_equal (iota1' 0) [ ]

let test_iota2' _ = 
  assert_equal (iota2' 3) [ 1; 2; 3 ];
  assert_equal (iota2' 0) [ ]

let test_factors' _ = 
  assert_equal (factors' 9) [ 1; 3; 9 ];
  assert_equal (factors' 1) [ 1 ]

let test_insert_string' _ = 
assert_equal (insert_string "FPSE" [ "I"; "Love" ]) [ "FPSE"; "I"; "Love" ];
assert_equal
  (insert_string "baa" [ "0"; "111"; "abb"; "abc"; "bcd" ])
  [ "0"; "111"; "abb"; "abc"; "baa"; "bcd" ]



let part2_section1_tests = "Part 2 Section 1" 
  >: test_list 
        [
          "Iota1'" >:: test_iota1';
          "Iota2'" >:: test_iota2';
          "Factors'" >:: test_factors;
          "Insert String'" >:: test_insert_string';
        ]

(* Part II Section 2 *)
let tower_board_example_2x2 = [ [ 3; 2 ]; 
                                [ 2; 1 ] ]
let tower_board_ill_formed_example_2x2 = [ [ 1; 2 ]; 
                                           [ 2; 2 ] ]
let tower_board_not_square_example = [ [ 1; 2; 3 ]; 
                                       [ 2; 1 ] ]
let tower_board_example_3x3 = [ [ 1; 2; 3 ]; 
                                [ 3; 1; 2 ]; 
                                [ 2; 3; 1 ] ]
let tower_board_example_transposed_3x3 = [ [ 1; 3; 2 ];
                                           [ 2; 1; 3 ];
                                           [ 3; 2; 1 ] ]
let tower_board_example_reflected_3x3 = [ [ 3; 2; 1 ];
                                          [ 2; 1; 3 ];
                                          [ 1; 3; 2 ] ]
let tower_board_ill_formed_3x3 = [ [ 1; 2; 3];
                                   [ 3; 2; 1];
                                   [ 2; 3; 1] ]

let test_square_size _ = 
  assert_equal (square_size tower_board_example_2x2) (Ok 2);
  assert_equal (square_size tower_board_example_3x3) (Ok 3);
  assert_equal (square_size tower_board_not_square_example) (Error "not square")
  
let test_elements_span_range _ = 
  assert_equal (elements_span_range [3;2;1]) true;
  assert_equal (elements_span_range [3;2;2]) false;
  assert_equal (elements_span_range [1;7;3;4;5;6;7]) false
 
let test_well_formed_grid _ =
  assert_equal (well_formed_grid tower_board_example_2x2) true;
  assert_equal (well_formed_grid tower_board_ill_formed_example_2x2) false;
  assert_equal (well_formed_grid tower_board_ill_formed_3x3) false
  
let test_local_max_count _ =
  assert_equal (local_max_count [1;2;3]) 3;
  assert_equal (local_max_count [3;2;1]) 1;
  assert_equal (local_max_count [1;3;2;5;4]) 3
 
let test_verify_left_clues _ =
  assert_equal (verify_left_clues tower_board_example_2x2 [ 2; 1 ]) true;
  assert_equal (verify_left_clues tower_board_example_3x3 [ 3; 1; 2]) true;
  assert_equal (verify_left_clues tower_board_example_3x3 [ 2; 1; 2]) false

let test_transpose _ =
  assert_equal (transpose tower_board_example_2x2) tower_board_example_2x2;
  assert_equal (transpose tower_board_example_3x3) tower_board_example_transposed_3x3
 
let test_reflect_vertical_axis _ =
  assert_equal (reflect_vertical_axis tower_board_example_2x2)
    [[2; 1]; 
     [1; 2]];
     
  assert_equal (reflect_vertical_axis tower_board_example_3x3) tower_board_example_reflected_3x3

(* A useful invariant: four rotations is a no-op *)
let test_rotate_ccw _ =
  assert_equal
    (tower_board_example_2x2 |> rotate_ccw |> rotate_ccw |> rotate_ccw |> rotate_ccw)
    tower_board_example_2x2;
  assert_equal (rotate_ccw tower_board_example_3x3) [ [ 3; 2; 1 ]; [ 2; 1; 3 ]; [ 1; 3; 2 ] ]

let valid_counts_example_2x2 = [ [ 2; 1 ]; [ 1; 2 ]; [ 2; 1 ]; [ 1; 2 ] ]

(* Here is the "picture" of these counts on the 2x2 example above:

    2  1  
2   1  2  1 
1   2  1  2
    1  2

    - read succesive lists of counts by rotating the whole thing ccw 90.
*)   
let valid_counts_example_3x3 = [ [ 3; 1; 2 ]; [ 1; 2; 2 ]; [ 2; 2; 1 ]; [ 2; 1; 3 ] ]
let invalid_counts_example_2x2 = [ [ 1; 2 ]; [ 2; 1 ]; [ 1; 2 ]; [ 2; 1 ] ]

let test_verify_towers_solution _ =
  assert_equal
    (verify_towers_solution tower_board_example_2x2 valid_counts_example_2x2)
    true;
  assert_equal
    (verify_towers_solution tower_board_example_3x3 valid_counts_example_3x3)
    true;
  assert_equal
    (verify_towers_solution tower_board_example_2x2 invalid_counts_example_2x2)
    false

let part2_section2_tests =
  "Part 2 Section 2"
  >: test_list
       [
         "Square size" >:: test_square_size;
         "Elements_span_range" >:: test_elements_span_range;
         "Well_formed_grid" >:: test_well_formed_grid;
         "Local max count" >:: test_local_max_count;
         "Verify left clues" >:: test_verify_left_clues;
         "Rotate" >:: test_rotate_ccw;
         "Verify towers solution" >:: test_verify_towers_solution;
       ]

let series =
  "Assignment1 Tests"
  >::: [
         part1_section1_tests;
         part1_section2_tests;
         (* Keep these part 2 tests commented out while working on and submitting part 1. 
            Uncomment part 2 tests when working on and submitting part 2. *)
         (* part2_section1_tests;
         part2_section2_tests; *)
       ]

(* The following line runs all the tests put together into `series` above *)

let () = run_test_tt_main series
