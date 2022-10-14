(*
  Part II: Tests
 
  In this part, you will need to create and run your own tests.  Tests should
  cover both common cases and edge cases.  In previous assignments, we only
  asked for a specified number of additional tests, but in this assignment we
  will be grading based on code coverage.
 
  Aim for complete code coverage on all functions, and we will check 
  by running the bisect tool on your code.  For that reason, you need 
  to add the following line in the dune file for your library:
      
      (preprocess (pps bisect_ppx))
 
  or else your tests will not run in the autograder.

 Additionally, you will need to write a special suite of tests here which
 verifies some invariants.  See the assignment for details.
 
*)

open Core;;
open OUnit2;;
open Abstraction;;

(*
  This is a helper function for testing.  It takes a list of key-value pairs
  and returns a map with those key-value pairs inserted.  It is useful for
  creating test maps.
*)
let float_map = Float_cont_map.(empty |> insert 1. 2. |> insert 2. 3.);;

let test_lookup _ =
  assert_equal (Float_cont_map.lookup 1. float_map) (Some 2.);
  assert_equal (Float_cont_map.lookup 2. float_map) (Some 3.);
  assert_equal (Float_cont_map.lookup 3. float_map) None;;

let test_insert _ =
  assert_equal (Float_cont_map.insert 3. 4. float_map) (Float_cont_map.(empty |> insert 1. 2. |> insert 2. 3. |> insert 3. 4.));
  assert_equal (Float_cont_map.insert 1. 4. float_map) (Float_cont_map.(empty |> insert 1. 4. |> insert 2. 3.));;
  assert_equal (Float_cont_map.insert 2. 4. float_map) (Float_cont_map.(empty |> insert 1. 2. |> insert 2. 4.));;

let test_remove _ =
  assert_equal (Float_cont_map.(empty |> insert 2. 3.), Some 2.) @@ (Float_cont_map.remove 1. float_map);
  assert_equal (Float_cont_map.(empty |> insert 1. 2.), Some 3.) @@ (Float_cont_map.remove 2. float_map);
  assert_equal (float_map, None) @@ (Float_cont_map.remove 3. float_map) ;;

let test_lub_key _ =
  assert_equal (Some 1.) @@ (Float_cont_map.lub_key 1. float_map);
  assert_equal (Some 2.) @@ (Float_cont_map.lub_key 2. float_map);
  assert_equal None @@ (Float_cont_map.lub_key 3. float_map);
  assert_equal (Some 1.) @@ (Float_cont_map.lub_key 0. float_map);
  assert_equal (Some 1.) @@ (Float_cont_map.lub_key 0.5 (Float_cont_map.(empty |> insert 2. 4. |> insert 1. 3.)))
;;

let test_glb_key _ =
  assert_equal (Some 1.) @@ (Float_cont_map.glb_key 1. float_map);
  assert_equal (None) @@ (Float_cont_map.glb_key 0.5 float_map);
  assert_equal (Some 2.) @@ (Float_cont_map.glb_key 2.5 float_map);
  assert_equal (Some 2.) @@ (Float_cont_map.glb_key 3. float_map);;

let test_interpolated_lookup _ = 
  assert_equal (Some 2.5) @@ (Float_cont_map.interpolated_lookup 1.5 float_map);
  assert_equal None @@ (Float_cont_map.interpolated_lookup 2.5 float_map);
  assert_equal None @@ (Float_cont_map.interpolated_lookup 3.5 float_map);
  assert_equal None @@ (Float_cont_map.interpolated_lookup 0.5 float_map);
  assert_equal (Some 2.) @@ (Float_cont_map.interpolated_lookup 1. float_map);;

let part1_tests =
  "Part 1"
  >: test_list
        [
          "Lookup "
          >:: test_lookup;
          "Insert "
          >:: test_insert;
          "Remove " >:: test_remove;
          "Least upper bound key " >:: test_lub_key;
          "Greatest lowest bound key " >:: test_glb_key;
          "Interpolated lookup " >:: test_interpolated_lookup;
        ]

let test_is_digit _ = 
  assert_equal true @@ (is_digit '0');
  assert_equal true @@ (is_digit '4');
  assert_equal true @@ (is_digit '9');
  assert_equal false @@ (is_digit 'a');;

let test_from_string _ = 
  assert_equal (0) @@ (Int_Data.from_string "0");
  assert_equal (9) @@ (Int_Data.from_string "9");;

let test_to_string _ = 
  assert_equal ("0") @@ (Int_Data.to_string 0);
  assert_equal ("9") @@ (Int_Data.to_string 9);;

let test_next _ = 
  assert_equal (Some ("", 0)) @@ (Int_Data.next "0");
  assert_equal (Some ("2 +", 1)) @@ (Int_Data.next "1 2 +");
  assert_equal (Some ("2 +", 1)) @@ (Int_Data.next "1\t2 +");
  assert_equal (Some ("2 +", 1)) @@ (Int_Data.next "1\n2 +");
  assert_equal (Some ("+", 2)) @@ (Int_Data.next " 2 +");
  assert_equal (Some ("+", 2)) @@ (Int_Data.next "2+");
  assert_equal (Some ("", -2)) @@ (Int_Data.next "-2");
  assert_equal None @@ (Int_Data.next "-");
  assert_equal None @@ (Int_Data.next "");;
  
let test_plus _ = 
  assert_equal (1) @@ (Int_Data.plus (-1) 2);
  assert_equal 3 @@ (Int_Data.plus 1 2);;

let test_times _ = 
  assert_equal (-2) @@ (Int_Data.times (-1) 2);
  assert_equal 2 @@ (Int_Data.times 1 2);;

let test_eval _ = 
  assert_equal (Ok 3) @@ (Int_Eval.eval "1 2 +");
  assert_equal (Error "illegal character") @@ (Int_Eval.eval "2@");
  assert_equal (Error "illegal character") @@ (Int_Eval.eval "2@ 3");
  assert_equal (Ok 19) @@ (Int_Eval.eval "4 5 2 * + 5 +");
  assert_equal (Ok 11) @@ (Int_Eval.eval "1 5 2 * +");
  assert_equal (Ok 1) @@ (Int_Eval.eval "1");
  assert_equal (Error "unmatched +") @@ (Int_Eval.eval "1 2++");
  assert_equal (Error "unmatched") @@ (Int_Eval.eval "1 2");
  assert_equal (Error "unmatched +") @@ (Int_Eval.eval "1+");
  assert_equal (Error "unmatched *") @@ (Int_Eval.eval "1 *");
  assert_equal (Ok (-2)) @@ (Int_Eval.eval "-1 2 *");
  assert_equal (Ok (-2)) @@ (Int_Eval.eval "1 -2 *");
  assert_equal (Ok 0) @@ (Int_Eval.eval "0 -1 *");
  assert_equal (Ok 0) @@ (Int_Eval.eval "-1 1 +");
  assert_equal (Error "illegal character") @@ (Int_Eval.eval "1-")
;;

let test_rat_from_string _ = 
  assert_equal (1, 2) @@ (Rat_Data.from_string "1/2");
  assert_equal (0, 1) @@ (Rat_Data.from_string "0/2");
  assert_equal (-1, 2) @@ (Rat_Data.from_string "-1/2");
  assert_equal (1, 2) @@ (Rat_Data.from_string "2/4");
  assert_raises (Failure "denominator cannot be 0") @@ (fun () -> Rat_Data.from_string "1/0");
  assert_raises (Failure "invalid input") @@ (fun () -> Rat_Data.from_string "1/");
  assert_raises (Failure "invalid input") @@ (fun () -> Rat_Data.from_string "/1");
  assert_raises (Failure "invalid input") @@ (fun () -> Rat_Data.from_string "");;

let test_rat_to_string _ =
  assert_equal "1/2" @@ (Rat_Data.to_string (1, 2));
  assert_equal "0/2" @@ (Rat_Data.to_string (0, 2));
  assert_equal "-1/2" @@ (Rat_Data.to_string (-1, 2));
  assert_equal "2/4" @@ (Rat_Data.to_string (2, 4));;

let test_rat_next _ = 
  assert_equal (Some ("", (1, 2))) @@ (Rat_Data.next "1/2");
  assert_equal (Some ("3/4 +", (1, 2))) @@ (Rat_Data.next "1/2 3/4 +");
  assert_equal (Some ("3/4 +", (1, 2))) @@ (Rat_Data.next "1/2\t3/4 +");
  assert_equal (Some ("3/4 +", (1, 2))) @@ (Rat_Data.next "1/2\n3/4 +");;
  assert_equal (None) @@ (Rat_Data.next "2/");
  assert_equal (None) @@ (Rat_Data.next "/1");
  assert_equal (Some ("", (-1, 1))) @@ (Rat_Data.next "-1/1");;
  assert_equal (Some ("+", (-1, 1))) @@ (Rat_Data.next "-1/1+");
  assert_equal (Some ("-", (1, 1))) @@ (Rat_Data.next "1/1-");;
  assert_equal (None) @@ (Rat_Data.next "");
  assert_equal (None) @@ (Rat_Data.next "-");
  assert_equal (None) @@ (Rat_Data.next "1@");;

let test_rat_plus _ =
  assert_equal (1, 2) @@ (Rat_Data.plus (1, 2) (0, 1));
  assert_equal (1, 1) @@ (Rat_Data.plus (1, 2) (1, 2));
  assert_equal (1, 2) @@ (Rat_Data.plus (1, 4) (1, 4));
  assert_equal (0, 1) @@ (Rat_Data.plus (-1, 2) (1, 2));
  assert_equal (0, 1) @@ (Rat_Data.plus (1, 2) (-1, 2))
;;

let test_rat_times _ =
  assert_equal (1, 4) @@ (Rat_Data.times (1, 2) (1, 2));
  assert_equal (1, 4) @@ (Rat_Data.times (1, 2) (2, 4));
  assert_equal (0, 1) @@ (Rat_Data.times (1, 2) (0, 1));
  assert_equal (0, 1) @@ (Rat_Data.times (0, 1) (1, 2));
  assert_equal (-1, 4) @@ (Rat_Data.times (-1, 2) (1, 2));
  assert_equal (-1, 4) @@ (Rat_Data.times (1, 2) (-1, 2));;

(* invariant test *)
(* int_data.next will always return an integer on integers as string *)
let plus_test x y = ("test plus" >:: (fun _ -> assert_equal( Int_Data.plus x y ) (x+y) ));; 
let times_test x y = ("test times" >:: (fun _ -> assert_equal( Int_Data.times x y ) (x*y) ));;
let convert_test x = ("test to_string and from_string" >:: (fun _ -> assert_equal( Int_Data.to_string (Int_Data.from_string x) ) x ));;
let rat_convert_test x = ("test to_string and from_string" >:: (fun _ -> assert_equal( Rat_Data.to_string (Rat_Data.from_string x) ) x ));;

(* test next will always return None *)
let rec rec_next str = 
  match Abstraction.Int_Data.next str with
  | None -> []
  | Some (str, _) ->  (rec_next str)
let next_test str = ("test next" >:: (fun _ -> assert_equal( rec_next str ) [] ));;


let test_invariant = "Invariant Checking" >::: 
  [
    "Int_Data.plus x y = x + y" >::: List.map [(1, 2); (3, 4); (0, 5); (-6, 7); (8, 9)] ~f:(fun l -> match l with (x, y) -> plus_test x y);
    "Int_Data.times x y = x * y" >::: List.map [(1, 2); (3, 4); (0, 5); (-6, 7); (8, 9)] ~f:(fun l -> match l with (x, y) -> times_test x y);
    "Int_Data.to_string (Int_Data.from_string x) = x" >::: List.map ["12"; "234"; "30"; "-45"; "57890"; "6"; "-97"; "89"; "9"] ~f:convert_test;
    "Rat_Data.to_string (Rat_Data.from_string x) = x" >::: List.map ["1/2"; "3/4"; "6/7"; "8/9"; "1/1"; "-1/2"] ~f:rat_convert_test;
    "Int_Data.next str will always return an integer on integers as string" >::: List.map ["12 +"; "1 2+"; "1@"; "1"] ~f:next_test;
  ];;
  



let part2_tests = "Part 2" >::: [
  "Aux function 1 - check char is digit" >:: test_is_digit;
  "Int_Data function - convert string to int" >:: test_from_string;
  "Int_Data function - convert int to string" >:: test_to_string;
  "Int_Data function - next" >:: test_next;
  "Int_Data function - plus" >:: test_plus;
  "Int_Data function - times" >:: test_times;
  "Int_Eval function - eval" >:: test_eval;
  "Rat_Data function - convert string to rational" >:: test_rat_from_string;
  "Rat_Data function - convert rational to string" >:: test_rat_to_string;
  "Rat_Data function - next" >:: test_rat_next;
  "Rat_Data function - plus" >:: test_rat_plus;
  "Rat_Data function - times" >:: test_rat_times;
]

(* let invariant_checking = "nvariant Checking" >::: [
  "test plus" >:: test_plus_list
] *)
let series = "Assignment3 Tests" >::: 
[ 
  part1_tests
  ;part2_tests 
  ; test_invariant
]
let () = run_test_tt_main series