(*
  Put the tests for lib.ml functions here
*)

open Core
open OUnit2
open Lib
module R = Random
module NGrams = Lib.N_grams (R) (Int)

let bag = Bag.create ()
let _ = Bag.add bag 1
let empty_bag = Bag.create ()

let test_get_n_items _ =
  assert_equal [ 1; 2; 3 ] @@ get_n_items 3 [ 1; 2; 3; 4; 5 ];
  assert_equal [ 1 ] @@ get_n_items 1 [ 1; 2; 3 ];
  assert_equal [ 1; 2; 3 ] @@ get_n_items 4 [ 1; 2; 3 ]

let test_chunks _ =
  assert_equal [ [ 1; 2; 3 ]; [ 2; 3; 4 ]; [ 3; 4; 5 ] ]
  @@ chunks 3 [ 1; 2; 3; 4; 5 ];
  assert_equal [ [ 1 ]; [ 2 ]; [ 3 ] ] @@ chunks 1 [ 1; 2; 3 ];
  assert_equal [ [ "a"; "b" ]; [ "b"; "c" ]; [ "c"; "d" ]; [ "d"; "e" ] ]
  @@ chunks 2 [ "a"; "b"; "c"; "d"; "e" ];
  assert_equal [] @@ chunks 3 [ 1; 2 ];
  assert_raises (Failure "n should be positive") @@ fun () ->
  chunks 0 [ 1; 2; 3 ]

let test_split_last _ =
  assert_equal (3, [ 1; 2 ]) @@ split_last [ 1; 2; 3 ];
  assert_equal (3, [ 1; 2; 3; 4; 5 ]) @@ split_last [ 1; 2; 3; 4; 5; 3 ];
  assert_equal (1, []) @@ split_last [ 1 ];
  assert_raises (Failure "empty list") @@ (fun () -> split_last [ ])

let test_sample _ =
  assert_equal (Some 1) @@ sample (module Random) bag;
  assert_equal None @@ sample (module Random) empty_bag

let dist1 = NGrams.ngrams 2 [ 1; 2 ]
let dist2 = NGrams.ngrams 3 [ 1; 2; 3; 4; 5 ]
let dist3 = NGrams.ngrams 1 [ 1 ]

let dist4 = NGrams.ngrams 2 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1]

let test_sample_sequence _ =
  assert_equal [ 1; 2 ]
  @@ NGrams.sample_sequence dist1 ~max_length:2 ~initial_ngram:[ 1 ];
  assert_equal [ 1; 2; 3; 4 ]
  @@ NGrams.sample_sequence dist2 ~max_length:4 ~initial_ngram:[ 1; 2 ];
  assert_equal [ 1; 3 ]
  @@ NGrams.sample_sequence dist2 ~max_length:4 ~initial_ngram:[ 1; 3 ];
  assert_equal [ 1; 3; 2; 4 ]
  @@ NGrams.sample_sequence dist2 ~max_length:4 ~initial_ngram:[ 1; 3; 2; 4 ];
  assert_equal [ 1 ]
  @@ NGrams.sample_sequence dist3 ~max_length:1 ~initial_ngram:[ ];
  assert_equal [ 2 ] @@ NGrams.sample_sequence dist4 ~max_length:1 ~initial_ngram:[ 2 ]


let test_sanitize _ =
  assert_equal (Some "hello") @@ sanitize "hello";
  assert_equal (Some "hello") @@ sanitize "hello!";
  assert_equal (Some "hello123") @@ sanitize "HeLlo123";
  assert_equal (Some "hello123") @@ sanitize "hello!123";
  assert_equal None @@ sanitize "!!!"

let part1_tests =
  "Part 1"
  >: test_list
       [
         "get_n_items" >:: test_get_n_items;
         "chunks" >:: test_chunks;
         "split_last" >:: test_split_last;
         "sample" >:: test_sample;
         "sample_sequence" >:: test_sample_sequence;
         "sanitize" >:: test_sanitize;
       ]

let series = "Assignment1 Tests" >::: [ part1_tests ]
let () = run_test_tt_main series
