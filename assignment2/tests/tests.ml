open Core
open OUnit2
module D = Simpledict

let d1 =
  D.(
    Branch
      ("d", 1.0, Branch ("a", 0.0, Leaf, Leaf), Branch ("e", 2.0, Leaf, Leaf)))

let test_size _ =
  assert_equal 0 @@ D.size D.Leaf;
  assert_equal 3 @@ D.size d1

let test_assoc_of_dict _ =
  assert_equal [] @@ D.assoc_of_dict D.Leaf;
  assert_equal [ ("a", 0.0); ("d", 1.0); ("e", 2.0) ] @@ D.assoc_of_dict d1

let test_is_ordered _ =
  assert_equal true @@ D.is_ordered D.Leaf;
  assert_equal true @@ D.is_ordered d1;
  assert_equal false
  @@ D.is_ordered D.(Branch ("a", 1.0, Branch ("d", 0.0, Leaf, Leaf), Leaf));
  assert_equal true
  @@ D.is_ordered
       D.(
         Branch
           ( "d",
             3.0,
             Branch ("b", 2.0, Branch ("a", 0.0, Leaf, Leaf), Leaf),
             Branch ("e", 2.0, Leaf, Branch ("f", 4.0, Leaf, Leaf)) ))
(*
         "d"
         / \
       "b"  "e"
       /       \
     "a"       "f"
*)

let test_lookup _ =
  assert_equal None @@ D.lookup "a" D.Leaf;
  assert_equal (Some 0.0) @@ D.lookup "a" d1;
  assert_equal (Some 1.0) @@ D.lookup "d" d1;
  assert_equal (Some 2.0) @@ D.lookup "e" d1;
  assert_equal None @@ D.lookup "z" d1

let test_insert _ =
  assert_equal D.(Branch ("5", 5, Leaf, Leaf)) @@ D.insert D.Leaf "5" 5;
  assert_equal
    D.(
      Branch
        ( "8",
          1,
          Branch
            ("5", 0, Leaf, Branch ("7", 5, Branch ("6", 10, Leaf, Leaf), Leaf)),
          Branch ("9", -2, Leaf, Leaf) ))
  @@ D.insert
       D.(
         Branch
           ( "8",
             1,
             Branch ("5", 0, Leaf, Branch ("7", 5, Leaf, Leaf)),
             Branch ("9", -2, Leaf, Leaf) ))
       "6" 10

let test_map _ =
  assert_equal D.(Branch ("5", 10, Leaf, Leaf))
  @@ D.map D.(Branch ("5", 5, Leaf, Leaf)) ~f:(fun _ v -> v * 2);
  assert_equal
    D.(
      Branch
        ( "8",
          2,
          Branch ("5", 0, Leaf, Branch ("7", 10, Leaf, Leaf)),
          Branch ("9", -4, Leaf, Leaf) ))
  @@ D.map
       D.(
         Branch
           ( "8",
             1,
             Branch ("5", 0, Leaf, Branch ("7", 5, Leaf, Leaf)),
             Branch ("9", -2, Leaf, Leaf) ))
       ~f:(fun _ v -> v * 2)

(* test_filter requires working assoc_of_dict *)
let test_filter _ =
  assert_equal
    [ ("5", "qwe"); ("6", "abc") ]
    D.(
      Branch
        ( "5",
          "qwe",
          Branch ("4", "xyz", Branch ("3", "abc", Leaf, Leaf), Leaf),
          Branch ("6", "abc", Leaf, Leaf) )
      |> filter ~f:(fun k _ -> String.(k >= "5"))
      |> assoc_of_dict);
  assert_equal
    [ ("7", 5); ("8", 1) ]
    D.(
      Branch
        ( "8",
          1,
          Branch ("5", 0, Leaf, Branch ("7", 5, Leaf, Leaf)),
          Branch ("9", -2, Leaf, Leaf) )
      |> filter ~f:(fun _ v -> v > 0)
      |> assoc_of_dict)

let example_dict1 =
  D.(Branch ("9", 1, Branch ("8", 3, Branch ("1", 5, Leaf, Leaf), Leaf), Leaf))

let example_dict2 =
  D.(
    Branch ("8", 13, Branch ("1", 2, Leaf, Leaf), Branch ("99", 2, Leaf, Leaf)))

let merge_fun l r =
  match (l, r) with
  | None, None -> failwith "should not get here!"
  | Some _, None -> 0
  | None, Some _ -> 1
  | Some a, Some b -> a * b

(* test_merge_with requires working assoc_of_dict *)
let test_merge_with _ =
  assert_equal
    [ ("1", 10); ("8", 39); ("9", 0); ("99", 1) ]
    D.(
      merge_with ~merger:merge_fun example_dict1 example_dict2 |> assoc_of_dict)

let part1_tests =
  "Part 1"
  >: test_list
       [
         "Size" >:: test_size;
         "List" >:: test_assoc_of_dict;
         "Order" >:: test_is_ordered;
         "Lookup" >:: test_lookup;
         "Insert" >:: test_insert;
         "Map" >:: test_map;
         "Filter" >:: test_filter;
         "Merge with" >:: test_merge_with;
       ]

let test_string_to_list _ =
  assert_equal [ 'a'; 'b'; 'c' ] @@ D.string_to_list "abc";
  assert_equal [ 'f'; ' '; 'p'; ' '; 's'; ' '; 'e' ]
  @@ D.string_to_list "f p s e";
  assert_equal [] @@ D.string_to_list ""

let test_remove_literal_string _ =
  assert_equal [ 'a'; 'c' ]
  @@ D.remove_literal_string [ 'a'; '"'; 'b'; '"'; 'c' ];
  assert_equal [ 'a'; 'c' ]
  @@ D.remove_literal_string [ 'a'; '"'; 'b'; '"'; 'c' ]

let test_remove_comment _ =
  assert_equal [ 'a'; 'd' ]
  @@ D.remove_comment [ 'a'; '('; '*'; 'b'; 'c'; '*'; ')'; 'd' ];
  assert_equal [ 'a'; 'e' ]
  @@ D.remove_comment
       [ 'a'; '('; '*'; 'b'; '('; '*'; 'c'; '*'; ')'; 'd'; '*'; ')'; 'e' ]

let test_char_list_to_string_list _ =
  assert_equal [ "a"; "b"; "c" ]
  @@ D.split_chars [ 'a'; ' '; 'b'; ' '; 'c' ] [] "";
  assert_equal [ "if"; "x"; "0"; "" ]
  @@ D.split_chars [ 'i'; 'f'; '('; 'x'; '>'; '0'; ')' ] [] "";
  assert_equal [ ""; "f"; "fun"; ""; "" ]
  @@ D.split_chars [ '~'; 'f'; ':'; 'f'; 'u'; 'n'; '('; ')' ] [] ""

let test_count_keywords _ =
  assert_equal D.(Branch ("and", 1, Leaf, Branch ("let", 2, Leaf, Leaf)))
  @@ D.count_keywords [ "and"; "let"; "let" ];
  assert_equal
    D.(
      Branch
        ( "assert",
          1,
          Leaf,
          Branch ("if", 1, Leaf, Branch ("lsr", 1, Leaf, Leaf)) ))
  @@ D.count_keywords [ "assert"; "if"; "lsr" ];
  assert_equal D.Leaf @@ D.count_keywords [ "a"; "b"; "b"; "c"; "d" ]

let test_sort _ =
  assert_equal [ ("b", 2); ("a", 1) ] @@ D.sort [ ("a", 1); ("b", 2) ];
  assert_equal [] @@ D.sort []

let test_is_in_list _ =
  assert_equal true @@ D.is_in_list "a" [ "a"; "b"; "c" ];
  assert_equal false @@ D.is_in_list "d" [ "a"; "b"; "c" ]

let part2_tests =
  "Part 2"
  >: test_list
       [
         "Aux. function 1 - convert string into char list "
         >:: test_string_to_list;
         "Aux. function 2 - remove literal string"
         >:: test_remove_literal_string;
         "Aux. function 3 - remove comment" >:: test_remove_comment;
         "Aux. function 4 - convert char list into valid string list"
         >:: test_char_list_to_string_list;
         "Aux. function 5 - sort assoc list" >:: test_sort;
         "Aux. function 6 - check if a string is in a list" >:: test_is_in_list;
         "Aux. function 7 - count keywords" >:: test_count_keywords;
       ]

(* Add another suite for any of your part II functions needing testing as well.  Make sure to put those functions in simpledict.ml and headers in simpledict.mli as only libraries are unit tested; keywordcount.ml is an executable not a library. *)
let series = "Assignment2 Tests" >::: [ part1_tests; part2_tests ]
let () = run_test_tt_main series
