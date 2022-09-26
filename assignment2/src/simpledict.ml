(*

FPSE Assignment 2

Name                  : Shuyao Tan
List of Collaborators : Yuyang Zhou, Tingyao Li

Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.

See file simpledict.mli for the specification of Part I of the assignment, and keywordcount.ml for Part II.  Recall from lecture that .mli files are module signatures aka module types and you will need to provide implementations of all the functions listed there in this file. 

Your Part I answers go here, and the Part II application should go in the keywordcount.ml file.

For any auxiliary functions for Part II put them in this file as well and also add their headers to
`simpledict.mli`.  This will allow them to be run/tested without changing the dune file configuration.
Note that this is a bit sloppy, in a real app there would be a separate library file for the auxiliary
functions needed for Part II.

Hint: to start out you will want to copy over the .mli file and make dummy headers for each function similar to what we gave you for Assignment 1.  Recall that the syntax is slightly different in .ml and .mli declarations, e.g. `val` in .mli is `let` in .ml, etc.

Note that .ml files need to include all `type` declarations in .mli files.

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* You are required to use the Core libraries, don't remove the following line. If the editor is not recognizing Core (red squiggle under it for example), run a "dune build" from the shell -- the first time you build it will create some .merlin files which tells the editor where the libraries are.
*)
open Core


type 'a dict =
  | Leaf
  | Branch of string * 'a * 'a dict * 'a dict;;


let rec size : 'a dict -> int = 
  fun d -> 
    match d with
    | Leaf -> 0
    | Branch (_, _, l, r) -> 1 + size l + size r;;


(*
    Given a dict, flatten it into a list of (key, value) pairs which retains the tree's ordering (the 'inorder' traversal). This should take O(n) time; recall that each (::) is O(1) and each (@) is O(n).
    Note that this representation of a dictionary as a list of (key, value) pairs is called an *assoc* in OCaml.  It is not as efficient as a binary tree representation however.
*)
let assoc_of_dict : 'a dict -> (string * 'a) list = 
  fun d -> 
    let rec helper : 'a dict -> (string * 'a) list -> (string * 'a) list = 
      fun d acc -> 
        match d with
        | Leaf -> acc
        | Branch (k, v, l, r) -> helper l ((k, v) :: helper r acc)
    in helper d [];;


(*
    When using a tree to represent a dictionary the main advantage is keeping the tree *ordered* by key for faster access.  So, check whether a dict is *ordered*, i.e. that all left subtree keys are strictly less than the branch key, and all right subtree keys are strictly greater.

    Note that this requirement guarantees (by induction) that the dictionary has no duplicate keys.
*)
let is_ordered d =
  let list = assoc_of_dict d in
  let rec helper l = 
      match l with
      | [] -> true
      | [(k, _)] -> true
      | (k1, _) :: (k2, v2) :: t -> 
        if String.(k1 < k2) then helper ((k2, v2) :: t)
        else false
  in helper list;;
    

(*
    Given a string and a dictionary, look up the associated value, if any.  
    
    Your implementation must be O(log n) on average since you can take advanatge of the is_ordered requirement.
*)
let lookup : string -> 'a dict -> 'a option = 
  fun k d -> 
    let rec helper (d: 'a dict) (k: string) : 'a option = 
      match d with
      | Leaf -> None
      | Branch (k', v, l, r) -> 
        if String.(k=k') then Some v
        else if String.(k<k') then helper l k
        else helper r k
    in helper d k;;

(*
    Given a string key and a value, insert the pair into the dictionary, overwriting any existing value attached to the key in the dict if present.  
    This should also be O(log n) as we will cover in lecture.
*)
let insert : 'a dict -> string -> 'a -> 'a dict = 
  fun d k v -> 
    let rec helper (d: 'a dict) (k: string) (v: 'a) : 'a dict = 
      match d with
      | Leaf -> Branch(k, v, Leaf, Leaf)
      | Branch (k', v', l, r) -> 
        if String.(k=k') then Branch(k, v, l, r)
        else if String.(k<k') then Branch(k', v', helper l k v, r)
        else Branch(k', v', l, helper r k v)
    in helper d k v;;

(*
    Given a dict and some transforming operation, 
    apply the operation to each value within the dictionary to produce a new dict, 
    but keeping the keys constant.  
    
    Note that the mapping function f can use the key in its calculation so we also pass it as an argument.
*)
let rec map (tree : 'a dict) ~(f : string -> 'a -> 'b) : ('b dict) =
  match tree with
  | Leaf -> Leaf
  | Branch (k, v, l, r) -> Branch(k, f k v, map l ~f, map r ~f);;


(*
    Given two dictionaries, merge them into one, preserving the ordering property.
    If both contain a value associated to a key, retain the second dict's value in the result.
*)
let rec merge d1 d2 = 
  match d2 with
    | Leaf -> d1
    | Branch (k, v, l, r) -> merge (insert d1 k v) (merge l r);;

let rec get_unique_items (d1: 'a dict) (d2: 'b dict): 'b dict = 
  match d2 with
  | Leaf -> Leaf
  | Branch (k, v, l, r) -> 
    match lookup k d1 with
      | None -> merge (Branch (k, v, Leaf, Leaf)) (merge (get_unique_items d1 l) (get_unique_items d1 r))
      | Some _ -> merge (get_unique_items d1 l) (get_unique_items d1 r);;

let rec get_common_items d1 d2 ~merger = 
  match d2 with
  | Leaf -> Leaf
  | Branch (k, v, l, r) -> match lookup k d1 with
    | None -> merge (get_common_items d1 l ~merger) (get_common_items d1 r ~merger)
    | Some v1 -> merge (Branch (k, merger (Some v1) (Some v), Leaf, Leaf)) (merge (get_common_items d1 l ~merger) (get_common_items d1 r ~merger));;

let merge_with d1 d2 ~merger = 
  let single_in_tree1 = get_unique_items d2 d1 in
  let single_in_tree2 = get_unique_items d1 d2 in
  let double_merged = get_common_items d1 d2 ~merger in
  let single_merged1 = map single_in_tree1 ~f:(fun _ v -> merger (Some v) None) in
  let single_merged2 = map single_in_tree2 ~f:(fun _ v -> merger None (Some v))  in
  merge (merge single_merged1 single_merged2) double_merged;;

(*
    Define a filter operation over dictionaries, in analogy to filter on lists: 
    apply a predicate to each element of the dictionary and return a dictionary containing only the elements on which the predicate returns `true`.  
    We will allow the predicate `f` to access both the key and the value as can be seen from its type below.
*)
let filter : 'a dict -> f:(string -> 'a -> bool) -> 'a dict = 
  fun d ~f -> 
    let rec helper (d: 'a dict) : 'a dict = 
      match d with
      | Leaf -> Leaf
      | Branch (k, v, l, r) -> 
        let l' = helper l in
        let r' = helper r in
        if f k v then insert (merge l' r') k v
        else merge l' r'
    in helper d;;

(* 
    Show the usefulness of filtering by determining if a dict of numbers contains only even numbers, 
    *without using any recursion in your code for this function* - 
    use `filter` and other functions defined above only.
*) 
let is_all_evens (d: int dict) : bool = 
  if size (filter d ~f:(fun k v -> v mod 2 = 0)) = size d then true else false;;

(* 
    keyword_list:
    [
      "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"; "downto"; "else"; "end"; "exception"; 
      "external"; "false"; "for"; "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"; "land"; 
      "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"; "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; 
      "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val"; "virtual"; "when"; 
      "while"; "with";
    ]

    Given a list of strings, return a dictionary mapping each string to the number of times it appears in the list if it appears in keyword list.
*)
    (* check is a string is in a list of strings *)
let rec is_in_list (s: string) (l: string list) : bool =
  match l with
  | [] -> false
  | h::t -> if String.(s = h) then true else is_in_list s t;;

  
let count_keywords (lst: string list) : int dict = 
  let keyword_list = [
    "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"; "downto"; "else"; "end"; "exception"; 
    "external"; "false"; "for"; "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"; "land"; 
    "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"; "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; 
    "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val"; "virtual"; "when"; 
    "while"; "with";
  ] in
  let rec helper (lst: string list) (d: int dict) : int dict = 
    match lst with
    | [] -> d
    | h::t -> 
      (* if (is_in_list h keyword_list) then helper t (insert d h ((lookup h d |> Option.value ~default:0) + 1)) *)
      if (is_in_list h keyword_list) then helper t (insert d h ((lookup h d |> Option.value ~default:0) + 1))
      else helper t d
  in helper lst Leaf;;

(* 
  let test_string_list = ["and"; "as"; "and"; "dewfvsadc"];; 
  Simpledict.count_keywords test_string_list |> Simpledict.assoc_of_dict |> Simpledict.sort;;
*)
(* sort list of string * int according to the integer value in the tuple *)
let sort (lst: (string * int) list) : (string * int) list  = List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a) lst;;

      
