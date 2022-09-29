(* Part I: A Binary Tree Based Dictionary

   This file specifies the interface for your code and must not be edited (it will also not be included in your zip submission). 
   Your actual implementation should go in the file `simpledict.ml`  which satisfies this interface appropriately.

   Mutation operations of OCaml are not allowed, or required.
   
*)


(*
    What follows is a a binary tree, with (key,value) pairs at each branching node.  Each such (key,value) will be used to represent a dictionary entry of a key mapping to a value.  For simplicity we will restrict the keys to be `string`s only; the values are of type 'a.
*)

type 'a dict =
  | Leaf
  | Branch of string * 'a * 'a dict * 'a dict (* key, value, left subtree, right subtree *)
  [@@deriving yojson]

(* We will now define several natural operations on these dictionaries. *)

(*
    Given a dict, compute the total number of (key,value) mappings it contains.
*)
val size : 'a dict -> int

(*
    Given a dict, flatten it into a list of (key, value) pairs which retains the tree's ordering (the 'inorder' traversal). This should take O(n) time; recall that each (::) is O(1) and each (@) is O(n).
    Note that this representation of a dictionary as a list of (key, value) pairs is called an *assoc* in OCaml.  It is not as efficient as a binary tree representation however.
*)
val assoc_of_dict : 'a dict -> (string * 'a) list

(*
    When using a tree to represent a dictionary the main advantage is keeping the tree *ordered* by key for faster access.  So, check whether a dict is *ordered*, i.e. that all left subtree keys are strictly less than the branch key, and all right subtree keys are strictly greater.

    Note that this requirement guarantees (by induction) that the dictionary has no duplicate keys.
*)
val is_ordered : 'a dict -> bool

(*
    We will implicitly require all dict's provided to and created by functions below to obey the  `is_ordered` requirement.
*)

(*
    Given a string and a dictionary, look up the associated value, if any.  Your implementation must be O(log n) on average since you can take advanatge of the is_ordered requirement.
*)
val lookup : string -> 'a dict -> 'a option

(*
    Given a string key and a value, insert the pair into the dictionary, overwriting any existing value attached to the key in the dict if present.  This should also be O(log n) as we will cover in lecture.
*)
val insert : 'a dict -> string -> 'a -> 'a dict

(*
    Given a dict and some transforming operation, apply the operation to each value within the dictionary to produce a new dict, but keeping the keys constant.  Note that the mapping function f can use the key in its calculation so we also pass it as an argument.
*)
val map : 'a dict -> f:(string ->'a -> 'b) -> 'b dict

(*
    Given two dictionaries, merge them into one, preserving the ordering property.
    If both contain a value associated to a key, retain the second dict's value in the result.
*)
val merge : 'a dict -> 'a dict -> 'a dict

(*
    Given two dictionaries, merge them into one, preserving the ordering property.
    Compute the new values for each dictionary key using the supplied merger 
    function, feeding it Some or None depending on the presence of each key in the first and second dict.

    Note since we have a merger function here the dictionaries may in principle have different types.
*)
val merge_with : 'a dict -> 'b dict ->  merger:('a option -> 'b option -> 'c) -> 'c dict

(*
    Define a filter operation over dictionaries, in analogy to filter on lists: apply a predicate to each element of the dictionary and return a dictionary containing only the elements on which the predicate returns `true`.  We will allow the predicate `f` to access both the key and the value as can be seen from its type below.
*)
val filter : 'a dict -> f:(string -> 'a -> bool) -> 'a dict

(* 
    Show the usefulness of filtering by determining if a dict of numbers contains only even numbers, *without using any recursion in your code for this function* - use `filter` and other functions defined above only.
*)
val is_all_evens : int dict -> bool

val count_keywords: string list -> int dict
val sort: (string * int) list -> (string * int) list
