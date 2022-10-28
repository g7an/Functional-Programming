(*

FPSE Assignment 4
 
Name                  : 
List of Collaborators :

*)

open Core

(*
  In this assignment you will implement a simple n-gram model.  We will also give you some template code to give you a basis for an elegant and general library for modeling n-grams.

  Part I consists of these library routines, and Part II will be to develop a command-line tool to build and use an n-gram model which makes use of your library.

  Using the n-gram model of sequences and probabilities, we'll take in some sequence of items, 
  and then use it as a basis to generate more similar sequences (for example, sentences of words, lists of numbers, etc.), 
  or evaluate the likelihood of seeing particular sequences.

  First we need some helper functions and definitions.

*)

(* Part I consists of the following 6 Exercises *)

(*
  Exercise 1:

  Given a list of some type, and a positive integer `n`, produce a list of contiguous subsequences of length `n` of the original list. If `n` is greater than the length of the input, return an empty list.

  E.G.
    chunks 3 [1; 2; 3; 4; 5] =
      [ [1;2;3]; [2;3;4]; [3;4;5] ]

    chunks 2 ["a"; "b"; "c"; "d"; "e"] =
      [ ["a";"b"]; ["b";"c"]; ["c";"d"]; ["d";"e"] ]

    chunks 3 [1; 2] = []
*)
(* get n items in a list *)
let rec get_n_items n lst = 
  match lst with
  | [] -> []
  | h::t -> if n = 1 then [h] else h::(get_n_items (n-1) t)

let chunks (n: int) (l: 'a list): 'a list list =
    let rec chunks_helper (n: int) (l: 'a list) (acc: 'a list list): 'a list list =
        match l with
        | [] -> acc
        | hd::tl -> (
            let sub_list = hd:: (get_n_items (n-1) tl)
              in if List.length sub_list = n then chunks_helper n tl (acc@[sub_list]) else acc
    )
    in chunks_helper n l []


(*
  Exercise 2:

  Given a non-empty list of some type, return a pair of the last element, and a list of all previous elements. This should take O(n) time.

  E.G.
    split_last [1;2;3] = (3, [1;2])
*)
let split_last (l: 'a list): 'a * 'a list =
    let rec split_last_helper (l: 'a list) (acc: 'a list): 'a * 'a list =
        match l with
          | [] -> failwith "empty list"
          | hd::[] -> (hd, acc)
          | hd::tl -> split_last_helper tl (acc@[hd])
    in split_last_helper l []

(*
  Exercise 3:

  Here we will make a generic method for making map keys which are lists of some underlying element. 

  Given a data type module Elt which can be used as the key for a map (Map.Key module type in Core), 
  fill in the following functor to make a map key data type module for a *list* of Elt's.  
  Recall that the Map.Key module type needs a type `t`, a `compare` on it and the to/from s-expression conversions over `t`: 
  https://ocaml.org/p/core/v0.15.0/doc/Core/Map_intf/module-type-Key/index.html.
*)
module List_key (Elt: Map.Key): (Map.Key with type t = Elt.t list) = struct
  (*
    ... YOUR IMPLEMENTATION HERE ... 
  *)
  type t = Elt.t list

  let compare = List.compare Elt.compare

  let sexp_of_t = List.sexp_of_t Elt.sexp_of_t

  let t_of_sexp = List.t_of_sexp Elt.t_of_sexp
end

(*
  We will need randomness for this code, which can make things hard to test. 
  To make it repeatable, we will abstract away the randomizer as another parameter, of module type `Randomness`.
  
  Note that the standard `Core.Random` module is compatible with this module type, 
  but you could also provide alternative definitions which are deterministic, 
  give more debug info, guarantee a certain sequence of numbers, log to stderr, etc.
*)
module type Randomness = sig
  (*
    Given a maximum integer value, return a pseudorandom integer from 0 (inclusive) to this value (exclusive).
  *)
  val int : int -> int
end

(*
  Exercise 4:

  Given a multiset aka bag, select one element from it with uniform probability 
  (i.e. so that elements which appear multiple times should have a higher chance to be picked). Or, if the bag is empty, return None.

  See the (weighted) reservoir sampling algorithms for a simple potential approach to this.
  
  Use `Core.Bag` for your bag. This operation should not be destructive or mutate the bag, it should just extract a random element. 
  Be aware that Core.Bag is a mutable data structure.  
  Also be aware of the `'a Bag.Elt.t` type, which signifies a particular element within the Bag (distinguishing it from other, possibly equal elements).

  Several Bag functions take and return `'a Bag.Elt.t` values so look at the documentation for `Bag.Elt` to see how to e.g. extract the underlying value.

  Note that `Bag.choose` does *not* satisfy this definition;  it simply picks the first element always.

  Note we use a shorthand notation for first-class modules not covered in lecture: 
  the parameter R here is a first-class module which has already been unpacked from value-space to module-space.  
  Similarly, sample can be invoked as e.g. `sample (module Random) ...`

*)
let sample (module R: Randomness) (b: 'a Bag.t): 'a option =
    let len = Bag.length b in
    if len = 0 then None
    else
        let index = R.int len in
        let rec sample_helper (b: 'a Bag.t) (index: int): 'a option =
            match Bag.choose b with
            | None -> None
            | Some elt ->
              (
                if index = 0 then Some (Bag.Elt.value elt)
                else 
                  (* remove elt in b and pass the mutated bag b into recursive call *)
                  let _ = Bag.remove b elt in
                  sample_helper b (index-1)
              )
        in sample_helper b index

(* Exercise 5:

  Fill out the skeleton of the module N_grams below to make a common probabilistic model of sequences, the n-gram model, also called the Markov model.

  The general intuition is simple: if we want to be able to predict what comes next in a sequence of items, we can probably do so on the basis of the elements which preceeded it. Moreover, we can 
  probably ignore parts of the sequence which came _far_ before the element we want to predict, and focus our attention on the immediately previous couple of items.

  Consider sentences of words in english text, a very common type of sequence to apply this approach to. If we are given that the word we want to predict came after:
  
  "take this boat for a spin out on the" ???

  Then we could say that "water" is more likely than "town" to follow. If we have less context, say only 2 words:

  "on the" ???

  We will naturally make a poorer approximation of the true distribution, but it may be sufficient for some purposes anyway, and will be easier to estimate.  
  How can we estimate the actual distribution of words efficiently, then?
  
  We will need to take in some observed sequence of words or tokens, called a _corpus_.  
  Let's say we want to keep 2 words of context when predicting what comes next, based on the provided corpus. Then we can 
  just keep track of every 3-tuple of consecutive words in the input, and count how often they appear.

  For example, say we observe the triples

  ("take", "this", "boat"), ("this", "boat", "for"), ... ("on", "the", "water").

  Then, if we index these properly, we can predict what should follow ("on", "the") 
  by just sampling randomly from among all the tuples which started with that prefix, 
  and using the last element of the tuple as our prediction.  
  Naturally, words which appear more frequently in the context specified should then be given more weight, 
  and words which do not appear in our corpus after the given sequence will not be chosen at all, 
  so our prediction should be a reasonable estimate for the empirical distribution.

  If we instead count 5-tuples rather than 3-tuples, we can make better predictions with the greater context, 
  which will then more closely match the true sequence properties. 
  However, we will also be able to observe fewer unique 5-tuples overall than 3-tuples, 
  which will mean we need greater amounts of data to properly use a larger n-gram size.


  Feel free to read these useful resources to better understand n-grams:
  - https://blog.xrds.acm.org/2017/10/introduction-n-grams-need/
  - https://web.stanford.edu/~jurafsky/slp3/slides/LM_4.pdf
  - https://medium.com/mti-technology/n-gram-language-model-b7c2fc322799

  
  First define a module which holds our main functionality specific to a particular orderable type we'll call `Token`. 
  These tokens could be words (strings) of course, but could also be numbers, or DNA base pairs, etc.

  We also need randomness here, so we will abstract over it as well.
*)
module N_grams (Random: Randomness) (Token: Map.Key) = struct

  (*
    Define a module which is a Map satisfying the signature provided, so that sequences of tokens can be mapped to values.
  *)
  module Token_list_map : (Map.S with type Key.t = Token.t list) = 
    Map.Make (List_key (Token))
;;

  (*
    Based on how n-grams work, we will represent a probability distribution as mapping from prefixes of size `n`, 
    to tokens which followed this prefix in our training corpus. 
    The more times any particular token follows a prefix, the more likely it is to follow it again.

    Don't change this type; it is a map from token lists to bags of tokens.
  *)
  type distribution = (Token.t Bag.t) Token_list_map.t

  (*
    Given a positive integer `n` and a list of tokens, 
    add each token to a new distribution as an element of the set corresponding to the (n-1)-gram which preceeds it.

    e.g. (informally diagramming the map/bag of a `distribution`)

      ngrams 2 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1] =
        { 
          [1] -> {2}; 
          [2] -> {3; 2; 3};
          [3] -> {4; 1};
          [4] -> {4; 4; 2};
            |        |
            |        \------- ...was followed by each of these elements
            \-- this sequence (of length 1 in this example of 2-grams)  ...
        }

      ngrams 3 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1] =
        {
          [1; 2] -> {3};
          [2; 3] -> {4; 1};
          [3; 4] -> {4};
          [4; 4] -> {4; 2};
          [4; 2] -> {2};
          [2; 2] -> {3};
            |        |
            |        \------- ...was followed by each of these elements
            \-- this sequence...
        }

      ngrams 1 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1] =
        {
          [] -> {1; 2; 3; 4; 4; 4; 2; 2; 3; 1};
          |        |
          |        \------- ...was followed by each of these elements
          \-- this sequence...
        }
  *)
  let ngrams (n: int) (l: Token.t list): distribution =
    let lists = chunks n l in 
    (* build distribution by using (n-1) items in the list as key and the last item as value *)
    let rec build_distribution (lists: Token.t list list) (d: distribution): distribution =
      match lists with
      | [] -> d
      | list :: rest -> 
        let value, key = split_last list in 
        (* find if key is in d
          if yes, add value to the bag
          if no, insert key to the d and create a new bag with value *)
        let new_bag =
          match Token_list_map.find_exn d key with
          | bag -> let _ = Bag.add bag value in bag
          | exception Not_found_s _ -> 
            let new_bag = Bag.create () in
            let _ = Bag.add new_bag value
          in new_bag
        in
        let new_d = Token_list_map.set d ~key ~data:new_bag in
        build_distribution rest new_d
        (* use key as the key to get the bag of values from the distribution
        let bag = Token_list_map.find_opt key d |> Option.value ~default:Bag.empty in
        (* add the value to the bag *)
        let new_bag = Bag.add value bag in
        (* add the new bag to the distribution *)
        let new_distribution = Token_list_map.add key new_bag d in
        build_distribution rest new_distribution *)
    in
    build_distribution lists Token_list_map.empty


  (*

  Now, we can use the output of `ngrams` to create new, randomly sampled sequences.

    The arguments it expects are as follows:
    - an output from a call to `ngrams n` (above) representing a distribution
    - an integer for the maximum length of sequence to generate
    - a list of tokens (of length `n-1`) to kick off the random generation and sequence (consider it a 'seed' to look up in the distribution)

    It will then produce a sequence which is distributed according to the n-gram model it is given, terminating if either the resulting sequence reaches the maximum length, or when there are no observed
    n-grams which could follow.

  *)

  let sample_sequence (dist: distribution) ~(max_length: int) ~(initial_ngram: Token.t list): Token.t list =
    failwith "undefined"
  
end (* of module N_grams *)

(*
  Exercise 6:

  Given a string, perform basic sanitization/normalization by taking the following steps:

  - remove all characters not in the range [a-zA-Z0-9]
  - convert all characters [A-Z] to lowercase

  if the resulting string is empty, return None.

*)
let sanitize (s: string): string option =
  failwith "unimplemented"

(* See ngrams.ml for part II.

For any auxiliary functions needed in part II, put them here so they can
be unit tested.  

*)