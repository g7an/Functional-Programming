(*

FPSE Assignment 3

Name                  : Shuyao Tan
List of Collaborators :

Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.  

*)

(*
  This assignment covers many of the topics in the "More Modules" lecture so you will want to review that for lots of hints of how to proceed here.
  
  One thing which is ubiquitous in many object-oriented languages is the abilty to program against an interface, and not an underlying class or type, 
  enabling dependency injection, easier unit testing, and encapsulation of implementation details.

  We've seen in OCaml how to use polymorphic functions (those involving types like 'a) to operate on any kind of data, but in this case we need to know _nothing_ about the object.

  We've also seen how to hide parts of our implementation by not including certain details about types in our .mli files or module signatures (aka module types), 
  and not including functions which are only meant to be called by some more limited exposed API.

  But how can we write functions which operate on types which have a specific interface, rather than "any type"? Imperative
  languages would often use traits or interfaces for this, or simply dynamic dispatch and class hierarchies. What's idiomatic in OCaml?
*)

(*
  It turns out that module types and functors do the trick of specifying exactly what can be relied upon, and naming our dependencies.

  Better than that, we aren't restricted to specifying only one "class" and its interfaces, we can ask for any number and kind
  of functions operating over multiple types, and produce an entire module which makes use of those behaviors.

  Using functors in this way decouples our code from the implementation of functionality it depends on, 
  and from the representation of objects which it uses, allowing a very powerful kind of dependency injection.

  Let's start with a simple case and just consider a requirement of a type with an comparison operator on it.  
  We will specify this requirement with a module type as follows (note we covered a very similar module type in the More Modules lecture):
*)
module type Ord = sig
  (*
    Recall that t in a module is "its" underlying type of data,the data we are ordering here. Because we won't know what it actually is, 
    we'll only be allowed to use it with the operations defined in this module.
  *)
  type t

  (* We will assume there is a compare function on t as per the OCaml standard *)
  val compare : t -> t -> int
end

(* Now for some applications we may need more operations than just comparison, 
   and it is possible to extend the module type above for that purpose.  
   Concretely, we would now like to make a module type `Interp` and enforce that instances of this type include the operations from `Ord` plus some new functions. 
   We can do this using the `include` statement. 
   `include` in OCaml modules is not dissimilar to C's `#include` in that you can think of it as pasting the contents of the specified module here verbatim. *)
module type Interp = sig

  include Ord (* in effect copy/paste Ord's contents above here: the t and the compare *)

  (* We will require in addition a function to linearly *interpolate* between two mappings.  
     It takes two (in,out) mappings, plus a data item, and performs some form of interpolation to arrive a "reasonable" output for the data item.  
     We will see an example below. *)
  val interpolate : t * t -> t * t -> t -> t
end

(*
  Now, Let's get some use out of all of the above.  
  Let us make a variation on our previous assignment's dictionary implementation so that *any* interp-able type can be used as a key, not just string keys as we did there.  
  These dictionaries will be used for finite mappings over arbitrary data types, and will use the above interpolation function to interpolate missing keys.  
  So we will call the module a Cont_map, a continuous map which extends to values not explicitly in the map domain.

  You can implement the underlying mapping here as an association list if you want this time, just don't make a wrapper around OCaml's `Map`.

  Here is the module that this new form of mapping should have 
  (compare to simpledict.mli of the previous assignment - recall that a module type is what an .mli file is defining):
*)
module type Cont_map = sig
  (*
    As is usual, t is the type of the underlying data for the continuous map.  
    Note it has no parameter 'a on it since we will include a fixed type key below which is the type of both keys and values.
  *)
  type t

  (*
    Rather than strings as before, this dictionary will have keys (and values) of some arbitrary type.  
    Since we are making continuous maps over one type here the key and value types will be the same.  
    But to make the interface more clear we will declare an alias type. 

  *)
  type key
  type value = key (* values are the same type as keys *)
  val empty : t

  (* lookup finds the value of a key and returns None if the key is not in the domain of the map. *)
  val lookup : key -> t -> value option

  (* as with our previous dict, insert adds a key/value mapping and overrides if the key is already present.*)
  val insert : key -> value -> t -> t

  (*
    `remove` returns Some(v) if v was associated with the provided key, or None if there is no such key in the dictionary.

    Does not change the dict if the key was not present.
  *)
  val remove : key -> t -> t * value option

  (*
      Since we are aiming to support finitary mappings, we may also want to find the nearest key(s).
      The following function will find the *least* key in the mapping that is *greater or equal to* than the key we passed in.
      For example, for mapping { 0 |-> 3, 3 |-> 5, 5 |-> 9} lub_key on 4 will return Some(5), the next-largest key.  lub_key on 3 will return `Some(3).  Return `None` if there is no larger or equal key. *)

  val lub_key : key -> t -> key option

  (* Similarly glb_key returns the greatest lower bound key if it exists *)
  val glb_key : key -> t -> key option

  (* Interpolated lookup should use the Key.interpolate function to return an interpolated value even if the key is not directly present.  
     You should interpolate from the two keys nearest to the key provided here; the previous two functions will provide those keys.  
     If the key here is not between two existing keys there is no interpolation to be done so return None.  
     Note that if the key is in fact already in the mapping its value can directly be returned - there is no need to interpolate.  *)
  val interpolated_lookup : key -> t -> value option

end

(*
  Exercise 1:

  Provide an implementation of `Cont_map` as a functor, making use of a parameter module Key for the keys and values which meet the `Interp` interface.
  Note this version will in fact not work properly due to the types, so we call it `Cont_dud`.  A minor tweak below will fix it.

*)
module Cont_dud (Key : Interp) : Cont_map = struct
  (*
    ... Put implementation here ... will be a type error until you fill in ..
  *)
  
end

(*
  Subtle point: "Over-encapsulation"

  The great advantage of programming against the `Interp` or `Cont_map` interface is precisely not having to know
  what the underlying type `key` and `t` is; this restricts your dependent code from becoming tightly coupled to any particular instantiation of the interface.

  However, the definition of the `Cont_map` interface makes no distinction between how "hidden" the `t` and `key` types should be. This means that both are only presented as abstract types to any code outside the module... but we need to create a value of type `key` in order to use any of the Cont_map's features! We've been so successful at hiding the implementation, that we can't even use it - !

  Fortunately OCaml has a way to specifically un-hide an abstract type in a module type for this reason. Consider the following definition of `Interp` for floats:
*)
module Float_interp : Interp with type t = float = struct
  type t = float

  let compare = Float.compare

  let interpolate (x1,y1) (x2,y2) x = y1 +. ((x -. x1) /. (x2 -. x1)) *. (y2 -. y1)
end

(*
  The module type `(Interp with type t = float)` guarantees that all of the contents of `Interp` are satisfied, and additionally provides the information that the type `t` is `float` and so makes it visible - !
*)

(*
  Exercise 2:

  Modify the signature given to `Cont_map` in your implementation using `with` so that the functor will expose the `key` type, solving the over-hiding problem so that the data structure can finally be used.

  Hint: remember that you can externally refer to a type ty defined within a module Mod by the notation "Mod.ty".  You will need that here.

*)

module Cont_map (Key : Interp) : Cont_map = struct (* CHANGE this line this time to make it work *)
  (*
    ... You can copy/paste your implementation from above here ... 
  *)

end

(* With the above in place we can make a continuous map for floats *)

module Float_cont_map = Cont_map (Float_interp)

(* This should then interpolate to Some(2.5):
   Float_cont_map.(empty |> insert 1. 2. |> insert 2. 3. |> interpolated_lookup 1.5)
*)

(* ************************************************************ *)
(* ********************** P A R T II ************************** *)
(* ************************************************************ *)

(*
  Exercise 3:

  For this question we will develop a generic operator language parser / evaluator. We will make it generic by allowing us to "plug in" the underlying data type.

  Note we will simplify several things so this is primarily an OCaml abstraction exercise and not a parsing exercise.  We will have only `+` and `*` binary operations, will just parse a string rather than reading from a stream, and will use postfix (aka RPN) notation for operators: "1 2 +" will return 3.
  
*)

(* Here is the module type for the underlying data.
   The key function is next, it reads a `t` off of the front of the string,
   and returns the remainder of the string as well as the `t` element.

   Here are some clarifications on how next works.
   1. whitespace (space, tab, newline) is a separator, the `t` value ends at that point.
   2. `next` is only reponsible for reading a `t` off the front of the string
   3. It should obey the "maximal munch" principle, read in as many characters as possible
     whilst still making a `t`.  So for example on input `"12"` for `t = int` read in `12`, not `1`.
     On "12@" next will return `12` plus remainder "@", `next` is not responsible for checking
     for other errors except for illegal types (see point 5).
   4. If `next` returned `None`, the output string remains unchanged from the input string
   5. `next` will return `None` in 4 cases. If we reached the end of the string (no more `t` to read),
       we encountered an illegal character like `@`, we encountered an operator, or we encountered an illegal type.
       An illegal type is defined based on what kind of concrete Data we are implementing. See more below on Int_Data and Rat_Data
*)

module type Data = sig
  type t

  val from_string : string -> t
  val to_string : t -> string
  val next : string -> (string * t) option
  val plus : t -> t -> t
  val times : t -> t -> t
end

(* The Evaluator for this simple language is then a functor of this type.
   If the input cannot be parsed, `eval` will return `Error "string"`, otherwise it will
   return `Some(t-value)`.

   Clarifications:
   1. If an illegal character or illegal type is encountered, the evaluator will return `Error "illegal character"`.
   2. If there are too few or too many operators (as in "1 2 + +" or "1 2") return `Error "unmatched"`.
   3. Eval called on empty string should also return `Error "unmatched"`.
   4. Note that operators need not be space-separated, e.g. "1 2 3++" returns `6`.
   5. '+' and '*' are the characters corresponding to the `plus` and `times` binary operations.
     There are no other operations supported, all others are illegal characters.
*)

module type Eval = functor (Data : Data) -> sig
  type t = Data.t

  val eval : string -> (t, string) result
end

(* a. Write the evaluator functor matching this signature.  *)

(* Uncomment this code and fill in the ...

   module Eval : Eval = functor (Data : Data) -> struct
     ...
   end
*)

(* b. Make Int_Data and Rat_Data modules for parsing integers and rationals.

    - Integers may optionally be signed, so -4 is an integer
    - Rationals are written as "3/4", integers separated by "/".
    - While there are no illegal Int types, there are illegal Rational types.
      While we allow negative Rationals like "-1/3", we disallow "-1/-3" or "5/-2". The negative sign must not be on the denominator
      We also disallow division by zero. Such as "5/0"
    - When we return a Rational from eval, the Rational must be in simple fractions. e.g. "3/6" is not correct, but "1/2" is.
      Additionally, for whole numbers, format them with the number divided by 1, e.g. "-1/1". "0/1", "1/1", "2/1".
*)

(* remove this comment and fill in:

   module Int_Data ...
*)

(* remove this comment and fill in:

   module Rat_Data ...
*)

(* With this we may now create evaluators for integers and rationals. *)

(* Uncomment the below lines (unchanged) when you have the above modules defined.

   module Int_Eval = Eval(Int_Data)

   module Rat_Eval = Eval(Rat_Data)

   At this point you should look at the types and see if you "hid too much"
   as per the Dict example above; you might need some `with` declarations
   here to solve the over-hiding problem.

   Hint: this mostly depends on how you want to test your functions
*)

(*
Int_Eval.eval "2 3 +" should now return `OK(5).  Make sure to 
write a good array of such tests in `tests.ml` to make sure your
implementation is working.
*)