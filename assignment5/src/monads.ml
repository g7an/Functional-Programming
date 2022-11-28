open Core

[@@@warning "-27"]

(*
  In this assignment you will be refactoring a simple stateful OCaml program to use a state monad (a stateful stack in particular).

  Recall the monads lecture here:
  
  http://pl.cs.jhu.edu/fpse/lecture/encoding-effects.ml
  
  You also may want to re-watch some of the "Encoding Effects" lectures on Panopto, see Dateline for the dates.

*)

(*
  First let us recall the simple state monad we used in lecture: the entire "heap" was just one integer
*)

module State_int = struct
  module T = struct
    (* Here is the monad type: we need to *thread* the int through all computations
       So, pass the int in like Reader *and* return it like Logger *)
    type 'a t = int -> 'a * int

    (* Let us now construct bind.
       1) Like Reader, the result is a fun i : int -> ... since we pass in i
       2) First we pass i to the first computation x
       3) x returns a pair with a potentially **different** state, i'
       4) Now the key to being truly stateful is to thread that latest state on to f
    *)
    let bind (x : 'a t) ~(f : 'a -> 'b t) : 'b t =
     fun (i : int) ->
      let x', i' = x i in
      (f x') i'

    let return (x : 'a) : 'a t = fun i -> (x, i)

    let map = `Define_using_bind

    type 'a result = 'a * int

    (* Run needs to pass in an initial i, 0 *)
    let run (i : 'a t) : 'a result = i 0

    let set (n : int) (_ : int) = ((), n)
    (* return () as value, CHANGE state to n *)

    let get () (n : int) = (n, n)
    (* return the state n AND propagate n as state *)

    (* Lets also build in ++ for fun *)
    let inc () : 'a t = fun (n : int) -> (n + 1, n + 1)
  end

  include T
  include Monad.Make (T)
end

(* See the lecture notes for examples of using this monad. *)

(* Exercise 1.

   Suppose instead of a single integer in our "state" as above, we wanted a stack. 
   For this question you are to modify the above to make the state be a stack. 
   The main differences are to replace the `int`'s in the monad type with a list to represent the stack, 
   and then to replace set/get/inc with push/pop/is_empty operations on a stack.  
   There is also one extra type parameter on the monad as we want our stacks to work at any type, not just a single type.

   In order to make things a bit simpler we will give you lots of hints by providing the skeleton below.

   Note that you CANNOT use any OCaml data structures besides List and Option.  No mutable structures in particular!
*)

module Stack_monad = struct
  module T = struct
    type 's stack = 's list (* this is our "heap type" *)

    type ('a,'s) t = 's stack -> 'a * 's stack 
    (* 'a is the underlying value in the monad, 's is type of stack elements *)

    (* Let us now construct bind.
       1) Like Reader, the result is a fun i : int -> ... since we pass in i
       2) First we pass i to the first computation x
       3) x returns a pair with a potentially **different** state, i'
       4) Now the key to being truly stateful is to thread that latest state on to f
    *)
    (* let bind (x : 'a t) ~(f : 'a -> 'b t) : 'b t =
      fun (i : int) ->
       let x', i' = x i in
       (f x') i' *)
    let bind (x : ('a,'s) t) ~(f : 'a -> ('b,'s) t) : ('b,'s) t =
     fun (s : 's stack) -> let x', s' = x s in (f x') s'

     (* let return (x : 'a) : 'a t = fun i -> (x, i) *)
    let return (x : 'a) : ('a, 's) t = fun (s : 's stack) -> (x, s)

    let map = `Define_using_bind

    (* Run puts us in c's monad-land with an empty stack
       Unlike with state monad above just throw away final stack here
    *)
    let run (c : ('a,'s) t) : 'a = match c [] with a, s -> a

    (* let set (n : int) (_ : int) = ((), n) *)
    (* push should "push" the element on the stack and return () as the value *)
    let push (c : 's) : (unit,'s) t = fun (s : 's stack) -> ((), c :: s)

    (* let get () (n : int) = (n, n) *)
    (* pop should pop off and return the top element, i.e. the list head.
       Note for now if the stack was empty you can just `failwith "empty pop"`.
       Also get() above had a unit argument but it is not needed, the
       state monad delays execution. *)
    let pop : 'a = fun (s : 's stack) -> match s with
      | [] -> failwith "empty pop"
      | h :: t -> (h, t)

    let is_empty : (bool,'s) t = fun (s : 's stack) -> 
      (* check if the stack is empty *)
      match s with
      | [] -> (true, s)
      | _ -> (false, s)
  end

  include T
  include Monad.Make2 (T) (* Make2 is like Make but for TWO type variable parameters *)
end

(* Note that having pop raise an exception on an empty stack is a bit of a cop-out,
   we did not get rid of all effects.  To "do stack 100% monadically" we would
   need to wrap the above in an exception monad.  Shudder! We will spare you the pain. *)

open Stack_monad
open Stack_monad.Let_syntax

(* Here is a simple example based on how we used the integer state monad *)
(* Note this doesn't run yet, the state monad puts a fun ... around it all *)
let simple_stack : ('a,char) t =
  let%bind () = push 'a' in
  let%bind () = push 'b' in
  let%bind () = push 'c' in
  let%bind c = pop in
  return Char.(c = 'c')
;;

(* OK this will now run the above *)
let r = run simple_stack in
assert r

(* Exercise 2.

   For this exercise we are going to refactor a simple mutable-stack program into
   a program with the same structure, but using the above stack monad for the mutable stack *)

(* Here is the original OCaml program, which is simplified from one we covered in lecture
   Note that this whole program returns false if we do an illegal pop. 
    
   This program checks if a string s has all parentheses (/) balanced.  It uses
   the Core.Stack module which is a mutable stack.  *)

let are_balanced_exn s =
  let stack_of_lefts = Stack.create () in
  let match_with s c = Char.( = ) c (Stack.pop_exn s) in
  let parse = function
    | '(' -> Fn.const true @@ Stack.push stack_of_lefts '('
    | ')' -> match_with stack_of_lefts '('
    | _ -> true
  in
  try
    let r = String.fold ~init:true ~f:(fun b c -> b && parse c) s in
    r && Stack.is_empty stack_of_lefts
  with _ -> false

(* Now for the exercise: redo this code turning all the "real" stack operations into Stack_monad ones.  You can still use try/with since we the pop in our monad may raise an exception.  But, you may not use any mutable state, you must use Stack_monad above for all stack operations, and you must write the program monadically. 
   
   To make things easier we will extract some of the auxiliary functions we had above as separate functions with types declared for your benefit.  Pay close attention to those types, the auxiliary functions are returning monadic values. *)

let parse (c : char) : (bool, char) t = failwith "FILL THIS IN"

let main_monadic (s : string) : (bool, char) t = failwith "FILL THIS IN"

let are_balanced_monadic (s : string) : bool =
  try run @@ main_monadic s with _ -> false

(* Exercise 3.

    One problem with monad encodings is the run-time complexity can be greater.

    For a string s of length n, calculate the asymptotic complexity of
      (a) are_balanced_exn s
      (b) are_balanced_monadic s

    respectively.  Show your work so you can get partial credit.

*)

(* Extra Credit

For extra credit, make a Exception_stack monad which wraps the stack in an exception monad: if pop is attempted on an empty stack, a monadic exception will be generated.

When you have your monad, re-write `are_balanced_monadic` as `are_balanced_more_monadic` so it can avoid any OCaml exceptions. 

*)