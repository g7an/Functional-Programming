(*

FPSE Assignment 1
 
Name                  : Shuyao Tan
List of Collaborators :

Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.  CAs/Prof need not be listed!

Fill in the function definitions below replacing the 

  unimplemented ()

with your code.  Feel free to add "rec" to any function listed to make it recursive. In some cases, you will find it helpful to define auxillary functions, feel free to.

You must not use any mutation operations of OCaml for any of these questions (which we have not taught yet in any case): no arrays, for- or while-loops, references, etc.

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* You are required to use the Core libraries, don't remove the following line. If the editor is not recognizing Core (red squiggle under it for example), run a "dune build" from the shell -- the first time you build it will create some .merlin files which tells the editor where the libraries are.
*)
open Core

(* Here is a simple function which gets passed unit, (), as argument and raises an exception.  It is the initial implementation below. *)

let unimplemented () =
	failwith "unimplemented"
	
(*
	Part I Section 1: simple numeric recursions.
	
	All functions must be total for the specified domain;	overflow is excluded from this restriction but should be avoided.
	
*)

(*
	Given a non-negative integer `n`, compute `0+1+2+ ... +n` using recursion
	(don't use the closed-form solution, do the actual addition).
*)
let rec summate (n: int): int = 
	match n with
		| 0 -> 0
		| n -> n + summate (n-1);;
	

(*
	Given non-negative integers `n` and `m`, compute their least common multiple.
*)
(* compute the greatest common divisor of two integers using recursion.
	 The most efficient algorithm is Euclid's algorithm:
	 两个正整数a和b（a>b），它们的最大公约数等于a除以b的余数c和b之间的最大公约数
	 2 positive integers a and b, the greatest common divisor of a and b is the greatest common divisor of (the remainder when a is divided by b) and b.
*)
let rec gcd (n: int) (m: int): int = 
	match n with
		| 0 -> m
		| n -> match m with
			| 0 -> n
			| m -> gcd (m) (n mod m);;

	
(* least common multiple of 2 numbers is the product of the numbers divided by the greatest common divisor of the numbers. *)
let lcm (n: int) (m: int): int =
	n * m / gcd (n) (m);;

(*
	Given a non-negative integer `n`, compute the n-th fibonacci number.	Give an implementation that does not take exponential time; the naive version from lecture is exponential	since it has two recursive calls for each call.
*)
let fibonacci (n: int): int =
	let rec helper (n: int) (index: int) (a: int) (b: int): int = 
		if n = 0 then 0
		else 
			if index < n then
				helper (n) (index+1) (b) (a+b)
			else
				a+b
		in helper n 2 0 1;;

(*
	Part I Section 2: building lists. The List. module functions may NOT be used (yet).
*)
	
(*
	Given a non-negative integer `n`, produce a list [n; n-1; ...; 2; 1].
*)
let rec iota1 (n: int): int list =
	match n with
		| 0 -> []
		| n -> n :: iota1 (n-1)
	;;

	
(*
	Given a non-negative integer `n`, produce a list [1; 2; ...; n-1; n],	without taking O(n^2) time.
*)
let rec iota2 (n: int): int list =
	match n with
		| 0 -> []
		| n -> iota2 (n-1) @ [n];;

	
(*
	Given a positive integer `n`, produce the list of integers in the range (0, n] which it is divisible by, in ascending order.
*)
let factors (n: int): int list =
	let rec helper (n: int) (divisor: int): int list = 
		match divisor with
		| 0 -> []
		| divisor -> if n mod divisor = 0 then helper (n) (divisor-1) @ [divisor] else helper (n) (divisor-1)
	in (helper n n);;

	
(*
	Part I Section 3: strings, lists, and sorting.  The List module functions cannot be used.	String comparisons operations such as String.(<=) can be used, but no other String module functions.
*)

(*
	Given a list of strings, check to see if it is ordered, i.e. whether earlier elements are less than or equal to later elements.
*)
let rec is_ordered (ls: string list): bool =
	match ls with
		| [] -> true
		| [x] -> true
		| x :: y :: xs -> if String.(x<=y) then is_ordered (y :: xs) else false;;

(*
	Given a string and an ordered list of strings, insert the string into the list so	that the list remains ordered.  Return the list with the string inserted.

	Note this is an example of a *functional data structure*, instead of mutating	you return a fresh copy with the element added.
*)

let rec insert_string (s: string) (l: string list): string list =
	match l with
		| [] -> [s]
		| x :: xs -> if String.(s<=x) then s :: l else x :: insert_string (s) (xs);;
	

(*
	Define a variation on the previous function which before inserting the element verifies that the input list is indeed sorted.  Use the built-in `Base` function invalid_arg	which will raise an exception.	Note that invalid_arg is a function that raises the Invalid_argument exception

	Important: Your invalid_arg function must take this exact string (without quotes):
	"List not sorted" 

	e.g. invalid_arg "List not sorted"

	This is because OUnit2 tests check not only for exception type but also the message. If you don't have the same error message as us, the autograder will fail you. 
*)

let insert_string_exn (s: string) (l: string list): string list =
	if is_ordered (l) then insert_string s l else invalid_arg "List not sorted";;
	

(*
	Define a function to sort a list of strings by a functional version of the insertion sort method: repeatedly invoke insert_string to add elements one by one to an initially empty list.

	The sorted list should be sorted from smallest to largest string lexicographically
*)

let rec insertion_sort (l: string list): string list =
	match l with 
	| [] -> []
	| x :: xs -> insert_string x (insertion_sort xs);;



(*
	Define a function to remove the lexicographically maximum string in a list of strings.
	Return
	 Error("empty list") if the input list is empty (and has no max)
	 OK(s,s_list) for s the maximum string and s_list the list with s removed, 
	  if the list is not empty.

	If there are more than one max string, remove the max string that occurs last in the list
*)

let rec is_max (s: string) (l: string list): bool =
	match l with
		| [] -> true
		| x :: xs -> if String.(x<=s) then is_max s xs else false;;

let rec remove_string (s: string) (l: string list): string list = 
	match l with
		| [] -> []
		| x :: xs -> if String.(s=x) then remove_string s xs else x :: remove_string s xs;;

let remove_max (l: string list): (string * string list, string) result =
	let rec helper (iter: string list) (l: string list): (string * string list, string) result =
	match iter with
		| [] -> Error "empty list"
		| x :: xs -> if (is_max x l) then Ok (x, remove_string x l) else helper xs l
	in helper l l;;


(*
	Write a sort routine by repeated invocations of remove_max to pull out the largest
	elements one-by-one.  You should never need to invoke `remove_max` on an empty
	list, and you can thus `assert false` (an invariant failure) if the `Error`
	case is ever returned from `remove_max`.  
	This problem shows how we can manually encode the exceptional condition in `
	remove_max` with `Ok`/`Error` but convert it to an actual side effect here
	(the `assert false` will raise an exception if hit).

	Max sort on an empty list should return an empty list, not throw an exception.
	The sorted list should be sorted from smallest to largest string lexicographically
*)

let rec max_sort (l: string list): string list =
	match l with 
		| [] -> []
		| l -> 
		match remove_max l with
			| Error _ -> assert false
			| Ok (s, l) ->  max_sort l @ [s];;


(* *************
    END PART I
   ************* *)		

(*
	Part II Section 1: for selected functions in Part I, provide a reimplementation of your previous code by refactoring the definition to use combinators provided by the List module.
	
	Care should be taken to use a concise, elegant combination of these provided functions to best express the task.
	
	These new implementations should not be explicitly recursive.	Note that the autograder is not aware if you cheated and used recursion; we will manually inspect your code and give you negative points if 
	you used recursion.
*)

let iota1' (n: int): int list = 
	unimplemented ()

let iota2' (n: int): int list =
	unimplemented ()

let factors' (n: int): int list =
	unimplemented ()

let insert_string' (s: string) (l: string list): string list =
	unimplemented ()

(*
Part II Section 2: Checking validity of a Towers game solution

Towers is a simple puzzle game, for rules and to play the game see
https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/towers.html

Here is a description taken from that site:

Your task is to build a tower on every square, in such a way that:
* Each row contains every possible height of tower once
* Each column contains every possible height of tower once
* Each numeric clue describes the number of towers that can be seen if you look into the square from that direction, assuming that shorter towers are hidden behind taller ones. For example, in a 5×5 grid, a clue marked ‘5’ indicates that the five tower heights must appear in increasing order (otherwise you would not be able to see all five towers), whereas a clue marked ‘1’ indicates that the tallest tower (the one marked 5) must come first.
	
Since this assignment is an exercise to become familiar with functional programming, we are going to give appropriate auxiliary functions which will decompose the problem into smaller pieces.  You should be able to compose these short functions to accomplish the ultimate goal of verifying whether a completely-filled grid is a solution based on the clues around the edges.  We will only be verifying the "easy" games which don't leave out any of the clues around the grid edges.
	
Use the List combinators, pipelining, etc. when possible and whenever it improves the readability of the solution.  All but the last function are directly code-able with the List combinators and no `rec`.
As in Part I feel free to write your own helper functions if needed.

If you are unsure on what the requirements of the functions are, look at the test cases we provide.

*)

(* We can represent the tower board itself as a list of lists of ints.  
  See `tower_board_example` in `tests.ml` for an example.

The first task is to determine if a list of lists of integers is "square", i.e. each list is the same length and there are the same number of lists as there are elements in any of the lists. If it is, return Ok(the dimension of the array).  If not, return Error "not square". *)

let square_size (grid: int list list) : (int, string) result =
	unimplemented ()

(* Given a list of integers of length n, determine if the list has exactly one occurrence
	 of each number in 1 .. n in it. Return false if not *)	
let elements_span_range (l : int list) : bool = 
	unimplemented ()

(* Check to see if a towers grid is well-formed, namely
	 1) it is square as per above,
   2) it is at least 1x1 in size (no 0x0 degenerates are allowed)
   2) each row and column spans the range as per above *)
let well_formed_grid (grid : int list list) : bool = 
	unimplemented ()

(* The next six auxiliary functions should only be called on well-formed grids, or rows from well-formed
	 grids, and so you don't need to deal with ill-formed cases there such as 0x0 or non-spanning grid rows.  *)	

(* The lowest level of the validity check for towers requires finding the number of local maxima going down a given row of the grid (i.e. down a list of integers).  Define a function local_max_count to find that value.  *)

let local_max_count (row : int list) : int =  
	unimplemented ()

(* Now we need to apply the above function to each row/column around the grid.  There are many reasonable ways to solve that task, but here we ask you to first write a function verify_left_clues to check the "left side grid" clues only are correct.  For this function the `edge_clues` list is only the left-side clues. *)

let verify_left_clues (grid : int list list) (edge_clues : int list) : bool =
	unimplemented ()

(* In order to check the clues on all four edges, we will rotate the grid counter-clockwise and call the above function at each rotation. 
There many ways we can rotate our grid.  Here we suggest using a combination of transpose (like for a matrix: flip rows and columns), and reflect.  Note you can assume the grid is well-formed.  *)	
let transpose (grid : int list list) : int list list = 
	unimplemented ()

let reflect_vertical_axis (grid : int list list) : int list list =
	unimplemented ()


(* Now it should not be difficult to define a function to rotate the grid counterclockwise *)
let rotate_ccw (grid : int list list) : int list list = 
	unimplemented ()

(* Finally, write a function verify_towers_solution which given a grid and the clues all around the grid, verifies that the solution is correct with respect to the clues: the grid is well-formed as per above, and the clues are all satisfied by the solution.  
The clues are a list of lists, the first element of the list is the edge_clues for the original board orientation, and the subsequent lists correspond to clues for successive rotations of the grid. 
If either the grid is ill-formed or the clues are not all satisfied, return false.  *)

let verify_towers_solution  (grid : int list list) (four_edge_clues : int list list) : bool = 
  unimplemented ()
