(*
    Part II: Sys_unix, Stdio, and making a useful utility
*)

(*
    You will be developing a simple command-line application which will count how many occurences of each OCaml language keyword occurs in a directory containing OCaml code.
    
    Given a directory path on the command line (or the current directory if none is given), you should:

    - traverse the directory recursively to find each OCaml source file (.ml or .mli)

    - filter out any comments and literal strings from the code

    - count the number of occurences of each keyword in the file: sum up all
    of the occurrences over all files and sort the resulting dictionary
    from most to least common occurrence.

    - report the total sum of keyword counts to stdout in JSON format, as a JSON structure of the form:
         [ { "keyword": <word>,
            "count": <number>
           }, 
           { "keyword": <word>,
            "count": <number>
           } ... ]

    Before counting keywords you should remove all comments and string literals; for example 

    (* 
       remove this text in case there is a match with a keyword here by mistake
    *)
    
    and 
    
    "remove this text too in case there is a match with a keyword in this literal string"
    
    Note that nested comments are supported in OCaml, so be sure to keep track of nesting.  
    
    Also, take into account comments spanning multiple lines.  You can assume literal strings are on one input line only, and can assume there are no escaped double quotes (which allow string literals to themselves contain double quotes).
    
    Once comments/literals are removed, you need to count each occurence of a keyword in the file which is delimited on both ends by non-characters. So for example `fun iffy -> 0` does not contain the keyword `if` since it is not delimited on both ends by non-characters, but `if x > 0` does.
      
    Libraries for you to use:

    Any of the default set of libraries for the course may be used.
    Core.Sys, Core_unix and Stdio provide basics for filesystem usage and accessing argv.

    You are of course welcome to use your Part I dict functions.

    Yojson provides JSON manipulation for output.  Also, we recommend 
    `ppx_deriving_yojson` as a way to convert your own record types into 
    JSON and then strings directly.  The assignment contains links to the docs.

    Note: don't use any other opam libraries beyond the official opam libraries
    on the FPSE Coding page.

    `jq` is a command-line JSON tool which could come in handy for debugging.
*)

(*
	We are providing you with some template code below.

	OCaml executables work by simply evaluating each top-level expression in the file, similar to a scripting language conceptually.  So all the let () = code will run.

	Feel free to modify the code below as much as you want.	Should you need or want more of the standard set of libraries	than just `Core`, `Core_unix`, and `Stdio` (and your own implementation of Part I), you will need to modify the dune file to specify this.

    Note that if you are using `ppx_deriving_yojson` you will both need to list the yojson library and also add
      
    (preprocess
    (pps ppx_deriving_yojson))

    to the executable build in the dune file to enable the ppx macros.

*)

open Core

(* Here is the official keyword list from the docs *)
(* 
  [
    "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"; "downto"; 
    "else"; "end"; "exception"; "external"; "false"; "for"; "fun"; "function"; "functor"; 
    "if"; "in"; "include"; "inherit"; "initializer"; "land"; "lazy"; "let"; "lor"; "lsl"; 
    "lsr"; "lxor"; "match"; "method"; "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; 
    "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; 
    "val"; "virtual"; "when"; "while"; "with";
  ]
*)

(* 
  As with C, the first argv is always the name of the executable, that's why we match on the second element in the list instead 
*)
(* let output target_dir = Stdio.In_channel.read_all target_dir;; *)
(* read content of file line by line *)
let read_file file =
  let cnt = ref 0 in
  let rec append_line acc file_lines = 
    match file_lines with
    | [] -> acc
    | h::t -> 
      (* trim the spaces before and after the line *)
      let trimmed_line = String.strip h in
      if String.is_prefix trimmed_line ~prefix:"\"" then
        append_line acc t
      else
     (* use a cnt to store the number of (; set cnt = cnt + 1 when meet "(", set cnt = cnt - 1 when meet ")"
        exclude all line when cnt > 0
     *)
        if String.is_prefix trimmed_line ~prefix:"(*" then
          (cnt := 1 + !cnt;
          append_line acc t)
        else
          if String.is_suffix trimmed_line ~suffix:"*)" then
            (cnt := !cnt - 1;
            append_line acc t)
          else
            if !cnt > 0 then
              append_line acc t
            else
              append_line (trimmed_line::acc) t 
            in
  let lines = Stdio.In_channel.read_lines file
  (* append the line only if it is not a comment or a string literal *)
      in 
   let res = append_line [] lines in
   (* generate a string list from split a string by space *)
  String.concat ~sep:" " res |> String.split ~on:' '


(* type ts = t Simpledict.dict [@@deriving yojson] *)
type occurence_list = (string * int) list [@@deriving yojson];;

let () =
  let target_dir = 
    match Sys.get_argv () |> Array.to_list with
    | _ :: dir :: _ ->  dir
    | _ -> Core_unix.getcwd ()
  in 
  read_file target_dir |> Simpledict.count_keywords |> Simpledict.assoc_of_dict |> Simpledict.sort |> occurence_list_to_yojson |> Yojson.Safe.to_string |> Stdio.print_endline
  
  (* let a = t_to_yojson [("A", 1),("B",42),("C",42)] |> Yojson.Safe.to_string |> Stdio.print_endline *)
(* type t = (string * int) list [@@deriving yojson]
let a = print_endline (Yojson.Safe.to_string (t_to_yojson [("foo", 42)]));; *)


(* print_endline (Yojson.Safe.to_string (v_to_yojson [("foo", 42); ("bar", 1)]));; *)
  (* Stdio.printf "Target dir: %s\n" target_dir *)
  (* let _ = List.iter target_dir ~f:(fun x -> Stdio.printf "%s " x) in *)
  (* ()  *)


(* 
    A rough idea of what the top-level program could be is something like:

		target_dir
    |> get_path_elts
    |> count_keywords
    |> keyword_counts_to_json_string
    |> print_string 

    Please make sure to break the tasks down into separate functions for each distinct task.  Put any auxiliary functions in simpledict.ml and simpledict.mli so they can be tested as well.

*)
