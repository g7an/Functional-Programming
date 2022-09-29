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

  (* let find_comment_sign lst = 
    let rec find_double_quote_helper lst index = 
      match lst with
      | [] -> None
      | h::t -> if String.(h="(*") then Some index else find_double_quote_helper t (index + 1)
    in find_double_quote_helper lst 0 *)
(* 
  As with C, the first argv is always the name of the executable, that's why we match on the second element in the list instead 
*)
(* convert string into string list *)
let string_to_list str = 
  let rec string_to_list_helper str index = 
    if index = String.length str then [] else (String.get str index)::(string_to_list_helper str (index + 1))
  in string_to_list_helper str 0

(* iterate through string list. Flip the boolean to false if a double quote occur. If the boolean is false, then we don't add the string to the result list *)
let remove_literal_string lst =
  let rec remove_literal_string_helper lst acc has_quote = 
    match lst with 
    | [] -> acc
    | h::t -> if has_quote then 
                if Char.(h='"') then remove_literal_string_helper t acc false
                else remove_literal_string_helper t (acc @ [h]) has_quote
              else 
                if Char.(h='"') then remove_literal_string_helper t acc true
                else remove_literal_string_helper t acc has_quote
  in remove_literal_string_helper lst [] true;;


(* iterate through string list. Use a count to store the occurence of the comment sign. Add to count if left comment sign occur. 
Minus from the count if right comment sign occur. If the count is greater than 0, then we don't add the string to the result list
Otherwise we add the string to the result list *)
let remove_comment lst =
  let rec remove_comment_helper lst acc count = 
    match lst with 
      | h1::h2::t ->  if Char.(h1='(') && Char.(h2='*') then remove_comment_helper t acc (count + 1)
                      else if Char.(h1='*') && Char.(h2=')') then remove_comment_helper t acc (count - 1)
                        else if count > 0 then remove_comment_helper (h2::t) acc count
                          else remove_comment_helper (h2::t) (acc @ [h1]) count
      | _ -> acc @ lst
  in remove_comment_helper lst [] 0;;

let remove_escapes lst = 
  let rec remove_escapes_helper lst acc = 
    match lst with
    | [] -> acc
    | h::t -> if Char.(h='\t') || Char.(h='\n') then remove_escapes_helper t acc
              else remove_escapes_helper t (acc @ [h])
  in remove_escapes_helper lst [];;

(* ['y';'('; '*'; 'a'; '*'; ')'; 'z'; 'h'] *)

(* iterate through string list. If the string is a keyword, then add to the result list. Otherwise, we don't add to the result list *)

(* iterate through a string, append the characters to a list until you find a double quote, then return the list *)
(* read content of file line by line *)
let read_file file =
  (* let cnt = ref 0 in *)
  (* get the file contents as a string *)
  In_channel.read_all file;;
  (* iterate over the lines *)

  (* iterate through a string, append the characters to a list until you find a double quote, then return the list *)
(* let rec get_path_elts path acc =
  let content = read_file path in *)


      (* append the line only if it is not a comment or a string literal *)
(* trim spaces before and after a string *)
(* split string if it has >= 1 whitespaces *)
(* let split_on_whitespaces str = 
  let rec split_on_whitespaces_helper str acc = 
    match str with 
    | [] -> acc
    | h::t -> if Char.(h=' ') then split_on_whitespaces_helper t acc
              else split_on_whitespaces_helper t (acc @ [h])
  in split_on_whitespaces_helper str [];; *)


let get_path_elts path =
  let content = read_file path in
  let trimmed_content = String.strip content in
  let content_lst = string_to_list trimmed_content in
  let removed_literal_string = remove_literal_string content_lst in
  remove_comment removed_literal_string |> remove_escapes ;;

(* convert char list to string *)
      (* if String.is_prefix trimmed_line ~prefix:"\"" then
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
  String.concat ~sep:" " res |> String.split ~on:' ' *)


(* type ts = t Simpledict.dict [@@deriving yojson] *)
type occurence_list = (string * int) list [@@deriving yojson];;

(* convert char list to string list by forming new strings with the characters until space is found *)
let rec char_list_to_string_list lst acc str =
  match lst with
  | [] -> if String.(str<>"") then acc@[str] else acc
  | h::t -> if Char.(h=' ') then char_list_to_string_list t (acc@[str]) ""
            else char_list_to_string_list t acc (str ^ String.of_char h);;

type result = { keyword: string; count: int } [@@deriving yojson];;
type result_list = result list [@@deriving yojson];;

(* iterate through the string list, if the string is a keyword, then add to the result list. Otherwise, we don't add to the result list *)

(** convert (string * int) list to result list  *)
let rec assoc_to_result lst acc =
  match lst with
  | [] -> acc
  | (k, v)::t -> assoc_to_result t (acc @ [{keyword = k; count = v}]);;
  (* [("else",2); ("if",2); ("then",2);("let",1);("rec",1)] *)

(** [dir_is_empty dir] is true, if [dir] contains no files except
 * "." and ".."
 *)
 let dir_is_empty dir =
  Array.length (Sys_unix.readdir dir) = 0;;

(** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let find_ocaml_files dir = 
  (* check if a file is a directory *)
  let is_dir file = Sys_unix.is_directory file in
  match (is_dir dir) with
  | `Yes -> (
      if dir_is_empty dir then [] else
      let files = Sys_unix.readdir dir in
      let files = Array.to_list files in
      let files = List.map ~f:(fun x -> dir ^ "/" ^ x) files in
      (* let files = List.filter ~f:(fun x -> not (is_dir x)) files in *)
      let files = List.filter ~f:(fun x -> String.is_suffix x ~suffix:".ml") files in
      files
    )
  | _ -> []
  (* check if a file is a regular file *)


let () =
  let target_dir = 
    match Sys.get_argv () |> Array.to_list with
    | _ :: dir :: _ ->  dir
    | _ -> Core_unix.getcwd ()
  in 
  let dir_list = (find_ocaml_files target_dir) in
  let rec helper dir_list result =
    match dir_list with
    | [] -> print_endline "No .ml files found"
    | file::tl -> 
      (
        (* print list of string *)
        let char_list = get_path_elts file in 
        let lst = char_list_to_string_list char_list [] "" |> Simpledict.count_keywords |> Simpledict.assoc_of_dict |> Simpledict.sort in 
        (* Simpledict.count_keywords |> Simpledict.assoc_of_dict |> Simpledict.sort |> occurence_list_to_yojson |> Yojson.Safe.to_string |> Stdio.print_endline;; *)
        let keyword_counts_to_json_string = assoc_to_result lst [] |> result_list_to_yojson |> Yojson.Safe.to_string |> Stdio.print_endline in
        keyword_counts_to_json_string; helper tl result
      )
  in helper dir_list [];;
  


  (* Stdio.printf "result: %s\n" content;; *)
  (* read_file target_dir |> List.iter ~f:(fun x -> print_endline (String.concat ~sep:" " x)); *)
  (* read_file target_dir |> Simpledict.count_keywords |> Simpledict.assoc_of_dict |> Simpledict.sort |> occurence_list_to_yojson |> Yojson.Safe.to_string |> Stdio.print_endline *)
  
  (* convert [[keyword1, count1], [keyword2, count2], ...] 
  to json format
  [ { "keyword": <word>,
  "count": <number>
 }, 
 { "keyword": <word>,
  "count": <number>
 } ... ] *)


(* let convert res =
  let rec convert_helper res acc =
    match res with
    | [] -> acc
    | h::t -> 
        convert_helper t (acc ^ "{ \"keyword\": \"" ^ (fst h) ^ "\", \"count\": " ^ (string_of_int (snd h)) ^ " }, ")
      
  in
  "[" ^ (convert_helper res "") ^ "]" *)
 (* let val =  [("k1", 1); ("k2", 2)]  *)

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
