(*
  Part II:

  Implement an executable `ngrams.exe` which can use n-gram models in several ways.   
  It should expect to be called with the following arguments, with bracketed ones optional:

    $ ngrams.exe N CORPUS-FILE [--sample SAMPLE-LENGTH [INITIAL-WORDS...]] [--most-frequent N-MOST-FREQUENT]

  
  Functionality should be as follows:

  - Load the file specified by `CORPUS-FILE` and split its contents into a sequence of strings based on whitespace. 
    Treat newlines and spaces, etc. equally.

  - Sanitize each of the strings in this sequence according to the `sanitize` function, 
    removing all strings which end up as `None` in this way, to produce a new sequence.

  - Initialize an n-gram distribution using `N` and the sanitized sequence of words.

  
  If the option `--sample SAMPLE-LENGTH` is provided:

    To stdout, output a sequence of `SAMPLE-LENGTH` words randomly sampled from the n-gram model.  Print them out separated by single spaces. 
    
    To begin the sequence, use the `INITIAL-WORDS` arguments provided after `--sample` to seed the sequence, or if none are provided, 
    choose a random starting n-gram to begin. 
    You may assume that the words provided as `INITIAL-WORDS` are already sanitized, and that there are at least (`N` - 1) of them.

  If the option `--most-frequent N-MOST-FREQUENT` is provided:
  
    To stdout, output a sorted JSON-formatted array of length `N-MOST-FREQUENT` containing information about the most common n-grams 
    (NB: NOT n - 1 gram) seen in the `CORPUS-FILE`, like so:

    [
      { 
        "ngram": ["array", "of", "strings", ...],
        "frequency": <number of times witnessed>
      },
      ...
    ]

  Frequency ties should be broken by n-gram alphabetical order. 

  You may assume that only one of `--sample` or `--most-frequent` will be supplied at a time, and that at least one will be given.

  To output JSON, you may again use yojson and `ppx_deriving_yojson`, 
  and to parse command line arguments, we recommend looking into the `Core.Command` module.  
  See Real World OCaml Chapter 14 https://dev.realworldocaml.org/command-line-parsing.html for some examples of Core.Command in action.
*)

open Core
(* let content = 
  "The quick brown fox jumps over the lazy dog." *)

(* check if a string is a space *)

let get_contents filename =
    match filename with
    | "-" -> In_channel.input_all In_channel.stdin
    | filename -> In_channel.read_all filename

let split_on_whitespace s =
  let is_whitespace char = 
    match char with
    | ' ' | '\n' | '\t' -> true
    | _ -> false
  in
  (* split if is_whitespace char is True *)
  let rec split_on_whitespace' s acc cur_string =
    match s with
    | "" -> if (String.length cur_string = 0) then acc else acc @ [cur_string]
    | _ -> 
      let c = String.get s 0 in
      if is_whitespace c then
        split_on_whitespace' (String.sub s ~pos:1 ~len:(String.length s - 1)) (acc@[cur_string]) ""
      else
        split_on_whitespace' (String.sub s ~pos:1 ~len:(String.length s - 1)) acc (cur_string ^ (String.make 1 c))
  in
  split_on_whitespace' s [] ""

let rec get_sanitized str_list = 
  match str_list with
  | [] -> []
  | hd::tl -> 
    match Lib.sanitize hd with
    | None -> get_sanitized tl
    | Some s -> s::(get_sanitized tl)

let get_random_init_words str_list key_count = 
  (* repeat to add random string from str_list to list for key_count times *)
  let rec get_random_init_words' str_list key_count acc = 
    match key_count with
    | 0 -> acc
    | _ -> 
      let len = List.length str_list in
      let rand = Random.int len in
      get_random_init_words' str_list (key_count - 1) (acc @ [List.nth_exn str_list rand])
  in
  get_random_init_words' str_list key_count []

let count_word (value_list: 'a list): (('a * int) list) = 
let rec count_word_helper value_list acc = 
  (* count the occurrence of value in value_list *)
  match value_list with
  | [] -> acc
  | hd::tl -> 
    (* find a tuple in acc with first element equal to hd *)
    match List.find_exn acc ~f:(fun (x, _) -> String.(x=hd)) with 
    | (x, y) -> count_word_helper tl ((x, y + 1)::(List.filter acc ~f:(fun (x, _) -> String.(x<>hd))))
    | exception Not_found_s _ -> count_word_helper tl ((hd, 1)::acc)
  in count_word_helper value_list []

  (* let sorted_value_count_tuple = List.sort value_count_tuple ~compare:(fun (_, x) (_, y) -> Int.compare y x) *)
module R = Random
module NGrams = Lib.N_grams (R) (String)
let get_preprocessed_corpus filename = 
  get_contents filename |> split_on_whitespace |> get_sanitized 


let generate_distribution n filename = 
  get_preprocessed_corpus filename |> NGrams.ngrams n

let generate_freq_list key value = 
  let value_count_tuple = count_word value in
  let rec generate_freq_list_helper key value_count_tuple res = 
  match value_count_tuple with
  | [] -> res
  | hd::tl -> 
    (* convert key to list *)
    let word, freq = hd in
   generate_freq_list_helper key tl (res@[(key @ [word], freq)])
    (* generate_freq_list_helper key tl (res @ [(key, word)]) *)
in generate_freq_list_helper key value_count_tuple []


let iter_token_list_map distribution = 
  let dist_list = NGrams.Token_list_map.to_alist distribution in
  let rec iter_token_list_map_helper dist_list acc =
    match dist_list with 
    | [] -> acc
    | hd::tl -> 
      let (key, value_in_bag) = hd in
      let value = Bag.to_list value_in_bag in
      let freq_list = generate_freq_list key value
    in iter_token_list_map_helper tl (acc@freq_list)
    in let value_count_tuple = iter_token_list_map_helper dist_list [] in
    List.sort value_count_tuple ~compare:(fun (x1, x) (y1, y) -> 
      if Int.compare y x = 0 then 
        String.compare (String.concat ~sep:" " x1) (String.concat ~sep:" " y1)
      else Int.compare y x)
      
type result = { ngram : string list; frequency : int } [@@deriving yojson]
type result_list = result list [@@deriving yojson]

let rec assoc_to_result lst acc =
  match lst with
  | [] -> acc
  | (k, v) :: t -> assoc_to_result t (acc @ [ { ngram = k; frequency = v } ])
    
  
  

let generate_sample n content sample_length initial_words = 
  let distribution = generate_distribution n content in
  let samples = 
  NGrams.sample_sequence distribution ~max_length: sample_length ~initial_ngram: initial_words in 
  String.concat ~sep:" " samples |> print_endline 
  

    (* let checksum_from_string buf =
      Md5.digest_string buf |> Md5.to_hex |> print_endline *)
    
  (* let checksum_from_file filename =
    let contents =
      match filename with
      | "-" -> In_channel.input_all In_channel.stdin
      | filename -> In_channel.read_all filename
    in
    Md5.digest_string contents |> Md5.to_hex |> print_endline *)
  
  let command =
    Command.basic
      ~summary:"Generate an MD5 hash of the input data"
      (
          let%map_open.Command sample_flag =
            flag
              "--sample"
              (listed string)
              ~doc:"string Checksum the given string"
          and frequency_flag = flag "--most-frequent" (optional string) ~doc:" run a built-in time trial"
          and 
          n =
          anon ("number of words in n_gram" %: int)
          and filename =
            anon ("filename" %: Filename_unix.arg_type)
          and initial_words_str =
            anon (maybe ("initial words" %: string))
          in
          fun () -> 
          (
            let preprocessed_corpus = get_preprocessed_corpus filename in
            match sample_flag with
            | [] -> 
            (
              match frequency_flag with
                | Some most_freq -> 
                  (
                    let distribution = generate_distribution n filename in
                    let value_count_tuple = iter_token_list_map distribution in
                    let result_list = assoc_to_result value_count_tuple [] in
                    (* take the first most_freq element of result_list *)
                    let most_freq_int = Int.of_string most_freq in
                    let most_freq_result_list = List.take result_list most_freq_int in
                    let result_list_json = result_list_to_yojson most_freq_result_list in
                    Yojson.Safe.pretty_to_channel stdout result_list_json
                  )
                | None -> 
                  (* TODO: implement when all labeled flag are missing *)
                  (* iter_token_list_map (generate_distribution n filename) |> List.iter ~f:(fun x -> print_endline x) *)
                  failwith "One of --sample or --most-frequent flag should be provided"
            )
            | sample_length::_ -> 
              let initial_words = 
                match initial_words_str with
                | Some s -> split_on_whitespace s
                | None -> get_random_init_words preprocessed_corpus (n-1) in 
              generate_sample n filename (int_of_string(sample_length)) initial_words
          )
        )
            
  let () = Command_unix.run command

(* let () = 
(* convert list to string *)
  generate_sample 3 "./src/md5.ml" 4  ["1"; "2"] *)