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
    You may assume that the words provided as `INITIAL-WORDS` are already sanitized, and that there are **at least** (`N` - 1) of them.

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

let get_contents filename =
    match filename with
    | "-" -> In_channel.input_all In_channel.stdin
    | filename -> In_channel.read_all filename


module R = Random
module NGrams = Lib.N_grams (R) (String)
let get_preprocessed_corpus filename = 
  get_contents filename |> Lib.split_on_whitespace |> Lib.get_sanitized 


let generate_distribution n filename = 
  get_preprocessed_corpus filename |> NGrams.ngrams n


let generate_sample n filename sample_length initial_words = 
  (* get last (n-1) items in initial_words *)
  let len = List.length initial_words in
  let start = len - (n-1) in
  let query = List.slice initial_words start len in
  let distribution = generate_distribution n filename in
  let samples = 
  NGrams.sample_sequence distribution ~max_length: (sample_length-start) ~initial_ngram: query in 
  if (start = 0) then samples else (
    let stub = List.slice initial_words 0 start in
    stub @ samples
  )
    
      
type result = { ngram : string list; frequency : int } [@@deriving yojson]
type result_list = result list [@@deriving yojson]


let assoc_to_result lst =
  let rec assoc_to_result_helper lst acc =
  match lst with
  | [] -> acc
  | (k, v) :: t -> assoc_to_result_helper t (acc @ [ { ngram = k; frequency = v } ])
  in assoc_to_result_helper lst []


let generate_result_list distribution most_freq =
  let most_freq_int = Int.of_string most_freq in
  let value_count_tuple = Lib.iter_token_list_map distribution in
  let result_list = assoc_to_result value_count_tuple in
  (* take the first most_freq element of result_list *)
  let most_freq_result_list = List.take result_list most_freq_int in
  let result_list_json = result_list_to_yojson most_freq_result_list in
  Yojson.Safe.to_string result_list_json |> print_endline

  
let command =
  Command.basic
    ~summary:"Generate n-gram distribution from a corpus"
    (
        let%map_open.Command sample_flag =
          flag
            "--sample"
            (optional string)
            ~doc:"string [string list] Sample the given corpus with the optional initial words"
        and frequency_flag = flag "--most-frequent" (optional string) ~doc:"string Return the most frequent n-grams"
        and n = anon ("num_n_gram" %: int)
        and filename =
          anon ("filename" %: Filename_unix.arg_type)
        and initial_words_str_list =
          anon (sequence ("initial words" %: (string)))
        in
        fun () -> 
        (
          (* let preprocessed_corpus = get_preprocessed_corpus filename in *)
          let distribution = generate_distribution n filename in
          match sample_flag with
          | None -> 
          (
            match frequency_flag with
              | Some most_freq -> (
                let result_list = generate_result_list distribution most_freq in
                result_list
              )
              | None -> 
                (* iter_token_list_map (generate_distribution n filename) |> List.iter ~f:(fun x -> print_endline x) *)
                failwith "One of --sample or --most-frequent flag should be provided"
          )
          | Some sample_length -> 
            (
              let initial_words = 
                match initial_words_str_list with 
                | [] -> Lib.get_random_init_words distribution
                | _ -> initial_words_str_list in
              let result = generate_sample n filename (int_of_string(sample_length)) initial_words
              in String.concat ~sep:" " result |> print_endline
            )
        )
      )
          
let () = Command_unix.run command