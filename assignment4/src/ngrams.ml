(*
  Part II:

  Implement an executable `ngrams.exe` which can use n-gram models in several ways.   It should expect to be called with the following arguments, with bracketed ones optional:

    $ ngrams.exe N CORPUS-FILE [--sample SAMPLE-LENGTH [INITIAL-WORDS...]] [--most-frequent N-MOST-FREQUENT]

  
  Functionality should be as follows:

  - Load the file specified by `CORPUS-FILE` and split its contents into a sequence of strings based on whitespace. Treat newlines and spaces, etc. equally.

  - Sanitize each of the strings in this sequence according to the `sanitize` function, removing all strings which end up as `None` in this way, to produce a new sequence.

  - Initialize an n-gram distribution using `N` and the sanitized sequence of words.

  
  If the option `--sample SAMPLE-LENGTH` is provided:

    To stdout, output a sequence of `SAMPLE-LENGTH` words randomly sampled from the n-gram model.  Print them out separated by single spaces. 
    
    To begin the sequence, use the `INITIAL-WORDS` arguments provided after `--sample` to seed the sequence, or if none are provided, choose a random starting n-gram to begin. You may assume that the words provided as `INITIAL-WORDS` are already sanitized, and that there are at least (`N` - 1) of them.

  If the option `--most-frequent N-MOST-FREQUENT` is provided:
  
    To stdout, output a sorted JSON-formatted array of length `N-MOST-FREQUENT` containing information about the most common n-grams (NB: NOT n - 1 gram) seen in the `CORPUS-FILE`, like so:

    [
      { 
        "ngram": ["array", "of", "strings", ...],
        "frequency": <number of times witnessed>
      },
      ...
    ]

  Frequency ties should be broken by n-gram alphabetical order. 

  You may assume that only one of `--sample` or `--most-frequent` will be supplied at a time, and that at least one will be given.

  To output JSON, you may again use yojson and `ppx_deriving_yojson`, and to parse command line arguments, we recommend looking into the `Core.Command` module.  See Real World OCaml Chapter 14 https://dev.realworldocaml.org/command-line-parsing.html for some examples of Core.Command in action.
*)

open Core

let () =
  print_string "Your implementation here!"