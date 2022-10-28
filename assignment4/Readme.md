Assignment 4: N-Grams and a real app
------------------------------------

### Part I

For Part I of this assignment you will be writing various modules and functions which will lead to a n-gram model generator.  This is Exercises 1-6 in the file `src/lib.ml`.

You will also need to write tests with good coverage for exercises 1-6 in your Part I submission.

### Part II

For Part II you will make a standalone app which uses the functions from part I.  Note if you need more library functions for your app, put them in `lib.ml`.

You will need to continue to keep good unit test coverage on your library code.  Note you do not need to make any acceptance tests for the executable itself, but you should test any auxiliary library functions which are amenable to unit testing.

In addition, for your exercise 6 sanitizer answer write a `Base_quickcheck` random test as one of your OUnit tests following the [Quickcheck lecture](https://pl.cs.jhu.edu/fpse/lecture/specification-test.html#quickcheck).  To partially verify the random test data follows the specification just perform sanity checks, e.g. verify there are no "%" etc in the output, and that no words in the input were dropped.

As usual we will give two due dates for the two parts.

### The file structure

* [Use the this zip file](https://pl.cs.jhu.edu/fpse/assignments/assignment4.zip) for your assignment. 
* Like assignment 1-3, we are giving you a skeleton to fill in.  Your Part I answers will go in the file  `src/lib.ml` and Part II will be in  `src/ngrams.ml` (plus, add any new auxiliary functions needed to Part I's `lib.ml` for unit testing).  You will also need to make a unit tester in `tests/tests.ml` following the previous assignments.

### Submission and Grading
* As usual, run a final `dune clean; dune build` and then upload `_build/default/assignment4.zip` to Gradescope.