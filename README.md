Advent of Code 2016
-------------------

There are many sets of solutions. This one is mine.

The code is structured as a wrapper/driver module (`src/aoc.ml`), a separate
module for each day that implements a common module type (`src/dayNN.ml`) and a
couple of utility modules (`src/utils.ml` for general stuff, `src/font.ml` for
some day 8 hackery). The `inputs/` and `answers/` dirs are a place to put your
input data (`inputs/dayNN.txt`) and expected answers (`answers/dayNN.txt`, one
line for each part) so that `check_day.sh` can verify that the solutions still
work.

You should be able to make this work by cloning the repo, installing the
dependencies (`angstrom` and `cmdliner`) with opam, and running `make` followed
by `./aoc.byte <day number> <input file> <part number>` with an optional `-q`
to suppress most output.
