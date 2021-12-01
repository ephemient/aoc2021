# [Advent of Code 2021](https://adventofcode.com/2021)
### my answers in [Haskell](https://www.haskell.org/) ![Haskell CI](https://github.com/ephemient/aoc2021/workflows/Haskell%20CI/badge.svg)

This project builds with [The Haskell Tool Stack](https://haskellstack.org/).

Setup:

```sh
curl -sSL https://get.haskellstack.org/ | sh -s -
stack setup
```

Run the [Hspec](https://hspec.github.io/) test suite:

```sh
stack test aoc2021:test:aoc2021-test
```

Run [criterion](http://www.serpentine.com/criterion/) benchmarks ([results online](https://ephemient.github.io/aoc2021/aoc2021-bench.html)):

```sh
stack bench aoc2021:bench:aoc2021-bench
```

Print solutions for the inputs provided in local data files:

```sh
stack build aoc2021:exe:aoc2021-exe --exec aoc2021-exe
```

Generate [Haddock](https://www.haskell.org/haddock/) API documentation:

```sh
stack haddock aoc2021:lib
```

Run [hlint](https://github.com/ndmitchell/hlint) source code suggestions:

```sh
stack build hlint --exec 'hlint src test bench'
```
