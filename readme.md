# Advent of Code 2021

This repository contains my solutions to the [Advent of Code 2021](https://adventofcode.com/2021) challenges.

Days 1-9 are in [Haskell](https://www.haskell.org/).

The rest are in [Rust](https://www.rust-lang.org/).

## Haskell run instructions
Install GHC 9.2.4 and Cabal 3.6.2.0 (with [GHCup](https://www.haskell.org/ghcup/))
```sh
cd 01
cabal run part1
cabal run part2
```

## Rust run instructions
Install rustc 1.63.0 (with [rustup](https://www.rust-lang.org/tools/install)) although should work with all later and most earlier versions
```sh
cd 10
cargo run --release --bin part1
cargo run --release --bin part2
```

