# Hashing Modulo Alpha-Equivalence

This repository is a PureScript port of the [supporting code](https://github.com/microsoft/hash-modulo-alpha) for the "Hashing Modulo Alpha-Equivalence" paper.

## Install

```bash
pnpm i && pnpm spago build
```

## Run the tests

```bash
pnpm spago run --exec-args "test"
```

## Run the benchmarks

Don't run the benchmarks. There's lots of iterator-like structures in the benchmarks that assume laziness and blow up in a strict environment. PRs are welcome!