module Main where

import Prelude

import Benchmark as Benchmark
import Collision as Collision
import ManualBenchmark as ManualBenchmark
import Data.Array as Array
import Effect (Effect)
import Effect.Console (log)
import Hash as Hash
import Node.Process as Process


main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case cliArgs of
    ["manual", "fast"] -> ManualBenchmark.specific_benchmarks Benchmark.fast
    ["manual", "full"] -> ManualBenchmark.specific_benchmarks Benchmark.full
    ["random", "fast"] -> Benchmark.benchmark Benchmark.fast
    ["random", "full"] -> Benchmark.benchmark Benchmark.full
    ["test"] -> Hash.testEverythingInFileStartingWith'prop_'
    ["collisions", "fast"] -> Collision.collisions 1
    ["collisions", "full"] -> Collision.collisions 10
    _ -> log "Unsupported argument"
