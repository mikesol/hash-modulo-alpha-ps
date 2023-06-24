{-# LANGUAGE DeriveTraversable #-}

module ManualBenchmark where

import Prelude

import Benchmark as Benchmark
import Data.Foldable (class Foldable, foldMap, foldMapDefaultL, foldl, foldr, traverse_)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Hashable (hash)
import Data.Int (round)
import Data.List (List(..), groupBy, head, sortBy)
import Data.List as List
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Console (log)
import Expr (Expr)
import Exprs.Bert (berts)
import Exprs.GMM as GMM
import Exprs.MNISTCNN as MNISTCNN
import Node.Path (FilePath)

data Expressions a = Expressions
  { eMnistCNN :: a
  , eGMM :: a
  , eBERTs :: List a
  }

instance Functor Expressions where
  map
    f
    ( Expressions
        { eMnistCNN
        , eGMM
        , eBERTs
        }
    ) = Expressions
    { eMnistCNN: f eMnistCNN
    , eGMM: f eGMM
    , eBERTs: map f eBERTs
    }

instance Foldable Expressions where
  foldr
    f
    z
    ( Expressions
        { eMnistCNN
        , eGMM
        , eBERTs
        }
    ) = f eMnistCNN (f eGMM (foldr f z eBERTs))
  foldl
    f
    z
    ( Expressions
        { eMnistCNN
        , eGMM
        , eBERTs
        }
    ) = foldl f (f (f z eMnistCNN) eGMM) eBERTs
  foldMap = foldMapDefaultL

instance Traversable Expressions where
  traverse
    f
    ( Expressions
        { eMnistCNN
        , eGMM
        , eBERTs
        }
    ) = Expressions <$> ({ eMnistCNN: _, eGMM: _, eBERTs: _ } <$> f eMnistCNN <*> f eGMM <*> traverse f eBERTs)
  sequence = sequenceDefault

expressions :: Expressions (Tuple String (Expr Unit String))
expressions = Expressions
  { eMnistCNN: "mnistcnn" /\ MNISTCNN.expr
  , eGMM: "gmm" /\ GMM.expr
  , eBERTs: mapWithIndex (\i a -> ("bert" <> show (i + 1)) /\ a) berts
  }

expressionGenerators :: Expressions Benchmark.ExpressionGenerator
expressionGenerators = flip map expressions $ \(expressionName /\ expr) ->
  Benchmark.ExpressionGenerator
    { bcGenExpr: Benchmark.iteratorPure $ do
        -- Pre-hash the variables, since our benchmark functions
        -- expect Int variables.  This doesn't priviledge any
        -- algorithm over any other.
        pure (map hash expr)
    , bcGenName: expressionName
    }

testcase_path :: String -> FilePath
testcase_path = \name -> "./exprs/" <> name <> ".expr"

process_stats :: Benchmark.AggregateStatistics -> (Int /\ Int)
process_stats aggregate_stats =
  let (_ /\ mean /\ _ /\ _ /\ stddev) = Benchmark.stats aggregate_stats in (round mean /\ round stddev)

specific_benchmarks :: Benchmark.BenchmarkParams -> Effect Unit
specific_benchmarks bps = do
  results <- Benchmark.benchmarkResults (List.fromFoldable expressionGenerators) bps

  log ("Outputs are algorithm name: [(size, time in seconds)]")

  let
    grouped :: List (List (String /\ String /\ (Int /\ Number)))
    grouped = map toList $ groupBy ((==) `on` fst3) $ sortBy (comparing fst3) $ flip foldMap results $ \(results' /\ genName) ->
      flip foldMap results' $ \((algorithmName /\ _ /\ _) /\ results'') -> case head results'' of
        Nothing -> Nil
        Just results''' -> pure (algorithmName /\ genName /\ results''')

  flip traverse_ grouped $ \algorithmName_results' -> do
    case head algorithmName_results' of
      Nothing -> pure unit
      Just (algorithmName /\ _ /\ _) -> do

        log ("Algorithm " <> algorithmName <> ":")
        flip traverse_ algorithmName_results' $ \(_ /\ genName /\ results'') -> do
          let
            (_ /\ minSecs) = results''
            minMicros = minSecs * 1000.0 * 1000.0

          log (genName <> ": " <> show (round minMicros :: Int))
    log ""
  where
  fst3 (x /\ _ /\ _) = x
