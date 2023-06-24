module Quick where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.List (List, (:))
import Data.List as List
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Random.LCG (Seed, randomSeed)
import Test.QuickCheck.Gen (Gen, runGen)

-- | Test a property, returning all test results as a List, with the Seed that
-- | was used for each result.
-- |
-- | The first argument is the _random seed_ to be passed to the random generator.
-- | The second argument is the number of tests to run.
type PureLoopState =
  { seed :: Seed
  , index :: Int
  , results :: List (Tuple Seed (Effect Unit))
  }

quickCheckToEffect :: Gen (Effect Unit) -> Effect Unit
quickCheckToEffect gen = do
  seed <- randomSeed
  let tests = snd <$> quickCheckToEffect' seed 100 gen
  sequence_ tests

quickCheckToEffect' :: Seed -> Int -> Gen (Effect Unit) -> List (Tuple Seed (Effect Unit))
quickCheckToEffect' s n prop = tailRec loop { seed: s, index: 0, results: mempty }
  where
  loop :: PureLoopState -> Step PureLoopState (List (Tuple Seed (Effect Unit)))
  loop { seed, index, results }
    | index == n = Done (List.reverse (results))
    | otherwise =
        case runGen prop { newSeed: seed, size: 10 } of
          Tuple r { newSeed } ->
            Loop
              { seed: newSeed
              , index: index + 1
              , results: (Tuple seed r) : results
              }
