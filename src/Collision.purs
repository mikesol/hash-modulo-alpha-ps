{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Collision where

import Prelude

import AlphaHashOptimizedHash16Bit as AlphaHashOptimizedHash16Bit
import Benchmark as Benchmark
import Data.Hashable (class Hashable)
import Data.Int (pow, toNumber)
import Data.List (foldMap, intercalate, zip)
import Data.List as List
import Data.Traversable (class Foldable, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Expr (Expr, annotation)
import Hash as Hash
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path as Path
import TmpDir (createTempDirectory)

-- import System.Effect.Temp (createTempDirectory, emptyTempFile)

genNotAlphaEquiv :: forall m h a . Monad m => Hashable a=> Ord a=> Eq h
                 => m (Expr h a)
                 -> m (Expr h a /\ Expr h a)
genNotAlphaEquiv gen = do
  expr1 <- gen
  expr2 <- gen

  if Hash.alphaEquivalentAccordingToUniquifyBinders expr1 expr2
    then genNotAlphaEquiv gen
    else pure (expr1 /\ expr2)

countCollisions :: (Int -> Effect (Expr Unit Int /\ Expr Unit Int)) -> Int -> Int -> Effect Number
countCollisions generate numNodes numBigIters = do
  let numHashcodes :: Int
      numHashcodes = 2 `pow` (16 :: Int)
      numTrials :: Int
      numTrials = numBigIters * numHashcodes

  (_ /\ collisionCount) <- Benchmark.times numTrials ((0 :: Int )/\ (0 :: Int)) $ \(loopCount /\ collisionCount) -> do
    when (loopCount `mod` 100 == 0) $ do
      let percentComplete :: Number
          percentComplete = toNumber loopCount / toNumber numTrials * 100.0

      log ("Iteration " <> show loopCount <> "/" <> show numTrials
               <> " (" <> show percentComplete <> "% complete)")

    ((expr1 :: Expr Unit Int) /\ (expr2 :: Expr Unit Int)) <- generate numNodes

    let h1 = annotation (AlphaHashOptimizedHash16Bit.alphaHash expr1)
        h2 = annotation (AlphaHashOptimizedHash16Bit.alphaHash expr2)
        collisionCount' =
          if h1 == h2
          then collisionCount + 1
          else collisionCount

    pure ((loopCount + 1) /\ collisionCount')

  pure ((toNumber collisionCount) / (toNumber numTrials) * (toNumber numHashcodes))

unwords :: forall f. Foldable f => f String -> String
unwords = intercalate " "
unlines :: forall f. Foldable f => f String -> String
unlines = intercalate "\n"
collisions :: Int -> Effect Unit
collisions numBigIters =
  let numsNodes = List.fromFoldable [128, 256, 512, 1024, 2048, 4096]
      getNumCollisions generate = traverse (\numNodes -> countCollisions generate numNodes numBigIters) numsNodes
  in do
    resultsRandom  <- getNumCollisions (\numNodes -> genNotAlphaEquiv (Hash.genExprWithVarsSize numNodes 10))
    resultsHard    <- getNumCollisions Hash.genExprAdversarialPair
    results        <- pure (zip numsNodes (zip resultsRandom resultsHard))

    let textOutput = flip foldMap results $ \(numNodes /\ (resultRandom /\ resultHard)) ->
          unwords [show numNodes, show resultRandom, show resultHard, "1", show (10 * numNodes), "\n"]

    resultsDir <- createTempDirectory
    let
      datFile    = Path.concat [resultsDir, "collisions.dat"]
      gnuplotFilename = Path.concat [resultsDir, "collisions.gnuplot"]
      gnuplotPdfFilename = Path.concat [resultsDir, "collisions-pdf.gnuplot"]
      outputPdf = Path.concat [resultsDir, "collisions.pdf"]

    FS.writeTextFile UTF8 datFile textOutput
    FS.writeTextFile UTF8 gnuplotFilename (gnuplotFileContents datFile)
    FS.writeTextFile UTF8 gnuplotPdfFilename (gnuplotFilePdf outputPdf datFile)

    log ("I put stuff in " <> resultsDir)
    log "If you have an X server and you want a live graph view run:"
    log ("gnuplot --persist " <> gnuplotFilename)
    log "If you want to generate a PDF run:"
    log ("gnuplot " <> gnuplotPdfFilename)
    log ("You will find the output PDF in " <> outputPdf)

gnuplotFileContents :: String -> String
gnuplotFileContents datFile =
  unlines [ "set key at graph 0.445, 0.985"
          , "set logscale xy 10"
          , "set xrange [120:4500]"
          , "set yrange [0.5:65536]"
          , "set xlabel \"Expression size\""
          , "set ylabel \"Number of collisions per 2^{16} trials\""
          , "plot "
            <> intercalate ", " [ quote datFile <> " using 1:2 title \"Empirical (random expr.)\" with linespoints linestyle 1"
                                , quote datFile <> " using 1:3 title \"Empirical (adversarial expr.)\" with linespoints linestyle 2"
                                , quote datFile <> " using 1:4 title \"Perfectly random hash\" with lines linestyle 4"
                                , quote datFile <> " using 1:5 title \"Bound from Theorem 6.7\" with lines linestyle 6" ]
          ]
  where quote s = "\"" <> s <> "\""

gnuplotFilePdf :: String -> String -> String
gnuplotFilePdf outputPdf datFile = unlines
  [ "set terminal pdf font \"Helvetica,13\""
  , "set output \"" <> outputPdf <> "\""
  , gnuplotFileContents datFile
  ]
