module Benchmark where

import Prelude

import AlphaHashEfficientHash (Hash)
import AlphaHashOptimizedHash as AlphaHashOptimizedHash
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldMapDefaultL, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Hashable (class Hashable, hash)
import Data.Int (floor, toNumber)
import Data.JSDate (getTime, now)
import Data.List (List(..), intercalate, length, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Data.Number.Format as Number
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Expr (Expr, exprSize)
import Exprs.Bert (berts)
import Hash (deBruijnHash, structuralHashNested)
import Hash as Hash
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import TmpDir (createTempDirectory)

-- import System.Clock as Clock
-- import System.Effect.Temp (createTempDirectory, emptyTempFile)
-- import System.Mem as System.Mem
-- import Text.Printf (printf)
-- import Text.Read (readMaybe)

data BenchmarkParams = BenchmarkParams
  { runsToMinimizeOver :: Int
  , minimumMeasurableTime_secs :: Number
  , maximumTime_micro :: Number
  , sizeScale :: Number
  }

data ExpressionGenerator = ExpressionGenerator
  { bcGenExpr :: Iterator Effect (Effect (Expr Unit Int))
  , bcGenName :: String
  }

data Algorithms a = Algorithms
  { aLocallyNameless :: a
  --, aAlphaHashFromPaper       :: a
  , aAlphaHashFromPaperFaster :: a
  , aDeBrujinHash :: a
  , aStructuralHashNested :: a
  }

derive instance Functor Algorithms

instance Foldable Algorithms where
  foldl bab b (Algorithms a) = bab (bab (bab (bab b a.aLocallyNameless) a.aAlphaHashFromPaperFaster) a.aDeBrujinHash) a.aStructuralHashNested
  foldr abb b (Algorithms a) = abb a.aStructuralHashNested (abb a.aDeBrujinHash (abb a.aAlphaHashFromPaperFaster (abb a.aLocallyNameless b)))
  foldMap = foldMapDefaultL

instance Traversable Algorithms where
  traverse amb (Algorithms a) = Algorithms <$>
    ( { aLocallyNameless: _
      , aAlphaHashFromPaperFaster: _
      , aDeBrujinHash: _
      , aStructuralHashNested: _
      } <$> amb a.aLocallyNameless <*> amb a.aAlphaHashFromPaperFaster <*> amb a.aDeBrujinHash <*> amb a.aStructuralHashNested
    )
  sequence = sequenceDefault

algorithms_
  :: forall a h
   . Hashable a
  => Ord a
  => Algorithms (String /\ (Expr h a -> Expr Hash a) /\ String)
algorithms_ = Algorithms
  { aLocallyNameless: ("LocallyNameless" /\ Hash.locallyNameless /\ baseline)
  --, aAlphaHashFromPaper   = ("Ours as in paper (will not have this one on final version)", AlphaHashFastOrigHash.alphaHash, paper)
  , aAlphaHashFromPaperFaster: ("Ours" /\ AlphaHashOptimizedHash.alphaHash /\ good)
  , aDeBrujinHash: ("DeBruijn*" /\ deBruijnHash /\ prettyBad)
  , aStructuralHashNested: ("Structural*" /\ structuralHashNested /\ veryBad)
  }
  where
  veryBad = "red"
  prettyBad = "orange"
  good = "web-green"
  baseline = "web-blue"

--paper     = "purple"

fast :: BenchmarkParams
fast = BenchmarkParams
  { runsToMinimizeOver: 3
  , minimumMeasurableTime_secs: 0.01
  , maximumTime_micro: 1000.0
  , sizeScale: 2.0
  }

full :: BenchmarkParams
full = BenchmarkParams
  { runsToMinimizeOver: 10
  , minimumMeasurableTime_secs: 0.1
  , maximumTime_micro: 1000.0 * 1000.0
  , sizeScale: 1.1
  }

expressionSets :: Number -> List ExpressionGenerator
expressionSets sizeScale_ =
  ( ExpressionGenerator
      { bcGenExpr:
          map (flip Hash.genExprWithVarsUnbalancedSize 10) scaleIterator
      , bcGenName: "unbalanced expressions"
      }
      : ExpressionGenerator
          { bcGenExpr:
              map (flip Hash.genExprWithVarsSize 10) scaleIterator
          , bcGenName: "balanced expressions"
          }
      : ExpressionGenerator
          { bcGenExpr: flip map (iteratorOfList berts) $ \expr -> do
              -- Pre-hash the BERT variables, since our benchmark functions
              -- expect Int variables.  This doesn't priviledge any algorithm
              -- over any other.
              pure (map hash expr)
          , bcGenName: "BERT"
          }
      : Nil
  )
  where -- scaleIterator is a stream of intergers, the next one is the
  -- smallest larger than the previous by a multiple of sizeScale_
  scaleIterator :: forall m. Monad m => Iterator m Int
  scaleIterator =
    fix
      ( \next size -> Iterator $ do
          let size' = floor (toNumber size * sizeScale_) + 1
          pure unit -- cuz PureScript isn't lazy
          pure (Just (size /\ next size'))
      )
      1

-- | This is the entry point to the module.  When run it will
-- benchmark the algorithms on a random set of expressions.  The data
-- from the run will be written out to a directory whose name is
-- displayed at the end of the run.
benchmark :: BenchmarkParams -> Effect Unit
benchmark bps@(BenchmarkParams bps') = do
  benchmarksDir <- createTempDirectory
  results_genNames <- benchmarkResults (expressionSets (bps'.sizeScale)) bps
  flip traverse_ results_genNames $ \(results' /\ genName) -> do
    datasets <- flip traverse results' $ \((algorithmName /\ _ /\ algorithmColor) /\ results) -> do
      let
        textOutput = flip foldMap results $ \(size /\ time) ->
          show size <> " " <> show time <> "\n"

      let filename = Path.concat [ benchmarksDir, algorithmName <> ".dat" ]

      FS.writeTextFile UTF8 filename textOutput

      pure $ PlotDataset
        { pdFile: filename
        , pdTitle: algorithmName
        , pdColor: algorithmColor
        , pdStyle: "7"
        , pdSize: "0.25"
        }

    makeGnuplot benchmarksDir genName datasets

benchmarkResults
  :: List ExpressionGenerator
  -> BenchmarkParams
  -> Effect
       ( List
           ( ( List
                 ( (String /\ (Expr Unit Int -> Expr Hash Int) /\ String) /\
                     (List (Int /\ Number))
                 )
             ) /\
               String
           )
       )
benchmarkResults expressionSets_ bps = do
  let algorithms = List.fromFoldable algorithms_

  flip traverse (enumFrom1 expressionSets_) $ \(i /\ bc'@(ExpressionGenerator bc)) -> do
    let
      expressionSet =
        ( show i <> "/" <> show (length expressionSets_)
            <> " ("
            <> bc.bcGenName
            <> ")"
        )
    results <- benchmarkNoPlot bps expressionSet algorithms bc'
    pure (results /\ bc.bcGenName)

benchmarkNoPlot
  :: forall h a string. BenchmarkParams
  -> String
  -> (List (String /\ (Expr Unit Int -> Expr h a )/\ string))
  -> ExpressionGenerator
  -> Effect (List ((String /\ (Expr Unit Int -> Expr h a )/\ string) /\ (List (Int /\ Number))))
benchmarkNoPlot
  (BenchmarkParams bps')
  expressionSet
  algorithms
  (ExpressionGenerator bc') = do
  let allParams = algorithms

  flip traverse (enumFrom1 allParams) $ \(i /\ algorithm_) -> do
    let (algorithmName /\ algorithm /\ _) = algorithm_
    results <- loop (bc'.bcGenExpr) Nil $ \genExpression rest -> do
      -- We force the expression after generating it.  The Expr type
      -- is strict, that is forcing it forces everything it contains,
      -- therefore no time is wasted forcing it in the hashing
      -- algorithm itself.  On the other hand adding this bang pattern
      -- made absolutely no difference to the benchmarks.  Presumably
      -- the expression is generated already in forced state.  But
      -- it's nice to keep this here for clarity.
      log ("Expression set " <> expressionSet)
      log
        ( "Parameter set "
            <> show i
            <> "/"
            <> show (length allParams)
            <> " ("
            <> algorithmName
            <> ")"
        )
      log ("Generated " <> show (length rest) <> " expressions")
      log ("Generating expression ...")
      expression <- genExpression
      let exprSize' = exprSize expression
      log ("done.  Size was " <> show exprSize')

      let minimumMeasureableTime_micro = bps'.minimumMeasurableTime_secs * 1000.0 * 1000.0

      (repeats /\ firstStats) <- benchmarkUntil minimumMeasureableTime_micro
        1
        (seqHashResult <<< algorithm)
        expression

      r <- benchmarkMore firstStats
        (bps'.runsToMinimizeOver - 1)
        repeats
        (seqHashResult <<< algorithm)
        expression

      let
        (n /\ mean_micro /\ tmin_micro /\ _ /\ stddev_micro) = stats r
        showFloat = Number.toString :: Number -> String

      log ("Count: " <> show n)
      log ("Mean: " <> showFloat mean_micro <> "us")
      log ("Min: " <> showFloat tmin_micro <> "us")
      log ("Std dev: " <> showFloat stddev_micro <> "us")

      let
        done = tmin_micro > bps'.maximumTime_micro
        tmin_secs = tmin_micro / (1000.0 * 1000.0)
        rest' = (exprSize' /\ tmin_secs) : rest

      pure $ (if done then Right else Left) $ rest'

    pure (algorithm_ /\ results)

makeGnuplot :: FilePath -> String -> List PlotDataset -> Effect Unit
makeGnuplot benchmarksDir xlabel results = do

  let
    gnuplotFilename = Path.concat [ benchmarksDir, "benchmarks.gnuplot" ]
    gnuplotPdfFilename = Path.concat [ benchmarksDir, "benchmarks-pdf.gnuplot" ]
    gnuplotFileContent = gnuplotFile xlabel results
    (outputPdf /\ gnuplotPdfFileContent) = gnuplotFilePdf benchmarksDir xlabel results

  FS.writeTextFile UTF8 gnuplotFilename gnuplotFileContent
  FS.writeTextFile UTF8 gnuplotPdfFilename gnuplotPdfFileContent

  log ("I put stuff in " <> benchmarksDir <> ".")
  log "If you have an X server and you want a live graph view run:"
  log ("gnuplot --persist " <> gnuplotFilename)
  log "If you want to generate a PDF run:"
  log ("gnuplot " <> gnuplotPdfFilename)
  log ("You will find the output PDF in " <> outputPdf)

type AggregateStatistics = (Int /\ Number /\ Number /\ Number)

stats :: AggregateStatistics -> (Int /\ Number /\ Number /\ Number /\ Number)
stats (n /\ tsum /\ tsquaredsum /\ tmin) = (n /\ mean /\ tmin /\ variance /\ stddev)
  where
  n' = toNumber  n
  mean = tsum / n'
  variance = tsquaredsum / n' - mean * mean
  stddev = sqrt variance

-- This is probably the entry point you want to use to benchmark an
-- algorithm on a list of expressions each read from a FilePath.
--
-- Runs algorithm on expression and produces aggregate timing
-- statistics.
--
-- benchmarkOne will seq the result of `algorithm expression`.  It is
-- the caller's responsibility to ensure that this causes *all*
-- desired work to be performed.  If you're not sure on this point
-- please ask the author.
benchmarkOne
  :: forall e r. Int
  -> Int
  -> (e -> r)
  -> e
  -> Effect AggregateStatistics
benchmarkOne = benchmarkMore (0 /\ 0.0 /\ 0.0 /\ infinity)
  where
  infinity = 1e60

benchmarkMore
  :: forall e r. AggregateStatistics
  -> Int
  -> Int
  -> (e -> r)
  -> e
  -> Effect AggregateStatistics
benchmarkMore already samplesPerExpression iterationsPerSample algorithm expression =
  times samplesPerExpression already $ \(n /\ t /\ tsquared /\ minSoFar) -> do
    -- System.Mem.performMajorGC
    start <- getTime <$> now
    times iterationsPerSample unit $ \_ ->
      evaluate algorithm expression
    stop <- getTime <$> now

    let
      elapsed_micro = iterationsElapsed_micro / toNumber iterationsPerSample
        where
        iterationsElapsed = stop - start
        iterationsElapsed_nano = iterationsElapsed * 1000000.0
        iterationsElapsed_micro = iterationsElapsed_nano / 1000.0

    pure
      ( (n + 1)
          /\ (t + elapsed_micro)
          /\ (tsquared + elapsed_micro * elapsed_micro)
          /\
            min minSoFar elapsed_micro
      )

benchmarkUntil
  ::forall e r.  Number
  -> Int
  -> (e -> r)
  -> e
  -> Effect (Int /\ AggregateStatistics)
benchmarkUntil minimumMeasurableTime_micro repeats f x = do
  -- System.Mem.performMajorGC
  --gcStart_nano <- map GHC.Stats.gc_elapsed_ns GHC.Stats.getRTSStats
  start <- getTime <$> now
  times repeats unit \_ ->
    evaluate f x
  stop <- getTime <$> now
  --gcStop_nano <- map GHC.Stats.gc_elapsed_ns GHC.Stats.getRTSStats

  let
    iterationsElapsed_micro = iterationsElapsed_nano / 1000.0
      where
      iterationsElapsed = stop - start
      iterationsElapsed_nano = iterationsElapsed * 1000000.0

    elapsed_micro = iterationsElapsed_micro / toNumber repeats

  {-
      gcElapsed_micro = fromIntegral (gcStop_nano - gcStart_nano) / 1000

      showFloat = printf "%.0f" :: Number -> String

  log ("Elapsed: " <> showFloat elapsed_micro <> "us")
  log ("GC Elapsed: " <> showFloat gcElapsed_micro <> "us")
  log ("Productivity: "
            <> showFloat ((1 - (gcElapsed_micro / elapsed_micro)) * 100)
            <> "%")
-}

  if iterationsElapsed_micro < minimumMeasurableTime_micro then benchmarkUntil minimumMeasurableTime_micro (2 * repeats) f x
  else pure
    ( repeats /\
        (1 /\ elapsed_micro /\ elapsed_micro * elapsed_micro /\ elapsed_micro)
    )

gnuplotFilePdf
  :: String
  -> String
  -> List PlotDataset
  -> (String /\ String)
gnuplotFilePdf benchmarksDir xlabel results =
  ( outputPdf /\ unlines
      [ "set terminal pdf font \"Helvetica,13\""
      , "set output \"" <> outputPdf <> "\""
      , gnuplotFile xlabel results
      ]
  )
  where
  outputPdf = benchmarksDir <> "/benchmark.pdf"

unlines :: forall f. Foldable f => f String -> String
unlines = intercalate "\n"

gnuplotFile :: String -> List PlotDataset -> String
gnuplotFile xlabel results =
  unlines
    [ "set xlabel \"Number of nodes in expression (" <> xlabel <> ")\""
    , "set ylabel \"Time taken to hash all subexpressions (s)"
    , "set format y '%.0se%S'"
    , "set format x '%.0se%S'"
    , "set size 1,1"
    , "set logscale xy 10"
    , "set key right bottom"
    , "set yrange [:1]"
    , "plot " <> intercalate ", " (map plotDataset results)
        <> ", "
        <> intercalate ", "
          [ "[x=2.5e3:] x / 100000000 title \"x\" at begin lt rgb \"gray\""
          , "[x=2.5e3:] x**2 / 100000000 title \"x^2\" at begin lt rgb \"gray\""
          , "[x=2.5e3:] x * log(x) ** 2 / 100000000 lt rgb \"gray\" title \"x log^2(x)\" at begin"
          ]
    ]

data PlotDataset = PlotDataset
  { pdFile :: String
  , pdTitle :: String
  , pdColor :: String
  , pdStyle :: String
  , pdSize :: String
  }

plotDataset :: PlotDataset -> String
plotDataset (PlotDataset pd) = intercalate " "
  [ quote (pd.pdFile)
  , "title " <> quote (pd.pdTitle)
  , "lt rgb " <> quote (pd.pdColor)
  , "pt " <> pd.pdStyle
  , "ps " <> pd.pdSize
  ]
  where
  quote s = "\"" <> s <> "\""

-- We apply the argument to the function here.  If we do it at the
-- call site then GHC may float it outside of the timing loop!
-- Therefore it's important that this function not be inlined.
-- It seems it's also important for it to return Effect so as not to be
-- floated outside the timing loop.
{-# NOINLINE evaluate #-}
evaluate :: forall e a. (e -> a) -> e -> Effect Unit
evaluate a e =
  let
    _ = a e
  in
    pure unit

seqHashResult :: forall h a. Expr h a -> Unit
seqHashResult _ = unit

times :: forall s m. Monad m => Int -> s -> (s -> m s) -> m s
times n s f = times_f 0 s
  where
  times_f m s_ =
    if m >= n then pure s_
    else do
      s' <- f s_
      times_f (m + 1) s'

enumFrom1 :: forall a. List a -> List (Int /\ a)
enumFrom1 = mapWithIndex (/\)

loop :: forall m b a. Monad m => Iterator m b -> a -> (b -> a -> m (Either a a)) -> m a
loop iterator a f = do
  mNext <- stepIterator iterator
  case mNext of
    Nothing -> pure a
    Just (b /\ iteratorNext) -> do
      ea <- f b a
      case ea of
        Left aNext -> loop iteratorNext aNext f
        Right aDone -> pure aDone

newtype Iterator m a = Iterator (m (Maybe (a /\ Iterator m a)))

instance (Functor m) =>  Functor (Iterator m) where
  map f (Iterator a) = Iterator (map (map (\(x /\ ia) -> (f x) /\ (f <$> ia))) a)

stepIterator :: forall m a. Iterator m a -> m (Maybe (a /\ (Iterator m a)))
stepIterator (Iterator a) = a

iteratorOfList :: forall m a. Applicative m => List a -> Iterator m a
iteratorOfList = case _ of
  Nil -> Iterator (pure Nothing)
  x : xs -> Iterator (pure (Just (x /\ iteratorOfList xs)))

iteratorPure :: forall m a. Applicative m => a -> Iterator m a
iteratorPure a = Iterator (pure (Just (a /\ Iterator (pure Nothing))))
