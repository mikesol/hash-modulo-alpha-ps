-- |
-- For example, have a look at example1:
--
--     > import Expr
--     > showExpr example1
--
--     ((lam x ((add x) x)) (lam y ((add y) y)))

module Hash where

import Prelude

import AlphaHashEfficient as AlphaHashEfficient
import AlphaHashEfficientHash (Hash, thenHash)
import AlphaHashEfficientHash as AlphaHashEfficientHash
import AlphaHashInefficient as AlphaHashInefficient
import AlphaHashOptimizedHash as AlphaHashOptimizedHash
import Control.Apply (lift2)
import Control.Monad.Except (runExceptT)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Char (toCharCode)
import Data.Either (Either(..), either)
import Data.Filterable (filter)
import Data.Foldable (for_, sequence_, traverse_)
import Data.Function (on)
import Data.Hashable (class Hashable, hash)
import Data.List (List, groupBy, length, sortBy)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..), toList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Random (randomBool, randomInt)
import Expr (Expr(Var, Lam, App), Path, allHashResults, annotation, mapAnnotation)
import MonadResult ((===))
import Property (Property, Property', PropertyM(..))
import Quick (quickCheckToEffect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

locallyNameless :: forall h a. Hashable a => Ord a => Expr h a -> Expr Hash a
locallyNameless = mapAnnotation hash <<< locallyNamelessExplicit

locallyNamelessExplicit :: forall h a. Ord a => Hashable a => Expr h a -> Expr Hash a
-- Decorates an expression with a
-- hash-code at every node
locallyNamelessExplicit (Var _ n) = Var (hash n) n
locallyNamelessExplicit (App _ e1 e2) = App h he1 he2
  where
  he1 = locallyNamelessExplicit e1
  he2 = locallyNamelessExplicit e2
  h = hash "App" `thenHash` annotation he1 `thenHash` annotation he2

locallyNamelessExplicit e_@(Lam _ n e) = Lam h n (locallyNamelessExplicit e)
  where
  h = hashOnly emptyEnv e_
  -- Yikes!  A second full traversal of e

  -- Does not return a decorated expression, only a hash code
  -- All nested lambdas are dealt with via deBruijn
  hashOnly env (Var _ n') = case lookupEnv env n' of
    Just h' -> hash (3 :: Int) `thenHash` h'
    Nothing -> hash (4 :: Int) `thenHash` hash n'
  hashOnly env (App _ e1 e2) = hash "App" `thenHash` h1 `thenHash` h2
    where
    h1 = hashOnly env e1
    h2 = hashOnly env e2
  hashOnly env (Lam _ n' e') = hash "Lam" `thenHash` h'
    where
    env' = (extendEnv env n')
    h' = hashOnly env' e'

  lookupEnv (_ /\ env) n' = Map.lookup n' env

  extendEnv (i /\ env) n' = ((/\) $ (i + 1)) $ Map.insert n' (hash i) env

  emptyEnv = (0 :: Int) /\ Map.empty

-- | (Broken) DeBruijin Algorithm from "Finding Identical
-- Subexpressions"
deBruijnHash :: forall h a. Hashable a => Ord a => Expr h a -> Expr Hash a
deBruijnHash expr = es
  where
  (_ /\ es) = deBruijnHashExplicit Map.empty expr

deBruijnHashExplicit
  :: forall h a
   . Hashable a
  => Ord a
  => Map.Map a Int
  -> Expr h a
  -> (Hash /\ Expr Hash a)
deBruijnHashExplicit = \env -> case _ of
  Var _ x -> (hash' /\ Var hash' x)
    where
    hash' = case dbLookupVar x env of
      Nothing -> hash "free" `thenHash` x
      Just i -> hash "bound" `thenHash` i
  Lam _ x e -> (hash' /\ Lam hash' x subExpressionHashesE)
    where
    env' = dbAddVar x env
    (hashE /\ subExpressionHashesE) =
      deBruijnHashExplicit env' e
    hash' = hash "lam" `thenHash` hashE
  App _ f e -> (hash' /\ App hash' lF lE)
    where
    (hashF /\ lF) = deBruijnHashExplicit env f
    (hashE /\ lE) = deBruijnHashExplicit env e
    hash' = hash "app" `thenHash` hashF `thenHash` hashE

dbAddVar :: forall k. Ord k => k -> Map k Int -> Map k Int
dbAddVar v env = Map.insert v (Map.size env) env

dbLookupVar :: forall k. Ord k => k -> Map k Int -> Maybe Int
dbLookupVar v env = map (Map.size env - _) (Map.lookup v env)

structuralHashNested :: forall h a. Hashable a => Expr h a -> Expr Hash a
structuralHashNested e = es
  where
  (_ /\ es) = structuralHashNestedExplicit e

structuralHashNestedExplicit
  :: forall h a
   . Hashable a
  => Expr h a
  -> (Hash /\ Expr Hash a)
structuralHashNestedExplicit = case _ of
  Var _ x -> (thisHash /\ Var thisHash x)
    where
    thisHash = hash "Var" `thenHash` x

  Lam _ x e -> (thisHash /\ Lam thisHash x subExpressionHashes)
    where
    (h /\ subExpressionHashes) = structuralHashNestedExplicit e
    thisHash = hash "Lam" `thenHash` x `thenHash` h

  App _ f e -> (thisHash /\ App thisHash subExpressionHashesL subExpressionHashesR)
    where
    (hL /\ subExpressionHashesL) = structuralHashNestedExplicit f
    (hR /\ subExpressionHashesR) = structuralHashNestedExplicit e
    thisHash = hash "App" `thenHash` hL `thenHash` hR

alphaEquivalentAccordingToHashExpr
  :: forall h a
   . Ord a
  => Hashable a
  => Expr h a
  -> Expr h a
  -> Boolean
alphaEquivalentAccordingToHashExpr = (==) `on` AlphaHashOptimizedHash.alphaHashTop

alphaEquivalentAccordingToSummariseExpr
  :: forall h name
   . Ord name
  => Expr h name
  -> Expr h name
  -> Boolean
alphaEquivalentAccordingToSummariseExpr = (==) `on` AlphaHashInefficient.summariseExpr

-- | Whether two expressions are alpha-equivalent, implemented using
-- 'uniquifyBinders'
alphaEquivalentAccordingToUniquifyBinders :: forall h a. Eq h => Ord a => Expr h a -> Expr h a -> Boolean
alphaEquivalentAccordingToUniquifyBinders = (==) `on` uniquifyBinders

-- | Makes binders unique whilst preserving alpha-equivalence.  The
-- binders are replaced with integers starting from zero and
-- increasing in left-to-right depth-first order.
--
-- In consequence, two expressions are alpha-equivalent if they are
-- equal under @uniqifyBinders@.
uniquifyBinders :: forall h a. Ord a => Expr h a -> Expr h (Either a Int)
uniquifyBinders = fst <<< uniquifyBindersExplicit Map.empty 0

-- | The internals of 'uniquifyBinders'
uniquifyBindersExplicit
  :: forall h a
   . Ord a
  => Map.Map a Int
  -> Int
  -> Expr h a
  -> (Expr h (Either a Int) /\ Int)
uniquifyBindersExplicit m n = case _ of
  Var h x -> case Map.lookup x m of
    Nothing -> (Var h (Left x) /\ n)
    Just i -> (Var h (Right i) /\ n)
  Lam h x e -> (Lam h (Right n) e' /\ n')
    where
    (e' /\ n') = uniquifyBindersExplicit (Map.insert x n m) (n + 1) e
  App h f x -> (App h f' x' /\ n'')
    where
    (f' /\ n') = uniquifyBindersExplicit m n f
    (x' /\ n'') = uniquifyBindersExplicit m n' x

normalizedGroupedEquivalentSubexpressions
  :: forall hash expr. Ord hash => List (hash /\ Path /\ expr) -> List (List (Path /\ expr))
normalizedGroupedEquivalentSubexpressions =
  sortBy (comparing (map fst))
    <<< filter ((notEq 1) <<< length)
    <<< (map <<< map) (\(_ /\ path /\ z) -> (path /\ z))
    <<< map toList
    <<< groupBy ((==) `on` (\(x /\ _ /\ _) -> x))
    <<< sortBy (comparing (\(x /\ _ /\ _) -> x))

-- | Generates random expressions for testing
genExprWithVarsTest :: forall m v. MonadGen m => NonEmptyList v -> m (Expr Unit v)
genExprWithVarsTest vars = genExprWithVars_vars
  -- Hedgehog has an example for exactly this use case!
  --
  -- http://hackage.haskell.org/package/hedgehog-1.0.2/docs/Hedgehog-Gen.html#v:recursive
  where
  genExprWithVars_vars :: m (Expr Unit v)
  genExprWithVars_vars = {-Gen.recursive-}  do
    pure unit -- boo
    Gen.choose
      (Var unit <$> Gen.elements vars)
      ( Gen.oneOf $ fromNonEmpty {-Gen.subtermM-}
          ( bind genExprWithVars_vars (\e -> Lam unit <$> Gen.elements vars <*> pure e) :|
              [ (\a b c -> lift2 c a b) {-Gen.subterm2-}  genExprWithVars_vars genExprWithVars_vars (App unit)
              ]
          )
      )

genExprWithVars :: FixMe -> Effect (Expr Unit FixMe)
genExprWithVars fresh = do
  size <- randomInt 0 2000
  genExprWithVarsSize size fresh

genExprWithVarsSize :: Int -> FixMe -> Effect (Expr Unit FixMe)
genExprWithVarsSize size fresh =
  if size <= 1 then Var unit <$> vars
  else do
    app <- randomBool
    if app then do
      sizeL <- randomInt 1 (size - 2)
      let sizeR = size - sizeL - 1
      App unit <$> genExprWithVarsSize sizeL fresh
        <*> genExprWithVarsSize sizeR fresh
    else
      Lam unit <$> binders <*> genExprWithVarsSize (size - 1) (fresh + 1)
  where
  vars = randomInt 0 (fresh - 1)
  binders = pure fresh

type FixMe = Int

genExprWithVarsUnbalancedSize :: Int -> FixMe -> Effect (Expr Unit FixMe)
genExprWithVarsUnbalancedSize size fresh =
  if size <= 1 then Var unit <$> vars
  else do
    vars0 <- vars
    mye <- genExprWithVarsUnbalancedSize (size - 3) (fresh + 1)
    pure $ App unit (Lam unit binders mye) (Var unit vars0)
  where
  vars = randomInt 0 (fresh - 1)
  binders = fresh

-- | Generates random expressions for testing
genExpr :: forall m. MonadGen m => m (Expr Unit Char)
genExpr = genExprWithVarsTest $ NonEmptyList $ 'u' :| List.fromFoldable [ 'v', 'w', 'x', 'y', 'z' ]

genExprNumVars :: Int -> Effect (Expr Unit Int)
genExprNumVars n = genExprWithVars (n + 1)

genExprAdversarialPair :: Int -> Effect (Expr Unit Int /\ Expr Unit Int)
genExprAdversarialPair size =
  let
    wrapWithApp expr = App unit expr (Var unit 2)
    wrapWithLam expr = Lam unit size expr
    baseExpr1 = Lam unit 1 (App unit (Var unit 1) (App unit (Var unit 1) (Var unit 1)))
    baseExpr2 = Lam unit 1 (App unit (App unit (Var unit 1) (Var unit 1)) (Var unit 1))
  in
    if size <= 6 then pure (baseExpr1 /\ baseExpr2)
    else if size == 7 then pure (wrapWithLam baseExpr1 /\ wrapWithLam baseExpr2)
    else do
      app <- randomBool

      if app then do
        (expr1 /\ expr2) <- genExprAdversarialPair (size - 2)
        pure (wrapWithApp expr1 /\ wrapWithApp expr2)
      else do
        (expr1 /\ expr2) <- genExprAdversarialPair (size - 1)
        pure (wrapWithLam expr1 /\ wrapWithLam expr2)

testEverythingInFileStartingWith'prop_' :: Effect Unit
testEverythingInFileStartingWith'prop_' = launchAff_ $ runSpec [ consoleReporter ] do
  describe "testEverythingInFileStartingWith'prop_'" do
    for_ tests \(does'something /\ (PropertyM prop)) ->
      if {-does'something /= "prop_fastFaster"-} false then pure unit
      else it does'something $ liftEffect do
        let
          test' = runReaderT (runExceptT (runWriterT prop))
            { structureShapeError: show >>> error
            , findSingletonError: show >>> error
            }
          test = test' <#> case _ of
            Left e -> fail (show e)
            Right (_ /\ w) -> sequence_ w
        quickCheckToEffect test

  where
  tests :: Array (String /\ Property)
  tests =
    [ "prop_stablePaths" /\ prop_stablePaths
    , "prop_hashAlphaEquivalence2" /\ prop_hashAlphaEquivalence2
    , "prop_rebuildFastOrig" /\ prop_rebuildFastOrig
    , "prop_equivCastFast" /\ prop_equivCastFast
    , "prop_uniquifyBindersExamples" /\ prop_uniquifyBindersExamples
    , "prop_fastFaster" /\ prop_fastFaster
    , "prop_uniquifyBindersIdempotent" /\ prop_uniquifyBindersIdempotent
    , "prop_hashAlphaEquivalence" /\ prop_hashAlphaEquivalence
    , "prop_rebuildSApp3_inverse" /\ prop_rebuildSApp3_inverse
    , "prop_rebuildSApp_inverse" /\ prop_rebuildSApp_inverse
    , "prop_rebuild3" /\ prop_rebuild3
    , "prop_hashUniquifyBinders" /\ prop_hashUniquifyBinders

    ]

numRandomTests :: Int
numRandomTests = 100 * 100

-- | Some specific test cases that demonstrate how 'uniquifyBinders'
-- works.  Please suggest more examples if you have ones that would be
-- helpful.
prop_uniquifyBindersExamples :: Property
prop_uniquifyBindersExamples = do
  let
    b = Right -- "bound"
    f = Left -- "free"
    examples =
      [ ( Lam unit "x" (Var unit "x") /\
            Lam unit (b 0) (Var unit (b 0))
        )

      , ( Lam unit "x" (Var unit "y") /\
            Lam unit (b 0) (Var unit (f "y"))
        )

      , ( Lam unit "x" (Lam unit "y" (Var unit "x")) /\
            Lam unit (b 0) (Lam unit (b 1) (Var unit (b 0)))
        )

      , ( Lam unit "x" (Lam unit "x" (Var unit "x")) /\
            Lam unit (b 0) (Lam unit (b 1) (Var unit (b 1)))
        )

      , ( Lam unit "x" (App unit (Var unit "x") (Var unit "x")) /\
            Lam unit (b 0) (App unit (Var unit (b 0)) (Var unit (b 0)))
        )

      , ( App unit (Lam unit "x" (Var unit "x")) (Lam unit "x" (Var unit "x")) /\
            App unit (Lam unit (b 0) (Var unit (b 0))) (Lam unit (b 1) (Var unit (b 1)))
        )
      ]

  flip traverse_ examples \(expression /\ uniquified) ->
    uniquifyBinders expression === uniquified

-- | Checks that the paths come out of the algorithms in the same
-- order (which just so happens to be depth first preorder).  This is
-- not an essential property of the algorithms, but it's nice that
-- they are thus normalised so that we can compare behaviour more
-- easily.
prop_stablePaths :: Property
prop_stablePaths = do
  let paths = map (\(_ /\ path /\ _) -> path) <<< allHashResults

  expr <- genExpr

  let
    d = deBruijnHash expr
    n = structuralHashNested expr

  paths d === paths n

-- | A sanity check for uniquifyBinders: it should be idempotent
prop_uniquifyBindersIdempotent :: Property
prop_uniquifyBindersIdempotent = do
  expr <- genExpr
  let
    uniquifyBinders_expr = uniquifyBinders expr
    -- For fairly boring type system reasons the types coming out of
    -- one iteration of uniquifyBinders are different from the types
    -- coming out of two iterations.
    --
    -- We just need to convert 'Left x' to 'Left (Left x)' so they
    -- types match.
    massageVariables = map (either (Left <<< Left) Right)

  massageVariables uniquifyBinders_expr === uniquifyBinders uniquifyBinders_expr

-- | A sanity check for both uniquifyBinders and castHashTop: uniquifying
-- binders should preserve alpha-equivalence and this equivalence
-- should be picked up by castHashTop.
prop_hashUniquifyBinders :: Property
prop_hashUniquifyBinders = do
  expr <- genExpr
  let massageVariables = map Left
  alphaEquivalentAccordingToHashExpr (uniquifyBinders expr)
    (massageVariables expr) === true

-- | A check for whether castHashTop respects alpha-equivalence (as
-- defined above) by checking it against alpha-equivalence in terms of
-- uniquifyBinders, which is presumably easier to get right.
prop_hashAlphaEquivalence :: Property
prop_hashAlphaEquivalence = do
  expr1 <- genExpr
  expr2 <- genExpr

  -- Or can use Hedgehog's "diff"
  alphaEquivalentAccordingToUniquifyBinders expr1 expr2
    === alphaEquivalentAccordingToHashExpr expr1 expr2

prop_hashAlphaEquivalence2 :: Property
prop_hashAlphaEquivalence2 = do
  expr1 <- genExpr
  expr2 <- genExpr

  -- Or can use Hedgehog's "diff"
  alphaEquivalentAccordingToUniquifyBinders expr1 expr2
    === alphaEquivalentAccordingToSummariseExpr expr1 expr2

propG_rebuild
  :: forall t
   . (Expr Unit Int -> t)
  -> ((Int -> Int) -> Int -> t -> Property' (Expr Unit Int))
  -> Property
propG_rebuild summariseExpr rebuild = do
  expr1Char <- genExpr
  let
    expr1 = map toCharCode expr1Char
    esummary = summariseExpr expr1
  expr2 <- rebuild (add 1) (0 :: Int) esummary
  alphaEquivalentAccordingToUniquifyBinders expr1 expr2 === true

prop_rebuild3 :: Property
prop_rebuild3 = propG_rebuild AlphaHashInefficient.summariseExpr AlphaHashInefficient.rebuild

prop_rebuildFastOrig :: Property
prop_rebuildFastOrig =
  propG_rebuild AlphaHashEfficient.summariseExpr AlphaHashEfficient.rebuild

-- | Shows equivalence the algorithms
prop_equivCastFast :: Property
prop_equivCastFast = do
  let
    n :: forall b a. Ord b => Expr b a -> List (List (Path /\ (Expr Unit a)))
    n = normalizedGroupedEquivalentSubexpressions <<< allHashResults
  expr <- map uniquifyBinders genExpr
  let
    locallyNameless_groups = n (locallyNameless expr)
    alphaHashEfficientHash_groups = n (AlphaHashEfficientHash.alphaHash expr)
    alphaHashFasterOrigHash_groups = n (AlphaHashOptimizedHash.alphaHash expr)

  --length original_groups === length locallyNameless_groups
  locallyNameless_groups === alphaHashEfficientHash_groups
  locallyNameless_groups === alphaHashFasterOrigHash_groups

prop_rebuildSApp3_inverse :: Property
prop_rebuildSApp3_inverse =
  AlphaHashInefficient.prop_rebuildSApp3_inverse genExpr

prop_rebuildSApp_inverse :: Property
prop_rebuildSApp_inverse =
  AlphaHashEfficient.prop_rebuildSApp_inverse genExpr

prop_fastFaster :: Property
prop_fastFaster = do
  expr <- genExpr
  AlphaHashEfficientHash.alphaHash expr === AlphaHashOptimizedHash.alphaHash expr
