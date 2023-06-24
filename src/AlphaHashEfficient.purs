-- | The efficient version of the algorithm (presented second in the
-- paper).  It is more efficient because we only ever accumulate the
-- smaller map into the larger map at each App node.  This
-- implementation does not do any hashing.
--
-- This is not benchmarked and only exists for informational purposes.

{-# LANGUAGE LambdaCase #-}

module AlphaHashEfficient where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), foldl, (:))
import Data.Map (Map, mapMaybe)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Errors (FindSingletonError(..), StructureShapeError(..))
import Expr (Expr(..))
import MonadResult (class MonadResult, (===))
import Test.QuickCheck.Gen (Gen)

data Positions
  = HerePL
  | JoinPL StructureTag (Maybe Positions) Positions

derive instance Eq Positions
derive instance Generic Positions _

instance Show Positions where
  show s = genericShow s

data Structure
  = SVar
  | SLam StructureTag (Maybe Positions) Structure
  | SApp StructureTag Boolean Structure Structure

derive instance Eq Structure
derive instance Generic Structure _

instance Show Structure where
  show s = genericShow s

type StructureTag = Int

structureTag :: Structure -> StructureTag
structureTag = case _ of
  SVar -> 0
  SLam t _ _ -> t
  SApp t _ _ _ -> t

removeFromVM :: forall v positions. Ord v => v -> Map v positions -> (Map v positions /\ Maybe positions)
removeFromVM v m = Map.delete v m /\ Map.lookup v m


findSingleton :: forall e m p r. MonadAsk { findSingletonError :: FindSingletonError -> e | r } m => MonadThrow e m => Map p Positions -> m p
findSingleton m = do
  { findSingletonError } <- ask
  case Map.toUnfoldable m of
    (v /\ HerePL) : Nil -> pure v
    (_ /\ _) : Nil -> throwError $ findSingletonError ExpectedHerePL
    Nil -> throwError $ findSingletonError ExpectedNonEmptyMap
    _ : _ : _ -> throwError $ findSingletonError ExpectedSingletonMap

extendVM :: forall k a. Ord k => Map k a -> k -> a -> Map k a
extendVM m x p = Map.insert x p m

summariseExpr
  :: forall name h
   . Ord name
  => Expr h name
  -> (Structure /\ Map name Positions)
summariseExpr = case _ of
  Var _ v -> (SVar /\ Map.singleton v HerePL)
  Lam _ x e ->
    let
      (str_body /\ map_body) = summariseExpr e
      (e_map /\ x_pos) = removeFromVM x map_body
    in
      (SLam (structureTag str_body) x_pos str_body /\ e_map)
  App _ e1 e2 ->
    let
      (str1 /\ map1) = summariseExpr e1
      (str2 /\ map2) = summariseExpr e2
      app_depth = max (structureTag str1) (structureTag str2) + 1
      tag = app_depth
      left_bigger = Map.size map1 >= Map.size map2
      str = SApp tag left_bigger str1 str2
      add_kv vm_ (v /\ p) = Map.alter (\mp -> Just (JoinPL tag mp p)) v vm_
      (big_vm /\ small_vm) =
        if left_bigger then (map1 /\ map2)
        else (map2 /\ map1)
      vm = foldl add_kv big_vm (Map.toUnfoldable small_vm :: List _)

    in
      (str /\ vm)

rebuild
  :: forall m e name r
   . MonadAsk { findSingletonError :: FindSingletonError -> e | r } m
  => MonadThrow e m
  => Ord name
  => (name -> name)
  -> name
  -> (Structure /\ Map name Positions)
  -> m (Expr Unit name)
rebuild freshen fresh (structure /\ m) = case structure of
  SVar -> Var unit <$> findSingleton m
  SLam _ mp s -> Lam unit x <$> rebuild freshen fresher (s /\ m')
    where
    x = fresh
    fresher = freshen fresh
    m' = case mp of
      Nothing -> m
      Just p -> extendVM m x p
  SApp tag left_bigger s1 s2 -> App unit <$> rebuild freshen fresh (s1 /\ m1) <*>
    (rebuild freshen fresh (s2 /\ m2))
    where
    (m1 /\ m2) = rebuildSApp m tag left_bigger

rebuildSApp
  :: forall k
   . Ord k
  => Map k Positions
  -> StructureTag
  -> Boolean
  -> (Map k Positions /\ Map k Positions)
rebuildSApp m tag left_bigger = (m1 /\ m2)
  where
  upd_small :: Positions -> Maybe Positions
  upd_small (JoinPL ptag _ pt) | ptag == tag = Just pt
  upd_small _ = Nothing

  upd_big :: Positions -> Maybe Positions
  upd_big (JoinPL ptag mpt _) | ptag == tag = mpt
  upd_big pt = Just pt

  small_m = mapMaybe upd_small m
  big_m = mapMaybe upd_big m
  (m1 /\ m2) =
    if left_bigger then (big_m /\ small_m)
    else (small_m /\ big_m)


prop_rebuildSApp_inverse
  :: forall e m r
   . MonadAsk { structureShapeError :: StructureShapeError -> e | r } (m Gen)
  => MonadResult (m Gen)
  => MonadTrans m
  => MonadThrow e (m Gen)
  => Gen (Expr Unit Char)
  -> m Gen Unit
prop_rebuildSApp_inverse gen = do
  { structureShapeError } <- ask
  e1 <- lift gen
  e2 <- lift gen

  let
    (_ /\ m1) = summariseExpr e1
    (_ /\ m2) = summariseExpr e2
    (s /\ m) = summariseExpr (App unit e1 e2)

  left_bigger <- case s of
    SApp _ left_bigger_ _ _ -> pure left_bigger_
    _ -> throwError $ structureShapeError UnexpectedShapeOfStructure

  let
    (m1' /\ m2') = rebuildSApp m (structureTag s) left_bigger
  m1 === m1'
  m2 === m2'