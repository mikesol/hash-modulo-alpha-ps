-- | The easy-to-understand version of algorithm (presented first in
-- the paper).  It is inefficient because it involves merging two maps
-- into a completely new map at each App node.
--
-- This is not benchmarked and only exists for informational purposes.

module AlphaHashInefficient where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, class MonadTrans, ask)
import Control.Monad.Trans.Class (lift)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Errors (FindSingletonError(..), StructureShapeError)
import Expr (Expr(..))
import Merge (MergeMaps(..))
import Merge as Merge
import MonadResult (class MonadResult, (===))
import Test.QuickCheck.Gen (Gen)

data Positions
  = HerePL
  | LeftOnlyPL Positions
  | RightOnlyPL Positions
  | BothPL Positions Positions

derive instance Eq Positions
derive instance Generic Positions _

instance Show Positions where
  show s = genericShow s

data Structure
  = SVar
  | SLam (Maybe Positions) Structure
  | SApp Structure Structure

derive instance Eq Structure
derive instance Generic Structure _

instance Show Structure where
  show s = genericShow s

removeFromVM :: forall v positions. Ord v => v -> Map v positions -> (Map v positions /\ Maybe positions)
removeFromVM v m = (Map.delete v m /\ Map.lookup v m)

unionVM :: forall k. Ord k => Map k Positions -> Map k Positions -> Map k Positions
unionVM = Merge.mergeMaps
            (case _ of
                LeftOnly l -> LeftOnlyPL l
                RightOnly r -> RightOnlyPL r
                Both l r -> BothPL l r
            )

findSingleton :: forall e m p r. MonadAsk { findSingletonError :: FindSingletonError -> e | r } m => MonadThrow e m => Map p Positions -> m p
findSingleton m = do
  { findSingletonError } <- ask
  case Map.toUnfoldable m of
    (v /\ HerePL) : Nil -> pure v
    (_ /\ _) : Nil -> throwError $ findSingletonError ExpectedHerePL
    Nil -> throwError $ findSingletonError ExpectedNonEmptyMap
    _ : _ : _ -> throwError $ findSingletonError ExpectedSingletonMap

extendVM :: forall a k. Ord k => Map k a -> k -> a -> Map k a
extendVM m x p = Map.insert x p m

pickL :: Positions -> Maybe Positions
pickL = case _ of
  LeftOnlyPL pl -> Just pl
  BothPL pl _ -> Just pl
  _ -> Nothing

pickR :: Positions -> Maybe Positions
pickR = case _ of
  RightOnlyPL pr -> Just pr
  BothPL _ pr -> Just pr
  _ -> Nothing

summariseExpr :: forall h name. Ord name
              => Expr h name
              -> (Structure /\ Map name Positions)
summariseExpr = case _ of
  Var _ v   -> (SVar /\ Map.singleton v HerePL)
  Lam _ x e ->
    let (str_body /\ map_body) = summariseExpr e
        (e_map /\ x_pos) = removeFromVM x map_body
    in (SLam x_pos str_body /\ e_map)
  App _ e1 e2 ->
    let (str1 /\ map1) = summariseExpr e1
        (str2 /\ map2) = summariseExpr e2
    in (SApp str1 str2 /\ unionVM map1 map2)

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
  SLam mp s -> Lam unit x <$> rebuild freshen fresher (s /\ m')
    where x = fresh
          fresher = freshen fresh
          m' = case mp of Nothing -> m
                          Just p -> extendVM m x p
  SApp s1 s2 -> App unit <$> rebuild freshen fresh (s1 /\ m1) <*>
                       rebuild freshen fresh (s2 /\ m2)
    where m1 = Map.mapMaybe pickL m
          m2 = Map.mapMaybe pickR m

rebuildSApp :: forall k. Ord k => Map k Positions -> (Map k Positions /\ Map k Positions)
rebuildSApp m = (Map.mapMaybe pickL m /\ Map.mapMaybe pickR m)

prop_rebuildSApp3_inverse 
  :: forall e m r
   . MonadAsk { structureShapeError :: StructureShapeError -> e | r } (m Gen)
  => MonadResult (m Gen)
  => MonadTrans m
  => MonadThrow e (m Gen)
  => Gen (Expr Unit Char)
  -> m Gen Unit
prop_rebuildSApp3_inverse gen = do
  e1 <- lift gen
  e2 <- lift gen

  let (_ /\ m1) = summariseExpr e1
      (_ /\ m2) = summariseExpr e2
      (_ /\ m) = summariseExpr (App unit e1 e2)

      (m1' /\ m2') = rebuildSApp m

  m1 === m1'
  m2 === m2'
