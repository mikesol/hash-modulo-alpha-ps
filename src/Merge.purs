module Merge where

import Prelude

import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple, fst, snd)

-- A slightly nicer API for merging maps
data MergeMaps l r
  = LeftOnly l
  | Both l r
  | RightOnly r

mergeMaps :: forall l r k a. Ord k => (MergeMaps l r -> a) -> Map k l -> Map k r -> Map k a
mergeMaps f l r = go ls rs
  where
  ls = Map.toUnfoldable l
  rs = Map.toUnfoldable r
  go :: List (Tuple k l) -> List (Tuple k r) -> Map k a
  go Nil Nil = Map.empty
  go (a : b) Nil = Map.insert (fst a) (f (LeftOnly (snd a))) (go b Nil)
  go Nil (a : b) = Map.insert (fst a) (f (RightOnly (snd a))) (go Nil b)
  go (a : b) (c : d) = case compare k1 k2 of
    LT -> Map.insert k1 (f (LeftOnly (snd a))) (go b (c : d))
    GT -> Map.insert k2 (f (RightOnly (snd c))) (go (a : b) d)
    EQ -> Map.insert k1 (f (Both (snd a) (snd c))) (go b d)
    where
    k1 = fst a
    k2 = fst c