-- | The efficient version of the algorithm that does do hashing with
-- some additional optimisations applied (not explicitly presented
-- third in the paper).  This is the version that is benchmarked.
--
-- The optimisations are strictness annotations and fusing two map
-- operations into one in a couple of places.  (A similar effort was
-- made to optimise the baseline algorithms against which we
-- benchmark).

module AlphaHashOptimizedHash16Bit where

import Prelude

import Data.Hashable (class Hashable)
import Data.Hashable as Data.Hashable
import Data.List (List, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt, fromInt, toInt)
import Data.UInt as UInt
import Expr (Expr(..))
import Expr as Expr

newtype MyUInt = MyUInt UInt

derive instance Eq MyUInt

instance Hashable MyUInt where
  hash (MyUInt i) = toInt i

type Hash = MyUInt
type Structure = Hash
type Positions = Hash
type StructureTag = Structure

hash :: forall a. Hashable a => a -> Hash
hash x = MyUInt $ fromInt (Data.Hashable.hash x)

shl :: MyUInt -> MyUInt -> MyUInt
shl (MyUInt a) (MyUInt b) = MyUInt $ UInt.shl a b

shr :: MyUInt -> MyUInt -> MyUInt
shr (MyUInt a) (MyUInt b) = MyUInt $ UInt.shr a b

xor :: MyUInt -> MyUInt -> MyUInt
xor (MyUInt a) (MyUInt b) = MyUInt $ UInt.xor a b

hashCombine :: Hash -> Hash -> Hash
hashCombine (MyUInt a) (MyUInt b) =
  -- combineWithSeed is based on the C++ boost::hash_combine.
  let
    combineWithSeed seed x = seed `UInt.xor` (x + fromInt 31161 + (seed `UInt.shl` fromInt 6) + (seed `UInt.shr` fromInt 2))
  in
    MyUInt $ combineWithSeed (combineWithSeed a b) $ fromInt 0

thenHash :: forall a. Hashable a => Hash -> a -> Hash
thenHash xHash y = hashCombine xHash (hash y)

---------------------------------------------------------------------

data VarMap name = VM (Map name Positions) Hash

mkSVar :: Structure
mkSVar = hash "SVar"

mkHerePL :: Structure
mkHerePL = hash "HerePL"

mkJoinPL :: StructureTag -> Maybe Positions -> Positions -> Positions
mkJoinPL a b c = hash "JoinPL" `thenHash` a `thenHash` b `thenHash` c

mkSLam :: StructureTag -> Maybe Positions -> Structure -> Structure
mkSLam a b c = hash "SLam" `thenHash` a `thenHash` b `thenHash` c

mkSApp :: StructureTag -> Boolean -> Structure -> Structure -> Structure
mkSApp a b c d = hash "SApp" `thenHash` a `thenHash` b `thenHash` c `thenHash` d

structureTag :: Structure -> StructureTag
structureTag = identity

-- O(log n). Lookup and update. See also updateWithKey. The function returns changed value, if it is updated. Returns the original key value if the map entry is deleted.
updateLookupWithKey :: forall k a. Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a /\ Map k a)
updateLookupWithKey f k m = case Map.pop k m of
  Nothing -> Nothing /\ m
  Just (del /\ new) -> do
    let changed = f k del
    case changed of
      Nothing -> Nothing /\ new
      Just v -> Just v /\ Map.insert k v new

removeFromVM :: forall v. Hashable v => Ord v => v -> VarMap v -> (VarMap v /\ Maybe Positions)
removeFromVM key (VM entries existingHash) = munge (updateLookupWithKey delete key entries)
  where
  munge (Nothing /\ m) = (VM m existingHash /\ Nothing)
  munge (Just pt /\ m) = (VM m (existingHash `xor` entryHash key pt) /\ Just pt)
  delete _ _ = Nothing

entryHash :: forall name. Hashable name => name -> Positions -> Hash
entryHash key pos = hash key `thenHash` pos

singletonVM :: forall name. Hashable name => name -> Positions -> VarMap name
singletonVM key pos = VM (Map.singleton key pos) (entryHash key pos)

alterF :: forall f k a. Functor f => Ord k => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
alterF f k m = f (k `Map.lookup` m) <#> case _ of
  Nothing -> Map.delete k m
  Just v -> Map.insert k v m

alterVM
  :: forall name
   . Hashable name
  => Ord name
  => (Maybe Positions -> Positions)
  -> name
  -> VarMap name
  -> VarMap name
alterVM f key (VM entries old_hash) = munge (alterF g key entries)
  where
  munge (h /\ m) = VM m h

  g :: Maybe Positions -> (Hash /\ Maybe Positions)
  g = case _ of
    Nothing -> (old_hash `xor` entryHash key new_pt /\ Just new_pt)
      where
      new_pt = f Nothing
    Just old_pt ->
      ( old_hash `xor` entryHash key old_pt
          `xor` entryHash key new_pt /\ Just new_pt
      )
      where
      new_pt = f (Just old_pt)

hashVM :: forall name. VarMap name -> Hash
hashVM (VM _ h) = h

sizeVM :: forall name. VarMap name -> Int
sizeVM (VM m _) = Map.size m

toListVM :: forall name. VarMap name -> List (name /\ Positions)
toListVM (VM m _) = Map.toUnfoldable m

summariseExpr
  :: forall name h
   . Hashable name
  => Ord name
  => Expr h name
  -> (Structure /\ VarMap name /\ Expr Hash name)
summariseExpr = case _ of
  Var _ v ->
    let
      structure = mkSVar
      positionMap = singletonVM v mkHerePL
    in
      (structure /\ positionMap /\ Var (hash structure `thenHash` hashVM positionMap) v)
  Lam _ x e ->
    let
      (str_body /\ map_body /\ e') = summariseExpr e
      (e_map /\ x_pos) = removeFromVM x map_body
      structure = mkSLam (structureTag str_body) x_pos str_body
      positionMap = e_map
    in
      (structure /\ positionMap /\ Lam (hash structure `thenHash` hashVM positionMap) x e')
  App _ e1 e2 ->
    let
      (str1 /\ map1 /\ e1') = summariseExpr e1
      (str2 /\ map2 /\ e2') = summariseExpr e2
      app_depth = hash (structureTag str1) `thenHash` structureTag str2
      tag = app_depth
      left_bigger = sizeVM map1 >= sizeVM map2
      str = mkSApp tag left_bigger str1 str2
      add_kv vm_ (v /\ p) = alterVM (\mp -> mkJoinPL tag mp p) v vm_
      (big_vm /\ small_vm) =
        if left_bigger then (map1 /\ map2)
        else (map2 /\ map1)
      vm = foldl add_kv big_vm (toListVM small_vm)

    in
      (str /\ vm /\ App (hash str `thenHash` hashVM vm) e1' e2')

alphaHash :: forall name h. Ord name => Hashable name => Expr h name -> Expr Hash name
alphaHash e = e'
  where
  (_ /\ _ /\ e') = summariseExpr e

alphaHashTop :: forall name h. Ord name => Hashable name => Expr h name -> Hash
alphaHashTop = Expr.annotation <<< alphaHash
