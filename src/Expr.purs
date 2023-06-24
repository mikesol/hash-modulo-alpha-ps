module Expr where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.List (List(..), elem, foldMap, mapMaybe, reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attributes (style_)
import Deku.Control (text_)
import Deku.Core (Nut, fixed)
import Deku.DOM as D

-- | Simple lambda expression type
data Expr h a
  = Var h a
  | Lam h a (Expr h a)
  | App h (Expr h a) (Expr h a)

derive instance Functor (Expr h)
derive instance Generic (Expr h a) _
derive instance (Eq h, Eq a) => Eq (Expr h a)

instance (Show h, Show a) => Show (Expr h a) where
  show s = genericShow s

exprSize :: forall h a. Expr h a -> Int
exprSize = case _ of
  Var _ _ -> 1
  Lam _ _ e -> 1 + exprSize e
  App _ f x -> 1 + exprSize f + exprSize x

showPath :: Path -> String
showPath = foldMap show <<< reverse

data Step = Apl | Apr | L

derive instance Eq Step
derive instance Ord Step
derive instance Generic Step _
instance Show Step where
  show = genericShow

instance Hashable Step where
  hash = case _ of
    Apl -> 0
    Apr -> 1
    L -> 2

type Path = List Step

mapAnnotation :: forall h h' a. (h -> h') -> Expr h a -> Expr h' a
mapAnnotation f = case _ of
  Var h a -> Var (f h) a
  Lam h x e -> Lam (f h) x (mapAnnotation f e)
  App h e1 e2 -> App (f h) (mapAnnotation f e1) (mapAnnotation f e2)

-- I'm not convinced that fmapping the return value is better than
-- passing down an accumulating path, but it is marginally easier to
-- write.
allSubexprs :: forall h a. Expr h a -> List (Path /\ (Expr h a))
allSubexprs e = (Nil /\ e) : case e of
  Var _ _ -> Nil
  Lam _ _ e' -> (map <<< first) (Cons L) (allSubexprs e')
  App _ e1 e2 -> (map <<< first) (Cons Apr) (allSubexprs e2)
    <> (map <<< first) (Cons Apl) (allSubexprs e1)

allHashResults :: forall b a. Expr b a -> List (b /\ Path /\ Expr Unit a)
allHashResults = f <<< allHashResults_
  where
  allHashResults_ = map (\(p /\ se) -> (annotation se /\ p /\ se)) <<< allSubexprs
  -- Warning: This mapAnnotation is slow, but it isn't involved
  -- in the benchmark and will likely disappear soon anyway.
  -- The reverse is also a pain.
  f = map (\(h__ /\ p /\ es) -> (h__ /\ reverse p /\ mapAnnotation (const unit) es))

annotation :: forall h a. Expr h a -> h
annotation = case _ of
  Var h _ -> h
  Lam h _ _ -> h
  App h _ _ -> h

-- Example expressions from "Finding Identical Subexpressions"
--
-- The examples are not supposed to be well-typed.  The common
-- subexpression identification algorithms do their job regardless of
-- well-typedness.

showExpr :: forall h. Expr h String -> String
showExpr = case _ of
  Var _ x -> x
  Lam _ x e -> "(lam " <> x <> " " <> showExpr e <> ")"
  App _ f e -> "(" <> showExpr f <> " " <> showExpr e <> ")"

-- | Render the Expr to Html.  The subtrees that occur at the end of
-- any of the paths are colored.
highlightedExprColor :: forall h. String -> List Path -> Expr h String -> Nut
highlightedExprColor color paths =
  if Nil `elem` paths then highlight <<< pure <<< text_ <<< showExpr
  else case _ of
    Var _ x -> text_ x
    Lam _ x e -> fixed
      [ text_ ("(lam " <> x <> " ")
      , e # recurse case _ of
          L : xs -> Just xs
          _ -> Nothing
      , text_ ")"
      ]
    App _ f e -> fixed
      [ text_ "("
      , f # recurse case _ of
          Apl : xs -> Just xs
          _ -> Nothing
      , text_ " "
      , e # recurse case _ of
          Apr : xs -> Just xs
          _ -> Nothing
      , text_ ")"
      ]

  where
  highlight = D.span [ style_ ("background-color: " <> color) ]

  recurse g = highlightedExprColor color (mapMaybe g paths)

highlightedExpr :: forall h. List Path -> Expr h String -> Nut
highlightedExpr = highlightedExprColor "#bbbbff"

app_ :: forall a. Expr Unit a -> Expr Unit a -> Expr Unit a
app_ = App unit

example1 :: Expr Unit String
example1 = (Lam unit "x" (Var unit "x" .+ Var unit "x"))
  `app_`
    (Lam unit "y" (Var unit "y" .+ Var unit "y"))

example2 :: Expr Unit String
example2 = Lam unit "x"
  ( (Var unit "x" .+ Var unit "x")
      `app_` (Lam unit "x" (Var unit "x" .+ Var unit "x"))
      `app_` (Var unit "x" .+ Var unit "x")
  )

example3 :: Expr Unit String
example3 = (Lam unit "x" (Var unit "x" .+ (Var unit "y" .+ Var unit "3")))
  `app_` (Var unit "y" .+ Var unit "3")
  `app_` (Lam unit "y" (Var unit "y" .+ Var unit "3"))

example4 :: Expr Unit String
example4 = Lam unit "x"
  ( sin_ (Var unit "x")
      .+
        ( Lam unit "y" (sin_ (Var unit "x") .+ Var unit "y")
            `app_` Var unit "3"
        )
  )

example5 :: Expr Unit String
example5 = ((Var unit "x" .+ Var unit "1") .* Var unit "y")
  ./
    ( Lam unit "tmp" (atan2 (Var unit "tmp") (Var unit "tmp" .* Var unit "y"))
        `app_`
          (Var unit "x" .+ Var unit "1")
    )

example6 :: Expr Unit String
example6 = (Var unit "x" .+ Var unit "x") `app_` (Var unit "x" .+ Var unit "x")

example7 :: Expr Unit String
example7 = (Lam unit "x" (Var unit "x" .+ Var unit "x")) `app_` (Lam unit "x" (Var unit "x" .+ Var unit "x"))

binOp :: String -> Expr Unit String -> Expr Unit String -> Expr Unit String
binOp opName e1 e2 = Var unit opName `app_` e1 `app_` e2

binOpAdd :: Expr Unit String -> Expr Unit String -> Expr Unit String
binOpAdd = binOp "add"

infixl 6 binOpAdd as .+

binOpMul :: Expr Unit String -> Expr Unit String -> Expr Unit String
binOpMul = binOp "mul"

infixl 7 binOpMul as .*

binOpDiv :: Expr Unit String -> Expr Unit String -> Expr Unit String
binOpDiv = binOp "div"

infixl 7 binOpDiv as ./

atan2 :: Expr Unit String -> Expr Unit String -> Expr Unit String
atan2 = binOp "atan2"

sin_ :: Expr Unit String -> Expr Unit String
sin_ = app_ (Var unit "sin")
