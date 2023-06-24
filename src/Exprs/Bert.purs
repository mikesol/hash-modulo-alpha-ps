module Exprs.Bert where

import Prelude

import Data.List (List(..), (:))
import Expr (Expr)
import Exprs.Bert.Bert1 as Bert1
import Exprs.Bert.Bert2 as Bert2
import Exprs.Bert.Bert3 as Bert3
import Exprs.Bert.Bert4 as Bert4

berts :: List (Expr Unit String)
berts =
  ( Bert1.expr : Bert2.expr
      : Bert3.expr
      : Bert4.expr
      : Nil
  )