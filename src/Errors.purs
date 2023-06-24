module Errors where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data FindSingletonError = ExpectedHerePL | ExpectedNonEmptyMap | ExpectedSingletonMap

derive instance Eq FindSingletonError
derive instance Generic FindSingletonError _
instance Show FindSingletonError where
  show = genericShow

data StructureShapeError = UnexpectedShapeOfStructure

derive instance Eq StructureShapeError
derive instance Generic StructureShapeError _
instance Show StructureShapeError where
  show = genericShow

type Errors e = { structureShapeError :: StructureShapeError -> e, findSingletonError :: FindSingletonError -> e }