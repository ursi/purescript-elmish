module Css.Variables.Internal
  ( Retrieve(..)
  , MapIndex(..)
  ) where

import MasonPrelude
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Heterogeneous.Mapping (class Mapping, class MappingWithIndex)
import Prim.Row (class Cons)

data MapIndex
  = MapIndex (String -> String)

instance mappingWithIndexMapIndex :: IsSymbol i => MappingWithIndex (MapIndex) (SProxy i) String String where
  mappingWithIndex (MapIndex f) _ _ = f (reflectSymbol (SProxy :: _ i))

data Retrieve r
  = Retrieve { | r } (String -> String)

instance mappingRetrieve :: (IsSymbol field, Cons field a x r) => Mapping (Retrieve r) (SProxy field) String where
  mapping (Retrieve r f) _ = f $ reflectSymbol (SProxy :: _ field)
