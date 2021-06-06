module Css.Variables.Internal
  ( Retrieve(..)
  , MapIndex(..)
  ) where

import MasonPrelude
import Data.Symbol (class IsSymbol, reflectSymbol)
import Heterogeneous.Mapping (class Mapping, class MappingWithIndex)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))

data MapIndex
  = MapIndex (String -> String)

instance IsSymbol i => MappingWithIndex (MapIndex) (Proxy i) String String where
  mappingWithIndex (MapIndex f) _ _ = f (reflectSymbol (Proxy :: _ i))

data Retrieve r
  = Retrieve { | r } (String -> String)

instance (IsSymbol field, Cons field a x r) => Mapping (Retrieve r) (Proxy field) String where
  mapping (Retrieve r f) _ = f $ reflectSymbol (Proxy :: _ field)
