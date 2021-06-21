module Css.Variables.Internal (MapIndex(..)) where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Heterogeneous.Mapping (class MappingWithIndex)
import Type.Proxy (Proxy(..))

data MapIndex
  = MapIndex (String -> String)

instance IsSymbol i => MappingWithIndex (MapIndex) (Proxy i) String String where
  mappingWithIndex (MapIndex f) _ _ = f (reflectSymbol (Proxy :: _ i))
