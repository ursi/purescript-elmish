module Css.Variables
  ( module Exports
  , makeVars
  , makeVarValue
  ) where

-- | Use CSS variables in a type-safe way
import MasonPrelude
import Css (Styles, variable)
import Css.Functions (var)
import Data.Batchable (Batched(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Foreign.Object (fromHomogeneous, toUnfoldable)
import Prim.Row (class Cons)
import Type.Row.Homogeneous (class Homogeneous)
import Css (variable) as Exports

makeVars :: ∀ r. Homogeneous r String => Record r -> Styles
makeVars vars =
  Batch $ toUnfoldable (fromHomogeneous vars)
    <#> uncurry variable

makeVarValue ::
  ∀ l r' r.
  Homogeneous r String =>
  IsSymbol l =>
  Cons l String r' r =>
  Record r ->
  SProxy l ->
  String
makeVarValue _ sym = var $ reflectSymbol sym
