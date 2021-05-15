-- | The type signatures for these functions are much more complicated than actually using them. We use type level programming to catch typos and minimize boilerplate.
-- |
-- | For the following examples, lets say this is the top of our `Design` module.
-- |
-- | ```
-- | module Design (vars, varStyles) where
-- |
-- | import Css.Variables (mkVarStyles, mkVarValues)
-- | import Type.Proxy (Proxy(..))
-- |
-- | varRec =
-- |   { blue1: "blue"
-- |   , red1: "red"
-- |   , red2: "#ee0000"
-- |   , white1: "white"
-- |   }
-- |
-- | ```
module Css.Variables
  ( module Exports
  , mkVarStyles
  , mkVarValues
  ) where

import MasonPrelude
import Css (Styles, variable)
import Css.Functions (var)
import Css.Variables.Internal (MapIndex(..), Retrieve(..))
import Data.Batched (Batched(..))
import Foreign.Object (fromHomogeneous, toUnfoldable)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, hmap, hmapWithIndex)
import Prim.Row (class Nub, class Union)
import Record as Record
import Type.Row.Homogeneous (class Homogeneous)
import Css (variable) as Exports

-- | Create the variable declaration styles to add to `Css.Global.body`/`Css.Global.html`, or whatever the root of your app is.
-- |
-- | ```
-- | varStyles :: Styles
-- | varStyles = mkVarStyles varRec
-- | ```
mkVarStyles :: ∀ r. Homogeneous r String => { | r } -> Styles
mkVarStyles vars =
  Batch $ toUnfoldable (fromHomogeneous vars)
    <#> uncurry variable

-- makeVarValue ::
--   ∀ a l r r'.
--   IsSymbol l =>
--   R.Cons l a r' r =>
--   Record r ->
--   Proxy l ->
--   String
-- makeVarValue _ sym = var $ reflectSymbol sym
-- | Use the variable values in your declarations
-- |
-- | ```
-- | vars =
-- |   mkVarValues
-- |     { background: Proxy :: _ "white1"
-- |     , accent: Proxy :: _ "red2"
-- |     }
-- |     varRec
-- |
-- | sameAsVars =
-- |   { background: "var(--white1)"
-- |   , accent: "var(--red2)"
-- |   , white1: "var(--white1)"
-- |   , red1: "var(--red1)"
-- |   , red2: "var(--red2)"
-- |   , blue1: "var(--blue1)"
-- |   }
-- | ```
mkVarValues ::
  ∀ aliases vars values a b c.
  HMap (Retrieve vars) aliases { | a } =>
  HMapWithIndex MapIndex { | vars } { | b } =>
  Union a b c =>
  Nub c values =>
  aliases ->
  { | vars } ->
  { | values }
mkVarValues aliases vars =
  Record.merge
    (hmap (Retrieve vars var) aliases)
    (hmapWithIndex (MapIndex var) vars)
