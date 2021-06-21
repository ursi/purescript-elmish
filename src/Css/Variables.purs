module Css.Variables
  ( module Exports
  , makeVariables
  )
  where

import MasonPrelude
import Css (Styles, variable)
import Css.Functions (var)
import Css.Variables.Internal (MapIndex(..))
import Data.Batched (Batched(..))
import Foreign.Object (fromHomogeneous, toUnfoldable)
import Heterogeneous.Mapping (class HMapWithIndex, hmapWithIndex)
import Prim.Row (class Nub, class Union)
import Record (disjointUnion)
import Type.Row.Homogeneous (class Homogeneous)
import Css (variable) as Exports

-- | The type signatures for this function is much more complicated than actually using it. We use type level programming to catch typos and minimize boilerplate.
-- |
-- | The following records are equivalent;
-- | ```
-- | sv1 =
-- |   something
-- |     { blue1: "blue"
-- |     , red1: "red"
-- |     , red2: "#ee0000"
-- |     , white1: "white"
-- |     }
-- |     \r ->
-- |       { accent: r.red2
-- |       , background: r.white1
-- |       }
-- |
-- | sv2 =
-- |   { styles:
-- |       -- this will create the following CSS values
-- |       {-
-- |       --blue1: "blue";
-- |       --red1: "red";
-- |       --red2: "#ee0000";
-- |       --white1: "white";
-- |       --accent: "var(--red1)";
-- |       --background: "var(--white1)";
-- |       -}
-- |   , values:
-- |       { blue1: "var(--blue1)"
-- |       , red1: "var(--red1)"
-- |       , red2: "var(--red2)"
-- |       , white1: "var(--white2)"
-- |       , accent: "var(--accent)"
-- |       , background: "var(--background)"
-- |       }
-- |   }
-- | ```
makeVariables :: ∀ a1 a2 b1 c1 c2.
  Union a1 b1 c1 =>
  HMapWithIndex MapIndex (Record a1) (Record a2) =>
  HMapWithIndex MapIndex (Record c1) (Record c2) =>
  Homogeneous c1 String =>
  Nub c1 c1
  => Record a1
  -> (Record a2 -> Record b1)
  -> { values :: Record c2
     , styles :: Styles
     }
makeVariables values fromVars =
  let
    valVars = hmapWithIndex (MapIndex var) values

    raw :: Record c1
    raw =
      disjointUnion
        values
        (fromVars valVars)
  in
  { styles:
      Batch $ toUnfoldable (fromHomogeneous raw)
      <#> uncurry variable
  , values: hmapWithIndex (MapIndex var) raw
  }
