module Design where

import Css (Styles)
import Css.Variables (mkVarStyles, mkVarValues)
import Data.Symbol (SProxy(..))

varRec =
  { green1: "green"
  , red1: "red"
  }

varStyles :: Styles
varStyles = mkVarStyles varRec

vars =
  mkVarValues
    { background: SProxy :: _ "red1"
    , accent: SProxy :: _ "green1"
    }
    varRec
