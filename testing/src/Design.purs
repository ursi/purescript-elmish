module Design where

import Css (Styles)
import Css.Variables (mkVarStyles, mkVarValues)
import Type.Proxy (Proxy(..))

varRec =
  { green1: "green"
  , red1: "red"
  }

varStyles :: Styles
varStyles = mkVarStyles varRec

vars =
  mkVarValues
    { background: Proxy :: _ "red1"
    , accent: Proxy :: _ "green1"
    }
    varRec
