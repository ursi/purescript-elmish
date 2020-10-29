module Ds where

import MasonPrelude
import Css (Styles)
import Css as C
import Css.Functions (var)
import Css.Variables (makeVars, makeVarValue)
import Data.Symbol (SProxy(..))

varRec =
  { background: "red"
  , accent: "green"
  }

vars :: Styles
vars = makeVars varRec

background :: String
background = (SProxy :: _ "background") # makeVarValue varRec

accent :: String
accent = (SProxy :: _ "accent") # makeVarValue varRec
