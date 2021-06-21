module Design where

import Css (Styles)
import Css.Variables (makeVariables)

sv =
  makeVariables
    { green1: "green"
    , red1: "red"
    }
    \r ->
      { accent: r.green1
      , background: r.red1
      }


varStyles :: Styles
varStyles = sv.styles

vars = sv.values
