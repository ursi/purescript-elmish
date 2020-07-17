module Cmd where

import Prelude
import Effect (Effect)

type Cmd msg
  = Effect Unit

none :: ∀ msg. Cmd msg
none = mempty
