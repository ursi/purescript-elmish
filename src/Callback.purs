module Callback where

import MasonPrelude

type Callback a
  = a -> Effect Unit
