module Data.JSValue where

import MasonPrelude

data JSValue

foreign import jseq :: ∀ a b. a -> b -> Boolean

instance Eq JSValue where
  eq = jseq

infix 4 jseq as ===

toJSValue :: ∀ a. a -> JSValue
toJSValue = unsafeCoerce
