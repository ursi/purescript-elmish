module JSEq ((===), jseq) where

foreign import jseq :: ∀ a b. a -> b -> Boolean

infix 4 jseq as ===
