-- TODO: give credit to original place which was the 30th source on the murmurhash wikipedia article
module Murmur3 (hash) where

import MasonPrelude

foreign import hashImpl :: Fn2 String Int Int

hash :: Int -> String -> Int
hash = flip $ runFn2 hashImpl
