-- source: https://github.com/garycourt/murmurhash-js/blob/master/murmurhash3_gc.js
module Murmur3 (hash) where

import MasonPrelude

foreign import hashImpl :: Fn2 String Int Int

hash :: Int -> String -> Int
hash = flip $ runFn2 hashImpl
