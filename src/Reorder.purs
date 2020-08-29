module Reorder where

import MasonPrelude
import Data.Array ((!!))
import Data.Array as Array

reorder :: ∀ a m. Monad m => (a -> Int) -> (Int -> Int -> Array a -> m (Array a)) -> Array a -> m Unit
reorder indexer mover = go 1
  where
  go :: Int -> Array a -> m Unit
  go index progress = case progress !! index of
    Just a -> do
      let
        indexForA = indexer a
      newProgress <- mover index indexForA progress
      go (index + 1) newProgress
    Nothing -> pure unit

move :: ∀ a. Int -> Int -> Array a -> Maybe (Array a)
move start end array = do
  a <- array !! start
  Array.deleteAt start array >>= Array.insertAt end a
