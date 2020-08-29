module Data.Diff
  ( class Diffable
  , Diff(..)
  , diff
  , diff2
  ) where

import MasonPrelude
import Data.Array as Array
import Data.List ((:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Debug as Debug

data Diff a b
  = Left a
  | Right b
  | Both a b

-- purty sucks
class Diffable f where
  diff :: ∀ a b c m. Monad m => (Diff a b -> m (Maybe c)) -> f a -> f b -> m (f c)

diff2 ::
  ∀ a b c f g m.
  Diffable f =>
  Diffable g =>
  Monoid (g a) =>
  Monoid (g b) =>
  Monad m =>
  (Diff a b -> m (Maybe c)) ->
  f (g a) ->
  f (g b) ->
  m (f (g c))
diff2 f fga fgb =
  {-- let --}
  {--   _ = Debug.debugger unit --}
  {-- in --}
  diff
    ( \x ->
        Just
          <$> case x of
              Left ga -> diff f ga mempty
              Right gb -> diff f mempty gb
              Both ga gb -> diff f ga gb
    )
    fga
    fgb

instance diffableList :: Diffable List where
  diff = diffList

diffList :: ∀ a b c m. Monad m => (Diff a b -> m (Maybe c)) -> List a -> List b -> m (List c)
diffList f = map List.reverse <.. go Nil
  where
  go :: List c -> List a -> List b -> m (List c)
  go acc = case _, _ of
    aHead : aTail, bHead : bTail -> do
      mc <- f $ Both aHead bHead
      go (maybeCons mc acc) aTail bTail
    head : tail, Nil -> do
      mc <- f $ Left head
      go (maybeCons mc acc) tail Nil
    Nil, head : tail -> do
      mc <- f $ Right head
      go (maybeCons mc acc) Nil tail
    Nil, Nil -> pure acc

maybeCons :: ∀ a. Maybe a -> List a -> List a
maybeCons ma list = case ma of
  Just a -> a : list
  Nothing -> list

instance diffableMap :: Ord k => Diffable (Map k) where
  diff = diffMap

diffMap :: ∀ a b c k m. Ord k => Monad m => (Diff a b -> m (Maybe c)) -> Map k a -> Map k b -> m (Map k c)
diffMap f = go mempty
  where
  go :: Map k c -> Map k a -> Map k b -> m (Map k c)
  go acc map1 map2 = case Map.findMin map1, Map.findMin map2 of
    Just { key, value }, _ -> case Map.pop key map2 of
      Just (map2Value /\ tail) -> do
        mc <- f $ Both value map2Value
        go
          (maybeInsert key mc acc)
          (Map.delete key map1)
          tail
      Nothing -> do
        mc <- f $ Left value
        go
          (maybeInsert key mc acc)
          (Map.delete key map1)
          map2
    Nothing, Just { key, value } -> do
      mc <- f $ Right value
      go
        (maybeInsert key mc acc)
        map1
        (Map.delete key map2)
    Nothing, Nothing -> pure acc

maybeInsert :: ∀ k v. Ord k => k -> Maybe v -> Map k v -> Map k v
maybeInsert key mvalue map = case mvalue of
  Just value -> Map.insert key value map
  Nothing -> map
