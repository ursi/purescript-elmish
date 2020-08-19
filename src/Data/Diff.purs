module Data.Diff
  ( class Diffable
  , Diff(..)
  , diff
  , diff2
  ) where

import Prelude
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UnconsMap (UnconsMap)
import Data.UnconsMap as UnconsMap
import Data.Witherable (class Witherable, withered)
import Debug (todo)
import Debug as Debug

data Diff a b
  = Left a
  | Right b
  | Both a b

-- purty sucks
class
  Witherable f <= Diffable f where
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

{-- instance diffArray :: Diffable Array where --}
{--   diff = diffArray' --}
{-- diffArray' :: ∀ a b c m. Applicative m => (Diff a b -> m (Maybe c)) -> Array a -> Array b -> m (Array c) --}
{-- diffArray' f as' bs' = withered $ go [] as' bs' --}
{--   where --}
{--   go :: Array (m (Maybe c)) -> Array a -> Array b -> Array (m (Maybe c)) --}
{--   go acc as bs = case Array.uncons as, Array.uncons bs of --}
{--     Just a, Just b -> --}
{--       (Array.snoc acc $ f $ Both a.head b.head) --}
{--         <> go acc a.tail b.tail --}
{--     Just a, Nothing -> --}
{--       (Array.snoc acc $ f $ Left a.head) --}
{--         <> go acc a.tail [] --}
{--     Nothing, Just b -> --}
{--       (Array.snoc acc $ f $ Right b.head) --}
{--         <> go acc [] b.tail --}
{--     Nothing, Nothing -> acc --}
diffList' :: ∀ a b c m. Applicative m => (Diff a b -> m (Maybe c)) -> List a -> List b -> m (List c)
diffList' f as' bs' = withered $ List.reverse $ go Nil as' bs'
  where
  go :: List (m (Maybe c)) -> List a -> List b -> List (m (Maybe c))
  go acc as bs = case List.uncons as, List.uncons bs of
    Just a, Just b -> go (f (Both a.head b.head) : acc) a.tail b.tail
    Just a, Nothing -> go (f (Left a.head) : acc) a.tail Nil
    Nothing, Just b -> go (f (Right b.head) : acc) Nil b.tail
    Nothing, Nothing -> acc

instance diffableList :: Diffable List where
  diff = diffList

diffList :: ∀ a b c m. Monad m => (Diff a b -> m (Maybe c)) -> List a -> List b -> m (List c)
diffList f as' bs' = List.reverse <$> go Nil as' bs'
  where
  go :: List c -> List a -> List b -> m (List c)
  go acc as bs = case List.uncons as, List.uncons bs of
    Just a, Just b -> do
      mc <- f $ Both a.head b.head
      go (maybeCons mc acc) a.tail b.tail
    Just a, Nothing -> do
      mc <- f $ Left a.head
      go (maybeCons mc acc) a.tail Nil
    Nothing, Just b -> do
      mc <- f $ Right b.head
      go (maybeCons mc acc) Nil b.tail
    Nothing, Nothing -> pure acc

maybeCons :: ∀ a. Maybe a -> List a -> List a
maybeCons ma list = case ma of
  Just a -> a : list
  Nothing -> list

instance diffableMap :: Ord k => Diffable (Map k) where
  diff = diffMap

diffMap :: ∀ a b c k m. Ord k => Monad m => (Diff a b -> m (Maybe c)) -> Map k a -> Map k b -> m (Map k c)
diffMap f =
  go
    mempty
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
