module Data.Diff
  ( class Diffable
  , Diff(..)
  , diff
  , diff2
  ) where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Witherable (class Witherable, withered)

data Diff a b
  = Left a
  | Right b
  | Both a b

-- purty sucks
class
  Witherable f <= Diffable f where
  diff :: ∀ a b c m. Applicative m => (Diff a b -> m (Maybe c)) -> f a -> f b -> m (f c)

diff2 ::
  ∀ a b c f g m.
  Diffable f =>
  Diffable g =>
  Monoid (g a) =>
  Monoid (g b) =>
  Applicative m =>
  (Diff a b -> m (Maybe c)) ->
  f (g a) ->
  f (g b) ->
  m (f (g c))
diff2 f fga fgb =
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

instance diffArray :: Diffable Array where
  diff = diffArray'

diffArray' :: ∀ a b c m. Applicative m => (Diff a b -> m (Maybe c)) -> Array a -> Array b -> m (Array c)
diffArray' f as' bs' = withered $ go [] as' bs'
  where
  go :: Array (m (Maybe c)) -> Array a -> Array b -> Array (m (Maybe c))
  go acc as bs = case Array.uncons as, Array.uncons bs of
    Just a, Just b ->
      (Array.snoc acc $ f $ Both a.head b.head)
        <> go acc a.tail b.tail
    Just a, Nothing ->
      (Array.snoc acc $ f $ Left a.head)
        <> go acc a.tail []
    Nothing, Just b ->
      (Array.snoc acc $ f $ Right b.head)
        <> go acc [] b.tail
    Nothing, Nothing -> acc
