module Data.Batchable where

import Prelude
import Data.Array as Array
import Data.Compactable (class Compactable, compact, separate)
import Data.Diff (class Diffable, Diff, diff2)
import Data.Diff as Diff
import Data.Either (Either(..))
import Data.Filterable (class Filterable, filterDefault, filterMapDefault, partitionDefault, partitionMapDefault)
import Data.Foldable (class Foldable, fold, foldlDefault, foldMap, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndexDefault, foldrWithIndexDefault)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (class Traversable, traverseDefault, sequence)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndexDefault)
import Data.Tuple (Tuple(..), uncurry)
import Data.Witherable (class Witherable, wiltDefault, witherDefault, withered)

data Batchable a
  = Single a
  | Batch (Array (Batchable a))

derive instance genericBatchable :: Generic (Batchable a) _

-- writing this point free causes a stack overflow
instance showBatchable :: Show a => Show (Batchable a) where
  show a = genericShow a

derive instance eqBatchable :: Eq a => Eq (Batchable a)

instance semigroupBatchable :: Semigroup (Batchable a) where
  append = case _, _ of
    Batch [], b -> b
    b, Batch [] -> b
    Batch b1, Batch b2 -> Batch $ b1 <> b2
    Batch b, s -> Batch $ Array.snoc b s
    s, Batch b -> Batch $ Array.cons s b
    s1, s2 -> Batch [ s1, s2 ]

instance monoidBatchable :: Monoid (Batchable a) where
  mempty = Batch mempty

instance functorBatchable :: Functor Batchable where
  map f = case _ of
    Single a -> Single $ f a
    Batch bs -> Batch $ map f <$> bs

instance applyBatchable :: Apply Batchable where
  apply = ap

instance applicativeBatchable :: Applicative Batchable where
  pure = Single

instance bindBatchable :: Bind Batchable where
  bind b fb = joinBatchable $ fb <$> b

instance monadBatchable :: Monad Batchable

instance foldableBatchable :: Foldable Batchable where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f = foldMap f <<< flatten

instance traversable :: Traversable Batchable where
  traverse = traverseDefault
  sequence = case _ of
    Single ma -> pure Single <*> ma
    Batch bs -> pure Batch <*> (sequence $ sequence <$> bs) --bs :: Array (Batchable (m a))

instance functorWithIndexBatchable :: FunctorWithIndex (List Int) Batchable where
  mapWithIndex f = case _ of
    Single a -> Single $ f (Nil) a
    Batch bs ->
      Batch $ bs
        # mapWithIndex \i ->
            mapWithIndex (f <<< (i : _))

instance foldableWithIndexBatchable :: FoldableWithIndex (List Int) Batchable where
  foldrWithIndex a = foldrWithIndexDefault a
  foldlWithIndex a = foldlWithIndexDefault a
  foldMapWithIndex f =
    mapWithIndex Tuple
      >>> flatten
      >>> foldMap (uncurry f)

instance traversableWithIndexBatchable :: TraversableWithIndex (List Int) Batchable where
  traverseWithIndex = traverseWithIndexDefault

instance compactableBatchable :: Compactable Batchable where
  compact = case _ of
    Single ma -> case ma of
      Just a -> Single a
      Nothing -> mempty
    Batch bs -> fold $ compact <$> bs
  separate = case _ of
    Single e -> case e of
      Left l -> { left: Single l, right: mempty }
      Right r -> { left: mempty, right: Single r }
    Batch bs -> fold $ separate <$> bs

instance filterableBatchable :: Filterable Batchable where
  partitionMap f = partitionMapDefault f
  partition f = partitionDefault f
  filterMap f = filterMapDefault f
  filter f = filterDefault f

instance witherableBatchable :: Witherable Batchable where
  wilt = wiltDefault
  wither = witherDefault

instance diffableBatchable :: Diffable Batchable where
  diff f b1 b2 = case b1, b2 of
    Single s1, Single s2 -> withered $ Single $ f $ Diff.Both s1 s2
    Single s, Batch [] -> withered $ Single $ f $ Diff.Left s
    Batch [], Single s -> withered $ Single $ f $ Diff.Right s
    Single _, Batch bs -> Batch <$> diff2 f [ b1 ] bs
    Batch bs, Single _ -> diff2 f bs [ b2 ] <#> fromMaybe mempty <<< flip Array.index 0
    Batch bs1, Batch bs2 -> Batch <$> diff2 f bs1 bs2

flatten :: ∀ a. Batchable a -> Array a
flatten = case _ of
  Single a -> [ a ]
  Batch bs -> go [] bs
  where
  go :: Array a -> Array (Batchable a) -> Array a
  go acc bs = case Array.uncons bs of
    Just { head, tail } -> case head of
      Single a -> go (Array.snoc acc a) tail
      Batch bs' -> go (go acc bs') tail
    Nothing -> acc

joinBatchable :: ∀ a. Batchable (Batchable a) -> Batchable a
joinBatchable = case _ of
  Single (Single a) -> Single a
  Single (Batch a) -> Batch a
  Batch bs -> Batch $ joinBatchable <$> bs
