module Data.Batchable where

import Prelude
import Data.Array as Array
import Data.Compactable (class Compactable, compact, separate)
import Data.Diff (class Diffable, Diff, diff2)
import Data.Diff as Diff
import Data.Either (Either(..))
import Data.Filterable (class Filterable, filterDefault, filterMapDefault, partitionDefault, partitionMapDefault)
import Data.Foldable (class Foldable, fold, foldr, foldlDefault, foldMap, foldrDefault)
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

class Batchable b where
  single :: ∀ a. a -> b a
  batch :: ∀ a. Array (b a) -> b a
  flatten :: ∀ a. b a -> List a

class NestedBatchable b n | b -> n where
  nSingle :: ∀ a. n a -> b a
  nBatch :: ∀ a. Array (b a) -> b a
  nFlatten :: ∀ a. b a -> List (n a)

data Batched a
  = Single a
  | Batch (Array (Batched a))

instance batchableBatched :: Batchable Batched where
  single = Single
  batch = Batch
  flatten = flattenBatched

flattenBatched :: ∀ a. Batched a -> List a
flattenBatched = case _ of
  Single a -> pure a
  Batch bs -> foldr go Nil bs
  where
  go :: Batched a -> List a -> List a
  go b acc = case b of
    Single a -> a : acc
    Batch bs -> foldr go acc bs

derive instance genericBatched :: Generic (Batched a) _

-- writing this point free causes a stack overflow
instance showBatched :: Show a => Show (Batched a) where
  show a = genericShow a

instance eqBatched :: Eq a => Eq (Batched a) where
  eq b1 b2 = flatten b1 == flatten b2

instance semigroupBatched :: Semigroup (Batched a) where
  append b1 b2 = case b1, b2 of
    Batch [], _ -> b2
    _, Batch [] -> b1
    _, _ -> Batch [ b1, b2 ]

instance monoidBatched :: Monoid (Batched a) where
  mempty = Batch []

instance functorBatched :: Functor Batched where
  map f = case _ of
    Single a -> Single $ f a
    Batch bs -> Batch $ map f <$> bs

instance applyBatched :: Apply Batched where
  apply = ap

instance applicativeBatched :: Applicative Batched where
  pure = Single

instance bindBatched :: Bind Batched where
  bind b fb = joinBatched $ fb <$> b

instance monadBatched :: Monad Batched

instance foldableBatched :: Foldable Batched where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f = foldMap f <<< flatten

instance traversable :: Traversable Batched where
  traverse = traverseDefault
  sequence = case _ of
    Single ma -> pure Single <*> ma
    Batch bs -> pure Batch <*> (sequence $ sequence <$> bs) --bs :: Array (Batched (m a))

instance functorWithIndexBatched :: FunctorWithIndex (List Int) Batched where
  mapWithIndex f = case _ of
    Single a -> Single $ f (Nil) a
    Batch bs ->
      Batch $ bs
        # mapWithIndex \i ->
            mapWithIndex (f <<< (i : _))

instance foldableWithIndexBatched :: FoldableWithIndex (List Int) Batched where
  foldrWithIndex a = foldrWithIndexDefault a
  foldlWithIndex a = foldlWithIndexDefault a
  foldMapWithIndex f =
    mapWithIndex Tuple
      >>> flatten
      >>> foldMap (uncurry f)

instance traversableWithIndexBatched :: TraversableWithIndex (List Int) Batched where
  traverseWithIndex = traverseWithIndexDefault

instance compactableBatched :: Compactable Batched where
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

instance filterableBatched :: Filterable Batched where
  partitionMap f = partitionMapDefault f
  partition f = partitionDefault f
  filterMap f = filterMapDefault f
  filter f = filterDefault f

instance witherableBatched :: Witherable Batched where
  wilt = wiltDefault
  wither = witherDefault

{-- instance diffableBatched :: Diffable Batched where --}
{--   diff f b1 b2 = case b1, b2 of --}
{--     Single s1, Single s2 -> withered $ Single $ f $ Diff.Both s1 s2 --}
{--     Single s, Batch [] -> withered $ Single $ f $ Diff.Left s --}
{--     Batch [], Single s -> withered $ Single $ f $ Diff.Right s --}
{--     Single _, Batch bs -> Batch <$> diff2 f [ b1 ] bs --}
{--     Batch bs, Single _ -> diff2 f bs [ b2 ] <#> fromMaybe mempty <<< flip Array.index 0 --}
{--     Batch bs1, Batch bs2 -> Batch <$> diff2 f bs1 bs2 --}
joinBatched :: ∀ a. Batched (Batched a) -> Batched a
joinBatched = case _ of
  Single (Single a) -> Single a
  Single (Batch a) -> Batch a
  Batch bs -> Batch $ joinBatched <$> bs
