module Data.Batchable where

import MasonPrelude
import Data.Array ((!!))
import Data.Array as Array
import Data.Diff (class Diffable, Diff, diff2)
import Data.Diff as Diff
import Data.Foldable (foldlDefault, foldrDefault)
import Data.FoldableWithIndex (foldlWithIndexDefault, foldrWithIndexDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List ((:))
import Data.Traversable (traverseDefault)
import Data.TraversableWithIndex (traverseWithIndexDefault)
import Data.Tuple (Tuple(..), uncurry)

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
  foldMap f = foldMap f <. flatten

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
            mapWithIndex (f <. (i : _))

instance foldableWithIndexBatched :: FoldableWithIndex (List Int) Batched where
  foldrWithIndex a = foldrWithIndexDefault a
  foldlWithIndex a = foldlWithIndexDefault a
  foldMapWithIndex f =
    mapWithIndex Tuple
      .> flatten
      .> foldMap (uncurry f)

instance traversableWithIndexBatched :: TraversableWithIndex (List Int) Batched where
  traverseWithIndex = traverseWithIndexDefault

joinBatched :: ∀ a. Batched (Batched a) -> Batched a
joinBatched = case _ of
  Single (Single a) -> Single a
  Single (Batch a) -> Batch a
  Batch bs -> Batch $ joinBatched <$> bs

first :: ∀ a. Batched a -> Maybe a
first = case _ of
  Single a -> Just a
  Batch bs -> bs !! 0 >>= first
