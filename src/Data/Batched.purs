module Data.Batched where

import MasonPrelude hiding (foldl, foldr, foldMap)
import Data.Array ((!!))
import Data.Foldable as Foldable
import Data.List ((:))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)

data Batched :: (Type -> Type) -> Type -> Type
data Batched a b
  = Single (a b)
  | Batch (Array (Batched a b))

instance Functor a => Functor (Batched a) where
  map f = case _ of
    Single ab -> Single $ f <$> ab
    Batch as -> Batch $ (map f) <$> as

instance Apply a => Apply (Batched a) where
  apply batched_af batched_ab = case batched_af, batched_ab of
    Single af, Single ab -> Single $ af <*> ab
    Batch array_batched_afs, _ ->
      Batch $ array_batched_afs
        <#> \batched_af' ->
            batched_af' <*> batched_ab
    _, Batch array_abs -> Batch $ (apply batched_af) <$> array_abs

instance Applicative a => Applicative (Batched a) where
  pure = Single <. pure

instance Semigroup (Batched a b) where
  append b1 b2 = case b1, b2 of
    Batch [], _ -> b2
    _, Batch [] -> b1
    _, _ -> Batch [ b1, b2 ]

instance Monoid (Batched a b) where
  mempty = Batch []

class NestedFoldable :: ((Type -> Type) -> Type -> Type) -> Constraint
class NestedFoldable f where
  foldr :: ∀ a b c. (a b -> c -> c) -> c -> f a b -> c
  foldl :: ∀ a b c. (c -> a b -> c) -> c -> f a b -> c
  foldMap :: ∀ a b m. Monoid m => (a b -> m) -> f a b -> m

foldrDefault :: ∀ f a b c. NestedFoldable f => (a b -> c -> c) -> c -> f a b -> c
foldrDefault c u xs = unwrap (foldMap (Endo <. c) xs) u

foldlDefault :: ∀ f a b c. NestedFoldable f => (c -> a b -> c) -> c -> f a b -> c
foldlDefault c u xs = unwrap (unwrap (foldMap (Dual <. Endo <. flip c) xs)) u

instance NestedFoldable Batched where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap ab2m = case _ of
    Single ab -> ab2m ab
    Batch as -> Foldable.foldMap (foldMap ab2m) as

flattenMap :: ∀ a b c. (a b -> c) -> Batched a b -> List c
flattenMap f = foldr ((:) <. f) Nil

flatten :: ∀ a b. Batched a b -> List (a b)
flatten = flattenMap identity

first :: ∀ a b. Batched a b -> Maybe (a b)
first = case _ of
  Single ab -> Just ab
  Batch bs -> bs !! 0 >>= first
