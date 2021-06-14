module MyMap where

import MasonPrelude
import Data.Map (Map)
import Data.Map as Map
import Safe.Coerce (coerce)

newtype MyMap a b = MyMap (Map a b)

instance Ord a => Semigroup (MyMap a b) where
  append (MyMap m1) (MyMap m2) = coerce $ Map.unionWith const m1 m2

instance Ord a => Monoid (MyMap a b) where
  mempty = MyMap Map.empty

derive newtype instance Foldable (MyMap a)
derive newtype instance FoldableWithIndex a (MyMap a)

singleton :: ∀ a b. a -> b -> MyMap a b
singleton = coerce <.. Map.singleton
