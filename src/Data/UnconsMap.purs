module Data.UnconsMap where

import Prelude
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))

newtype UnconsMap k v
  = UnconsMap (List (k /\ v) /\ Map k v)

uncons :: ∀ k v. Ord k => UnconsMap k v -> Maybe { head :: k /\ v, tail :: UnconsMap k v }
uncons (UnconsMap (list /\ map)) = do
  { head, tail } <- List.uncons list
  mapTail <- Tuple.snd <$> Map.pop (Tuple.fst head) map
  pure { head, tail: UnconsMap $ tail /\ mapTail }

fromMap :: ∀ k v. Map k v -> UnconsMap k v
fromMap map = UnconsMap $ Map.toUnfoldableUnordered map /\ map

toMap :: ∀ k v. UnconsMap k v -> Map k v
toMap (UnconsMap (_ /\ map)) = map
