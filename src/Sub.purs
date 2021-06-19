module Sub
  ( ActiveSub
  , Callback
  , Canceler
  , CC
  , fromForeign
  , SingleSub(..)
  , Sub(..)
  , something
  , every
  , every_
  ) where

import MasonPrelude
import Data.Array as Array
import Data.Batched (Batched(..), flatten)
import Debug as Debug
import Partial.Unsafe (unsafePartial)
import Producer (Producer)
import Producer as P
import RefEq (RefEq(..))


type Callback a = a -> Effect Unit
type Canceler = Effect Unit
type CC a = Callback a -> Effect Canceler
type ForeignCC a = EffectFn1 (EffectFn1 a Unit) Canceler

fromForeign :: ∀ a. ForeignCC a -> CC a
fromForeign fcc = runEffectFn1 fcc <. mkEffectFn1

newtype SingleSub a = SingleSub (Producer (CC a))

derive newtype instance Eq (SingleSub a)

instance Functor SingleSub where
  map f (SingleSub p) = SingleSub $ P.producer mapHelper $ RefEq f /\ p

mapHelper :: ∀ a b. RefEq (a -> b) /\ Producer (CC a) -> CC b
mapHelper (RefEq f /\ ccP) = \callback -> P.produce ccP $ callback <. f

type Sub a = Batched SingleSub a

data ActiveSub a =
  ActiveSub
    { canceler :: Canceler
    , sub :: SingleSub a
    }

something :: ∀ a. Array (ActiveSub a) -> Sub a -> Callback a -> Effect (Array (ActiveSub a))
something activeSubs newSub callback =
  go
    activeSubs
    (Array.fromFoldable $ flatten newSub)
    { keep: [], cancel: mempty }
  where
  go ::
    Array (ActiveSub a)
    -> Array (SingleSub a)
    -> { keep :: Array (ActiveSub a)
       , cancel :: Effect Unit
       }
    -> Effect (Array (ActiveSub a))
  go current newSubs acc = case Array.uncons current of
    Just { head, tail } -> case Array.findIndex (sameAsActive head) newSubs of
      Just i ->
        go
          tail
          (unsafePartial unsafeDeleteAt i newSubs)
          acc { keep = Array.snoc acc.keep head }
      Nothing ->
        -- let
        --   _ = Debug.taggedLog "canceled" head
        -- in
        go
          tail
          newSubs
          $ acc { cancel = acc.cancel *> getCanceler head }
    Nothing -> do
      acc.cancel
      newAcitveSubs <- traverse (launch callback) newSubs
      -- let
      --   _ = Debug.taggedLog "kept" acc.keep
      pure $ (acc.keep <> newAcitveSubs)

sameAsActive :: ∀ a. ActiveSub a -> SingleSub a -> Boolean
sameAsActive (ActiveSub a) ss = a.sub == ss

getCanceler :: ∀ a. ActiveSub a -> Canceler
getCanceler (ActiveSub { canceler }) = canceler

launch :: ∀ a. Callback a -> SingleSub a -> Effect (ActiveSub a)
launch callback sub@(SingleSub p) = do
  canceler <- P.produce p callback
  pure $ ActiveSub { canceler, sub }

unsafeDeleteAt :: Partial => ∀ a. Int -> Array a -> Array a
unsafeDeleteAt index array = case Array.deleteAt index array of
  Just a -> a

foreign import everyImpl :: ∀ a. Number -> (Number -> a) -> ForeignCC a

everyImpl' :: ∀ a. Number /\ RefEq (Number -> a) -> CC a
everyImpl' (n /\ RefEq toA) = fromForeign $ everyImpl n toA

every :: ∀ a. Number -> (Number -> a) -> Sub a
every ms toA = Single $ SingleSub $ P.producer everyImpl' $ ms /\ RefEq toA

foreign import every_Impl :: ∀ a. Number -> a -> ForeignCC a

every_Impl' :: ∀ a. Number /\ a -> CC a
every_Impl' (n /\ a) = fromForeign $ every_Impl n a

every_ :: ∀ a. Eq a => Number -> a -> Sub a
every_ ms a = Single $ SingleSub $ P.producer every_Impl' $ ms /\ a
