module Sub
  ( ActiveSub
  , Callback
  , Canceler
  , Sub(Sub)
  , SubBuilder
  , SubImpl
  , addArg
  , new
  , something
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Partial.Unsafe (unsafePartial)

type Callback a
  = EffectFn1 a Unit

type Canceler
  = Effect Unit

type SubImpl a
  = (EffectFn1 (Callback a) Canceler)

mapSubImpl :: ∀ a b. (a -> b) -> SubImpl a -> SubImpl b
mapSubImpl f s =
  mkEffectFn1 \callback ->
    runEffectFn1 s $ mkEffectFn1 \a -> runEffectFn1 callback $ f a

data SubBuilder a
  = SubBuilder
    { id :: String
    , args :: Array Json
    , sub :: a
    }

data Sub a
  = Sub (SubBuilder (SubImpl a))
  | Batch (Array (Sub a))

data ActiveSub
  = ActiveSub
    { id :: String
    , args :: Array Json
    , canceler :: Effect Unit
    }

{-- instance eqSub :: Eq (Sub a) where --}
{--   eq (Sub (SubBuilder s1)) (Sub (SubBuilder s2)) = s1.id == s2.id && s1.args == s2.args --}
{--   eq (Batch subs1) (Batch subs2) = subs1 == subs2 --}
{--   eq _ _ = false --}
instance semigroupSub :: Semigroup (Sub a) where
  append s1 s2 = case s1, s2 of
    Sub _, Sub _ -> Batch [ s1, s2 ]
    Sub _, Batch subs -> Batch $ Array.snoc subs s1
    Batch subs, Sub _ -> Batch $ Array.snoc subs s2
    Batch subs1, Batch subs2 -> Batch $ subs1 <> subs2

instance monoidSub :: Monoid (Sub a) where
  mempty = Batch []

instance functorSub :: Functor Sub where
  map f sub_ = case sub_ of
    Sub (SubBuilder s@{ sub }) -> Sub $ SubBuilder $ s { sub = mapSubImpl f sub }
    Batch subs -> Batch $ map f <$> subs

addArg :: ∀ a b. a -> (a -> Json) -> SubBuilder (a -> b) -> SubBuilder b
addArg arg toJson (SubBuilder subBuilder@{ args, sub }) =
  SubBuilder
    $ subBuilder
        { args = Array.snoc args $ toJson arg
        , sub = sub arg
        }

-- | The `String` is the ID of your subscription. It is used to test equality between new and current subscriptions, and, therefore, must be globally unique.
new :: ∀ a. String -> a -> SubBuilder a
new id a = SubBuilder { id, args: [], sub: a }

flatten :: ∀ a. Sub a -> Array (SubBuilder (SubImpl a))
flatten = case _ of
  Sub ss -> [ ss ]
  Batch subs -> join $ flatten <$> subs

something :: ∀ a. Array ActiveSub -> Sub a -> Callback a -> Effect (Array ActiveSub)
something activeSubs newSub callback =
  go
    activeSubs
    (flatten newSub)
    { keep: [], cancel: mempty }
  where
  go ::
    Array ActiveSub ->
    Array (SubBuilder (SubImpl a)) ->
    { keep :: Array ActiveSub
    , cancel :: Effect Unit
    } ->
    Effect (Array ActiveSub)
  go current newSubs acc = case Array.uncons current of
    Just { head, tail } -> case head of
      ActiveSub { id: "", canceler } ->
        go
          tail
          newSubs
          $ acc { cancel = acc.cancel *> canceler }
      ActiveSub { canceler } -> case Array.findIndex (sameAsActive head) newSubs of
        Just i ->
          go
            tail
            (unsafePartial unsafeDeleteAt i newSubs)
            acc { keep = Array.snoc acc.keep head }
        Nothing ->
          go
            tail
            newSubs
            $ acc { cancel = acc.cancel *> canceler }
    Nothing -> do
      acc.cancel
      newAcitveSubs <- traverse (launch callback) newSubs
      pure $ acc.keep <> newAcitveSubs

sameAsActive :: ∀ a. ActiveSub -> SubBuilder (SubImpl a) -> Boolean
sameAsActive (ActiveSub a) (SubBuilder s) = a.id == s.id && a.args == s.args

unsafeDeleteAt :: Partial => ∀ a. Int -> Array a -> Array a
unsafeDeleteAt index array = case Array.deleteAt index array of
  Just a -> a

getCanceler :: ActiveSub -> Effect Unit
getCanceler (ActiveSub { canceler }) = canceler

launch :: ∀ a. Callback a -> SubBuilder (SubImpl a) -> Effect ActiveSub
launch callback (SubBuilder { id, args, sub }) = do
  canceler <- runEffectFn1 sub callback
  pure $ ActiveSub { id, args, canceler }
