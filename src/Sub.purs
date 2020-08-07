module Sub
  ( ActiveSub
  , Callback
  , Canceler
  , Sub
  , SubBuilder
  , SubImpl
  , new
  , something
  ) where

import Prelude
import JSEq (jseq)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

data JSValue

instance eqJSValue :: Eq JSValue where
  eq = jseq

toJSValue :: ∀ a. a -> JSValue
toJSValue = unsafeCoerce

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

type SubProperties
  = ( id :: String
    , args :: Array JSValue
    , maps :: Array JSValue
    )

data SubBuilder a
  = SubBuilder { sub :: a | SubProperties }

instance functorSubBuilder :: Functor SubBuilder where
  map f (SubBuilder a@{ maps, sub }) =
    SubBuilder
      $ a
          { maps = Array.snoc maps $ toJSValue f
          , sub = f sub
          }

instance applySubBuilder :: Apply SubBuilder where
  apply (SubBuilder f@{ args, sub }) (SubBuilder a) =
    SubBuilder
      $ f
          { args = Array.snoc args $ toJSValue a.sub
          , sub = sub a.sub
          }

instance applicativeSubBuilder :: Applicative SubBuilder where
  pure a =
    SubBuilder
      { id: ""
      , args: []
      , maps: []
      , sub: a
      }

data Sub a
  = Sub (SubBuilder (SubImpl a))
  | Batch (Array (Sub a))

data ActiveSub
  = ActiveSub { canceler :: Effect Unit | SubProperties }

{-- instance eqSub :: Eq (Sub a) where --}
{--   eq (Sub (SubBuilder s1)) (Sub (SubBuilder s2)) = s1.id == s2.id && s1.args == s2.args --}
{--   eq (Batch subs1) (Batch subs2) = subs1 == subs2 --}
{--   eq _ _ = false --}
instance semigroupSub :: Semigroup (Sub a) where
  append = case _, _ of
    Batch subs1, Batch subs2 -> Batch $ subs1 <> subs2
    Batch subs, sub -> Batch $ Array.snoc subs sub
    sub, Batch subs -> Batch $ Array.snoc subs sub
    sub1, sub2 -> Batch [ sub1, sub2 ]

instance monoidSub :: Monoid (Sub a) where
  mempty = Batch []

instance functorSub :: Functor Sub where
  map f sub_ = case sub_ of
    Sub (SubBuilder s@{ maps, sub }) ->
      Sub $ SubBuilder
        $ s
            { maps = Array.snoc maps (unsafeCoerce f)
            , sub = mapSubImpl f sub
            }
    Batch subs -> Batch $ map f <$> subs

-- | The `String` is the ID of your subscription. It is used to test equality between new and current subscriptions, and, therefore, must be globally unique.
new :: ∀ a. String -> SubBuilder (SubImpl a) -> Sub a
new id (SubBuilder sb) = Sub $ SubBuilder $ sb { id = id }

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
sameAsActive (ActiveSub a) (SubBuilder s) = a.id == s.id && a.args == s.args && a.maps == s.maps

unsafeDeleteAt :: Partial => ∀ a. Int -> Array a -> Array a
unsafeDeleteAt index array = case Array.deleteAt index array of
  Just a -> a

getCanceler :: ActiveSub -> Effect Unit
getCanceler (ActiveSub { canceler }) = canceler

launch :: ∀ a. Callback a -> SubBuilder (SubImpl a) -> Effect ActiveSub
launch callback (SubBuilder { id, args, maps, sub }) = do
  canceler <- runEffectFn1 sub callback
  pure $ ActiveSub { id, args, maps, canceler }

foreign import everyImpl :: Number -> SubImpl Number

every :: ∀ a. Number -> (Number -> a) -> Sub a
every ms toA = toA <$> (new "every" $ pure everyImpl <*> pure ms)

foreign import every_Impl :: ∀ a. Number -> a -> SubImpl a

every_ :: ∀ a. Number -> a -> Sub a
every_ ms a = new "every_" $ pure $ every_Impl ms a
