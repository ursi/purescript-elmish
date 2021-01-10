module Sub
  ( ActiveSub
  , Callback
  , Canceler
  , ForeignSub
  , Presub
  , SingleSub(..)
  , Sub(..)
  , SubBuilder
  , (<@@>)
  , flap
  , new
  , newBuilder
  , newForeign
  , something
  , every
  , every_
  ) where

import MasonPrelude
import Data.Array as Array
import Data.Batched (Batched(..), flatten)
import Data.JSValue (JSValue, toJSValue)
import Data.Newtype (class Newtype, unwrap, wrap)
import Debug as Debug
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

type Callback a
  = a -> Effect Unit

type Canceler
  = Effect Unit

type ForeignSub a
  = (EffectFn1 (EffectFn1 a Unit) Canceler)

type Presub a
  = Callback a -> Effect Canceler

mapPresub :: ∀ a b. (a -> b) -> Presub a -> Presub b
mapPresub f p callback = p $ callback <. f

fromForeign :: ∀ a. SubBuilder (ForeignSub a) -> SingleSub a
fromForeign (SubBuilder s@{ sub }) =
  SingleSub $ SubBuilder
    $ s { sub = runEffectFn1 sub <. mkEffectFn1 }

type SubProperties
  = ( impl :: JSValue
    , args :: Array JSValue
    , maps :: Array JSValue
    )

newtype SubBuilder a
  = SubBuilder { sub :: a | SubProperties }

newBuilder :: ∀ a. a -> SubBuilder a
newBuilder impl =
  SubBuilder
    { impl: toJSValue impl
    , args: []
    , maps: []
    , sub: impl
    }

flap :: ∀ a b. SubBuilder (a -> b) -> a -> SubBuilder b
flap (SubBuilder s@{ args, sub }) a =
  SubBuilder
    $ s
        { args = Array.snoc args $ toJSValue a
        , sub = sub a
        }

infixl 4 flap as <@@>

newtype SingleSub a
  = SingleSub (SubBuilder (Presub a))

instance functorSingleSub :: Functor SingleSub where
  map f (SingleSub s) =
    SingleSub
      $ ( \(SubBuilder s'@{ maps, sub }) ->
            SubBuilder
              $ s'
                  { maps = Array.snoc maps $ toJSValue f
                  , sub = mapPresub f sub
                  }
        )
          s

type Sub a
  = Batched SingleSub a

new :: ∀ a. SingleSub a -> Sub a
new = Single

newForeign :: ∀ a. SubBuilder (ForeignSub a) -> Sub a
newForeign = Single <. fromForeign

data ActiveSub
  = ActiveSub { canceler :: Effect Unit | SubProperties }

something :: ∀ a. Array ActiveSub -> Sub a -> Callback a -> Effect (Array ActiveSub)
something activeSubs newSub callback =
  go
    activeSubs
    (Array.fromFoldable $ flatten newSub)
    { keep: [], cancel: mempty }
  where
  go ::
    Array ActiveSub ->
    Array (SingleSub a) ->
    { keep :: Array ActiveSub
    , cancel :: Effect Unit
    } ->
    Effect (Array ActiveSub)
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

sameAsActive :: ∀ a. ActiveSub -> SingleSub a -> Boolean
sameAsActive (ActiveSub a) (SingleSub (SubBuilder s)) = a.impl == s.impl && a.args == s.args && a.maps == s.maps

unsafeDeleteAt :: Partial => ∀ a. Int -> Array a -> Array a
unsafeDeleteAt index array = case Array.deleteAt index array of
  Just a -> a

getCanceler :: ActiveSub -> Canceler
getCanceler (ActiveSub { canceler }) = canceler

launch :: ∀ a. Callback a -> SingleSub a -> Effect ActiveSub
launch callback (SingleSub (SubBuilder { impl, args, maps, sub })) = do
  canceler <- sub callback
  pure $ ActiveSub { impl, args, maps, canceler }

foreign import everyImpl :: Number -> ForeignSub Number

every :: ∀ a. Number -> (Number -> a) -> Sub a
every ms toA = toA <$> (newForeign $ newBuilder everyImpl <@@> ms)

foreign import every_Impl :: ∀ a. Number -> a -> ForeignSub a

every_ :: ∀ a. Number -> a -> Sub a
every_ ms a = newForeign $ newBuilder every_Impl <@@> ms <@@> a
