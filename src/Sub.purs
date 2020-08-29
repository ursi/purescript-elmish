module Sub
  ( ActiveSub
  , Callback
  , Canceler
  , Presub
  , SingleSub
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
import JSEq (jseq)
import Data.Array as Array
import Data.Batchable (Batched(..))
import Data.Batchable as Batchable
import Data.Newtype (class Newtype, unwrap, wrap)
import Debug as Debug
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

data JSValue

instance eqJSValue :: Eq JSValue where
  eq = jseq

toJSValue :: ∀ a. a -> JSValue
toJSValue = unsafeCoerce

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
  SubBuilder
    $ s { sub = \callback -> runEffectFn1 sub (mkEffectFn1 callback) }

type SubProperties
  = ( impl :: JSValue
    , args :: Array JSValue
    , maps :: Array JSValue
    )

data SubBuilder a
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

type SingleSub a
  = SubBuilder (Presub a)

newtype Sub a
  = Sub (Batched (SingleSub a))

instance functorSub :: Functor Sub where
  map f (Sub s) =
    Sub
      $ map
          ( \(SubBuilder s'@{ maps, sub }) ->
              SubBuilder
                $ s'
                    { maps = Array.snoc maps $ toJSValue f
                    , sub = mapPresub f sub
                    }
          )
          s

derive newtype instance semigroupSub :: Semigroup (Sub a)

derive newtype instance monoidSub :: Monoid (Sub a)

new :: ∀ a. SubBuilder (Presub a) -> Sub a
new = Sub <. Single

newForeign :: ∀ a. SubBuilder (ForeignSub a) -> Sub a
newForeign = Sub <. Single <. fromForeign

data ActiveSub
  = ActiveSub { canceler :: Effect Unit | SubProperties }

something :: ∀ a. Array ActiveSub -> Sub a -> Callback a -> Effect (Array ActiveSub)
something activeSubs (Sub newSub) callback =
  go
    activeSubs
    (Array.fromFoldable $ Batchable.flatten newSub)
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
        let
          _ = Debug.taggedLog "canceled" head
        in
          go
            tail
            newSubs
            $ acc { cancel = acc.cancel *> getCanceler head }
    Nothing -> do
      acc.cancel
      newAcitveSubs <- traverse (launch callback) newSubs
      let
        _ = Debug.taggedLog "kept" acc.keep
      pure $ (acc.keep <> newAcitveSubs)

sameAsActive :: ∀ a. ActiveSub -> SingleSub a -> Boolean
sameAsActive (ActiveSub a) (SubBuilder s) = a.impl == s.impl && a.args == s.args && a.maps == s.maps

unsafeDeleteAt :: Partial => ∀ a. Int -> Array a -> Array a
unsafeDeleteAt index array = case Array.deleteAt index array of
  Just a -> a

getCanceler :: ActiveSub -> Canceler
getCanceler (ActiveSub { canceler }) = canceler

launch :: ∀ a. Callback a -> SingleSub a -> Effect ActiveSub
launch callback (SubBuilder { impl, args, maps, sub }) = do
  canceler <- sub callback
  pure $ ActiveSub { impl, args, maps, canceler }

foreign import everyImpl :: Number -> ForeignSub Number

every :: ∀ a. Number -> (Number -> a) -> Sub a
every ms toA = toA <$> (newForeign $ newBuilder everyImpl <@@> ms)

foreign import every_Impl :: ∀ a. Number -> a -> ForeignSub a

every_ :: ∀ a. Number -> a -> Sub a
every_ ms a = newForeign $ newBuilder every_Impl <@@> ms <@@> a
