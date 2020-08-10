module Sub
  ( ActiveSub
  , Callback
  , Canceler
  , Presub
  , SingleSub
  , Sub(..)
  , SubBuilder
  , new
  , something
  ) where

import Prelude
import JSEq (jseq)
import Data.Array as Array
import Data.Batchable (Batchable(..))
import Data.Batchable as Batchable
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
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
  = a -> Effect Unit

type Canceler
  = Effect Unit

type ForeignSub a
  = (EffectFn1 (EffectFn1 a Unit) Canceler)

type Presub a
  = Callback a -> Effect Canceler

mapPresub :: ∀ a b. (a -> b) -> Presub a -> Presub b
mapPresub f p callback = p $ callback <<< f

fromForeign :: ∀ a. ForeignSub a -> Presub a
fromForeign f callback = runEffectFn1 f (mkEffectFn1 callback)

type SubProperties
  = ( args :: Array JSValue
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
      { args: []
      , maps: []
      , sub: a
      }

data SingleSub a
  = SingleSub JSValue (SubBuilder (Presub a))

instance functorSingleSub :: Functor SingleSub where
  map f (SingleSub impl sb) = SingleSub impl (mapPresub f <$> sb)

newtype Sub a
  = Sub (Batchable (SingleSub a))

instance functorSub :: Functor Sub where
  map f (Sub s) = Sub $ map f <$> s

derive newtype instance semigroupSub :: Semigroup (Sub a)

derive newtype instance monoidSub :: Monoid (Sub a)

new :: ∀ impl a. impl -> SubBuilder (Presub a) -> Sub a
new impl sb = Sub $ Single $ SingleSub (toJSValue impl) sb

data ActiveSub
  = ActiveSub JSValue { canceler :: Effect Unit | SubProperties }

something :: ∀ a. Array ActiveSub -> Sub a -> Callback a -> Effect (Array ActiveSub)
something activeSubs (Sub newSub) callback =
  go
    activeSubs
    (Batchable.flatten newSub)
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
        go
          tail
          newSubs
          $ acc { cancel = acc.cancel *> getCanceler head }
    Nothing -> do
      acc.cancel
      newAcitveSubs <- traverse (launch callback) newSubs
      pure $ acc.keep <> newAcitveSubs

sameAsActive :: ∀ a. ActiveSub -> SingleSub a -> Boolean
sameAsActive (ActiveSub impl1 a) (SingleSub impl2 (SubBuilder s)) = impl1 == impl2 && a.args == s.args && a.maps == s.maps

unsafeDeleteAt :: Partial => ∀ a. Int -> Array a -> Array a
unsafeDeleteAt index array = case Array.deleteAt index array of
  Just a -> a

getCanceler :: ActiveSub -> Canceler
getCanceler (ActiveSub _ { canceler }) = canceler

launch :: ∀ a. Callback a -> SingleSub a -> Effect ActiveSub
launch callback (SingleSub impl (SubBuilder { args, maps, sub })) = do
  canceler <- sub callback
  pure $ ActiveSub impl { args, maps, canceler }

foreign import everyImpl :: Number -> ForeignSub Number

{-- every :: ∀ a. Number -> (Number -> a) -> Sub a --}
{-- every ms toA = --}
{--   toA --}
{--     <$> ( new everyImpl --}
{--           $ pure everyImpl --}
{--           <*> pure ms --}
{--           <#> fromForeign --}
{--       ) --}
foreign import every_Impl :: ∀ a. Number -> a -> ForeignSub a

{-- every_ :: ∀ a. Number -> a -> Sub a --}
{-- every_ ms a = new every_Impl $ pure every_Impl <*> pure ms <*> pure a <#> fromForeign --}
