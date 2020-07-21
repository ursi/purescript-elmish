module Sub where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Array as Array
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried
import Partial.Unsafe (unsafePartial)

type Callback a
  = EffectFn1 a Unit

type Canceler
  = Effect Unit

newtype SubImpl a
  = SubImpl (EffectFn1 (Callback a) Canceler)

foreign import mapSubImplImpl :: ∀ a b. (a -> b) -> SubImpl a -> SubImpl b

instance functorSubImpl :: Functor SubImpl where
  map = mapSubImplImpl

type SubBuilder a
  = { id :: String
    , args :: Array Json
    , sub :: a
    }

data Sub a
  = Sub (SubBuilder (SubImpl a))
  | Batch (Array (Sub a))

instance eqSub :: Eq (Sub a) where
  eq (Sub s1) (Sub s2) = s1.id == s2.id && s1.args == s2.args
  eq (Batch subs1) (Batch subs2) = subs1 == subs2
  eq _ _ = false

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
    Sub s@{ sub } -> Sub $ s { sub = f <$> sub }
    Batch subs -> Batch $ map f <$> subs

addArg :: ∀ a b. a -> (a -> Json) -> SubBuilder (a -> b) -> SubBuilder b
addArg arg toJson subBuilder@{ args, sub } =
  subBuilder
    { args = Array.snoc args $ toJson arg
    , sub = sub arg
    }

new :: ∀ a. String -> a -> SubBuilder a
new id a = { id, args: [], sub: a }

flatten :: ∀ a. Sub a -> Array (Sub a)
flatten sub = case sub of
  Sub _ -> [ sub ]
  Batch subs -> join $ flatten <$> subs

-- not good enough, gotta do something like current too
something ::
  ∀ a.
  Sub a ->
  Sub a ->
  { new :: Array (Sub a)
  , cancel :: Array (Sub a)
  }
something oldSub newSub =
  let
    oldSubs = flatten oldSub

    newSubs = flatten newSub
  in
    go oldSubs newSubs { new: [], cancel: [] }
  where
  go ::
    Array (Sub a) ->
    Array (Sub a) ->
    { new :: Array (Sub a)
    , cancel :: Array (Sub a)
    } ->
    { new :: Array (Sub a)
    , cancel :: Array (Sub a)
    }
  go old new acc = case Array.uncons old of
    Just { head, tail } -> case Array.elemIndex head new of
      Just i ->
        go
          tail
          (unsafePartial unsafeDeleteAt i new)
          acc
      Nothing -> go tail new $ acc { cancel = Array.snoc acc.cancel head }
    Nothing -> acc { new = new }

unsafeDeleteAt :: Partial => ∀ a. Int -> Array a -> Array a
unsafeDeleteAt index array = case Array.deleteAt index array of
  Just a -> a

-- name can change later
newtype ActiveSub
  = ActiveSub
  { id :: String
  , args :: Array Json
  , canceler :: Effect Unit
  }

launch :: ∀ a. SubBuilder (SubImpl a) -> Callback a -> Effect ActiveSub
launch { id, args, sub } callback = case sub of
  SubImpl f -> do
    canceler <- runEffectFn1 f callback
    pure $ ActiveSub { id, args, canceler }

-- | Represents a function that takes in a value of type `a` and sends a value of type `b` to the update function.
-- | A simple callback function is of type `Sender a a` for some `a`.
{-- foreign import none :: ∀ a. Sub a --}
foreign import everyImpl :: Number -> (SubImpl Number)

every :: ∀ msg. Number -> (Number -> msg) -> Sub msg
every ms toMsg =
  new "every" everyImpl
    # addArg ms A.fromNumber
    # Sub
    <#> toMsg
