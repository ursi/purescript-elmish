module Task
  ( Callback
  , Promise
  , Task(..)
  , TaskImpl
  , capture
  , fromPromise
  , logSuccess
  , logShowSuccess
  , onError
  , parallel
  , report
  , run
  , unsafeCapture
  ) where

import MasonPrelude
import Control.Apply (lift2)
import Data.Bifunctor (class Bifunctor)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, logShow)
import Effect.Uncurried (EffectFn1, EffectFn2)

type Callback a
  = EffectFn1 a Unit

type TaskImpl x a
  = EffectFn2 (Callback a) (Callback x) Unit

newtype Task x a
  = Task (TaskImpl x a)

foreign import data Promise :: Type -> Type

foreign import mapImpl :: ∀ x a b. (a -> b) -> Task x a -> Task x b

instance functorTask :: Functor (Task x) where
  map = mapImpl

foreign import applyImpl :: ∀ x a b. Task x (a -> b) -> Task x a -> Task x b

instance applyTask :: Apply (Task x) where
  apply = applyImpl

foreign import pureImpl :: ∀ x a. a -> Task x a

instance applicativeTask :: Applicative (Task x) where
  pure = pureImpl

foreign import bindImpl :: ∀ x a b. Task x a -> (a -> Task x b) -> Task x b

instance bindTask :: Bind (Task x) where
  bind = bindImpl

instance monadTask :: Monad (Task x)

instance semigroupTask :: Semigroup a => Semigroup (Task x a) where
  append = lift2 append

instance monoidTask :: Monoid a => Monoid (Task x a) where
  mempty = pure mempty

foreign import mapError :: ∀ x a y. (x -> y) -> Task x a -> (Task y a)

instance bifunctorTask :: Bifunctor Task where
  bimap lmap rmap task = task <#> rmap # mapError lmap

foreign import fromEffect :: ∀ x a. Effect a -> Task x a

instance monadEffectTask :: MonadEffect (Task x) where
  liftEffect = fromEffect

foreign import onErrorImpl :: ∀ x a y. (x -> Task y a) -> Task x a -> Task y a

onError :: ∀ x a y. (x -> Task y a) -> Task x a -> Task y a
onError = onErrorImpl

foreign import reportImpl ::
  ∀ x a.
  (∀ b c. b -> Either b c) ->
  (∀ b c. b -> Either c b) ->
  (∀ b. b -> Effect Unit) ->
  (Either x a -> Effect Unit) ->
  Task x a ->
  Effect Unit

report ::
  ∀ x a.
  (∀ b. b -> Effect Unit) ->
  (Either x a -> Effect Unit) ->
  Task x a ->
  Effect Unit
report = reportImpl Left Right

foreign import logError :: ∀ a. a -> Effect Unit

capture :: ∀ x a. (Either x a -> Effect Unit) -> Task x a -> Effect Unit
capture = report logError

foreign import unsafeCaptureImpl :: ∀ a b. (a -> b) -> Task Void a -> Effect Unit

unsafeCapture :: ∀ a b. (a -> b) -> Task Void a -> Effect Unit
unsafeCapture = unsafeCaptureImpl

run :: ∀ x a. Task x a -> Effect Unit
run = capture $ const mempty

foreign import fromPromiseImpl :: ∀ a b. (a -> Promise b) -> a -> (Task String b)

fromPromise :: ∀ a b. (a -> Promise b) -> a -> Task String b
fromPromise = fromPromiseImpl

foreign import parallel :: ∀ x a. Array (Task x a) -> Task x (Array a)

logSuccess :: ∀ x. Task x String -> Task x String
logSuccess t = do
  str <- t
  liftEffect $ log str
  pure str

logShowSuccess :: ∀ x a. Show a => Task x a -> Task x a
logShowSuccess t = do
  a <- t
  liftEffect $ logShow a
  pure a
