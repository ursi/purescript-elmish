module Task
  ( Task(..)
  , Canceler
  , bindError
  , capture

  , fail
  , makeTask
  , run
  , main
  ) where

import MasonPrelude
import Callback (Callback)
import Control.Apply (lift2)
import Control.Parallel (class Parallel, parSequence)
import Data.Bifunctor (class Bifunctor)
import Data.Newtype (class Newtype, unwrap)
import Effect.Class.Console (log, logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (setTimeout, clearTimeout)

type Canceler
  = Effect Unit

newtype Task x a
  = Task (Callback a -> Callback x -> Ref Canceler -> Effect Unit)

derive instance newtypeTask :: Newtype (Task x a) _

instance functorTask :: Functor (Task x) where
  map f (Task t) = Task $ t <. (.>) f

instance applyTask :: Apply (Task x) where
  apply (Task tf) (Task ta) = Task $ \bC xC ref -> tf (\f -> ta (bC <. f) xC ref) xC ref

instance applicativeTask :: Applicative (Task x) where
  pure a = Task \aC _ _ -> aC a

instance bindTask :: Bind (Task x) where
  bind (Task ta) f = Task \bC xC ref -> ta (\a -> unwrap (f a) bC xC ref) xC ref

instance monadTask :: Monad (Task x)

instance semigroupTask :: Semigroup a => Semigroup (Task x a) where
  append = lift2 append

instance monoidTask :: Monoid a => Monoid (Task x a) where
  mempty = pure mempty

instance bifunctorTask :: Bifunctor Task where
  bimap lmap rmap task = task <#> rmap # mapError lmap

instance monadEffectTask :: MonadEffect (Task x) where
  liftEffect aEff = Task $ \aC _ _ -> aEff >>= aC

newtype ParTask x a
  = ParTask (Callback a -> Callback x -> Ref Canceler -> Effect Unit)

instance functorParTask :: Functor (ParTask x) where
  map f (ParTask t) = ParTask $ t <. (.>) f

instance applyParTask :: Apply (ParTask x) where
  apply (ParTask tf) (ParTask ta) =
    ParTask
      $ \bC xC ref -> do
          fRef <- Ref.new Nothing
          fErrorRef <- Ref.new false
          fCancelerRef <- Ref.new $ pure unit
          aRef <- Ref.new Nothing
          aErrorRef <- Ref.new false
          aCancelerRef <- Ref.new $ pure unit
          let
            errorCallback :: Ref Boolean -> Ref Boolean -> Callback x
            errorCallback myErrorRef otherErrorRef x = do
              otherError <- Ref.read otherErrorRef
              if otherError then
                pure unit
              else do
                join $ Ref.read fCancelerRef
                join $ Ref.read aCancelerRef
                Ref.write true myErrorRef
                xC x
          tf
            ( \f -> do
                ma <- Ref.read aRef
                case ma of
                  Just a -> bC $ f a
                  Nothing -> Ref.write (Just f) fRef
            )
            (errorCallback fErrorRef aErrorRef)
            fCancelerRef
          ta
            ( \a -> do
                mf <- Ref.read fRef
                case mf of
                  Just f -> bC $ f a
                  Nothing -> Ref.write (Just a) aRef
            )
            (errorCallback aErrorRef fErrorRef)
            aCancelerRef
          Ref.write
            ( do
                join $ Ref.read fCancelerRef
                join $ Ref.read aCancelerRef
            )
            ref

instance applicativePartask :: Applicative (ParTask x) where
  pure a = ParTask \aC _ _ -> aC a

instance parallelTask :: Parallel (ParTask x) (Task x) where
  parallel (Task t) = ParTask t
  sequential (ParTask p) = Task p

mapError :: ∀ a x y. (x -> y) -> Task x a -> Task y a
mapError f (Task t) = Task $ t <~. (.>) f

fail :: ∀ a x. x -> Task x a
fail x = Task \_ xC _ -> xC x

bindError :: ∀ a x y. Task x a -> (x -> Task y a) -> Task y a
bindError (Task tx) f = Task \aC yC ref -> tx aC (\x -> unwrap (f x) aC yC ref) ref

capture :: ∀ a x. (Either x a -> Effect Unit) -> Task x a -> Effect Unit
capture handler (Task t) = do
  ref <- Ref.new $ pure unit
  t (handler <. Right) (handler <. Left) ref

run :: ∀ a x. Task x a -> Effect Unit
run = capture $ const $ pure unit

makeTask :: ∀ a x. (Callback a -> Callback x -> Effect Canceler) -> Task x a
makeTask f = Task \aC xC ref -> f aC xC >>= Ref.write ~$ ref

-- TEST
wait :: ∀ x. Int -> Task x Unit
wait ms =
  makeTask \cb _ -> do
    id <- setTimeout ms (cb unit)
    pure $ clearTimeout id

main :: Effect Unit
main =
  capture logShow
    $ parSequence
        [ do
            wait 500
            log "1"
            wait 500
            log "2"
            wait 500
            log "3"
        , do
            wait 500
            log "4"
            wait 500
            log "5"
            wait 500
            log "6"
        , wait 700 *> fail 5
        -- , fail 3
        -- , fail 2
        ]
