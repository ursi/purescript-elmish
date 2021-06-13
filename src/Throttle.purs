module Throttle
  ( Skipped
  , throttle
  )
  where

import MasonPrelude
import Callback (Callback)
import Effect.Ref as Ref

type Skipped = Boolean

throttle :: ∀ a. (Effect Unit -> Effect Unit) -> Callback a -> Effect (a -> Effect Skipped)
throttle switch callback = do
  throttlingRef <- Ref.new false
  savedValueRef <- Ref.new (Nothing :: Maybe a)
  let
    go :: Effect Unit
    go =
      switch do
        savedValue <- Ref.read savedValueRef

        case savedValue of
          Just a -> do
            Ref.write Nothing savedValueRef
            Ref.write true throttlingRef
            go
            callback a

          Nothing -> Ref.write false throttlingRef

  pure \a -> do
    throttling <- Ref.read throttlingRef

    if throttling then do
      Ref.write (Just a) savedValueRef
      pure true
    else do
      Ref.write true throttlingRef
      go
      callback a
      pure false

-- dontSavehrottle :: ∀ a. (Effect Unit -> Effect Unit) -> Callback a -> Effect (Callback a)
-- dontSavehrottle switch effect = do
--   throttlingRef <- Ref.new false
--   pure \a -> do
--     throttling <- Ref.read throttlingRef
--     if (not throttling) then do
--       switch $ Ref.write false throttlingRef
--       Ref.write true throttlingRef
--       effect a
--     else
--       pure unit
