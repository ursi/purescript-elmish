module Throttle
  ( throttle
  ) where

import MasonPrelude
import Callback (Callback)
import Effect.Ref as Ref

throttle :: ∀ a. (Effect Unit -> Effect Unit) -> Callback a -> Effect (Callback a)
throttle switch callback = do
  throttlingRef <- Ref.new false
  savedValueRef <- Ref.new (Nothing :: Maybe a)
  let
    go :: Effect Unit
    go =
      switch
        ( do
            savedValue <- Ref.read savedValueRef
            case savedValue of
              Just a -> do
                go
                callback a
                Ref.write Nothing savedValueRef
                Ref.write true throttlingRef
              Nothing -> Ref.write false throttlingRef
        )
  pure \a -> do
    throttling <- Ref.read throttlingRef
    if throttling then
      Ref.write (Just a) savedValueRef
    else do
      go
      callback a
      Ref.write true throttlingRef

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
