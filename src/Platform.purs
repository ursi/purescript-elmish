module Platform
  ( Program
  , attemptTask
  , performTask
  , worker
  ) where

import Prelude
import Data.Either (Either)
import Effect (Effect)
import Task (Task)
import Task as Task
import Sub (Sub)

foreign import data Program :: Type -> Type -> Type -> Type

foreign import worker ::
  ∀ flags model msg.
  { init :: flags -> Effect model
  , update :: msg -> model -> Effect model
  , subscriptions :: model -> Array (Sub msg)
  } ->
  Program flags model msg

foreign import sendMsg :: ∀ msg. msg -> Effect Unit

performTask :: ∀ a msg. (a -> msg) -> Task Void a -> Effect Unit
performTask toMsg = Task.unsafeCapture (toMsg >>> sendMsg)

attemptTask :: ∀ x a msg. (Either x a -> msg) -> Task x a -> Effect Unit
attemptTask toMsg = Task.capture (toMsg >>> sendMsg)
