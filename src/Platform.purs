module Platform where

import Prelude
import Data.Identity
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (StateT(..), evalStateT, get)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Task (Task)
import Task as Task
import Sub (Sub)

{-- foreign import data Program :: Type -> Type -> Type -> Type --}
newtype Cmd msg
  = Cmd ((msg -> Effect Unit) -> Effect Unit)

derive instance newtypeCmd :: Newtype (Cmd a) _

derive newtype instance semigroupCmd :: Semigroup (Cmd a)

derive newtype instance monoidCmd :: Monoid (Cmd a)

foreign import worker ::
  ∀ flags model msg.
  { init :: flags -> Effect model
  , update :: msg -> model -> WriterT (Cmd msg) Effect model
  {-- , subscriptions :: model -> Array (Sub msg) --}
  } ->
  Program flags model msg

{-- foreign import sendMsg :: ∀ msg. msg -> Effect Unit --}
sendMsg :: ∀ msg. msg -> Effect Unit
sendMsg msg = updateRef >>= Ref.read >>= (#) msg

{-- performTask :: ∀ a msg. (a -> msg) -> Task Void a -> Cmd msg --}
{-- performTask toMsg = Cmd <<< Task.unsafeCapture (sendMsg <<< toMsg) --}
attemptTask :: ∀ x a msg. (Either x a -> msg) -> Task x a -> Cmd msg
attemptTask toMsg task = Cmd \sendMsg -> Task.capture (sendMsg <<< toMsg) task

type Program flags model msg
  = EffectFn1 flags Unit

type Shorten msg model
  = WriterT (Cmd msg) Effect model

worker2 ::
  ∀ flags model msg.
  { init :: flags -> Shorten msg model
  , update :: model -> msg -> Shorten msg model
  {-- , subscriptions :: model -> Array (Sub msg) --}
  } ->
  Program flags model msg
worker2 init =
  mkEffectFn1 \flags -> do
    let
      update :: model -> msg -> Effect Unit
      update model msg = do
        newModel /\ cmds <- runWriterT $ init.update model msg
        setUpdate $ update newModel
        unwrap cmds
    initialModel /\ cmds <- runWriterT $ init.init flags
    setUpdate $ Debug.debugger $ update initialModel
    unwrap cmds

{-- foreign import setUpdate :: ∀ msg model. (msg -> Effect Unit) -> Effect Unit --}
setUpdate :: ∀ msg. (msg -> Effect Unit) -> Effect Unit
setUpdate newUpdate = do
  currentUpdate <- updateRef
  Ref.write newUpdate currentUpdate

updateRef :: ∀ msg. Effect (Ref (msg -> Effect Unit))
updateRef = Ref.new \_ -> log "original"
