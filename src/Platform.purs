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
import Sub (Sub, ActiveSub, SubBuilder)
import Sub as Sub
import Html (Html)
import Html as H
import Attribute as A
import VirtualDom as VDom

newtype Cmd msg
  = Cmd ((msg -> Effect Unit) -> Effect Unit)

derive instance newtypeCmd :: Newtype (Cmd a) _

instance semigroupCmd :: Semigroup (Cmd a) where
  append (Cmd c1) (Cmd c2) =
    Cmd \sendMsg -> do
      c1 sendMsg
      c2 sendMsg

instance monoidCmd :: Monoid (Cmd a) where
  mempty = Cmd $ const mempty

attemptTask :: ∀ x a msg. (Either x a -> msg) -> Task x a -> Cmd msg
attemptTask toMsg task = Cmd \sendMsg -> Task.capture (sendMsg <<< toMsg) task

type Program flags model msg
  = EffectFn1 flags Unit

type Shorten msg model
  = WriterT (Cmd msg) Effect model

{-- worker2 :: --}
{--   ∀ flags model msg. --}
{--   { init :: flags -> Shorten msg model --}
{--   , update :: model -> msg -> Shorten msg model --}
{--   {-1- , subscriptions :: model -> Array (Sub msg) -1-}
--}
{--   } -> --}
{--   Program flags model msg --}
{-- worker2 init = --}
{--   mkEffectFn1 \flags -> do --}
{--     initialModel /\ cmd <- runWriterT $ init.init flags --}
{--     unwrap cmd $ update initialModel --}
{--   where --}
{--   update :: model -> msg -> Effect Unit --}
{--   update model msg = do --}
{--     newModel /\ cmd <- runWriterT $ init.update model msg --}
{--     unwrap cmd $ update newModel --}
worker ::
  ∀ flags model msg.
  { init :: flags -> Shorten msg model
  , update :: model -> msg -> Shorten msg model
  , subscriptions :: model -> Sub msg
  } ->
  Program flags model msg
worker init =
  mkEffectFn1 \flags -> do
    activeSubsRef <- Ref.new []
    go activeSubsRef $ init.init flags
  where
  go :: Ref (Array ActiveSub) -> Shorten msg model -> Effect Unit
  go activeSubsRef w = do
    newModel /\ cmd <- runWriterT w
    let
      sendMsg = go activeSubsRef <<< init.update newModel
    activeSubs <- Ref.read activeSubsRef
    newActiveSubs <-
      Sub.something
        activeSubs
        (init.subscriptions newModel)
        (mkEffectFn1 sendMsg)
    Ref.write newActiveSubs activeSubsRef
    unwrap cmd sendMsg

app ::
  ∀ flags model msg.
  { init :: flags -> Shorten msg model
  , update :: model -> msg -> Shorten msg model
  , subscriptions :: model -> Sub msg
  , view :: model -> Array (Html msg)
  } ->
  Program flags model msg
app init =
  mkEffectFn1 \flags -> do
    activeSubsRef <- Ref.new []
    go activeSubsRef $ init.init flags
  where
  go :: Ref (Array ActiveSub) -> Shorten msg model -> Effect Unit
  go activeSubsRef w = do
    newModel /\ cmd <- runWriterT w
    let
      sendMsg = go activeSubsRef <<< init.update newModel
    domSubs <- VDom.render [] $ init.view newModel
    activeSubs <- Ref.read activeSubsRef
    newActiveSubs <-
      Sub.something
        activeSubs
        (domSubs <> init.subscriptions newModel)
        (mkEffectFn1 sendMsg)
    _ <- pure $ Debug.log newActiveSubs
    Ref.write newActiveSubs activeSubsRef
    unwrap cmd sendMsg
