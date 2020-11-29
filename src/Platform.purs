module Platform
  ( module Exports
  , Cmd(..)
  , Program
  , Update
  , app
  , worker
  , html
  , attemptTask
  ) where

import MasonPrelude
import Attribute as A
import Data.Identity
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (StateT(..), evalStateT, get)
import Control.Monad.Trans.Class (lift)
import Data.Batchable (Batched(..), flatten)
import Data.Newtype (class Newtype, unwrap)
import Debug as Debug
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Html (Html)
import Html as H
import Sub (Sub, ActiveSub, SubBuilder)
import Sub as Sub
import Task (Task)
import Task as Task
import Throttle (throttle)
import VirtualDom (VDOM)
import VirtualDom as VDom
import WHATWG.HTML.All (Document, Element)
import WHATWG.HTML.All as H
import Control.Monad.Writer (tell) as Exports

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
attemptTask toMsg task = Cmd \sendMsg -> Task.capture (sendMsg <. toMsg) task

type Program flags model msg
  = EffectFn1 flags Unit

type Update model msg
  = WriterT (Cmd msg) Effect model

{-- worker2 :: --}
{--   ∀ flags model msg. --}
{--   { init :: flags -> Update model msg --}
{--   , update :: model -> msg -> Update model msg --}
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
  { init :: flags -> Update model msg
  , update :: model -> msg -> Update model msg
  , subscriptions :: model -> Sub msg
  } ->
  Program flags model msg
worker init =
  mkEffectFn1 \flags -> do
    activeSubsRef <- Ref.new []
    go activeSubsRef $ init.init flags
  where
  go :: Ref (Array ActiveSub) -> Update model msg -> Effect Unit
  go activeSubsRef w = do
    newModel /\ cmd <- runWriterT w
    let
      sendMsg = go activeSubsRef <. init.update newModel
    activeSubs <- Ref.read activeSubsRef
    newActiveSubs <-
      Sub.something
        activeSubs
        (init.subscriptions newModel)
        sendMsg
    Ref.write newActiveSubs activeSubsRef
    unwrap cmd sendMsg

app ::
  ∀ flags model msg.
  { init :: flags -> Update model msg
  , update :: model -> msg -> Update model msg
  , subscriptions :: model -> Sub msg
  , view ::
      model ->
      { head :: Array (Html msg)
      , body :: Array (Html msg)
      }
  } ->
  Program flags model msg
app init =
  mkEffectFn1 \flags -> do
    initialModel /\ cmd <- runWriterT $ init.init flags
    modelRef <- Ref.new initialModel
    doc <- H.window >>= H.document
    head <- H.toElement <$> H.unsafeHead doc
    body <- H.toElement <$> H.unsafeBody doc
    let
      initialView = init.view initialModel
    newVDoms /\ domSubs <-
      VDom.render doc { head, body } mempty
        $ { head: flatten $ Batch $ initialView.head
          , body: flatten $ Batch $ initialView.body
          }
    vdomsRef <- Ref.new newVDoms
    domSubsRef <- Ref.new domSubs
    render <-
      throttle raf \oldVDOMs -> do
        vdoms <- Ref.read vdomsRef
        newVDOMs /\ subs <- VDom.render doc { head, body } vdoms oldVDOMs
        Ref.write newVDOMs vdomsRef
        Ref.write subs domSubsRef
    activeSubsRef <- Ref.new []
    let
      refs =
        { model: modelRef
        , vdoms: vdomsRef
        , domSubs: domSubsRef
        , activeSubs: activeSubsRef
        }
    newActiveSubs <-
      Sub.something
        []
        (domSubs <> init.subscriptions initialModel)
        (sendMsg render refs)
    Ref.write newActiveSubs activeSubsRef
    unwrap cmd $ sendMsg render refs
  where
  sendMsg ::
    ( { head :: VDOM msg
      , body :: VDOM msg
      } ->
      Effect Unit
    ) ->
    { model :: Ref model
    , vdoms ::
        Ref
          { head :: VDOM msg
          , body :: VDOM msg
          }
    , domSubs :: Ref (Sub msg)
    , activeSubs :: Ref (Array ActiveSub)
    } ->
    msg ->
    Effect Unit
  sendMsg render refs msg = do
    currentModel <- Ref.read refs.model
    newModel /\ cmd <- runWriterT $ init.update currentModel msg
    Ref.write newModel refs.model
    let
      { head, body } = init.view newModel
    render
      { head: flatten $ Batch $ head
      , body: flatten $ Batch $ body
      }
    domSubs <- Ref.read refs.domSubs
    activeSubs <- Ref.read refs.activeSubs
    newActiveSubs <-
      Sub.something
        activeSubs
        (domSubs <> init.subscriptions newModel)
        (sendMsg render refs)
    Ref.write newActiveSubs refs.activeSubs
    unwrap cmd $ sendMsg render refs

foreign import raf :: ∀ a. Effect a -> Effect Unit

html ::
  { head :: Array (Html Unit)
  , body :: Array (Html Unit)
  } ->
  Effect Unit
html vdom = do
  doc <- H.window >>= H.document
  head <- H.toElement <$> H.unsafeHead doc
  body <- H.toElement <$> H.unsafeBody doc
  _ <-
    VDom.render doc { head, body } mempty
      $ { head: flatten $ Batch $ vdom.head
        , body: flatten $ Batch $ vdom.body
        }
  pure unit
