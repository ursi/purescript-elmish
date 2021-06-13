module Platform
  ( module Exports
  , Cmd(..)
  , Program
  , Update
  , app
  , worker
  , html
  , attemptTask
  , batch
  , afterRender
  ) where

import MasonPrelude
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Batched (Batched(..), flatten)
import Data.Newtype (class Newtype, unwrap)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Html (Html)
import Sub (Callback, Sub, ActiveSub)
import Sub as Sub
import Task (Task)
import Task as Task
import Throttle (throttle)
import VirtualDom as VDom
import WHATWG.HTML.All as H
import Control.Monad.Writer (tell) as Exports

batch :: ∀ a b. Array (Batched a b) -> Batched a b
batch = Batch

afterRender :: ∀ msg. Effect Unit -> Update msg Unit
afterRender = tell <. Cmd <. const

newtype Cmd msg
  = Cmd ((msg -> Effect Unit) -> Effect Unit)

derive instance Newtype (Cmd a) _

instance Semigroup (Cmd a) where
  append (Cmd c1) (Cmd c2) =
    Cmd \sendMsg -> do
      c1 sendMsg
      c2 sendMsg

instance Monoid (Cmd a) where
  mempty = Cmd $ const mempty

attemptTask :: ∀ x a msg. (Either x a -> msg) -> Task x a -> Cmd msg
attemptTask toMsg task = Cmd \sendMsg -> Task.capture (sendMsg <. toMsg) task

type Program :: Type -> Type -> Type -> Type
type Program flags msg model
  = EffectFn1 flags Unit

type Update msg model
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
  ∀ flags msg model.
  { init :: flags -> Update msg model
  , update :: model -> msg -> Update msg model
  , subscriptions :: model -> Sub msg
  } ->
  Program flags msg model
worker init =
  mkEffectFn1 \flags -> do
    activeSubsRef <- Ref.new []
    go activeSubsRef $ init.init flags
  where
  go :: Ref (Array (ActiveSub msg)) -> Update msg model -> Effect Unit
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

app :: ∀ flags msg model.
  { init :: flags -> Update msg model
  , update :: model -> msg -> Update msg model
  , subscriptions :: model -> Sub msg
  , view ::
      model
      -> { head :: Array (Html msg)
         , body :: Array (Html msg)
         }
  }
  -> Program flags msg model
app init =
  mkEffectFn1
    \flags -> do
      initialModel /\ initialCmd <- runWriterT $ init.init flags
      modelRef <- Ref.new initialModel
      doc <- H.window >>= H.document
      head <- H.toElement <$> H.unsafeHead doc
      body <- H.toElement <$> H.unsafeBody doc
      let initialView = init.view initialModel

      newVDoms /\ initialDomSubs <-
        VDom.render doc { head, body } mempty
          { head: flatten $ Batch $ initialView.head
          , body: flatten $ Batch $ initialView.body
          }

      vdomsRef <- Ref.new newVDoms
      domSubsRef <- Ref.new initialDomSubs
      activeSubsRef <- Ref.new []

      let
        refs =
          { model: modelRef
          , vdoms: vdomsRef
          , domSubs: domSubsRef
          , activeSubs: activeSubsRef
          }

      render <-
        throttle raf
          \(handleSubs /\ newVDOMs) -> do
            oldVDOMs <- Ref.read vdomsRef

            newVDOMsWithNodes /\ subs <-
              VDom.render doc { head, body } oldVDOMs newVDOMs

            Ref.write newVDOMsWithNodes vdomsRef
            Ref.write subs domSubsRef
            model <- Ref.read modelRef
            handleSubs model subs
            pure unit

      let
        sendMsg :: Callback msg
        sendMsg msg = do
          currentModel <- Ref.read refs.model
          newModel /\ cmd <- runWriterT $ init.update currentModel msg
          Ref.write newModel refs.model
          let { head, body } = init.view newModel

          skipped <-
            render
            $ handleSubs
            /\ { head: flatten $ Batch $ head
               , body: flatten $ Batch $ body
               }

          if skipped then do
            domSubs <- Ref.read refs.domSubs
            handleSubs newModel domSubs
          else
            pure unit

          unwrap cmd sendMsg

        handleSubs :: model -> Sub msg -> Effect Unit
        handleSubs model domSubs = do
          activeSubs <- Ref.read refs.activeSubs

          newActiveSubs <-
            Sub.something
              activeSubs
              (domSubs <> init.subscriptions model)
              sendMsg

          Ref.write newActiveSubs refs.activeSubs

      newActiveSubs <-
        Sub.something
          []
          (initialDomSubs <> init.subscriptions initialModel)
          sendMsg

      Ref.write newActiveSubs activeSubsRef
      unwrap initialCmd sendMsg

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
