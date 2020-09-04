module Platform where

import MasonPrelude
import Data.Identity
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (StateT(..), evalStateT, get)
import Control.Monad.Trans.Class (lift)
import Data.Batchable (Batched(..), batch, flatten)
import Data.Newtype (class Newtype, unwrap)
import Debug as Debug
import Effect.Console (log)
import Effect.Exception (throw)
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
import VirtualDom (VDOM)
import VirtualDom as VDom
import Web.DOM.Document (Document)
import Web.DOM.Node (Node)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

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
    htmlDocument <- HTML.window >>= Window.document
    head <-
      HTMLDocument.head htmlDocument
        >>= maybe
            (throw "error: no head element")
            (pure <. HTMLElement.toNode)
    body <-
      HTMLDocument.body htmlDocument
        >>= maybe
            (throw "error: no body element")
            (pure <. HTMLElement.toNode)
    let
      doc :: Document
      doc = HTMLDocument.toDocument htmlDocument
    newHeadVDom <- fst <$> VDom.render doc head mempty (flatten $ batch $ (init.view initialModel).head)
    newBodyVDom /\ domSubs <- VDom.render doc body mempty $ flatten $ batch $ (init.view initialModel).body
    vdomsRef <-
      Ref.new
        { head: newHeadVDom
        , body: newBodyVDom
        }
    activeSubsRef <- Ref.new []
    let
      refs =
        { model: modelRef
        , vdoms: vdomsRef
        , subs: activeSubsRef
        }
    newActiveSubs <-
      Sub.something
        []
        (domSubs <> init.subscriptions initialModel)
        (sendMsg doc { head, body } refs)
    Ref.write newActiveSubs activeSubsRef
    unwrap cmd $ sendMsg doc { head, body } refs
  where
  sendMsg ::
    Document ->
    { head :: Node
    , body :: Node
    } ->
    { model :: Ref model
    , vdoms ::
        Ref
          { head :: VDOM msg
          , body :: VDOM msg
          }
    , subs :: Ref (Array ActiveSub)
    } ->
    msg ->
    Effect Unit
  sendMsg doc parents refs msg = do
    currentModel <- Ref.read refs.model
    newModel /\ cmd <- runWriterT $ init.update currentModel msg
    Ref.write newModel refs.model
    vdoms <- Ref.read refs.vdoms
    newHeadVDom <- fst <$> VDom.render doc parents.head vdoms.head (flatten $ batch $ (init.view newModel).head)
    newBodyVDom /\ domSubs <- VDom.render doc parents.body vdoms.body $ flatten $ batch $ (init.view newModel).body
    Ref.write
      { head: newHeadVDom
      , body: newBodyVDom
      }
      refs.vdoms
    activeSubs <- Ref.read refs.subs
    newActiveSubs <-
      Sub.something
        activeSubs
        (domSubs <> init.subscriptions newModel)
        (sendMsg doc parents refs)
    Ref.write newActiveSubs refs.subs
    unwrap cmd $ sendMsg doc parents refs
