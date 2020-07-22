module VirtualDom where

import Prelude
import Control.Monad.Writer.Trans (WriterT, lift, runWriterT, tell)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Effect.Uncurried
import Sub (Sub(..), SubImpl)
import Sub as Sub
import Web.DOM.Document (Document, createElement, createTextNode)
import Web.DOM.Element (Element, setAttribute)
import Web.DOM.Element as Element
import Web.DOM.Node (Node, appendChild)
import Web.DOM.Text as Text
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener, addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement

main :: Effect Unit
main =
  render
    $ [ div [ logOnClick ]
          [ label [ className "test" ]
              [ text "what is your favourite language?"
              , select []
                  [ option [] [ text "Elm" ]
                  , option [] [ text "PureScript" ]
                  , option [] [ text "JavaScript" ]
                  ]
              ]
          ]
      , div []
          [ label []
              [ text "what is your favourite language?"
              , select []
                  [ option [] [ text "Elm" ]
                  , option [] [ text "PureScript" ]
                  , option [] [ text "JavaScript" ]
                  ]
              ]
          ]
      ]

className :: ∀ msg. String -> Attribute msg
className = Str "class"

text :: ∀ msg. String -> VNode msg
text = VText

createVNode :: ∀ msg. String -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
createVNode tag attributes children =
  VNode
    { tag
    , attributes
    , children
    }

div :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
div = createVNode "div"

label :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
label = createVNode "label"

select :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
select = createVNode "select"

option :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
option = createVNode "option"

data VNode msg
  = VNode
    { tag :: String
    , attributes :: Array (Attribute msg)
    , children :: Array (VNode msg)
    }
  | VText String

data Attribute msg
  = Str String String
  | Listener (Element -> Sub msg)

logOnClick :: Attribute Unit
logOnClick =
  Listener \element -> do
    let
      eventTarget = Element.toEventTarget element

      mouseDown = EventType "mousedown"

      callback = \_ -> log "clicked"
    Sub
      $ Sub.new ""
      $ mkEffectFn1 \_ -> do
          eventL <- eventListener callback
          addEventListener mouseDown eventL false eventTarget
          pure $ removeEventListener mouseDown eventL false eventTarget

render :: ∀ msg. Array (VNode msg) -> Effect Unit
render vnodes = do
  htmlDocument <- HTML.window >>= Window.document
  let
    document :: Document
    document = HTMLDocument.toDocument htmlDocument
  realNodes /\ sub <- runWriterT $ traverse (toRealNode document) vnodes
  mbodyNode <- map HTMLElement.toNode <$> HTMLDocument.body htmlDocument
  fromMaybe (pure unit)
    (appendChildren realNodes <$> mbodyNode)

toRealNode :: ∀ msg. Document -> VNode msg -> WriterT (Sub msg) Effect Node
toRealNode doc = case _ of
  VNode { tag, attributes, children } -> do
    element <- lift $ createElement tag doc
    let
      elementNode = Element.toNode element
    childNodes <- traverse (toRealNode doc) children
    _ <- lift $ setAttributes attributes element
    lift $ appendChildren childNodes elementNode
    pure elementNode
  VText text ->
    lift $ Text.toNode
      <$> createTextNode text doc

setAttributes :: ∀ msg. Array (Attribute msg) -> Element -> Effect (Array (Sub msg))
setAttributes a e = go a e []
  where
  go :: Array (Attribute msg) -> Element -> Array (Sub msg) -> Effect (Array (Sub msg))
  go attributes element acc = case Array.uncons attributes of
    Just { head, tail } -> case head of
      Str prop value -> do
        setAttribute prop value element
        setAttributes tail element
      Listener toSub -> go tail element (Array.snoc acc $ toSub element)
    Nothing -> pure acc

appendChildren :: Array Node -> Node -> Effect Unit
appendChildren children node = case Array.uncons children of
  Just { head, tail } -> do
    _ <- appendChild head node
    appendChildren tail node
  Nothing -> pure unit
