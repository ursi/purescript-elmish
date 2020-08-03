module Attribute where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried
import Foreign.Object as FO
import Sub as Sub
import Unsafe.Coerce (unsafeCoerce)
import VirtualDom (Attribute(..))
import Web.DOM.Element as Element
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (EventListener, addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (Event)

class_ :: ∀ msg. String -> Attribute msg
class_ = Str "class"

onClick :: ∀ msg. msg -> Attribute msg
onClick = on_ "click"

onMouseDown :: ∀ msg. msg -> Attribute msg
onMouseDown = on_ "mousedown"

onInput :: ∀ msg. (String -> msg) -> Attribute msg
onInput =
  on'
    ( \event -> case unsafeCoerce event # FO.lookup "target" >>= FO.lookup "value" of
        Just v -> v
        Nothing -> "event.target.value did not exist"
    )
    "input"

on :: ∀ msg. String -> (Event -> msg) -> Attribute msg
on = on' identity

on_ :: ∀ msg. String -> msg -> Attribute msg
on_ eventName msg =
  Listener \position element ->
    let
      eventTarget = Element.toEventTarget element

      event = EventType eventName
    in
      Sub.new
        (eventName <> ":" <> position)
        $ ( pure
              $ \target msg' ->
                  mkEffectFn1 \send -> do
                    eventL <- eventListener \_ -> runEffectFn1 send msg'
                    addEventListener event eventL false target
                    pure $ removeEventListener event eventL false target
          )
        <*> pure eventTarget
        <*> pure msg

on' :: ∀ a msg. (Event -> a) -> String -> (a -> msg) -> Attribute msg
on' eventToA eventName aToMsg =
  Listener \position element ->
    let
      eventTarget = Element.toEventTarget element

      event = EventType eventName
    in
      Sub.new
        (eventName <> ":" <> position)
        ( ( pure
              $ \target ->
                  mkEffectFn1 \send -> do
                    eventL <- eventListener $ runEffectFn1 send <<< eventToA
                    addEventListener event eventL false target
                    pure $ removeEventListener event eventL false target
          )
            <*> pure eventTarget
        )
        <#> aToMsg
