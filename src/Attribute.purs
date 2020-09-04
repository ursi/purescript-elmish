module Attribute where

import MasonPrelude
import Control.Apply (lift3)
import Data.Array as Array
import Data.Batchable (Batched(..))
import Data.JSValue (JSValue, toJSValue)
import Effect.Uncurried
import Foreign.Object as FO
import Sub (Callback, Sub(..), SubBuilder, Presub, (<@@>))
import Sub as Sub
import Unsafe.Coerce (unsafeCoerce)
import VirtualDom (Attribute, SingleAttribute(..))
import Web.DOM.Element (Element, setAttribute, tagName)
import Web.DOM.Element as Element
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (EventTarget, EventListener, addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (Event)

attribute :: ∀ msg. String -> String -> Attribute msg
attribute = Single <.. Attr

property :: ∀ msg. String -> JSValue -> Attribute msg
property = Single <.. Prop

value :: ∀ msg. String -> Attribute msg
value = Single <. Prop "value" <. toJSValue

class_ :: ∀ msg. String -> Attribute msg
class_ = Single <. Attr "class"

onClick :: ∀ msg. msg -> Attribute msg
onClick = on_ "click"

onMouseDown :: ∀ msg. msg -> Attribute msg
onMouseDown = on_ "mousedown"

onInput :: ∀ msg. (String -> msg) -> Attribute msg
onInput = on' onInputRefEq "input"

onInputRefEq :: Event -> String
onInputRefEq event = case unsafeCoerce event # FO.lookup "target" >>= FO.lookup "value" of
  Just v -> v
  Nothing -> "event.target.value did not exist"

on :: ∀ msg. String -> (Event -> msg) -> Attribute msg
on = on' identity

on_ :: ∀ msg. String -> msg -> Attribute msg
on_ eventName msg =
  Single
    $ Listener \element ->
        Sub.new $ Sub.newBuilder on_RefEq
          <@@> msg
          <@@> element
          <@@> eventName

makeListener :: ∀ msg. (Callback msg -> Callback Event) -> Element -> String -> Presub msg
makeListener toEventCallback element eventName send = do
  let
    target = Element.toEventTarget element

    event = EventType eventName
  eventL <- eventListener $ toEventCallback send
  addEventListener event eventL false target
  pure $ removeEventListener event eventL false target

on_RefEq :: ∀ msg. msg -> Element -> String -> Presub msg
on_RefEq msg = makeListener (\send -> \_ -> send msg)

on' :: ∀ a msg. (Event -> a) -> String -> (a -> msg) -> Attribute msg
on' toA eventName toMsg =
  Single
    $ Listener \element ->
        toMsg
          <$> ( Sub.new $ Sub.newBuilder on'RefEq
                <@@> toA
                <@@> element
                <@@> eventName
            )

on'RefEq :: ∀ a. (Event -> a) -> Element -> String -> Presub a
on'RefEq = makeListener <. (.>)
