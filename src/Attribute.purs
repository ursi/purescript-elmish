module Attribute where

import MasonPrelude
import Control.Apply (lift3)
import Data.Array as Array
import Data.Batchable (Batched(..))
import Data.Int as Int
import Data.JSDate as Date
import Data.JSValue (JSValue, toJSValue)
import Effect.Console (logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import Effect.Uncurried
import HTML.All (Event, EventTarget, HTMLInputElement)
import HTML.All as H
import Sub (Callback, Sub(..), SubBuilder, Presub, (<@@>))
import Sub as Sub
import Throttle (throttle)
import Unsafe.Coerce (unsafeCoerce)
import VirtualDom (Attribute, SingleAttribute(..))

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

onMouseMove :: ∀ msg. (Number /\ Number -> msg) -> Attribute msg
onMouseMove = on' "mousemove" onMouseMoveRefEq

onMouseMoveRefEq :: Event -> Effect (Number /\ Number)
onMouseMoveRefEq =
  unsafeCoerce
    .> \e -> do
        x <- H.clientX e
        y <- H.clientY e
        pure $ x /\ y

onInput :: ∀ msg. (String -> msg) -> Attribute msg
onInput = on' "input" onInputRefEq

onInputRefEq :: Event -> Effect String
onInputRefEq = H.unsafeTarget >=> unsafeCoerce .> H.value

on :: ∀ msg. String -> (Event -> Effect msg) -> Attribute msg
on = on' ~~$ identity

on_ :: ∀ msg. String -> msg -> Attribute msg
on_ eventName msg =
  Single
    $ Listener \target ->
        Sub.new $ Sub.newBuilder on_RefEq
          <@@> msg
          <@@> target
          <@@> eventName

makeListener :: ∀ msg. (Callback msg -> Callback Event) -> EventTarget -> String -> Presub msg
makeListener toEventCallback target event = \send -> do
  let
    callback = mkEffectFn1 $ toEventCallback send
  H.addEventListener event callback {} target
  pure $ H.removeEventListener event callback {} target

on_RefEq :: ∀ msg. msg -> EventTarget -> String -> Presub msg
on_RefEq msg = makeListener (\send -> \_ -> send msg)

on' :: ∀ a msg. String -> (Event -> Effect a) -> (a -> msg) -> Attribute msg
on' eventName toA toMsg =
  Single
    $ Listener \target ->
        toMsg
          <$> ( Sub.new $ Sub.newBuilder on'RefEq
                <@@> toA
                <@@> target
                <@@> eventName
            )

on'RefEq :: ∀ a. (Event -> Effect a) -> EventTarget -> String -> Presub a
on'RefEq = makeListener <. (flip <. (<.) bind)

-- must use a referance-equal function
throttledOn :: ∀ msg. Number -> String -> (Event -> Effect msg) -> Attribute msg
throttledOn = throttledOn' ~~~$ identity

throttledOn_ :: ∀ msg. Number -> String -> msg -> Attribute msg
throttledOn_ ms eventName msg =
  Single
    $ Listener \target ->
        Sub.new $ Sub.newBuilder throttledOn_RefEq
          <@@> ms
          <@@> msg
          <@@> target
          <@@> eventName

throttledOn_RefEq :: ∀ msg. Number -> msg -> EventTarget -> String -> Presub msg
throttledOn_RefEq ms msg = makeThrottledListener ms \send -> \_ -> send msg

makeThrottledListener ::
  ∀ msg.
  Number ->
  (Callback msg -> Callback Event) ->
  EventTarget ->
  String ->
  Presub msg
makeThrottledListener ms toEventCallback target event = \send -> do
  callback <-
    toEventCallback send
      # throttleMs ms
      <#> mkEffectFn1
  H.addEventListener event callback {} target
  pure $ H.removeEventListener event callback {} target

throttledOn' :: ∀ a msg. Number -> String -> (Event -> Effect a) -> (a -> msg) -> Attribute msg
throttledOn' ms eventName toA toMsg =
  Single
    $ Listener \target ->
        toMsg
          <$> ( Sub.new $ Sub.newBuilder throttledOn'RefEq
                <@@> ms
                <@@> toA
                <@@> target
                <@@> eventName
            )

throttledOn'RefEq :: ∀ a. Number -> (Event -> Effect a) -> EventTarget -> String -> Presub a
throttledOn'RefEq = makeThrottledListener <~. (flip <. (<.) bind)

throttleMs :: ∀ a. Number -> Callback a -> Effect (Callback a)
throttleMs ms = throttle (\c -> setTimeout (Int.round ms) c *> pure unit)
