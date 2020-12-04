module Attribute
  ( module Exports
  , attribute
  , property
  , addClass
  , value
  , abbr
  , accept
  , acceptCharset
  , accesskey
  , action
  , allow
  , allowfullscreen
  , alt
  , as
  , async
  , autocapitalize
  , autocomplete
  , autofocus
  , autoplay
  , charset
  , checked
  , cite
  , color
  , cols
  , colspan
  , content
  , contenteditable
  , controls
  , coords
  , crossorigin
  , data_
  , datetime
  , decoding
  , loading
  , default
  , defer
  , dir
  , dirname
  , disabled
  , download
  , draggable
  , enctype
  , enterkeyhint
  , for
  , form
  , formaction
  , formenctype
  , formmethod
  , formnovalidate
  , formtarget
  , headers
  , height
  , hidden
  , high
  , href
  , hreflang
  , httpEquiv
  , id
  , imagesizes
  , imagesrcset
  , inputmode
  , integrity
  , is
  , ismap
  , itemid
  , itemprop
  , itemref
  , itemscope
  , itemtype
  , kind_
  , label
  , lang
  , list
  , loop
  , low
  , manifest
  , max
  , maxlength
  , media
  , method
  , min
  , minlength
  , multiple
  , muted
  , name
  , nomodule
  , nonce
  , novalidate
  , open
  , optimum
  , pattern
  , ping
  , placeholder
  , playsinline
  , poster
  , preload
  , readonly
  , referrerpolicy
  , rel
  , required
  , reversed
  , rows
  , rowspan
  , sandbox
  , scope
  , selected
  , shape
  , size
  , sizes
  , slot
  , span
  , spellcheck
  , src
  , srcdoc
  , srclang
  , srcset
  , start
  , step
  , style
  , tabindex
  , target
  , title
  , translate
  , type_
  , usemap
  , width
  , wrap
  , onClick
  , onMouseDown
  , onMouseMove
  , onInput
  , on
  , on_
  , throttledOn
  , throttledOn_
  ) where

import MasonPrelude
import Data.Batchable (Batched(..))
import Data.Int as Int
import Data.JSValue (JSValue, toJSValue)
import Effect.Timer (setTimeout)
import Sub (Callback, Presub, (<@@>))
import Sub as Sub
import Throttle (throttle)
import Unsafe.Coerce (unsafeCoerce)
import VirtualDom (Attribute, SingleAttribute(..))
import VirtualDom (Attribute) as Exports
import WHATWG.HTML.All (Event, EventTarget)
import WHATWG.HTML.All as H
import WHATWG.Internal (unsafeGet)

attribute :: ∀ msg. String -> String -> Attribute msg
attribute = Single <.. Attr

property :: ∀ msg. String -> JSValue -> Attribute msg
property = Single <.. Prop

-- SPECIAL
addClass :: ∀ msg. String -> Attribute msg
addClass = Single <. AddClass

value :: ∀ msg. String -> Attribute msg
value = Single <. Prop "value" <. toJSValue

-- STANDARD
abbr :: ∀ msg. String -> Attribute msg
abbr = Single <. Attr "abbr"

accept :: ∀ msg. String -> Attribute msg
accept = Single <. Attr "accept"

acceptCharset :: ∀ msg. String -> Attribute msg
acceptCharset = Single <. Attr "accept-charset"

accesskey :: ∀ msg. String -> Attribute msg
accesskey = Single <. Attr "accesskey"

action :: ∀ msg. String -> Attribute msg
action = Single <. Attr "action"

allow :: ∀ msg. String -> Attribute msg
allow = Single <. Attr "allow"

allowfullscreen :: ∀ msg. String -> Attribute msg
allowfullscreen = Single <. Attr "allowfullscreen"

alt :: ∀ msg. String -> Attribute msg
alt = Single <. Attr "alt"

as :: ∀ msg. String -> Attribute msg
as = Single <. Attr "as"

async :: ∀ msg. String -> Attribute msg
async = Single <. Attr "async"

autocapitalize :: ∀ msg. String -> Attribute msg
autocapitalize = Single <. Attr "autocapitalize"

autocomplete :: ∀ msg. String -> Attribute msg
autocomplete = Single <. Attr "autocomplete"

autofocus :: ∀ msg. String -> Attribute msg
autofocus = Single <. Attr "autofocus"

autoplay :: ∀ msg. String -> Attribute msg
autoplay = Single <. Attr "autoplay"

charset :: ∀ msg. String -> Attribute msg
charset = Single <. Attr "charset"

checked :: ∀ msg. String -> Attribute msg
checked = Single <. Attr "checked"

cite :: ∀ msg. String -> Attribute msg
cite = Single <. Attr "cite"

color :: ∀ msg. String -> Attribute msg
color = Single <. Attr "color"

cols :: ∀ msg. String -> Attribute msg
cols = Single <. Attr "cols"

colspan :: ∀ msg. String -> Attribute msg
colspan = Single <. Attr "colspan"

content :: ∀ msg. String -> Attribute msg
content = Single <. Attr "content"

contenteditable :: ∀ msg. String -> Attribute msg
contenteditable = Single <. Attr "contenteditable"

controls :: ∀ msg. String -> Attribute msg
controls = Single <. Attr "controls"

coords :: ∀ msg. String -> Attribute msg
coords = Single <. Attr "coords"

crossorigin :: ∀ msg. String -> Attribute msg
crossorigin = Single <. Attr "crossorigin"

data_ :: ∀ msg. String -> Attribute msg
data_ = Single <. Attr "data"

datetime :: ∀ msg. String -> Attribute msg
datetime = Single <. Attr "datetime"

decoding :: ∀ msg. String -> Attribute msg
decoding = Single <. Attr "decoding"

loading :: ∀ msg. String -> Attribute msg
loading = Single <. Attr "loading"

default :: ∀ msg. String -> Attribute msg
default = Single <. Attr "default"

defer :: ∀ msg. String -> Attribute msg
defer = Single <. Attr "defer"

dir :: ∀ msg. String -> Attribute msg
dir = Single <. Attr "dir"

dirname :: ∀ msg. String -> Attribute msg
dirname = Single <. Attr "dirname"

disabled :: ∀ msg. String -> Attribute msg
disabled = Single <. Attr "disabled"

download :: ∀ msg. String -> Attribute msg
download = Single <. Attr "download"

draggable :: ∀ msg. String -> Attribute msg
draggable = Single <. Attr "draggable"

enctype :: ∀ msg. String -> Attribute msg
enctype = Single <. Attr "enctype"

enterkeyhint :: ∀ msg. String -> Attribute msg
enterkeyhint = Single <. Attr "enterkeyhint"

for :: ∀ msg. String -> Attribute msg
for = Single <. Attr "for"

form :: ∀ msg. String -> Attribute msg
form = Single <. Attr "form"

formaction :: ∀ msg. String -> Attribute msg
formaction = Single <. Attr "formaction"

formenctype :: ∀ msg. String -> Attribute msg
formenctype = Single <. Attr "formenctype"

formmethod :: ∀ msg. String -> Attribute msg
formmethod = Single <. Attr "formmethod"

formnovalidate :: ∀ msg. String -> Attribute msg
formnovalidate = Single <. Attr "formnovalidate"

formtarget :: ∀ msg. String -> Attribute msg
formtarget = Single <. Attr "formtarget"

headers :: ∀ msg. String -> Attribute msg
headers = Single <. Attr "headers"

height :: ∀ msg. String -> Attribute msg
height = Single <. Attr "height"

hidden :: ∀ msg. String -> Attribute msg
hidden = Single <. Attr "hidden"

high :: ∀ msg. String -> Attribute msg
high = Single <. Attr "high"

href :: ∀ msg. String -> Attribute msg
href = Single <. Attr "href"

hreflang :: ∀ msg. String -> Attribute msg
hreflang = Single <. Attr "hreflang"

httpEquiv :: ∀ msg. String -> Attribute msg
httpEquiv = Single <. Attr "http-equiv"

id :: ∀ msg. String -> Attribute msg
id = Single <. Attr "id"

imagesizes :: ∀ msg. String -> Attribute msg
imagesizes = Single <. Attr "imagesizes"

imagesrcset :: ∀ msg. String -> Attribute msg
imagesrcset = Single <. Attr "imagesrcset"

inputmode :: ∀ msg. String -> Attribute msg
inputmode = Single <. Attr "inputmode"

integrity :: ∀ msg. String -> Attribute msg
integrity = Single <. Attr "integrity"

is :: ∀ msg. String -> Attribute msg
is = Single <. Attr "is"

ismap :: ∀ msg. String -> Attribute msg
ismap = Single <. Attr "ismap"

itemid :: ∀ msg. String -> Attribute msg
itemid = Single <. Attr "itemid"

itemprop :: ∀ msg. String -> Attribute msg
itemprop = Single <. Attr "itemprop"

itemref :: ∀ msg. String -> Attribute msg
itemref = Single <. Attr "itemref"

itemscope :: ∀ msg. String -> Attribute msg
itemscope = Single <. Attr "itemscope"

itemtype :: ∀ msg. String -> Attribute msg
itemtype = Single <. Attr "itemtype"

kind_ :: ∀ msg. String -> Attribute msg
kind_ = Single <. Attr "kind"

label :: ∀ msg. String -> Attribute msg
label = Single <. Attr "label"

lang :: ∀ msg. String -> Attribute msg
lang = Single <. Attr "lang"

list :: ∀ msg. String -> Attribute msg
list = Single <. Attr "list"

loop :: ∀ msg. String -> Attribute msg
loop = Single <. Attr "loop"

low :: ∀ msg. String -> Attribute msg
low = Single <. Attr "low"

manifest :: ∀ msg. String -> Attribute msg
manifest = Single <. Attr "manifest"

max :: ∀ msg. String -> Attribute msg
max = Single <. Attr "max"

maxlength :: ∀ msg. String -> Attribute msg
maxlength = Single <. Attr "maxlength"

media :: ∀ msg. String -> Attribute msg
media = Single <. Attr "media"

method :: ∀ msg. String -> Attribute msg
method = Single <. Attr "method"

min :: ∀ msg. String -> Attribute msg
min = Single <. Attr "min"

minlength :: ∀ msg. String -> Attribute msg
minlength = Single <. Attr "minlength"

multiple :: ∀ msg. String -> Attribute msg
multiple = Single <. Attr "multiple"

muted :: ∀ msg. String -> Attribute msg
muted = Single <. Attr "muted"

name :: ∀ msg. String -> Attribute msg
name = Single <. Attr "name"

nomodule :: ∀ msg. String -> Attribute msg
nomodule = Single <. Attr "nomodule"

nonce :: ∀ msg. String -> Attribute msg
nonce = Single <. Attr "nonce"

novalidate :: ∀ msg. String -> Attribute msg
novalidate = Single <. Attr "novalidate"

open :: ∀ msg. String -> Attribute msg
open = Single <. Attr "open"

optimum :: ∀ msg. String -> Attribute msg
optimum = Single <. Attr "optimum"

pattern :: ∀ msg. String -> Attribute msg
pattern = Single <. Attr "pattern"

ping :: ∀ msg. String -> Attribute msg
ping = Single <. Attr "ping"

placeholder :: ∀ msg. String -> Attribute msg
placeholder = Single <. Attr "placeholder"

playsinline :: ∀ msg. String -> Attribute msg
playsinline = Single <. Attr "playsinline"

poster :: ∀ msg. String -> Attribute msg
poster = Single <. Attr "poster"

preload :: ∀ msg. String -> Attribute msg
preload = Single <. Attr "preload"

readonly :: ∀ msg. String -> Attribute msg
readonly = Single <. Attr "readonly"

referrerpolicy :: ∀ msg. String -> Attribute msg
referrerpolicy = Single <. Attr "referrerpolicy"

rel :: ∀ msg. String -> Attribute msg
rel = Single <. Attr "rel"

required :: ∀ msg. String -> Attribute msg
required = Single <. Attr "required"

reversed :: ∀ msg. String -> Attribute msg
reversed = Single <. Attr "reversed"

rows :: ∀ msg. String -> Attribute msg
rows = Single <. Attr "rows"

rowspan :: ∀ msg. String -> Attribute msg
rowspan = Single <. Attr "rowspan"

sandbox :: ∀ msg. String -> Attribute msg
sandbox = Single <. Attr "sandbox"

scope :: ∀ msg. String -> Attribute msg
scope = Single <. Attr "scope"

selected :: ∀ msg. String -> Attribute msg
selected = Single <. Attr "selected"

shape :: ∀ msg. String -> Attribute msg
shape = Single <. Attr "shape"

size :: ∀ msg. String -> Attribute msg
size = Single <. Attr "size"

sizes :: ∀ msg. String -> Attribute msg
sizes = Single <. Attr "sizes"

slot :: ∀ msg. String -> Attribute msg
slot = Single <. Attr "slot"

span :: ∀ msg. String -> Attribute msg
span = Single <. Attr "span"

spellcheck :: ∀ msg. String -> Attribute msg
spellcheck = Single <. Attr "spellcheck"

src :: ∀ msg. String -> Attribute msg
src = Single <. Attr "src"

srcdoc :: ∀ msg. String -> Attribute msg
srcdoc = Single <. Attr "srcdoc"

srclang :: ∀ msg. String -> Attribute msg
srclang = Single <. Attr "srclang"

srcset :: ∀ msg. String -> Attribute msg
srcset = Single <. Attr "srcset"

start :: ∀ msg. String -> Attribute msg
start = Single <. Attr "start"

step :: ∀ msg. String -> Attribute msg
step = Single <. Attr "step"

style :: ∀ msg. String -> Attribute msg
style = Single <. Attr "style"

tabindex :: ∀ msg. String -> Attribute msg
tabindex = Single <. Attr "tabindex"

target :: ∀ msg. String -> Attribute msg
target = Single <. Attr "target"

title :: ∀ msg. String -> Attribute msg
title = Single <. Attr "title"

translate :: ∀ msg. String -> Attribute msg
translate = Single <. Attr "translate"

type_ :: ∀ msg. String -> Attribute msg
type_ = Single <. Attr "type"

usemap :: ∀ msg. String -> Attribute msg
usemap = Single <. Attr "usemap"

width :: ∀ msg. String -> Attribute msg
width = Single <. Attr "width"

wrap :: ∀ msg. String -> Attribute msg
wrap = Single <. Attr "wrap"

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
onInputRefEq = H.unsafeTarget .> unsafeGet "value"

on :: ∀ msg. String -> (Event -> Effect msg) -> Attribute msg
on = on' ~~$ identity

on_ :: ∀ msg. String -> msg -> Attribute msg
on_ eventName msg =
  Single
    $ Listener \targ ->
        Sub.new $ Sub.newBuilder on_RefEq
          <@@> msg
          <@@> targ
          <@@> eventName

makeListener :: ∀ msg. (Callback msg -> Callback Event) -> EventTarget -> String -> Presub msg
makeListener toEventCallback targ event = \send -> do
  let
    callback = mkEffectFn1 $ toEventCallback send
  H.addEventListener event callback {} targ
  pure $ H.removeEventListener event callback {} targ

on_RefEq :: ∀ msg. msg -> EventTarget -> String -> Presub msg
on_RefEq msg = makeListener (\send -> \_ -> send msg)

on' :: ∀ a msg. String -> (Event -> Effect a) -> (a -> msg) -> Attribute msg
on' eventName toA toMsg =
  Single
    $ Listener \targ ->
        toMsg
          <$> ( Sub.new $ Sub.newBuilder on'RefEq
                <@@> toA
                <@@> targ
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
    $ Listener \targ ->
        Sub.new $ Sub.newBuilder throttledOn_RefEq
          <@@> ms
          <@@> msg
          <@@> targ
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
makeThrottledListener ms toEventCallback targ event = \send -> do
  callback <-
    toEventCallback send
      # throttleMs ms
      <#> mkEffectFn1
  H.addEventListener event callback {} targ
  pure $ H.removeEventListener event callback {} targ

throttledOn' :: ∀ a msg. Number -> String -> (Event -> Effect a) -> (a -> msg) -> Attribute msg
throttledOn' ms eventName toA toMsg =
  Single
    $ Listener \targ ->
        toMsg
          <$> ( Sub.new $ Sub.newBuilder throttledOn'RefEq
                <@@> ms
                <@@> toA
                <@@> targ
                <@@> eventName
            )

throttledOn'RefEq :: ∀ a. Number -> (Event -> Effect a) -> EventTarget -> String -> Presub a
throttledOn'RefEq = makeThrottledListener <~. (flip <. (<.) bind)

throttleMs :: ∀ a. Number -> Callback a -> Effect (Callback a)
throttleMs ms = throttle (\c -> setTimeout (Int.round ms) c *> pure unit)
