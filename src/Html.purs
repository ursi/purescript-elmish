module Html
  ( module Css.Global
  , Html
  , rawHtml
  , keyed
  , keyedS
  , element
  , elementS
  , noDiff
  , text
  , a
  , aS
  , abbr
  , abbrS
  , address
  , addressS
  , area
  , areaS
  , article
  , articleS
  , aside
  , asideS
  , audio
  , audioS
  , b
  , bS
  , base
  , baseS
  , bdi
  , bdiS
  , bdo
  , bdoS
  , blockquote
  , blockquoteS
  , body
  , bodyS
  , br
  , brS
  , button
  , buttonS
  , canvas
  , canvasS
  , caption
  , captionS
  , cite
  , citeS
  , code
  , codeS
  , col
  , colS
  , colgroup
  , colgroupS
  , data_
  , data_S
  , datalist
  , datalistS
  , dd
  , ddS
  , del
  , delS
  , details
  , detailsS
  , dfn
  , dfnS
  , dialog
  , dialogS
  , div
  , divS
  , dl
  , dlS
  , dt
  , dtS
  , em
  , emS
  , embed
  , embedS
  , fieldset
  , fieldsetS
  , figcaption
  , figcaptionS
  , figure
  , figureS
  , footer
  , footerS
  , form
  , formS
  , h1
  , h1S
  , h2
  , h2S
  , h3
  , h3S
  , h4
  , h4S
  , h5
  , h5S
  , h6
  , h6S
  , head
  , headS
  , header
  , headerS
  , hgroup
  , hgroupS
  , hr
  , hrS
  , html
  , htmlS
  , i
  , iS
  , iframe
  , iframeS
  , img
  , imgS
  , input
  , inputS
  , ins
  , insS
  , kbd
  , kbdS
  , label
  , labelS
  , legend
  , legendS
  , li
  , liS
  , link
  , linkStylesheet
  , main
  , mainS
  , map
  , mapS
  , mark
  , markS
  , mathML
  , mathMLS
  , math
  , mathS
  , menu
  , menuS
  , meta
  , meter
  , meterS
  , nav
  , navS
  , noscript
  , noscriptS
  , object
  , objectS
  , ol
  , olS
  , optgroup
  , optgroupS
  , option
  , optionS
  , output
  , outputS
  , p
  , pS
  , param
  , paramS
  , picture
  , pictureS
  , pre
  , preS
  , progress
  , progressS
  , q
  , qS
  , rp
  , rpS
  , rt
  , rtS
  , ruby
  , rubyS
  , s
  , sS
  , samp
  , sampS
  , script
  , scriptS
  , section
  , sectionS
  , select
  , selectS
  , slot
  , slotS
  , small
  , smallS
  , source
  , sourceS
  , span
  , spanS
  , strong
  , strongS
  , sub
  , subS
  , summary
  , summaryS
  , sup
  , supS
  , svg
  , svgS
  , table
  , tableS
  , tbody
  , tbodyS
  , td
  , tdS
  , template
  , templateS
  , textarea
  , textareaS
  , tfoot
  , tfootS
  , th
  , thS
  , thead
  , theadS
  , time
  , timeS
  , title
  , tr
  , trS
  , track
  , trackS
  , u
  , uS
  , ul
  , ulS
  , var
  , varS
  , video
  , videoS
  , wbr
  , wbrS
  ) where

import MasonPrelude
import Attribute as A
import Css (Styles)
import Css.Global (style)
import Data.Batched (Batched(..), flatten, flattenMap)
import Data.Batched as Batched
import Data.List ((:))
import Data.Newtype (unwrap)
import MyMap as Map
import VirtualDom (Attribute, SingleAttribute(..), SingleVNode(..), VNode)
import VirtualDom as VD
import VirtualDom.Css as VC

type Html msg
  = VNode msg

rawHtml :: ∀ msg. String -> Html msg
rawHtml theHtml = Single
  $ VRaw
      { html: theHtml
      , node: Nothing
      }

keyed :: ∀ msg. String -> Array (Attribute msg) -> Array (String /\ Html msg) -> Html msg
keyed tag attributes children =
  Single
    $ KeyedElement
        { tag
        , styles: mempty
        , css: Nothing
        , attributes: flatten $ Batch attributes
        , children:
            foldr
              ( \(key /\ child) acc ->
                  fromMaybe acc do
                    first <- Batched.first child
                    pure $ (key /\ first) : acc
              )
              Nil
              children
        , node: Nothing
        }

keyedS :: ∀ msg.
  String ->
  Array Styles ->
  Array (Attribute msg) ->
  Array (String /\ Html msg) ->
  Html msg
keyedS tag styles attributes children =
  let
    styles' = flattenMap unwrap $ Batch styles

    mstyles = VC.process styles'
  in
  Single
  $ KeyedElement
  $ { tag
    , styles: styles'
    , css: Nothing
    , attributes: flatten $ Batch attributes
    , children:
        foldr
          (\(key /\ child) acc ->
             fromMaybe acc do
               first <- Batched.first child
               pure $ (key /\ first) : acc
          )
          Nil
          children
    , node: Nothing
    }
    # \r ->
        case mstyles of
          Just css ->
            r { attributes = AddClass css.class : r.attributes
              , css = Just $ Map.singleton css.class css.css
              }

          Nothing -> r

element :: ∀ msg. String -> Array (Attribute msg) -> Array (Html msg) -> Html msg
element tag attributes children =
  Single
    $ VElement
        { tag
        , styles: Nil
        , css: Nothing
        , attributes: flatten $ Batch attributes
        , children: flatten $ Batch children
        , node: Nothing
        , noDiff: false
        }

elementS :: ∀ msg. String -> Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
elementS tag styles attributes children =
  let
    styles' = flattenMap unwrap $ Batch styles

    mstyles = VC.process styles'
  in
    Single
      $ VElement
      $ { tag
        , styles: styles'
        , css: Nothing
        , attributes: flatten $ Batch attributes
        , children: flatten $ Batch children
        , node: Nothing
        , noDiff: false
        }
      # \r -> case mstyles of
          Just css ->
            r
              { attributes = AddClass css.class : r.attributes
              , css = Just $ Map.singleton css.class css.css
              }
          Nothing -> r

noDiff :: ∀ msg. String -> Array (Attribute msg) -> Array (Html msg) -> Html msg
noDiff tag attributes children =
  Single
    $ VElement
        { tag
        , styles: Nil
        , css: Nothing
        , attributes: flatten $ Batch attributes
        , children: flatten $ Batch children
        , node: Nothing
        , noDiff: true
        }

text :: ∀ msg. String -> Html msg
text = Single <. VD.text

a :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
a = element "a"

aS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
aS = elementS "a"

abbr :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
abbr = element "abbr"

abbrS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
abbrS = elementS "abbr"

address :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
address = element "address"

addressS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
addressS = elementS "address"

area :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
area = element "area"

areaS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
areaS = elementS "area"

article :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
article = element "article"

articleS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
articleS = elementS "article"

aside :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
aside = element "aside"

asideS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
asideS = elementS "aside"

audio :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
audio = element "audio"

audioS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
audioS = elementS "audio"

b :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
b = element "b"

bS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
bS = elementS "b"

base :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
base = element "base"

baseS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
baseS = elementS "base"

bdi :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
bdi = element "bdi"

bdiS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
bdiS = elementS "bdi"

bdo :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
bdo = element "bdo"

bdoS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
bdoS = elementS "bdo"

blockquote :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
blockquote = element "blockquote"

blockquoteS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
blockquoteS = elementS "blockquote"

body :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
body = element "body"

bodyS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
bodyS = elementS "body"

br :: ∀ msg. Array (Attribute msg) -> Html msg
br = element "br" ~$ []

brS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Html msg
brS = elementS "br" ~~$ []

button :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
button = element "button"

buttonS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
buttonS = elementS "button"

canvas :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
canvas = element "canvas"

canvasS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
canvasS = elementS "canvas"

caption :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
caption = element "caption"

captionS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
captionS = elementS "caption"

cite :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
cite = element "cite"

citeS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
citeS = elementS "cite"

code :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
code = element "code"

codeS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
codeS = elementS "code"

col :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
col = element "col"

colS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
colS = elementS "col"

colgroup :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
colgroup = element "colgroup"

colgroupS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
colgroupS = elementS "colgroup"

data_ :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
data_ = element "data"

data_S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
data_S = elementS "data"

datalist :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
datalist = element "datalist"

datalistS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
datalistS = elementS "datalist"

dd :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
dd = element "dd"

ddS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
ddS = elementS "dd"

del :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
del = element "del"

delS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
delS = elementS "del"

details :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
details = element "details"

detailsS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
detailsS = elementS "details"

dfn :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
dfn = element "dfn"

dfnS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
dfnS = elementS "dfn"

dialog :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
dialog = element "dialog"

dialogS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
dialogS = elementS "dialog"

div :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
div = element "div"

divS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
divS = elementS "div"

dl :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
dl = element "dl"

dlS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
dlS = elementS "dl"

dt :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
dt = element "dt"

dtS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
dtS = elementS "dt"

em :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
em = element "em"

emS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
emS = elementS "em"

embed :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
embed = element "embed"

embedS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
embedS = elementS "embed"

fieldset :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
fieldset = element "fieldset"

fieldsetS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
fieldsetS = elementS "fieldset"

figcaption :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
figcaption = element "figcaption"

figcaptionS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
figcaptionS = elementS "figcaption"

figure :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
figure = element "figure"

figureS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
figureS = elementS "figure"

footer :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
footer = element "footer"

footerS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
footerS = elementS "footer"

form :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
form = element "form"

formS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
formS = elementS "form"

h1 :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h1 = element "h1"

h1S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
h1S = elementS "h1"

h2 :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h2 = element "h2"

h2S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
h2S = elementS "h2"

h3 :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h3 = element "h3"

h3S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
h3S = elementS "h3"

h4 :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h4 = element "h4"

h4S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
h4S = elementS "h4"

h5 :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h5 = element "h5"

h5S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
h5S = elementS "h5"

h6 :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h6 = element "h6"

h6S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
h6S = elementS "h6"

head :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
head = element "head"

headS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
headS = elementS "head"

header :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
header = element "header"

headerS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
headerS = elementS "header"

hgroup :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
hgroup = element "hgroup"

hgroupS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
hgroupS = elementS "hgroup"

hr :: ∀ msg. Array (Attribute msg) -> Html msg
hr = element "hr" ~$ []

hrS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Html msg
hrS = elementS "hr" ~~$ []

html :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
html = element "html"

htmlS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
htmlS = elementS "html"

i :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
i = element "i"

iS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
iS = elementS "i"

iframe :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
iframe = element "iframe"

iframeS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
iframeS = elementS "iframe"

img :: ∀ msg. Array (Attribute msg) -> Html msg
img = element "img" ~$ []

imgS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Html msg
imgS = elementS "img" ~~$ []

input :: ∀ msg. Array (Attribute msg) -> Html msg
input = element "input" ~$ []

inputS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Html msg
inputS = elementS "input" ~~$ []

ins :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
ins = element "ins"

insS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
insS = elementS "ins"

kbd :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
kbd = element "kbd"

kbdS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
kbdS = elementS "kbd"

label :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
label = element "label"

labelS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
labelS = elementS "label"

legend :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
legend = element "legend"

legendS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
legendS = elementS "legend"

li :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
li = element "li"

liS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
liS = elementS "li"

link :: ∀ msg. Array (Attribute msg) -> Html msg
link = element "link" ~$ []

linkStylesheet :: ∀ msg. String -> Html msg
linkStylesheet src = link [ A.rel "stylesheet", A.href src ]

main :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
main = element "main"

mainS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
mainS = elementS "main"

map :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
map = element "map"

mapS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
mapS = elementS "map"

mark :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
mark = element "mark"

markS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
markS = elementS "mark"

mathML :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
mathML = element "MathML"

mathMLS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
mathMLS = elementS "MathML"

math :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
math = element "math"

mathS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
mathS = elementS "math"

menu :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
menu = element "menu"

menuS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
menuS = elementS "menu"

meta :: ∀ msg. Array (Attribute msg) -> Html msg
meta = element "meta" ~$ []

meter :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
meter = element "meter"

meterS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
meterS = elementS "meter"

nav :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
nav = element "nav"

navS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
navS = elementS "nav"

noscript :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
noscript = element "noscript"

noscriptS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
noscriptS = elementS "noscript"

object :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
object = element "object"

objectS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
objectS = elementS "object"

ol :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
ol = element "ol"

olS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
olS = elementS "ol"

optgroup :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
optgroup = element "optgroup"

optgroupS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
optgroupS = elementS "optgroup"

option :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
option = element "option"

optionS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
optionS = elementS "option"

output :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
output = element "output"

outputS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
outputS = elementS "output"

p :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
p = element "p"

pS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
pS = elementS "p"

param :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
param = element "param"

paramS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
paramS = elementS "param"

picture :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
picture = element "picture"

pictureS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
pictureS = elementS "picture"

pre :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
pre = element "pre"

preS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
preS = elementS "pre"

progress :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
progress = element "progress"

progressS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
progressS = elementS "progress"

q :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
q = element "q"

qS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
qS = elementS "q"

rp :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
rp = element "rp"

rpS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
rpS = elementS "rp"

rt :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
rt = element "rt"

rtS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
rtS = elementS "rt"

ruby :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
ruby = element "ruby"

rubyS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
rubyS = elementS "ruby"

s :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
s = element "s"

sS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
sS = elementS "s"

samp :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
samp = element "samp"

sampS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
sampS = elementS "samp"

script :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
script = element "script"

scriptS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
scriptS = elementS "script"

section :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
section = element "section"

sectionS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
sectionS = elementS "section"

select :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
select = element "select"

selectS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
selectS = elementS "select"

slot :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
slot = element "slot"

slotS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
slotS = elementS "slot"

small :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
small = element "small"

smallS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
smallS = elementS "small"

source :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
source = element "source"

sourceS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
sourceS = elementS "source"

span :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
span = element "span"

spanS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
spanS = elementS "span"

strong :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
strong = element "strong"

strongS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
strongS = elementS "strong"

sub :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
sub = element "sub"

subS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
subS = elementS "sub"

summary :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
summary = element "summary"

summaryS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
summaryS = elementS "summary"

sup :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
sup = element "sup"

supS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
supS = elementS "sup"

svg :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
svg = element "svg"

svgS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
svgS = elementS "svg"

table :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
table = element "table"

tableS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
tableS = elementS "table"

tbody :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
tbody = element "tbody"

tbodyS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
tbodyS = elementS "tbody"

td :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
td = element "td"

tdS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
tdS = elementS "td"

template :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
template = element "template"

templateS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
templateS = elementS "template"

textarea :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
textarea = element "textarea"

textareaS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
textareaS = elementS "textarea"

tfoot :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
tfoot = element "tfoot"

tfootS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
tfootS = elementS "tfoot"

th :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
th = element "th"

thS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
thS = elementS "th"

thead :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
thead = element "thead"

theadS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
theadS = elementS "thead"

time :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
time = element "time"

timeS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
timeS = elementS "time"

title :: ∀ msg. String -> Html msg
title t = element "title" [] [ text t ]

tr :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
tr = element "tr"

trS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
trS = elementS "tr"

track :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
track = element "track"

trackS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
trackS = elementS "track"

u :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
u = element "u"

uS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
uS = elementS "u"

ul :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
ul = element "ul"

ulS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
ulS = elementS "ul"

var :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
var = element "var"

varS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
varS = elementS "var"

video :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
video = element "video"

videoS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
videoS = elementS "video"

wbr :: ∀ msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
wbr = element "wbr"

wbrS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (Html msg) -> Html msg
wbrS = elementS "wbr"
