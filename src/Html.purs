module Html where

import MasonPrelude
import Css (Style, Styles)
import Data.Batchable (Batched(..), flatten)
import Data.Batchable as Batchable
import Data.List ((:))
import VirtualDom (Attribute, SingleVNode(..), VNode)

type Html msg
  = VNode msg

keyed :: ∀ msg. String -> Array (Attribute msg) -> Array (String /\ VNode msg) -> VNode msg
keyed tag attributes children =
  Single
    $ KeyedElement
        { tag
        , styles: mempty
        , attributes: flatten $ Batch attributes
        , children:
            foldr
              ( \(key /\ child) acc ->
                  fromMaybe acc do
                    first <- Batchable.first child
                    pure $ (key /\ first) : acc
              )
              Nil
              children
        , node: Nothing
        }

element :: ∀ msg. String -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
element tag attributes children =
  Single
    $ VElement
        { tag
        , styles: mempty
        , attributes: flatten $ Batch attributes
        , children: flatten $ Batch children
        , node: Nothing
        }

elementS :: ∀ msg. String -> Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
elementS tag styles attributes children =
  Single
    $ VElement
        { tag
        , styles: flatten $ Batch styles
        , attributes: flatten $ Batch attributes
        , children: flatten $ Batch children
        , node: Nothing
        }

text :: ∀ msg. String -> VNode msg
text = Single <. VText <. { text: _, node: Nothing }

a :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
a = element "a"

aS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
aS = elementS "a"

abbr :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
abbr = element "abbr"

abbrS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
abbrS = elementS "abbr"

address :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
address = element "address"

addressS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
addressS = elementS "address"

area :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
area = element "area"

areaS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
areaS = elementS "area"

article :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
article = element "article"

articleS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
articleS = elementS "article"

aside :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
aside = element "aside"

asideS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
asideS = elementS "aside"

audio :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
audio = element "audio"

audioS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
audioS = elementS "audio"

b :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
b = element "b"

bS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
bS = elementS "b"

base :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
base = element "base"

baseS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
baseS = elementS "base"

bdi :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
bdi = element "bdi"

bdiS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
bdiS = elementS "bdi"

bdo :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
bdo = element "bdo"

bdoS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
bdoS = elementS "bdo"

blockquote :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
blockquote = element "blockquote"

blockquoteS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
blockquoteS = elementS "blockquote"

body :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
body = element "body"

bodyS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
bodyS = elementS "body"

br :: ∀ msg. Array (Attribute msg) -> VNode msg
br = element "br" ~$ []

brS :: ∀ msg. Array Styles -> Array (Attribute msg) -> VNode msg
brS = elementS "br" ~~$ []

button :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
button = element "button"

buttonS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
buttonS = elementS "button"

canvas :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
canvas = element "canvas"

canvasS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
canvasS = elementS "canvas"

caption :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
caption = element "caption"

captionS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
captionS = elementS "caption"

cite :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
cite = element "cite"

citeS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
citeS = elementS "cite"

code :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
code = element "code"

codeS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
codeS = elementS "code"

col :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
col = element "col"

colS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
colS = elementS "col"

colgroup :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
colgroup = element "colgroup"

colgroupS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
colgroupS = elementS "colgroup"

data_ :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
data_ = element "data"

data_S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
data_S = elementS "data"

datalist :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
datalist = element "datalist"

datalistS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
datalistS = elementS "datalist"

dd :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
dd = element "dd"

ddS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
ddS = elementS "dd"

del :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
del = element "del"

delS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
delS = elementS "del"

details :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
details = element "details"

detailsS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
detailsS = elementS "details"

dfn :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
dfn = element "dfn"

dfnS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
dfnS = elementS "dfn"

dialog :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
dialog = element "dialog"

dialogS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
dialogS = elementS "dialog"

div :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
div = element "div"

divS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
divS = elementS "div"

dl :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
dl = element "dl"

dlS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
dlS = elementS "dl"

dt :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
dt = element "dt"

dtS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
dtS = elementS "dt"

em :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
em = element "em"

emS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
emS = elementS "em"

embed :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
embed = element "embed"

embedS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
embedS = elementS "embed"

fieldset :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
fieldset = element "fieldset"

fieldsetS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
fieldsetS = elementS "fieldset"

figcaption :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
figcaption = element "figcaption"

figcaptionS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
figcaptionS = elementS "figcaption"

figure :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
figure = element "figure"

figureS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
figureS = elementS "figure"

footer :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
footer = element "footer"

footerS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
footerS = elementS "footer"

form :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
form = element "form"

formS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
formS = elementS "form"

h1 :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h1 = element "h1"

h1S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h1S = elementS "h1"

h2 :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h2 = element "h2"

h2S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h2S = elementS "h2"

h3 :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h3 = element "h3"

h3S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h3S = elementS "h3"

h4 :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h4 = element "h4"

h4S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h4S = elementS "h4"

h5 :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h5 = element "h5"

h5S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h5S = elementS "h5"

h6 :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h6 = element "h6"

h6S :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
h6S = elementS "h6"

head :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
head = element "head"

headS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
headS = elementS "head"

header :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
header = element "header"

headerS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
headerS = elementS "header"

hgroup :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
hgroup = element "hgroup"

hgroupS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
hgroupS = elementS "hgroup"

hr :: ∀ msg. Array (Attribute msg) -> VNode msg
hr = element "hr" ~$ []

hrS :: ∀ msg. Array Styles -> Array (Attribute msg) -> VNode msg
hrS = elementS "hr" ~~$ []

html :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
html = element "html"

htmlS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
htmlS = elementS "html"

i :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
i = element "i"

iS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
iS = elementS "i"

iframe :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
iframe = element "iframe"

iframeS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
iframeS = elementS "iframe"

img :: ∀ msg. Array (Attribute msg) -> VNode msg
img = element "img" ~$ []

imgS :: ∀ msg. Array Styles -> Array (Attribute msg) -> VNode msg
imgS = elementS "img" ~~$ []

input :: ∀ msg. Array (Attribute msg) -> VNode msg
input = element "input" ~$ []

inputS :: ∀ msg. Array Styles -> Array (Attribute msg) -> VNode msg
inputS = elementS "input" ~~$ []

ins :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
ins = element "ins"

insS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
insS = elementS "ins"

kbd :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
kbd = element "kbd"

kbdS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
kbdS = elementS "kbd"

label :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
label = element "label"

labelS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
labelS = elementS "label"

legend :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
legend = element "legend"

legendS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
legendS = elementS "legend"

li :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
li = element "li"

liS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
liS = elementS "li"

link :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
link = element "link"

linkS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
linkS = elementS "link"

main :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
main = element "main"

mainS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
mainS = elementS "main"

map :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
map = element "map"

mapS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
mapS = elementS "map"

mark :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
mark = element "mark"

markS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
markS = elementS "mark"

mathML :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
mathML = element "MathML"

mathMLS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
mathMLS = elementS "MathML"

math :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
math = element "math"

mathS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
mathS = elementS "math"

menu :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
menu = element "menu"

menuS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
menuS = elementS "menu"

meta :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
meta = element "meta"

metaS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
metaS = elementS "meta"

meter :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
meter = element "meter"

meterS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
meterS = elementS "meter"

nav :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
nav = element "nav"

navS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
navS = elementS "nav"

noscript :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
noscript = element "noscript"

noscriptS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
noscriptS = elementS "noscript"

object :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
object = element "object"

objectS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
objectS = elementS "object"

ol :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
ol = element "ol"

olS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
olS = elementS "ol"

optgroup :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
optgroup = element "optgroup"

optgroupS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
optgroupS = elementS "optgroup"

option :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
option = element "option"

optionS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
optionS = elementS "option"

output :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
output = element "output"

outputS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
outputS = elementS "output"

p :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
p = element "p"

pS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
pS = elementS "p"

param :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
param = element "param"

paramS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
paramS = elementS "param"

picture :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
picture = element "picture"

pictureS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
pictureS = elementS "picture"

pre :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
pre = element "pre"

preS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
preS = elementS "pre"

progress :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
progress = element "progress"

progressS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
progressS = elementS "progress"

q :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
q = element "q"

qS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
qS = elementS "q"

rp :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
rp = element "rp"

rpS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
rpS = elementS "rp"

rt :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
rt = element "rt"

rtS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
rtS = elementS "rt"

ruby :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
ruby = element "ruby"

rubyS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
rubyS = elementS "ruby"

s :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
s = element "s"

sS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
sS = elementS "s"

samp :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
samp = element "samp"

sampS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
sampS = elementS "samp"

script :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
script = element "script"

scriptS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
scriptS = elementS "script"

section :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
section = element "section"

sectionS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
sectionS = elementS "section"

select :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
select = element "select"

selectS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
selectS = elementS "select"

slot :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
slot = element "slot"

slotS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
slotS = elementS "slot"

small :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
small = element "small"

smallS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
smallS = elementS "small"

source :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
source = element "source"

sourceS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
sourceS = elementS "source"

span :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
span = element "span"

spanS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
spanS = elementS "span"

strong :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
strong = element "strong"

strongS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
strongS = elementS "strong"

style :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
style = element "style"

styleS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
styleS = elementS "style"

sub :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
sub = element "sub"

subS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
subS = elementS "sub"

summary :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
summary = element "summary"

summaryS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
summaryS = elementS "summary"

sup :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
sup = element "sup"

supS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
supS = elementS "sup"

svg :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
svg = element "SVG"

svgS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
svgS = elementS "SVG"

table :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
table = element "table"

tableS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
tableS = elementS "table"

tbody :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
tbody = element "tbody"

tbodyS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
tbodyS = elementS "tbody"

td :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
td = element "td"

tdS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
tdS = elementS "td"

template :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
template = element "template"

templateS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
templateS = elementS "template"

textarea :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
textarea = element "textarea"

textareaS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
textareaS = elementS "textarea"

tfoot :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
tfoot = element "tfoot"

tfootS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
tfootS = elementS "tfoot"

th :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
th = element "th"

thS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
thS = elementS "th"

thead :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
thead = element "thead"

theadS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
theadS = elementS "thead"

time :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
time = element "time"

timeS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
timeS = elementS "time"

title :: ∀ msg. String -> VNode msg
title t = element "title" [] [ text t ]

tr :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
tr = element "tr"

trS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
trS = elementS "tr"

track :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
track = element "track"

trackS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
trackS = elementS "track"

u :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
u = element "u"

uS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
uS = elementS "u"

ul :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
ul = element "ul"

ulS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
ulS = elementS "ul"

var :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
var = element "var"

varS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
varS = elementS "var"

video :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
video = element "video"

videoS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
videoS = elementS "video"

wbr :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
wbr = element "wbr"

wbrS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
wbrS = elementS "wbr"
