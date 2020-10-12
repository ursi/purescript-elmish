module Css.Global where

import MasonPrelude
import Css as C
import VirtualDom.Css as CV
import Data.Array as Array
import Data.Foldable (surroundMap)
import Data.List ((:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Batchable (Batched(..), flatten)
import Murmur3 (hash)
import VirtualDom.Css (Styles, Style)
import VirtualDom.Css as VC
import VirtualDom (SingleVNode, text)
import VirtualDom (element, text)
import VirtualDom (VNode)
import VirtualDom as VD

data Statement
  = Rule Rule_
  | Import (Array String)

--  | Keyframes Keyframes_
type Statements
  = Batched Statement

type Rule_
  = { selector :: String
    , declarations :: List Style
    }

type Keyframes_
  = { name :: String
    , rules :: Array Rule_
    }

type Stylesheet
  = { imports :: Array String
    , keyframes :: List Keyframes_
    , rules :: Map String (List Style)
    }

style :: ∀ msg. Array (Statements) -> VNode msg
style =
  Batch
    .> flatten
    .> toChildNodes
    .> VD.keyedElement "style" Nil
    .> Single

toChildNodes :: ∀ msg. List Statement -> List (String /\ SingleVNode msg)
toChildNodes =
  go
    { imports: []
    , keyframes: Nil
    , rules: mempty
    }
  where
  go :: Stylesheet -> List Statement -> List (String /\ SingleVNode msg)
  go stylesheet = case _ of
    head : tail -> case head of
      Rule rule_ ->
        go
          ( stylesheet
              { rules =
                Map.lookup rule_.selector stylesheet.rules
                  <#> ((<>) rule_.declarations)
                  # fromMaybe rule_.declarations
                  # Map.insert rule_.selector
                  ~$ stylesheet.rules
              }
          )
          tail
      Import imports -> go (stylesheet { imports = imports }) tail
    -- Keyframes keyframes ->
    --   go
    --     (stylesheet { keyframes = keyframes : stylesheet.keyframes })
    --     tail
    Nil ->
      ( if stylesheet.imports == [] then
          identity
        else
          Cons
            $ surroundMap "\n"
                (\import_ -> "@import '" <> import_ <> "';")
                stylesheet.imports
            # text
            # pure
            # element "style" Nil
            # Tuple "imports"
      )
        (rulesToStyleNodes stylesheet.rules)

rulesToStyleNodes :: ∀ msg. Map String (List Style) -> List (String /\ SingleVNode msg)
rulesToStyleNodes =
  Map.toUnfoldable
    .> List.mapMaybe \(selector /\ declarations) ->
        (VC.process declarations)
          <#> _.css
          <#> \ruleText ->
              show (hash 0 ruleText)
                /\ element "style" Nil (pure $ text ruleText)

rule :: String -> Array Styles -> Statements
rule selector styles =
  Single
    $ Rule
        { selector
        , declarations: flatten $ C.mapSelector (C.Const selector) styles
        }

imports :: Array String -> Statement
imports = Import

a :: Array Styles -> Statements
a = rule "a"

abbr :: Array Styles -> Statements
abbr = rule "abbr"

address :: Array Styles -> Statements
address = rule "address"

area :: Array Styles -> Statements
area = rule "area"

article :: Array Styles -> Statements
article = rule "article"

aside :: Array Styles -> Statements
aside = rule "aside"

audio :: Array Styles -> Statements
audio = rule "audio"

b :: Array Styles -> Statements
b = rule "b"

base :: Array Styles -> Statements
base = rule "base"

bdi :: Array Styles -> Statements
bdi = rule "bdi"

bdo :: Array Styles -> Statements
bdo = rule "bdo"

blockquote :: Array Styles -> Statements
blockquote = rule "blockquote"

body :: Array Styles -> Statements
body = rule "body"

br :: Array Styles -> Statements
br = rule "br"

button :: Array Styles -> Statements
button = rule "button"

canvas :: Array Styles -> Statements
canvas = rule "canvas"

caption :: Array Styles -> Statements
caption = rule "caption"

cite :: Array Styles -> Statements
cite = rule "cite"

code :: Array Styles -> Statements
code = rule "code"

col :: Array Styles -> Statements
col = rule "col"

colgroup :: Array Styles -> Statements
colgroup = rule "colgroup"

data_ :: Array Styles -> Statements
data_ = rule "data"

datalist :: Array Styles -> Statements
datalist = rule "datalist"

dd :: Array Styles -> Statements
dd = rule "dd"

del :: Array Styles -> Statements
del = rule "del"

details :: Array Styles -> Statements
details = rule "details"

dfn :: Array Styles -> Statements
dfn = rule "dfn"

dialog :: Array Styles -> Statements
dialog = rule "dialog"

div :: Array Styles -> Statements
div = rule "div"

dl :: Array Styles -> Statements
dl = rule "dl"

dt :: Array Styles -> Statements
dt = rule "dt"

em :: Array Styles -> Statements
em = rule "em"

embed :: Array Styles -> Statements
embed = rule "embed"

fieldset :: Array Styles -> Statements
fieldset = rule "fieldset"

figcaption :: Array Styles -> Statements
figcaption = rule "figcaption"

figure :: Array Styles -> Statements
figure = rule "figure"

footer :: Array Styles -> Statements
footer = rule "footer"

form :: Array Styles -> Statements
form = rule "form"

h1 :: Array Styles -> Statements
h1 = rule "h1"

h2 :: Array Styles -> Statements
h2 = rule "h2"

h3 :: Array Styles -> Statements
h3 = rule "h3"

h4 :: Array Styles -> Statements
h4 = rule "h4"

h5 :: Array Styles -> Statements
h5 = rule "h5"

h6 :: Array Styles -> Statements
h6 = rule "h6"

head :: Array Styles -> Statements
head = rule "head"

header :: Array Styles -> Statements
header = rule "header"

hgroup :: Array Styles -> Statements
hgroup = rule "hgroup"

hr :: Array Styles -> Statements
hr = rule "hr"

html :: Array Styles -> Statements
html = rule "html"

i :: Array Styles -> Statements
i = rule "i"

iframe :: Array Styles -> Statements
iframe = rule "iframe"

img :: Array Styles -> Statements
img = rule "img"

input :: Array Styles -> Statements
input = rule "input"

ins :: Array Styles -> Statements
ins = rule "ins"

kbd :: Array Styles -> Statements
kbd = rule "kbd"

label :: Array Styles -> Statements
label = rule "label"

legend :: Array Styles -> Statements
legend = rule "legend"

li :: Array Styles -> Statements
li = rule "li"

link :: Array Styles -> Statements
link = rule "link"

main :: Array Styles -> Statements
main = rule "main"

map :: Array Styles -> Statements
map = rule "map"

mark :: Array Styles -> Statements
mark = rule "mark"

mathML :: Array Styles -> Statements
mathML = rule "MathML"

math :: Array Styles -> Statements
math = rule "math"

menu :: Array Styles -> Statements
menu = rule "menu"

meta :: Array Styles -> Statements
meta = rule "meta"

meter :: Array Styles -> Statements
meter = rule "meter"

nav :: Array Styles -> Statements
nav = rule "nav"

noscript :: Array Styles -> Statements
noscript = rule "noscript"

object :: Array Styles -> Statements
object = rule "object"

ol :: Array Styles -> Statements
ol = rule "ol"

optgroup :: Array Styles -> Statements
optgroup = rule "optgroup"

option :: Array Styles -> Statements
option = rule "option"

output :: Array Styles -> Statements
output = rule "output"

p :: Array Styles -> Statements
p = rule "p"

param :: Array Styles -> Statements
param = rule "param"

picture :: Array Styles -> Statements
picture = rule "picture"

pre :: Array Styles -> Statements
pre = rule "pre"

progress :: Array Styles -> Statements
progress = rule "progress"

q :: Array Styles -> Statements
q = rule "q"

rp :: Array Styles -> Statements
rp = rule "rp"

rt :: Array Styles -> Statements
rt = rule "rt"

ruby :: Array Styles -> Statements
ruby = rule "ruby"

s :: Array Styles -> Statements
s = rule "s"

samp :: Array Styles -> Statements
samp = rule "samp"

script :: Array Styles -> Statements
script = rule "script"

section :: Array Styles -> Statements
section = rule "section"

select :: Array Styles -> Statements
select = rule "select"

slot :: Array Styles -> Statements
slot = rule "slot"

small :: Array Styles -> Statements
small = rule "small"

source :: Array Styles -> Statements
source = rule "source"

span :: Array Styles -> Statements
span = rule "span"

strong :: Array Styles -> Statements
strong = rule "strong"

sub :: Array Styles -> Statements
sub = rule "sub"

summary :: Array Styles -> Statements
summary = rule "summary"

sup :: Array Styles -> Statements
sup = rule "sup"

svg :: Array Styles -> Statements
svg = rule "svg"

table :: Array Styles -> Statements
table = rule "table"

tbody :: Array Styles -> Statements
tbody = rule "tbody"

td :: Array Styles -> Statements
td = rule "td"

template :: Array Styles -> Statements
template = rule "template"

textarea :: Array Styles -> Statements
textarea = rule "textarea"

tfoot :: Array Styles -> Statements
tfoot = rule "tfoot"

th :: Array Styles -> Statements
th = rule "th"

thead :: Array Styles -> Statements
thead = rule "thead"

time :: Array Styles -> Statements
time = rule "time"

title :: Array Styles -> Statements
title = rule "title"

tr :: Array Styles -> Statements
tr = rule "tr"

track :: Array Styles -> Statements
track = rule "track"

u :: Array Styles -> Statements
u = rule "u"

ul :: Array Styles -> Statements
ul = rule "ul"

var :: Array Styles -> Statements
var = rule "var"

video :: Array Styles -> Statements
video = rule "video"

wbr :: Array Styles -> Statements
wbr = rule "wbr"
