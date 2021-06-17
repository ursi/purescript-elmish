module Css.Functions where

import MasonPrelude
import Data.Foldable (surroundMap)

functionJ :: String -> Array String -> String
functionJ name args =
  name <> "(" <> intercalate ", " args <> ")"

function :: String -> String -> String
function name a = functionJ name [ a ]

function2 :: String -> String -> String -> String
function2 name a b = functionJ name [ a, b ]

function3 :: String -> String -> String -> String -> String
function3 name a b c = functionJ name [ a, b, c ]

function4 :: String -> String -> String -> String -> String -> String
function4 name a b c d = functionJ name [ a, b, c, d ]

-- transform
matrix :: Array Number -> String
matrix m =
  m
    # surroundMap ", " show
    # function "matrix"

translate :: String -> String
translate = function "translate"

translateX :: String -> String
translateX = function "translateX"

translateY :: String -> String
translateY = function "translateY"

scale :: Number -> Number -> String
scale x y =
  function2 "scale"
    (show x)
    (show y)

scaleX :: Number -> String
scaleX = function "scaleX" <. show

scaleY :: Number -> String
scaleY = function "scaleY" <. show

rotate :: String -> String
rotate = function "rotate"

skew :: String -> String
skew = function "skew"

skewX :: String -> String
skewX = function "skewX"

skewY :: String -> String
skewY = function "skewY"

-- color
rgb :: String -> String -> String -> String
rgb = function3 "rgb"

rgba :: String -> String -> String -> String -> String
rgba = function4 "rgba"

hls :: String -> String -> String -> String
hls = function3 "hls"

hlsa :: String -> String -> String -> String -> String
hlsa = function4 "hlsa"

-- filter
blur :: String -> String
blur = function "blur"

brightness :: String -> String
brightness = function "brightness"

contrast :: String -> String
contrast = function "contrast"

dropShadow :: String -> String
dropShadow = function "drop-shadow"

dropShadowJ :: Array String -> String
dropShadowJ = function "drop-shadow" <. intercalate " "

grayscale :: String -> String
grayscale = function "grayscale"

hueRotate :: String -> String
hueRotate = function "hue-rotate"

invert :: String -> String
invert = function "invert"

opacity :: String -> String
opacity = function "opacity"

sepia :: String -> String
sepia = function "sepia"

saturate :: String -> String
saturate = function "saturate"

-- calc
calc :: String -> String
calc = function "calc"

biOperator :: String -> String -> String -> String
biOperator operator operand1 operand2 = "(" <> operand1 <> " " <> operator <> " " <> operand2 <> ")"

neg :: String -> String
neg = (<>) "-"

add :: String -> String -> String
add = biOperator "+"

sub :: String -> String -> String
sub = biOperator "-"

mul :: Number -> String -> String
mul = biOperator "*" <. show

div :: String -> Number -> String
div value = biOperator "/" value <. show

-- misc
-- | [spec](https:://drafts.csswg.org/css-values-3/#urls)
url :: String -> String
url url_ = "url('" <> url_ <> "')"

var :: String -> String
var name = "var(--" <> name <> ")"
