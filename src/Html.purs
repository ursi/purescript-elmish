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

div :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
div = element "div"

divS :: ∀ msg. Array Styles -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
divS = elementS "div"

label :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
label = element "label"

select :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
select = element "select"

option :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
option = element "option"

button :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
button = element "button"

input :: ∀ msg. Array (Attribute msg) -> VNode msg
input = element "input" ~$ []

title :: ∀ msg. String -> VNode msg
title t = element "title" [] [ text t ]
