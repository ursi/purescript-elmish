module Html where

import MasonPrelude
import Data.Batchable (Batched(..), batch, flatten)
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

createVNode :: ∀ msg. String -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
createVNode tag attributes children =
  Single
    $ VElement
        { tag
        , attributes: flatten $ batch attributes
        , children: flatten $ batch children
        , node: Nothing
        }

text :: ∀ msg. String -> VNode msg
text = Single <. VText <. { text: _, node: Nothing }

div :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
div = createVNode "div"

label :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
label = createVNode "label"

select :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
select = createVNode "select"

option :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
option = createVNode "option"

button :: ∀ msg. Array (Attribute msg) -> Array (VNode msg) -> VNode msg
button = createVNode "button"

input :: ∀ msg. Array (Attribute msg) -> VNode msg
input = flip (createVNode "input") []
