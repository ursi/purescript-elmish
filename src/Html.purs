module Html where

import Prelude
import Data.Batchable (Batchable(..))
import Data.Maybe (Maybe(..))
import VirtualDom (Attribute, SingleVNode(..), VNode)

type Html msg
  = VNode msg

createVNode :: ∀ msg. String -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
createVNode tag attributes children =
  Single
    $ VNode
        { tag
        , attributes: Batch attributes
        , children: Batch children
        , node: Nothing
        }

text :: ∀ msg. String -> VNode msg
text = Single <<< VText <<< { text: _, node: Nothing }

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
