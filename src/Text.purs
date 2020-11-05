module Text where

import Attribute (Attribute)
import Attribute as A
import Html (Html)
import Html as H

type URL
  = String

textIn :: ∀ msg. (Array (Attribute msg) -> Array (Html msg) -> Html msg) -> String -> Html msg
textIn elem t = elem [] [ H.text t ]

a :: ∀ msg. String -> URL -> Html msg
a text url = H.a [ A.href url ] [ H.text text ]

b :: ∀ msg. String -> Html msg
b = textIn H.b

code :: ∀ msg. String -> Html msg
code = textIn H.code

i :: ∀ msg. String -> Html msg
i = textIn H.i
