Elm's buttons example

```purescript
module Main (main) where

import Prelude
import Attribute as A
import Html (Html)
import Html as H
import Platform (Program, Update)
import Platform as Platform

main :: Program Unit Model Msg
main =
  Platform.app
    { init
    , update
    , view
    , subscriptions: mempty
    }

type Model
  = Int

init :: Unit -> Update Model Msg
init _ = pure 0

data Msg
  = Increment
  | Decrement

update :: Model -> Msg -> Update Model Msg
update model = pure <<< case _ of
  Increment -> model + 1
  Decrement -> model - 1

view ::
  Model ->
  { head :: Array (Html Msg)
  , body :: Array (Html Msg)
  }
view model =
  { head: []
  , body:
      [ H.div []
          [ H.button [ A.onClick Decrement ] [ H.text "-" ]
          , H.div [] [ H.text $ show model ]
          , H.button [ A.onClick Increment ] [ H.text "+" ]
          ]
      ]
  }
```
