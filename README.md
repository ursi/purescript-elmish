Elm's buttons example

```purescript
module Main (main) where

import Prelude
import Attribute as A
import Html (Html)
import Html as H
import Platform (Program, Update)
import Platform as Platform

main :: Program Unit Msg Model
main =
  Platform.app
    { init
    , update
    , view
    , subscriptions: mempty
    }

type Model
  = Int

init :: Unit -> Update Msg Model
init _ = pure 0

data Msg
  = Increment
  | Decrement

derive instance Eq Msg

update :: Model -> Msg -> Update Msg Model
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
