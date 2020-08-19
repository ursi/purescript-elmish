module Main where

import Prelude
import Attribute as A
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Control.Monad.Writer.Class (tell)
import Effect (Effect)
import Data.Array ((..))
import Data.Batchable (batch)
import Data.DateTime.Instant as Instant
import Data.Foldable (fold)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Debug as Debug
import Effect.Console (log, logShow)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Html (Html)
import Html as H
import Platform (Cmd, Program)
import Platform as Platform
import Task
import Sub (Sub(..), SubBuilder)
import Sub as Sub
import VirtualDom

main :: Program Unit Model Msg
main =
  Platform.app
    { init
    , update
    , subscriptions
    , view
    }

-- MODEL
type Model
  = { input :: String
    , counter :: Int
    , showingInput :: Boolean
    , time :: Number
    }

init :: Unit -> WriterT (Cmd Msg) Effect Model
init _ = do
  currentTime <- lift $ now <#> Instant.unInstant >>> unwrap
  pure
    { input: ""
    , counter: 0
    , showingInput: true
    , time: currentTime
    }

-- UPDATE
data Msg
  = Increment
  | Decrement
  | InputChanged String
  | ToggleShowingInput
  | UpdateTime Number

update :: Model -> Msg -> WriterT (Cmd Msg) Effect Model
update model msg = do
  let
    newModel = case msg of
      UpdateTime time -> model { time = time }
      ToggleShowingInput -> model { showingInput = not model.showingInput }
      InputChanged str -> model { input = str }
      Increment -> model { counter = model.counter + 1 }
      Decrement -> model { counter = model.counter - 1 }
  lift $ logShow newModel
  pure newModel

-- SUBSCRIPTIONS
subscriptions :: Model -> Sub Msg
subscriptions model = Sub.every (if model.showingInput then 1000.0 else 2000.0) UpdateTime

-- VIEW
view :: Model -> Array (Html Msg)
view model =
  [ H.div [] [ H.text $ show model.time ]
  , H.div []
      [ H.button [ A.onClick ToggleShowingInput ] [ H.text "Toggle Input" ]
      , if model.showingInput then
          H.input
            [ A.value model.input
            , A.onInput InputChanged
            ]
        else
          mempty
      , H.div [] [ H.text model.input ]
      ]
  , (if model.showingInput then H.button else H.div) [ A.onClick Increment ] [ H.text "+" ]
  , H.button [ A.onClick Decrement ] [ H.text "-" ]
  , batch $ 0 .. model.counter <#> \i -> H.div [] [ H.text $ show i ]
  ]
