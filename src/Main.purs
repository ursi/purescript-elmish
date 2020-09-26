module Main (main) where

import MasonPrelude
import Attribute as A
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (tell)
import Css as C
import Data.Array ((..))
import Data.Array as Array
import Data.Batchable (Batched(..))
import Data.DateTime.Instant as Instant
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Debug as Debug
import Effect.Console (log, logShow)
import Effect.Now (now)
import Html (Html)
import Html as H
import HTML.All as HA
import Platform (Cmd, Program, Update)
import Platform as Platform
import Task
import Sub (Sub(..), SubBuilder)
import Sub as Sub
import VirtualDom

people :: Array String
people = [ "Mason", "Belle", "Luke", "Nic" ]

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
    , people :: Array String
    , newPerson :: String
    , mousePosition :: Number /\ Number
    }

init :: Unit -> Update Model Msg
init _ = do
  currentTime <- lift $ now <#> Instant.unInstant >>> unwrap
  pure
    { input: ""
    , counter: 0
    , showingInput: true
    , time: currentTime
    , people: Array.sort people
    , newPerson: ""
    , mousePosition: 0.0 /\ 0.0
    }

-- UPDATE
data Msg
  = Increment
  | Decrement
  | InputChanged String
  | ToggleShowingInput
  | UpdateTime Number
  | Delete String
  | AddPerson
  | UpdateNewPerson String
  | MouseMoved (Number /\ Number)
  | NoOp

update :: Model -> Msg -> Update Model Msg
update model msg = do
  let
    newModel = case msg of
      MouseMoved pos -> model { mousePosition = pos }
      UpdateNewPerson str -> model { newPerson = str }
      AddPerson ->
        if model.newPerson == "" then
          model
        else
          model { people = Array.insert model.newPerson model.people }
      Delete person -> model { people = Array.delete person model.people }
      UpdateTime time -> model { time = time }
      ToggleShowingInput -> model { showingInput = not model.showingInput }
      InputChanged str -> model { input = str }
      Increment -> model { counter = model.counter + 1 }
      Decrement -> model { counter = model.counter - 1 }
      NoOp -> model
  lift $ logShow newModel
  pure newModel

-- SUBSCRIPTIONS
subscriptions :: Model -> Sub Msg
subscriptions model = mempty --Sub.every (if model.showingInput then 1000.0 else 2000.0) UpdateTime

-- VIEW
view ::
  Model ->
  { head :: Array (Html Msg)
  , body :: Array (Html Msg)
  }
view model =
  { head:
      [ H.title model.newPerson
      , H.keyed "style" []
          [ "style"
              /\ H.element "style" []
                  [ H.text
                      """
  body {
    background: red;
  }
"""
                  ]
          ]
      ]
  , body:
      [ H.divS
          [ C.declaration "background" "green"
          , C.mapSelector
              (C.append " < div")
              [ C.declaration "font-weight" "bold"
              , C.declaration "font-family" "serif"
              ]
          ]
          [ A.attribute "style" "border: 1px solid black"
          --, A.onMouseMove MouseMoved
          ]
          [ H.divS [ C.declaration "font-size" "20px" ]
              [ A.attribute "style" $ "position: absolute; top: "
                  <> show (snd model.mousePosition)
                  <> "px; left: calc("
                  <> show (fst model.mousePosition)
                  <> "px + 1px); width: 20px; height: 20px; background: black"
              ]
              []
          , H.div []
              [ H.text $ "("
                  <> show (fst model.mousePosition)
                  <> ","
                  <> show (snd model.mousePosition)
                  <> ")"
              ]
          , H.div []
              [ H.button [ A.onClick AddPerson ] [ H.text "Add Person" ]
              , H.input
                  [ A.value $ model.newPerson
                  , A.onInput UpdateNewPerson
                  ]
              ]
          , H.keyed "div" [] $ model.people
              <#> \person ->
                  person
                    /\ H.div []
                        [ H.text $ person <> " "
                        , H.button [ A.onClick $ Delete person ] [ H.text "x" ]
                        ]
          , H.div [] [ H.text $ show model.time ]
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
          , Batch $ 0 .. model.counter <#> \i -> H.div [] [ H.text $ show i ]
          ]
      ]
  }

handler :: HA.Event -> Effect Msg
handler =
  HA.toMaybeMouseEvent
    .> maybe (pure NoOp)
        ( \e -> do
            x <- HA.clientX e
            y <- HA.clientY e
            pure $ MouseMoved $ x /\ y
        )
