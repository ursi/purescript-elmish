module Test.Main (main) where

import MasonPrelude
import Attribute (Attribute)
import Attribute as A
import Control.Monad.Trans.Class (lift)
import Css as C
import Css.Functions as CF
import Css.Global as CG
import Data.Array ((..))
import Data.Array as Array
import Data.Batched (Batched(..))
import Data.DateTime.Instant as Instant
import Data.Newtype (unwrap)
import Design as Ds
import Effect.Now (now)
import Html (Html)
import Html as H
import Platform (Program, Update)
import Platform as Platform
import Producer (producer)
import Sub (Sub)
import Sub as Sub
import WHATWG.HTML.All (KeyboardEvent)
import WHATWG.HTML.All as HTML

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
    , mousePosition :: Int /\ Int
    , lastKey :: String
    , emptyNonemptyText :: EmptyState
    }

init :: Unit -> Update Msg Model
init _ = do
  currentTime <- lift $ now <#> Instant.unInstant >>> unwrap
  pure
    { input: ""
    , counter: 0
    , showingInput: true
    , time: currentTime
    , people: Array.sort people
    , newPerson: ""
    , mousePosition: 0 /\ 0
    , lastKey: ""
    , emptyNonemptyText: Empty
    }

data EmptyState
  = Empty
  | NonEmpty

derive instance Eq EmptyState

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
  | MouseMoved (Int /\ Int)
  | KeyPressed KeyboardEvent
  | SwitchEmptyState
  | NoOp

instance Eq Msg where
  eq =
    case _, _ of
      Increment, Increment -> true
      Decrement, Decrement -> true
      InputChanged s1, InputChanged s2 -> s1 == s2
      ToggleShowingInput, ToggleShowingInput -> true
      UpdateTime t1, UpdateTime t2 -> t1 == t2
      Delete s1, Delete s2 -> s1 == s2
      AddPerson, AddPerson -> true
      UpdateNewPerson s1, UpdateNewPerson s2 -> s1 == s2
      MouseMoved c1, MouseMoved c2 -> c1 == c2
      SwitchEmptyState, SwitchEmptyState -> true
      NoOp, NoOp -> true
      _, _ -> false

update :: Model -> Msg -> Update Msg Model
update model msg = do
  case msg of
    SwitchEmptyState ->
      pure
        (model
           { emptyNonemptyText =
               if model.emptyNonemptyText == Empty then
                 NonEmpty
               else
                 Empty
           }
        )

    KeyPressed kbe -> pure $ model { lastKey = HTML.key kbe }
    MouseMoved pos -> pure $ model { mousePosition = pos }
    UpdateNewPerson str -> pure $ model { newPerson = str }

    AddPerson ->
      if model.newPerson == "" then
        pure model
      else do
        pure $ model { people = Array.insert model.newPerson model.people }

    Delete person -> pure $ model { people = Array.delete person model.people }
    UpdateTime time -> pure $ model { time = time }
    ToggleShowingInput -> pure $ model { showingInput = not model.showingInput }
    InputChanged str -> pure $ model { input = str }
    Increment -> pure $ model { counter = model.counter + 1 }
    Decrement -> pure $ model { counter = model.counter - 1 }
    NoOp -> pure $ model

-- SUBSCRIPTIONS
subscriptions :: Model -> Sub Msg
subscriptions _ = Sub.on "keydown" hitKey

hitKey :: HTML.Event -> Effect (Maybe Msg)
hitKey =
  HTML.toMaybeKeyboardEvent
  .> maybe (pure Nothing) (pure <. Just <. KeyPressed)

-- VIEW
view ::
  Model ->
  { head :: Array (Html Msg)
  , body :: Array (Html Msg)
  }
view model =
  { head:
      [ H.title model.newPerson
      , default
      , H.style
          [ CG.body
              [ Ds.varStyles
              , C.background Ds.vars.background
              ]
          ]
      ]
  , body:
      [ H.divS
          [ C.background Ds.vars.accent
          , C.border "1px solid black"
          , C.mapSelector (C.append " > div")
              [ C.fontWeight "bold"
              , C.fontFamily "serif"
              ]
          ]
          [ A.onMouseMove MouseMoved ]
          -- []
          [ H.divS
              [ C.fontSize "20px"
              , C.position "absolute"
              , C.top $ C.px $ toNumber $ snd model.mousePosition
              , C.left $ CF.calc $ CF.add (C.px $ toNumber $ fst model.mousePosition) "2px"
              , C.width "20px"
              , C.height "20px"
              , C.background "black"
              ]
              [ A.addClass "test" ]
              [ H.text "t" ]
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
          , H.div [] [ H.text $ model.lastKey ]
          , H.div []
              [ H.button [ A.onClick ToggleShowingInput ] [ H.text "Toggle Input" ]
              , if model.showingInput then
                  H.input
                    [ A.value model.input
                    , A.onInput InputChanged
                    ]
                else
                  mempty
              , H.div []
                  [ H.text "Only send to model if it switches from empty to nonemepty"
                  , H.input [ reportEmptyStateChange model.emptyNonemptyText ]
                  ]
              , H.div [] [ H.text model.input ]
              , H.div [] [ H.rawHtml model.input ]
              ]
          , (if model.showingInput then H.button else H.div) [ A.onClick Increment ] [ H.text "+" ]
          , H.button [ A.onClick Decrement ] [ H.text "-" ]
          , Batch $ 0 .. model.counter <#> \i -> H.div [] [ H.text $ show i ]
          ]
      , H.buttonS
          [ C.fontSize "3em"
          , C.hover [ C.background "pink" ]
          , C.active [ C.background "cyan" ]
          ]
          []
          [ H.text "button" ]
      ]
  }

reportEmptyStateChange :: EmptyState -> Attribute Msg
reportEmptyStateChange =
  A.onInput' <. producer reportEmptyStateChangeRE

reportEmptyStateChangeRE :: EmptyState -> String -> Effect (Maybe Msg)
reportEmptyStateChangeRE currentEmptyState =
  \value ->
    pure
      if value == "" then
        if currentEmptyState == Empty then
          Nothing
        else
          Just SwitchEmptyState
      else
        if currentEmptyState == Empty then
          Just SwitchEmptyState
        else
          Nothing

default :: ∀ msg. Html msg
default =
  Batch
    [ H.meta [ A.charset "utf-8" ]
    , H.meta
        [ A.name "viewport"
        , A.content "width=device-width"
        ]
    , H.style [ CG.body [ C.margin "0" ] ]
    ]
