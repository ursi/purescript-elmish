module Main where

import Prelude
import Attribute as A
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Control.Monad.Writer.Class (tell)
import Effect (Effect)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Foldable (fold)
import Effect.Console (log, logShow)
import Effect.Class (liftEffect)
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
    , subscriptions: const mempty
    , view
    }

type Model
  = Int

init :: Unit -> WriterT (Cmd Msg) Effect Model
init _ = pure 0

data Msg
  = Increment
  | Decrement

update :: Model -> Msg -> WriterT (Cmd Msg) Effect Model
update model msg =
  pure case msg of
    Increment -> model + 1
    Decrement -> model - 1

view :: Model -> Array (Html Msg)
view model =
  [ H.button [ A.onClick Increment ] [ H.text "+" ]
  , H.div [] [ H.text $ show model ]
  , H.button [ A.onClick Decrement ] [ H.text "-" ]
  ]
