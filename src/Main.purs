module Main where

import Prelude
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Control.Monad.Writer.Class (tell)
import Effect.Console (log, logShow)
import Effect.Class (liftEffect)
import Platform
import Task

main :: Program Unit Model Msg
main =
  worker2
    { init
    , update
    }

type Model
  = Int

init :: Unit -> Shorten Msg Model
init _ = do
  tell $ attemptTask (const unit) $ wait 1000.0
  pure 0

waitAndLog :: Model -> Shorten Msg Unit
waitAndLog model =
  tell
    $ attemptTask (const unit) do
        wait 1000.0
        liftEffect $ logShow model

type Msg
  = Unit

update :: Model -> Msg -> Shorten Msg Model
update model msg = do
  let
    newModel = model + 1
  tell $ attemptTask (const unit) $ wait 1000.0
  lift
    $ log
        if mod newModel 15 == 0 then
          "FizzBuzz"
        else
          if mod newModel 5 == 0 then
            "Buzz"
          else
            if mod newModel 3 == 0 then
              "Fizz"
            else
              show newModel
  pure $ model + 1

foreign import wait :: ∀ x. Number -> Task x Unit
