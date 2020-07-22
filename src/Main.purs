module Main where

import Prelude
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Control.Monad.Writer.Class (tell)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Foldable (fold)
import Effect.Console (log, logShow)
import Effect.Class (liftEffect)
import Platform
import Task
import Sub (Sub(..), SubBuilder, SubImpl)
import Sub as Sub

main :: Program Unit Model Msg
main =
  worker
    { init
    , update
    , subscriptions:
        \_ ->
          fold
            [ every 1000.0 ReceiveTime
            , every 5000.0 ReceiveTime
            ]
    }

type Model
  = Int

init :: Unit -> Shorten Msg Model
init _ = do
  pure 0

data Msg
  = ReceiveTime Number

update :: Model -> Msg -> Shorten Msg Model
update model msg = case msg of
  ReceiveTime t -> do
    liftEffect $ logShow t
    pure $ model + 1

foreign import waitImpl :: ∀ x. Number -> TaskImpl x Unit

wait :: ∀ x. Number -> Task x Unit
wait ms = Task $ waitImpl ms

foreign import everyImpl :: Number -> SubImpl Number

every :: ∀ msg. Number -> (Number -> msg) -> Sub msg
every ms toMsg =
  Sub.new "every" everyImpl
    # Sub.addArg ms A.fromNumber
    # Sub
    <#> toMsg
