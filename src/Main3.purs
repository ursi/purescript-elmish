module Main3 where

import Prelude
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, intercalate)
import Data.Function (on)
import Data.Int (toNumber)
import Data.String.Common (toUpper)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Effect (Effect)
import Effect.Console (log, logShow)
import Fs as Fs
import Path as Path
import Platform (Program, attemptTask, performTask)
import Platform as Platform
import Sub (Arg(..), SubImpl1, SubImpl2, Sub)
import Sub as Sub
import Task (Task)
import Task as Task

{-- import Task as Task --}
main :: Program Unit Model Msg
main =
  Platform.worker
    { init
    , update
    , subscriptions
    }

type Model
  = { one :: Int
    , two :: Int
    , secondTimer :: Boolean
    , stdin :: Boolean
    }

init :: Unit -> Effect Model
init _ = do
  attemptTask (const StartSecond) $ wait 500.0
  pure
    { one: 0
    , two: 0
    , secondTimer: false
    , stdin: true
    }

foreign import wait :: ∀ x. Number -> Task x Unit

data Msg
  = Receive Number
  | Receive2 Number
  | StartSecond
  | Stdin String

derive instance eqMsg :: Eq Msg

foreign import exit :: Effect Unit

update :: Msg -> Model -> Effect Model
update msg model = case msg of
  Stdin str -> do
    if str == "q" then
      exit
    else
      log $ toUpper str
    pure model
  StartSecond -> pure $ model { secondTimer = true }
  Receive2 n -> do
    let
      newModel = model { two = model.two + 1 }
    logShow newModel
    pure newModel
  Receive n -> do
    let
      newModel = model { one = model.one + 1 }
    logShow newModel
    pure newModel

subscriptions :: Model -> Array (Sub Msg)
subscriptions model =
  [ if model.stdin then
      stdin Stdin
    else
      Sub.none
  ]

newtype NumToMsg
  = NumToMsg (Number -> Msg)

instance eqNumToMsg :: Eq (NumToMsg) where
  eq (NumToMsg f1) (NumToMsg f2) = (eq `on` (_ $ 0.0)) f1 f2

every :: Number -> (Number -> Msg) -> Sub Msg
every ms toMsg =
  Sub.toSub2 "main3every"
    { arg: ms
    , eq
    }
    { arg: toMsg
    , eq: eq `on` (_ $ 0.0)
    }
    everyImpl

foreign import everyImpl :: SubImpl2 Number (Number -> Msg) Msg

foreign import stdinImpl :: SubImpl1 (String -> Msg) Msg

stdin :: (String -> Msg) -> Sub Msg
stdin toMsg =
  Sub.toSub1 "stdin"
    { arg: toMsg
    , eq: eq `on` (_ $ "")
    }
    stdinImpl
