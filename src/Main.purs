module Main where

import Prelude
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, intercalate)
import Data.Traversable (traverse)
import Debug as Debug
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Fs as Fs
import Path as Path
import Platform (Program, attemptTask, performTask)
import Platform as Platform
import Task (Task)
import Task as Task

main :: Program Flags Model Msg
main =
  Platform.worker
    { init
    , update
    , subscriptions: const []
    }

type Model
  = Unit

init :: Flags -> Effect Model
init { args, __dirname } =
  let
    path = Path.join [ __dirname, "args" ]
  in
    do
      ( let
          mkdir :: Task String Unit
          mkdir = Fs.mkdir path

          writeArgs :: Task String Unit
          writeArgs = do
            Fs.readdir path
              >>= map (\file -> Fs.unlink $ Path.join [ path, file ])
              >>> fold
            args
              # Array.mapWithIndex
                  ( \i arg ->
                      Fs.writeFile
                        (Path.join [ path, show i <> ".txt" ])
                        arg
                  )
              # fold
        in
          Fs.exists path
            >>= case _, Array.null args of
                true, true ->
                  Fs.readdir path
                    >>= traverse (\file -> Fs.readFile $ Path.join [ path, file ])
                    <#> intercalate " "
                    >>= (liftEffect <<< log)
                true, false -> writeArgs
                false, true -> mkdir
                false, false -> do
                  mkdir
                  writeArgs
      )
        # Task.run

type Flags
  = { args :: Array String
    , __dirname :: String
    }

data Msg
  = NoOp

update :: Msg -> Model -> Effect Model
update msg model = case msg of
  _ -> pure model
