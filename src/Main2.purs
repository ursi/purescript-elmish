module Main2 where

import Prelude
import Data.Array (intercalate)
import Data.Bifunctor (lmap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Task (Task)
import Task as Task

main :: Effect Unit
main =
  Task.capture logShowId
    ( Task.parallel
        [ wait 1000
            <#> const "Hello,"
            # Task.logSuccess
        , waitFail 1000
            <#> const "parallel tasks!"
            # Task.onError (\_ -> pure "cool")
            # Task.logSuccess
        ]
        <#> intercalate " "
        # Task.logSuccess ::
        Task Unit String
    )

foreign import wait :: ∀ x. Int -> Task x Unit

foreign import waitFail :: ∀ a. Int -> Task Unit a

foreign import consoleLog :: ∀ a. a -> a

foreign import logShowIdImpl :: ∀ a. (a -> String) -> a -> a

logShowId :: ∀ a. Show a => a -> a
logShowId = logShowIdImpl show

logShowTask :: ∀ x a. Show a => a -> Task x Unit
logShowTask = liftEffect <<< logShow

logTask :: ∀ x. String -> Task x Unit
logTask = liftEffect <<< log
