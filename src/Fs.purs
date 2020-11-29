module Fs where

import MasonPrelude
import Effect.Exception (Error)
import Task (Task, Promise)
import Task as Task

foreign import exists :: ∀ x. String -> Task x Boolean

foreign import mkdirPromise :: String -> Effect (Promise Error Unit)

mkdir :: String -> Task Error Unit
mkdir = Task.fromPromise <. mkdirPromise

foreign import readdirPromise :: String -> Effect (Promise Error (Array String))

readdir :: String -> Task Error (Array String)
readdir = Task.fromPromise <. readdirPromise

foreign import unlinkPromise :: String -> Effect (Promise Error Unit)

unlink :: String -> Task Error Unit
unlink = Task.fromPromise <. unlinkPromise

foreign import writeFilePromise :: String -> String -> Effect (Promise Error Unit)

writeFile :: String -> String -> Task Error Unit
writeFile = Task.fromPromise <.. writeFilePromise

foreign import readFilePromise :: String -> Effect (Promise Error String)

readFile :: String -> Task Error String
readFile = Task.fromPromise <. readFilePromise

main :: Effect Unit
main = Task.run $ writeFile "task.txt" "it worked!"
