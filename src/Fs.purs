module Fs where

import MasonPrelude
import Effect.Exception (Error)
import Task (Task, Promise)
import Task as Task

foreign import exists :: ∀ x. String -> Task x Boolean

foreign import mkdirPromise :: String -> Promise Unit

mkdir :: String -> Task Error Unit
mkdir = Task.fromPromise <. const <. mkdirPromise

foreign import readdirPromise :: String -> Promise (Array String)

readdir :: String -> Task Error (Array String)
readdir = Task.fromPromise <. const <. readdirPromise

foreign import unlinkPromise :: String -> Promise Unit

unlink :: String -> Task Error Unit
unlink = Task.fromPromise <. const <. unlinkPromise

foreign import writeFilePromise :: String -> String -> Promise Unit

writeFile :: String -> String -> Task Error Unit
writeFile = Task.fromPromise <. const <.. writeFilePromise

foreign import readFilePromise :: String -> Promise String

readFile :: String -> Task Error String
readFile = Task.fromPromise <. const <. readFilePromise

main :: Effect Unit
main = Task.run $ writeFile "task.txt" "it worked!"
