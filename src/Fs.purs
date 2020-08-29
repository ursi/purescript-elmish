module Fs where

import MasonPrelude
import Task (Task, Promise)
import Task as Task

foreign import exists :: ∀ x. String -> Task x Boolean

foreign import mkdirPromise :: String -> Promise Unit

mkdir :: String -> Task String Unit
mkdir = Task.fromPromise mkdirPromise

foreign import readdirPromise :: String -> Promise (Array String)

readdir :: String -> Task String (Array String)
readdir = Task.fromPromise readdirPromise

foreign import unlinkPromise :: String -> Promise Unit

unlink :: String -> Task String Unit
unlink = Task.fromPromise unlinkPromise

foreign import writeFilePromise :: String -> String -> Promise Unit

writeFile :: String -> String -> Task String Unit
writeFile path = Task.fromPromise (writeFilePromise path)

foreign import readFilePromise :: String -> Promise String

readFile :: String -> Task String String
readFile = Task.fromPromise readFilePromise
