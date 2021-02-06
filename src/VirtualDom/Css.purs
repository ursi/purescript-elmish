module VirtualDom.Css where

import MasonPrelude hiding (apply)
import Data.Array as Array
import Data.Batched (Batched, flatten, flattenMap)
import Data.Identity (Identity)
import Data.List ((:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Debug as Debug
import Murmur3 as Murmur3

data Style
  = Declaration StringOp String String

type Styles
  = Batched Identity Style

data StringOp
  = Id
  | Const String
  | Combine StringOp StringOp
  | Compose StringOp StringOp

derive instance eqStringOp :: Eq StringOp

derive instance ordStringOp :: Ord StringOp

instance semigroupStringOp :: Semigroup StringOp where
  append = Combine

apply :: StringOp -> String -> String
apply stringOp str = case stringOp of
  Id -> str
  Const s -> s
  Combine so1 so2 -> apply so1 str <> apply so2 str
  Compose so1 so2 -> apply so1 $ apply so2 str

process :: List Style -> Maybe { class :: String, css :: String }
process styles =
  if List.null styles then
    Nothing
  else
    toMap styles
      # foldlWithIndex
          ( \k acc v ->
              let
                declarations =
                  " {\n"
                    <> joinMap
                        ( \(Declaration _ prop value) ->
                            "\t" <> prop <> ": " <> value <> ";"
                        )
                        "\n"
                        v
                    <> "\n}"
              in
                case acc of
                  Id -> k <> Const declarations
                  _ -> acc <> Const "\n\n" <> k <> Const declarations
          )
          Id
      # Just
      <. makeHash

makeHash :: StringOp -> { class :: String, css :: String }
makeHash toCssOp =
  let
    toCss = apply toCssOp
  in
    toCss "o"
      # hash
      # show
      # (<>) "_"
      # \c ->
          { "class": c
          , css: toCss $ "." <> c
          }

toMap :: List Style -> Map StringOp (List Style)
toMap =
  foldr
    ( \style@(Declaration op _ _) acc ->
        Map.alter
          ( case _ of
              Just list -> Just $ style : list
              Nothing -> Just $ pure $ style
          )
          op
          acc
    )
    mempty

hash :: String -> Int
hash = Murmur3.hash 0

joinMap :: ∀ a. (a -> String) -> String -> List a -> String
joinMap f sep list = case list of
  only : Nil -> f only
  first : rest -> f first <> sep <> joinMap f sep rest
  Nil -> ""
