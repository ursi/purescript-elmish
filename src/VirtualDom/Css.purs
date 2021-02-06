module VirtualDom.Css where

import MasonPrelude hiding (apply)
import Data.Array as Array
import Data.Batched (Batched, flatten, flattenMap)
import Data.Identity (Identity)
import Data.List ((:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as Nel
import Data.NonEmpty (NonEmpty(..))
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
    toLists styles
      # foldr
          ( \styleList acc ->
              let
                (Declaration op _ _) = Nel.head styleList

                declarations =
                  " {\n"
                    <> reverseJoinMap
                        ( \(Declaration _ prop value) ->
                            "\t" <> prop <> ": " <> value <> ";"
                        )
                        "\n"
                        styleList
                    <> "\n}"
              in
                case acc of
                  Id -> op <> Const declarations
                  _ -> acc <> Const "\n\n" <> op <> Const declarations
          )
          Id
      # Just
      <. makeHash

makeHash :: StringOp -> { class :: String, css :: String }
makeHash toCssOp =
  let
    toCss = apply toCssOp
  in
    -- there's nothing special about this "o", it could be any string
    toCss "o"
      # hash
      # show
      # (<>) "_"
      # \c ->
          { "class": c
          , css: toCss $ "." <> c
          }

toLists :: List Style -> Array (NonEmptyList Style)
toLists =
  foldl
    ( \acc style@(Declaration op _ _) -> case acc of
        Right (styleList@(NonEmptyList (NonEmpty (Declaration lastOp _ _) _)) /\ array) ->
          if op == lastOp then
            Right $ (Nel.cons style styleList) /\ array
          else
            Right $ pure style /\ Array.cons styleList array
        Left array -> Right $ pure style /\ array
    )
    (Left [])
    .> case _ of
        Right (styleNel /\ stylesArr) -> Array.cons styleNel stylesArr
        Left arr -> arr

hash :: String -> Int
hash = Murmur3.hash 0

joinMap :: ∀ a. (a -> String) -> String -> List a -> String
joinMap f sep list = case list of
  only : Nil -> f only
  first : rest -> f first <> sep <> joinMap f sep rest
  Nil -> ""

reverseJoinMap :: ∀ a. (a -> String) -> String -> NonEmptyList a -> String
reverseJoinMap f sep (NonEmptyList (NonEmpty first rest)) = case Nel.fromList rest of
  Just nel -> reverseJoinMap f sep nel <> sep <> f first
  Nothing -> f first
