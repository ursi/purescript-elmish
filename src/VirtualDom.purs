module VirtualDom where

import Prelude
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Writer.Trans (WriterT, lift, runWriterT, tell)
import Data.Argonaut (Json, (.:))
import Data.Argonaut as A
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (intercalate)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Sub (Callback, Sub, SubBuilder, SubImpl)
import Sub as Sub
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document, createElement, createTextNode)
import Web.DOM.Element (Element, setAttribute, tagName)
import Web.DOM.Element as Element
import Web.DOM.Node (Node, appendChild, firstChild, removeChild)
import Web.DOM.Text as Text
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement

createVNode :: ∀ msg. String -> Array (Attribute msg) -> Array (VNode msg) -> VNode msg
createVNode tag attributes children =
  VNode
    { tag
    , attributes
    , children
    }

data VNode msg
  = VNode
    { tag :: String
    , attributes :: Array (Attribute msg)
    , children :: Array (VNode msg)
    }
  | VText String

data Attribute msg
  = Str String String
  | Listener (String -> Element -> Sub msg)

type Position
  = List Int

render :: ∀ msg. Array (VNode msg) -> Array (VNode msg) -> Effect (Sub msg)
render oldVNodes newVNodes = do
  htmlDocument <- HTML.window >>= Window.document
  let
    document :: Document
    document = HTMLDocument.toDocument htmlDocument
  realNodes /\ sub <- runWriterT $ traverseWithIndex (\i -> toRealNode (pure i) document) newVNodes
  {-- _ <- Sub.something [] sub callback --}
  mbodyNode <- map HTMLElement.toNode <$> HTMLDocument.body htmlDocument
  fromMaybe (pure unit)
    ( mbodyNode
        <#> \body -> do
            removeAllChildren body
            appendChildren realNodes body
    )
  pure sub

removeAllChildren :: Node -> Effect Unit
removeAllChildren node =
  firstChild node
    >>= case _ of
        Just child -> do
          _ <- removeChild child node
          removeAllChildren node
        Nothing -> pure unit

{-- renderMaybeT :: ∀ msg. Callback msg -> Array (VNode msg) -> Array (VNode msg) -> Effect Unit --}
{-- renderMaybeT callback oldVNodes newVNodes = do --}
{--   result <- --}
{--     runMaybeT do --}
{--       htmlDocument <- lift $ HTML.window >>= Window.document --}
{--       let --}
{--         document :: Document --}
{--         document = HTMLDocument.toDocument htmlDocument --}
{--       realNodes /\ sub <- lift $ runWriterT $ traverseWithIndex (\i -> toRealNode (pure i) document) newVNodes --}
{--       _ <- lift $ Sub.something [] sub callback --}
{--       body <- MaybeT $ map HTMLElement.toNode <$> HTMLDocument.body htmlDocument --}
{--       lift $ appendChildren realNodes body --}
{--   pure $ fromMaybe unit result --}
toRealNode :: ∀ msg. Position -> Document -> VNode msg -> WriterT (Sub msg) Effect Node
toRealNode position doc = case _ of
  VNode { tag, attributes, children } -> do
    element <- lift $ createElement tag doc
    childNodes <- traverseWithIndex (\i -> toRealNode (i : position) doc) children
    tell =<< (lift $ setAttributes position attributes element)
    let
      elementNode = Element.toNode element
    lift $ appendChildren childNodes elementNode
    pure elementNode
  VText text -> lift $ Text.toNode <$> createTextNode text doc

setAttributes :: ∀ msg. Position -> Array (Attribute msg) -> Element -> Effect (Sub msg)
setAttributes position a element = go 0 a mempty
  where
  go :: Int -> Array (Attribute msg) -> Sub msg -> Effect (Sub msg)
  go attributePosition attributes acc = case Array.uncons attributes of
    Just { head, tail } -> case head of
      Str prop value -> do
        setAttribute prop value element
        go (attributePosition + 1) tail acc
      Listener toSub -> do
        go (attributePosition + 1)
          tail
          $ acc
          <> ( toSub
                (subIdPrefix (attributePosition : position))
                element
            )
    Nothing -> pure acc

subIdPrefix :: Position -> String
subIdPrefix = intercalate "-" <<< map show

appendChildren :: Array Node -> Node -> Effect Unit
appendChildren children node = case Array.uncons children of
  Just { head, tail } -> do
    _ <- appendChild head node
    appendChildren tail node
  Nothing -> pure unit

subIdRef :: Ref Int
subIdRef = unsafePerformEffect $ Ref.new 0

newSubId :: Effect String
newSubId = do
  currentId <- Ref.read subIdRef
  Ref.write (currentId + 1) subIdRef
  pure $ "dom sub " <> show currentId
