module VirtualDom where

import Prelude
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Writer.Trans (WriterT, lift, runWriterT, tell)
import Data.Argonaut (Json, (.:))
import Data.Argonaut as A
import Data.Array as Array
import Data.Batchable (Batchable(..))
import Data.Diff (diff)
import Data.Diff as Diff
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List, (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Foldable (fold, foldM, intercalate)
import Data.Traversable (sequence, traverse, traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Sub (Callback, Sub, SubBuilder)
import Sub as Sub
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document, createElement, createTextNode)
import Web.DOM.Element (Element, removeAttribute, setAttribute, tagName)
import Web.DOM.Element as Element
import Web.DOM.Node (Node, appendChild, firstChild, parentNode, removeChild, replaceChild)
import Web.DOM.Text as Text
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement

-- TODO: switch all Array (VNode msg) signatures to VNode msg signatures
todo :: ∀ a. a
todo = unsafeCoerce unit

todoTag :: ∀ a. String -> a
todoTag = unsafeCoerce

data SingleVNode msg
  = VElement
    { tag :: String
    , attributes :: Attribute msg
    , children :: VNode msg
    , node :: Maybe Node
    }
  | VText
    { text :: String
    , node :: Maybe Node
    }

type VNode msg
  = Batchable (SingleVNode msg)

data SingleAttribute msg
  = Str String String
  | Listener (Element -> Sub msg)

instance eqSingleAttribute :: Eq (SingleAttribute a) where
  eq (Str p1 v1) (Str p2 v2) = p1 == v1 && p2 == v2
  eq (Listener _) (Listener _) = true
  eq _ _ = false

type Attribute msg
  = Batchable (SingleAttribute msg)

render :: ∀ msg. VNode msg -> VNode msg -> Effect (VNode msg /\ Sub msg)
render oldVNode newVNode = do
  htmlDocument <- HTML.window >>= Window.document
  let
    document :: Document
    document = HTMLDocument.toDocument htmlDocument
  mbodyNode <- map HTMLElement.toNode <$> HTMLDocument.body htmlDocument
  maybe
    (throw "body not found")
    (\body -> runWriterT $ vDomDiff document body oldVNode newVNode)
    mbodyNode

vDomDiff :: ∀ msg. Document -> Node -> VNode msg -> VNode msg -> WriterT (Sub msg) Effect (VNode msg)
vDomDiff doc parent vn1 vn2 =
  diff
    ( case _ of
        Diff.Left sn -> do
          lift $ removeNode sn
          pure Nothing
        Diff.Right sn -> do
          singleNode <- addNode doc parent sn
          pure $ Just singleNode
        Diff.Both sn1 sn2 ->
          let
            replace :: WriterT (Sub msg) Effect (Maybe (SingleVNode msg))
            replace = do
              -- look into using replaceNode
              lift $ removeNode sn1
              node <- addNode doc parent sn2
              pure $ Just node
          in
            case sn1, sn2 of
              VElement r1, VElement r2 ->
                if r1.tag == r2.tag then
                  fromMaybe replace do
                    node <- r1.node
                    element <- Element.fromNode node
                    pure do
                      attributes <-
                        diff
                          ( case _ of
                              Diff.Left sa -> case sa of
                                Str prop _ -> do
                                  lift $ removeAttribute prop element
                                  pure Nothing
                                _ -> pure Nothing
                              Diff.Right sa -> case sa of
                                Str prop value -> do
                                  lift $ setAttribute prop value element
                                  pure $ Just sa
                                Listener toSub -> do
                                  tell $ toSub element
                                  pure $ Just sa
                              Diff.Both sa1 sa2 -> case sa1, sa2 of
                                Str prop1 value1, Str prop2 value2 ->
                                  if prop1 == prop2 then
                                    if value1 == value2 then
                                      pure $ Just sa1
                                    else do
                                      lift $ setAttribute prop1 value2 element
                                      pure $ Just sa2
                                  else do
                                    lift $ removeAttribute prop1 element
                                    lift $ setAttribute prop2 value2 element
                                    pure $ Just sa2
                                Listener _, Str prop value -> do
                                  lift $ setAttribute prop value element
                                  pure $ Just sa2
                                _, Listener toSub -> do
                                  tell $ toSub element
                                  pure $ Just sa2
                          )
                          r1.attributes
                          r2.attributes
                      children <-
                        {-- diff --}
                        {--   ( case _ of --}
                        {--       Diff.Left sn -> do --}
                        {--         _ <- lift $ removeChild node parent --}
                        {--         pure Nothing --}
                        {--       Diff.Right sn -> do --}
                        {--         singleNode <- addNode doc parent sn --}
                        {--         pure $ Just singleNode --}
                        {--       Diff.Both sn1' sn2' -> case sn1', sn2' of --}
                        {--         VNode r1', VNode r2' -> todo --}
                        {--         VText r1', VText r2' -> todo --}
                        {--         _, _ -> todo --}
                        {--   ) --}
                        vDomDiff doc node r1.children r2.children
                      pure $ Just
                        $ VElement
                            r1
                              { attributes = attributes
                              , children = children
                              }
                else
                  replace
              VText r1, VText r2 ->
                if r1.text == r2.text then
                  pure $ Just sn1
                else
                  replace
              _, _ -> replace
    )
    vn1
    vn2

addNode :: ∀ msg. Document -> Node -> SingleVNode msg -> WriterT (Sub msg) Effect (SingleVNode msg)
addNode doc parent = case _ of
  VElement { tag, attributes, children } -> do
    element <- lift $ createElement tag doc
    let
      elementNode = Element.toNode element
    childNodes <- traverse (addNode doc elementNode) children
    tell =<< (lift $ setAttributes attributes element)
    _ <- lift $ appendChild elementNode parent
    pure
      $ VElement
          { tag
          , attributes
          , children: childNodes
          , node: Just elementNode
          }
  VText { text } -> do
    textNode <- lift $ Text.toNode <$> createTextNode text doc
    _ <- lift $ appendChild textNode parent
    pure (VText { text, node: Just textNode })

removeNode :: ∀ msg. SingleVNode msg -> Effect Unit
removeNode =
  case _ of
    VElement r -> r.node
    VText r -> r.node
    >>> ifJust
        ( \node ->
            parentNode node
              >>= ifJust (removeChild node)
        )

ifJust :: ∀ a b. (a -> Effect b) -> Maybe a -> Effect Unit
ifJust f = maybe (pure unit) (f >>> (_ *> pure unit))

setAttributes :: ∀ msg. Attribute msg -> Element -> Effect (Sub msg)
setAttributes attribute element = foldM go mempty attribute
  where
  go :: Sub msg -> SingleAttribute msg -> Effect (Sub msg)
  go acc = case _ of
    Str prop value -> do
      setAttribute prop value element
      pure acc
    Listener toSub -> pure $ acc <> toSub element
