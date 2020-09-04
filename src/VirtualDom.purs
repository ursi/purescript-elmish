module VirtualDom where

import MasonPrelude
import Control.Monad.Reader.Trans (ReaderT, ask, local, runReaderT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Data.Array as Array
import Data.Batchable (class Batchable, Batched(..), batch)
import Data.Diff (Diff, diff)
import Data.Diff as Diff
import Data.Foldable (foldM)
import Data.JSValue (JSValue)
import Data.List ((:))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Newtype as Newtype
import Debug as Debug
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
import Web.DOM.Node (Node, appendChild, firstChild, insertBefore, parentNode, removeChild, replaceChild)
import Web.DOM.Text as Text
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

data SingleVNode msg
  = VElement
    { tag :: String
    , attributes :: List (SingleAttribute msg)
    , children :: VDOM msg
    , node :: Maybe Node
    }
  | KeyedElement
    { tag :: String
    , attributes :: List (SingleAttribute msg)
    , children :: List (String /\ SingleVNode msg)
    , node :: Maybe Node
    }
  | VText
    { text :: String
    , node :: Maybe Node
    }

type MyMonad a b
  = ReaderT PatchContext (WriterT (Sub a) Effect) b

type MyMonadCommon a
  = MyMonad a (SingleVNode a)

type VNode msg
  = Batched (SingleVNode msg)

type VDOM msg
  = List (SingleVNode msg)

data SingleAttribute msg
  = Attr String String
  | Prop String JSValue
  | Listener (Element -> Sub msg)

instance eqSingleAttribute :: Eq (SingleAttribute a) where
  eq (Attr p1 v1) (Attr p2 v2) = p1 == p2 && v1 == v2
  eq (Prop p1 v1) (Prop p2 v2) = p1 == p2 && v1 == v2
  eq (Listener _) (Listener _) = true
  eq _ _ = false

type Attribute msg
  = Batched (SingleAttribute msg)

type PatchContext
  = { doc :: Document, parent :: Node }

changeParent :: Node -> PatchContext -> PatchContext
changeParent node = _ { parent = node }

-- maybe just separate this into separate functions
data Patch msg
  = Remove Node
  | InsertLast (SingleVNode msg)
  | InsertBefore (SingleVNode msg) Node
  | Switch Node Node

applyPatch :: ∀ msg. Patch msg -> MyMonad msg (Maybe (SingleVNode msg))
applyPatch patch = case patch of
  Remove node -> do
    { parent } <- ask
    liftEffect do
      _ <- removeChild node parent
      pure Nothing
  InsertLast vnode -> Just <$> addNode vnode
  InsertBefore vnode node' ->
    vnode
      # placeNode (\{ node, parent } -> insertBefore node node' parent)
      <#> Just
  Switch node1 node2 -> do
    { parent } <- ask
    liftEffect do
      _ <- insertBefore node1 node2 parent
      pure Nothing

render :: ∀ msg. Document -> Node -> VDOM msg -> VDOM msg -> Effect (VDOM msg /\ Sub msg)
render doc parent oldVNode newVNode = do
  runWriterT
    $ runReaderT
        (vDomDiff oldVNode newVNode)
        { doc, parent }

vDomDiff :: ∀ msg. VDOM msg -> VDOM msg -> MyMonad msg (VDOM msg)
vDomDiff =
  diff
    ( case _ of
        Diff.Left svn -> do
          removeVNode svn
          pure Nothing
        Diff.Right svn -> do
          singleNode <- addNode svn
          pure $ Just singleNode
        Diff.Both svn1 svn2 -> Just <$> diffSingle svn1 svn2
    )

keyedDiff ::
  ∀ msg.
  List (String /\ SingleVNode msg) ->
  List (String /\ SingleVNode msg) ->
  MyMonad msg (List (String /\ SingleVNode msg))
keyedDiff = case _, _ of
  head1 : tail1, head2 : tail2 ->
    let
      key1 /\ vnode1 = head1

      key2 /\ vnode2 = head2
    in
      if key1 == key2 then
        keyedDiffReplace vnode1 head2 tail1 tail2
      else case tail1, tail2 of
        head1' : tail1', head2' : tail2' ->
          if key1 == fst head2' then
            if key2 == fst head1' then do
              newKey1VNode <- diffSingle vnode2 (snd head1')
              newKey2VNode <- diffSingle vnode1 (snd head2')
              _ <-
                fromMaybe noNodeError do
                  node1 <- getNode newKey1VNode
                  node2 <- getNode newKey2VNode
                  Just $ applyPatch $ Switch node1 node2
              rest <- keyedDiff tail1' tail2'
              pure $ (key2 /\ newKey1VNode) : (key1 /\ newKey2VNode) : rest
            else
              keyedDiffInsertBefore head1 (snd head2') head2 tail1 tail2'
          else
            if key2 == fst head1' then
              keyedDiffRemove vnode1 (snd head1') head2 tail1' tail2
            else
              keyedDiffReplace vnode1 head2 tail1 tail2
        head : tail, Nil ->
          if key2 == fst head then
            keyedDiffRemove vnode1 (snd head) head2 tail tail2
          else
            keyedDiffReplace vnode1 head2 tail1 tail2
        Nil, head : tail ->
          if key1 == fst head then
            keyedDiffInsertBefore head1 (snd head) head2 tail1 tail
          else
            keyedDiffReplace vnode1 head2 tail1 tail2
        Nil, Nil -> keyedDiffReplace vnode1 head2 tail1 tail2
  remove, Nil -> do
    traverse_ (snd .> removeVNode) remove
    pure mempty
  Nil, insert -> traverse addKeyedNode insert

keyedDiffRemove ::
  ∀ msg.
  SingleVNode msg ->
  SingleVNode msg ->
  String /\ SingleVNode msg ->
  List (String /\ SingleVNode msg) ->
  List (String /\ SingleVNode msg) ->
  MyMonad msg (List (String /\ SingleVNode msg))
keyedDiffRemove removeMe oldVNode (key /\ newVNode) tail1 tail2 = do
  newVNodeNoded <- diffSingle oldVNode newVNode
  removeVNode removeMe
  rest <- keyedDiff tail1 tail2
  pure $ (key /\ newVNodeNoded) : rest

keyedDiffReplace ::
  ∀ msg.
  SingleVNode msg ->
  String /\ SingleVNode msg ->
  List (String /\ SingleVNode msg) ->
  List (String /\ SingleVNode msg) ->
  MyMonad msg (List (String /\ SingleVNode msg))
keyedDiffReplace vnode1 (key /\ vnode2) tail1 tail2 = do
  newVNode <- diffSingle vnode1 vnode2
  rest <- keyedDiff tail1 tail2
  pure $ (key /\ newVNode) : rest

keyedDiffInsertBefore ::
  ∀ msg.
  String /\ SingleVNode msg ->
  SingleVNode msg ->
  String /\ SingleVNode msg ->
  List (String /\ SingleVNode msg) ->
  List (String /\ SingleVNode msg) ->
  MyMonad msg (List (String /\ SingleVNode msg))
keyedDiffInsertBefore (key1 /\ vnode1) newVNode1' (key2 /\ vnode2) tail1 tail2 = do
  newVNode1 <- diffSingle vnode1 newVNode1'
  newVNode2 <- case getNode newVNode1 of
    Just node1 -> fromMaybe vnode2 <$> applyPatch (InsertBefore vnode2 node1)
    Nothing -> noNodeError
  rest <- keyedDiff tail1 tail2
  pure $ (key2 /\ newVNode2) : (key1 /\ newVNode1) : rest

noNodeError :: ∀ a m. MonadEffect m => m a
noNodeError = liftEffect $ throw "there is no node"

diffSingle :: ∀ msg. SingleVNode msg -> SingleVNode msg -> MyMonadCommon msg
diffSingle svn1 svn2 =
  let
    replace :: MyMonadCommon msg
    replace = case getNode svn1 of
      Just svn1Node -> do
        svn <- replaceNode svn1Node svn2
        pure svn
      Nothing -> pure svn2
  in
    case svn1, svn2 of
      VElement r1, VElement r2 ->
        if r1.tag == r2.tag then
          fromMaybe replace do
            node <- r1.node
            element <- Element.fromNode node
            Just do
              attributes <- lift $ diffAttributes element r1.attributes r2.attributes
              children <-
                local (changeParent node) $ vDomDiff r1.children r2.children
              pure
                $ VElement
                    r1
                      { attributes = attributes
                      , children = children
                      }
        else
          replace
      KeyedElement r1, KeyedElement r2 ->
        if r1.tag == r2.tag then
          fromMaybe replace do
            node <- r1.node
            element <- Element.fromNode node
            Just do
              attributes <- lift $ diffAttributes element r1.attributes r2.attributes
              children <- local (changeParent node) $ keyedDiff r1.children r2.children
              pure
                $ KeyedElement
                    r1
                      { attributes = attributes
                      , children = children
                      }
        else
          replace
      VText r1, VText r2 ->
        if r1.text == r2.text then
          pure svn1
        else
          replace
      _, _ -> replace

diffAttributes :: ∀ msg. Element -> List (SingleAttribute msg) -> List (SingleAttribute msg) -> WriterT (Sub msg) Effect (List (SingleAttribute msg))
diffAttributes element =
  diff
    ( case _ of
        Diff.Left sa -> case sa of
          Attr prop _ -> do
            lift $ removeAttribute prop element
            pure Nothing
          Prop prop value -> pure Nothing
          _ -> pure Nothing
        Diff.Right sa -> case sa of
          Attr prop value -> do
            lift $ setAttribute prop value element
            pure $ Just sa
          Prop prop value -> do
            lift $ setProperty prop value element
            pure $ Just sa
          Listener toSub -> do
            tell $ toSub element
            pure $ Just sa
        Diff.Both sa1 sa2 -> case sa1, sa2 of
          Attr prop1 value1, Attr prop2 value2 ->
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
          Prop prop1 value1, Prop prop2 value2 ->
            if prop1 == prop2 then
              if value1 == value2 then
                pure $ Just sa1
              else do
                lift $ setProperty prop1 value2 element
                pure $ Just sa2
            else do
              lift $ setProperty prop2 value2 element
              pure $ Just sa2
          Attr prop1 _, Prop prop2 value2 -> do
            lift $ removeAttribute prop1 element
            lift $ setProperty prop2 value2 element
            pure $ Just sa2
          Attr prop _, Listener toSub -> do
            lift $ removeAttribute prop element
            tell $ toSub element
            pure $ Just sa2
          Listener _, Prop prop value -> do
            lift $ setProperty prop value element
            pure $ Just sa2
          _, Attr prop value -> do
            lift $ setAttribute prop value element
            pure $ Just sa2
          _, Listener toSub -> do
            tell $ toSub element
            pure $ Just sa2
    )

getNode :: ∀ msg. SingleVNode msg -> Maybe Node
getNode = case _ of
  VElement r -> r.node
  KeyedElement r -> r.node
  VText r -> r.node

addNode :: ∀ msg. SingleVNode msg -> MyMonadCommon msg
addNode svn = svn # placeNode (\{ node, parent } -> appendChild node parent)

addKeyedNode :: ∀ msg. String /\ SingleVNode msg -> MyMonad msg (String /\ SingleVNode msg)
addKeyedNode (key /\ vnode) = do
  addedVNode <- addNode vnode
  pure $ key /\ addedVNode

replaceNode :: ∀ msg. Node -> SingleVNode msg -> MyMonad msg (SingleVNode msg)
replaceNode oldNode = placeNode (\{ node, parent } -> replaceChild node oldNode parent)

placeNode :: ∀ a msg. ({ node :: Node, parent :: Node } -> Effect a) -> SingleVNode msg -> MyMonadCommon msg
placeNode placer svn = do
  { doc, parent } <- ask
  case svn of
    VElement { tag, attributes, children } -> do
      element <- liftEffect $ createElement tag doc
      let
        node = Element.toNode element
      childNodes <- traverse (local (changeParent node) <. addNode) children
      tell =<< (liftEffect $ setAttributes attributes element)
      _ <- liftEffect $ placer { node, parent }
      pure
        $ VElement
            { tag
            , attributes
            , children: childNodes
            , node: Just node
            }
    KeyedElement { tag, attributes, children } -> do
      element <- liftEffect $ createElement tag doc
      let
        node = Element.toNode element
      childNodes <- traverse (local (changeParent node) <. addKeyedNode) children
      tell =<< (liftEffect $ setAttributes attributes element)
      _ <- liftEffect $ placer { node, parent }
      pure
        $ KeyedElement
            { tag
            , attributes
            , children: childNodes
            , node: Just node
            }
    VText { text } -> do
      node <- liftEffect $ Text.toNode <$> createTextNode text doc
      _ <- liftEffect $ placer { node, parent }
      pure (VText { text, node: Just node })

removeVNode :: ∀ msg. SingleVNode msg -> MyMonad msg Unit
removeVNode svn = do
  _ <-
    fromMaybe noNodeError do
      node <- getNode svn
      Just $ applyPatch $ Remove node
  pure unit

ifJust :: ∀ a b. (a -> Effect b) -> Maybe a -> Effect Unit
ifJust f = maybe (pure unit) (f .> (_ *> pure unit))

foreign import setProperty :: String -> JSValue -> Element -> Effect Unit

setAttributes :: ∀ msg. List (SingleAttribute msg) -> Element -> Effect (Sub msg)
setAttributes attribute element = foldM go mempty attribute
  where
  go :: Sub msg -> SingleAttribute msg -> Effect (Sub msg)
  go acc = case _ of
    Attr prop value -> do
      setAttribute prop value element
      pure acc
    Prop prop value -> do
      setProperty prop value element
      pure acc
    Listener toSub -> pure $ acc <> toSub element
