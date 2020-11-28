module VirtualDom where

import MasonPrelude
import Control.Monad.Reader.Trans (ReaderT, ask, local, runReaderT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Data.Array as Array
import Data.Batchable (Batched(..))
import Data.Diff (Diff, diff)
import Data.Diff as Diff
import Data.Foldable (foldM)
import Data.JSValue (JSValue, toJSValue)
import Data.List ((:))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Newtype as Newtype
import Data.Nullable (null)
import Debug as Debug
import Effect.Console (log, logShow)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Sub (Callback, Sub, SubBuilder)
import Sub as Sub
import Unsafe.Coerce (unsafeCoerce)
import VirtualDom.Css (Style)
import VirtualDom.Css as VC
import WHATWG.HTML.All
  ( class IsNode
  , Document
  , Element
  , EventTarget
  , Node
  , Text
  )
import WHATWG.HTML.All as H

data SingleVNode msg
  = VElement
    { tag :: String
    , styles :: List Style
    , attributes :: List (SingleAttribute msg)
    , children :: VDOM msg
    , node :: Maybe Element
    }
  | KeyedElement
    { tag :: String
    , styles :: List Style
    , attributes :: List (SingleAttribute msg)
    , children :: List (String /\ SingleVNode msg)
    , node :: Maybe Element
    }
  | VText
    { text :: String
    , node :: Maybe Text
    }

text :: ∀ msg. String -> SingleVNode msg
text = VText <. { text: _, node: Nothing }

element ::
  ∀ msg.
  String ->
  List (SingleAttribute msg) ->
  List (SingleVNode msg) ->
  SingleVNode msg
element tag attributes children =
  VElement
    { tag
    , styles: Nil
    , attributes: attributes
    , children: children
    , node: Nothing
    }

keyedElement ::
  ∀ msg.
  String ->
  List (SingleAttribute msg) ->
  List (String /\ SingleVNode msg) ->
  SingleVNode msg
keyedElement tag attributes children =
  KeyedElement
    { tag
    , styles: Nil
    , attributes: attributes
    , children: children
    , node: Nothing
    }

type MyMonad a b
  = ReaderT PatchContext (WriterT (Sub a /\ Map String String) Effect) b

type MyMonadCommon a
  = MyMonad a (SingleVNode a)

type VNode msg
  = Batched (SingleVNode msg)

type VDOM msg
  = List (SingleVNode msg)

data SingleAttribute msg
  = Attr String String
  | Prop String JSValue
  | AddClass String
  | Listener (EventTarget -> Sub msg)

instance eqSingleAttribute :: Eq (SingleAttribute a) where
  eq (Attr p1 v1) (Attr p2 v2) = p1 == p2 && v1 == v2
  eq (Prop p1 v1) (Prop p2 v2) = p1 == p2 && v1 == v2
  eq (AddClass c1) (AddClass c2) = c1 == c2
  eq (Listener _) (Listener _) = true
  eq _ _ = false

type Attribute msg
  = Batched (SingleAttribute msg)

type PatchContext
  = { doc :: Document, parent :: Element }

changeParent :: Element -> PatchContext -> PatchContext
changeParent elem = _ { parent = elem }

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
      _ <- H.removeChild node parent
      pure Nothing
  InsertLast vnode -> Just <$> addNode vnode
  InsertBefore vnode node' ->
    vnode
      # placeNode (\{ node, parent } -> H.insertBefore node node' parent)
      <#> Just
  Switch node1 node2 -> do
    { parent } <- ask
    liftEffect do
      _ <- H.insertBefore node1 node2 parent
      pure Nothing

type HeadAndBody a
  = { head :: a, body :: a }

render ::
  ∀ msg.
  Document ->
  HeadAndBody Element ->
  HeadAndBody (VDOM msg) ->
  HeadAndBody (VDOM msg) ->
  Effect
    ( HeadAndBody (VDOM msg)
        /\ Sub msg
    )
render doc parents oldVNodes newVNodes = do
  bodyVdom /\ subs /\ styleMap <-
    runWriterT
      $ runReaderT
          (vDomDiff oldVNodes.body newVNodes.body)
          { doc, parent: parents.body }
  headVdom <-
    fst
      <$> runWriterT
          ( runReaderT
              (vDomDiff oldVNodes.head $ makeStyleNode styleMap : newVNodes.head)
              { doc, parent: parents.head }
          )
  pure
    $ { head: headVdom
      , body: bodyVdom
      }
    /\ subs

makeStyleNode :: ∀ msg. Map String String -> SingleVNode msg
makeStyleNode styleMap =
  KeyedElement
    { tag: "style"
    , styles: Nil
    , attributes: Nil
    , children:
        foldrWithIndex
          ( \k v acc ->
              ( k
                  /\ ( VElement
                        { tag: "style"
                        , styles: Nil
                        , attributes: Nil
                        , children: pure $ VText { text: v, node: Nothing }
                        , node: Nothing
                        } ::
                        SingleVNode msg
                    )
              )
                : acc
          )
          Nil
          styleMap
    , node: Nothing
    }

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
        Diff.Both svn1 svn2 -> do
          Just <$> diffSingle svn1 svn2
    )

processStyles :: ∀ msg. SingleVNode msg -> MyMonadCommon msg
processStyles svn = case svn of
  VElement r -> VElement <$> helper r
  KeyedElement r -> KeyedElement <$> helper r
  VText _ -> pure svn
  where
  helper ::
    ∀ r.
    { styles :: List Style
    , attributes :: List (SingleAttribute msg)
    | r
    } ->
    MyMonad msg
      { styles :: List Style
      , attributes :: List (SingleAttribute msg)
      | r
      }
  helper props@{ styles, attributes } = do
    let
      mstyles = VC.process styles
    case mstyles of
      Just r -> do
        tell $ mempty /\ Map.singleton r.class r.css
        pure $ props { attributes = AddClass r.class : attributes }
      Nothing -> pure props

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
          else if key2 == fst head1' then
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
diffSingle svn1 svn2 = do
  styledSvn2 <- processStyles svn2
  let
    replace :: MyMonadCommon msg
    replace = case getNode svn1 of
      Just svn1Node -> do
        svn <- replaceNode svn1Node styledSvn2
        pure svn
      Nothing -> pure styledSvn2
  case svn1, styledSvn2 of
    VElement r1, VElement r2 ->
      if r1.tag == r2.tag then
        fromMaybe replace do
          elem <- r1.node
          Just do
            attributes <- lift $ diffAttributes elem r1.attributes r2.attributes
            children <- local (changeParent elem) $ vDomDiff r1.children r2.children
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
          elem <- r1.node
          Just do
            attributes <- lift $ diffAttributes elem r1.attributes r2.attributes
            children <- local (changeParent elem) $ keyedDiff r1.children r2.children
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

diffAttributes ::
  ∀ msg.
  Element ->
  List (SingleAttribute msg) ->
  List (SingleAttribute msg) ->
  WriterT (Sub msg /\ Map String String) Effect (List (SingleAttribute msg))
diffAttributes elem =
  diff
    ( case _ of
        Diff.Left sa -> do
          lift $ removeAttribute sa elem
          pure Nothing
        Diff.Right sa -> do
          addAttribute sa elem
          pure $ Just sa
        Diff.Both sa1 sa2 ->
          let
            switch = do
              lift $ removeAttribute sa1 elem
              addAttribute sa2 elem
              pure $ Just sa2
          in
            case sa1, sa2 of
              Attr prop1 value1, Attr prop2 value2 ->
                if prop1 == prop2 then
                  if value1 == value2 then
                    pure $ Just sa1
                  else do
                    addAttribute sa2 elem
                    pure $ Just sa2
                else do
                  switch
              AddClass c1, AddClass c2 ->
                if c1 == c2 then
                  pure $ Just sa1
                else
                  switch
              Prop prop1 value1, Prop prop2 value2 ->
                if prop1 == prop2 then
                  if value1 == value2 then
                    pure $ Just sa1
                  else do
                    addAttribute sa2 elem
                    pure $ Just sa2
                else do
                  switch
              _, _ -> do
                switch
    )

removeAttribute :: ∀ msg. SingleAttribute msg -> Element -> Effect Unit
removeAttribute attr elem = case attr of
  Attr prop _ -> H.removeAttribute prop elem
  Prop prop _ -> setProperty prop (toJSValue null) elem
  AddClass c -> H.classList elem >>= H.remove [ c ]
  Listener _ -> pure unit

addAttribute :: ∀ msg. SingleAttribute msg -> Element -> WriterT (Sub msg /\ Map String String) Effect Unit
addAttribute attr elem = case attr of
  Attr prop value -> lift $ H.setAttribute prop value elem
  Prop prop value -> lift $ setProperty prop value elem
  AddClass c -> lift $ H.classList elem >>= H.add [ c ]
  Listener toSub -> tell $ toSub (H.toEventTarget elem) /\ mempty

getNode :: ∀ msg. SingleVNode msg -> Maybe Node
getNode = case _ of
  VElement r -> H.toNode <$> r.node
  KeyedElement r -> H.toNode <$> r.node
  VText r -> H.toNode <$> r.node

addNode :: ∀ msg. SingleVNode msg -> MyMonadCommon msg
addNode svn = svn # placeNode (\{ node, parent } -> H.appendChild node parent)

addKeyedNode :: ∀ msg. String /\ SingleVNode msg -> MyMonad msg (String /\ SingleVNode msg)
addKeyedNode (key /\ vnode) = do
  addedVNode <- addNode vnode
  pure $ key /\ addedVNode

replaceNode :: ∀ msg. Node -> SingleVNode msg -> MyMonad msg (SingleVNode msg)
replaceNode oldNode = placeNode (\{ node, parent } -> H.replaceChild node oldNode parent)

placeNode ::
  ∀ a msg.
  ( { node :: Node
    , parent :: Element
    } ->
    Effect a
  ) ->
  SingleVNode msg -> MyMonadCommon msg
placeNode placer svn = do
  styledNode <- processStyles svn
  { doc, parent } <- ask
  case styledNode of
    VElement r -> do
      newChildren /\ node <- placeNodeHelper placer r addNode
      pure $ VElement
        $ r
            { children = newChildren
            , node = Just node
            }
    KeyedElement r -> do
      newChildren /\ node <- placeNodeHelper placer r addKeyedNode
      pure $ KeyedElement
        $ r
            { children = newChildren
            , node = Just node
            }
    VText { text } -> do
      node <- liftEffect $ H.createTextNode text doc
      _ <- liftEffect $ placer { node: H.toNode node, parent }
      pure (VText { text, node: Just node })

placeNodeHelper ::
  ∀ a msg r children.
  ( { node :: Node
    , parent :: Element
    } ->
    Effect a
  ) ->
  { tag :: String
  , styles :: List Style
  , attributes :: List (SingleAttribute msg)
  , children :: List children
  | r
  } ->
  (children -> MyMonad msg children) ->
  MyMonad msg (List children /\ Element)
placeNodeHelper placer { tag, styles, attributes, children } traverser = do
  { doc, parent } <- ask
  elem <- liftEffect $ H.createElement tag {} doc
  subs <- liftEffect $ setAttributes attributes elem
  tell $ subs /\ mempty
  _ <- liftEffect $ placer { node: H.toNode elem, parent }
  newChildren <- traverse (local (changeParent elem) <. traverser) children
  pure $ newChildren /\ elem

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
setAttributes attribute elem = foldM go mempty attribute
  where
  go :: Sub msg -> SingleAttribute msg -> Effect (Sub msg)
  go acc = case _ of
    Attr prop value -> do
      H.setAttribute prop value elem
      pure acc
    Prop prop value -> do
      setProperty prop value elem
      pure acc
    AddClass c -> do
      H.classList elem >>= H.add [ c ]
      pure acc
    Listener toSub -> pure $ acc <> toSub (H.toEventTarget elem)
