{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Monomer.Widget.Core where

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.Typeable (Typeable)

import qualified Data.List as L
import qualified Data.Sequence as SQ

import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Common.Tree
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Widget.Internal
import Monomer.Widget.Types

cascadeStyle :: (Monad m) => Style -> WidgetNode s e m -> WidgetNode s e m
cascadeStyle parentStyle (Node (wn@WidgetInstance{..}) children) = newNode where
  newNode = Node (wn { _widgetInstanceStyle = newStyle }) newChildren
  newStyle = Style {
    _fixedWidth = _fixedWidth _widgetInstanceStyle,
    _fixedHeight = _fixedHeight _widgetInstanceStyle,
    _padding = _padding parentStyle <> _padding _widgetInstanceStyle,
    _bgRadius = _bgRadius parentStyle <> _bgRadius _widgetInstanceStyle,
    _bgColor = _bgColor parentStyle <|> _bgColor _widgetInstanceStyle,
    _border = _border parentStyle <> _border _widgetInstanceStyle,
    _textStyle = _textStyle parentStyle <> _textStyle _widgetInstanceStyle
  }
  newChildren = fmap (cascadeStyle newStyle) children

widgetMatches :: (Monad m) => WidgetInstance s e m -> WidgetInstance s e m -> Bool
widgetMatches wn1 wn2 = _widgetType (_widgetInstanceWidget wn1) == _widgetType (_widgetInstanceWidget wn2) && _widgetInstanceKey wn1 == _widgetInstanceKey wn2

mergeTrees :: (Monad m) => s -> WidgetNode s e m -> WidgetNode s e m -> WidgetNode s e m
mergeTrees app node1@(Node candidateInstance candidateChildren) (Node oldInstance oldChildren) = newNode where
  matches = widgetMatches candidateInstance oldInstance
  newNode = if | matches -> Node newInstance newChildren
               | otherwise -> node1
  oldWidget = _widgetInstanceWidget oldInstance
  oldState = _widgetSaveState oldWidget app
  candidateWidget = _widgetInstanceWidget candidateInstance
  newWidget = fromMaybe candidateWidget (_widgetRestoreState candidateWidget app oldState)
  newInstance = candidateInstance { _widgetInstanceWidget = newWidget }
  newChildren = mergedChildren SQ.>< addedChildren
  mergedChildren = fmap mergeChild (SQ.zip candidateChildren oldChildren)
  addedChildren = SQ.drop (SQ.length oldChildren) candidateChildren
  mergeChild = \(c1, c2) -> mergeTrees app c1 c2

handleWidgetEvents :: (Monad m) => s -> Widget s e m -> Rect -> SystemEvent -> Maybe (WidgetEventResult s e m)
handleWidgetEvents app widget viewport systemEvent = _widgetHandleEvent widget app viewport systemEvent

handleChildEvent :: (Monad m) => s -> ChildrenSelector s e m a -> a -> Path -> WidgetNode s e m -> SystemEvent -> ChildEventResult s e m
handleChildEvent app selectorFn selector path (Node widgetInstance children) systemEvent =
  createUpdatedNode widgetInstance children eventsParent eventsChildren
    where
      eventsParent = handleEventsParent app path widgetInstance children systemEvent
      eventsChildren = handleEventsChildren app selectorFn selector path children systemEvent

createUpdatedNode :: (Monad m) => WidgetInstance s e m -> WidgetChildren s e m -> EventsParent s e m -> EventsChildren s e m -> ChildEventResult s e m
createUpdatedNode widgetInstance children (EventsParent{..}) (EventsChildren{..}) = updatedNode where
  updatedNode = case (epIgnoreChildrenEvents, ecIgnoreParentEvents) of
    (True, _) -> ChildEventResult epIgnoreParentEvents epEventRequests epUserEvents epUpdatedNode epNewStates
    (_, True) -> ChildEventResult ecIgnoreParentEvents ecEventRequests ecUserEvents newNodeOldParent ecNewStates
    (_, False) -> ChildEventResult epIgnoreParentEvents (epEventRequests ++ ecEventRequests) (epUserEvents ++ ecUserEvents) newNodeMixed (epNewStates ++ ecNewStates)
  newNodeOldParent = createNewNodeOldParent widgetInstance children ecNodePosition ecUpdatedNode
  newNodeMixed = createNewNodeMixed widgetInstance children ecNodePosition epUpdatedNode ecUpdatedNode

handleEventsParent :: (Monad m) => s -> Path -> WidgetInstance s e m -> WidgetChildren s e m -> SystemEvent -> EventsParent s e m
handleEventsParent app path (wi@WidgetInstance{..}) children systemEvent = case handleWidgetEvents app _widgetInstanceWidget _widgetInstanceRenderArea systemEvent of
  Nothing -> EventsParent False False [] [] Nothing []
  Just (WidgetEventResult er ue nw ns) -> EventsParent {
      epIgnoreChildrenEvents = isJust $ L.find isIgnoreChildrenEvents er,
      epIgnoreParentEvents = isJust $ L.find isIgnoreParentEvents er,
      epEventRequests = fmap (path,) er,
      epUserEvents = ue,
      epUpdatedNode = if isNothing nw
                        then Nothing
                        else Just $ Node (wi { _widgetInstanceWidget = fromJust nw }) children,
      epNewStates = [ns]
    }

handleEventsChildren :: (Monad m) => s -> ChildrenSelector s e m a -> a -> Path -> WidgetChildren s e m -> SystemEvent -> EventsChildren s e m
handleEventsChildren app selectorFn selector path children systemEvent = case selectorFn selector children of
  (_, Nothing) -> EventsChildren False [] [] Nothing [] 0
  (newSelector, Just idx) -> EventsChildren {
      ecIgnoreParentEvents = ipe,
      ecEventRequests = er,
      ecUserEvents = ue,
      ecUpdatedNode = nw,
      ecNewStates = ns,
      ecNodePosition = idx
    }
    where
      (ChildEventResult ipe er ue nw ns) = handleChildEvent app selectorFn newSelector widgetPath (SQ.index children idx) systemEvent
      widgetPath = (idx:path)

replaceChild :: (Monad m) => WidgetInstance s e m -> WidgetChildren s e m -> Int -> WidgetNode s e m -> Maybe (WidgetNode s e m)
replaceChild widgetInstance children childIdx newChild = Just $ Node widgetInstance (SQ.update childIdx newChild children)

createNewNodeOldParent :: (Monad m) => WidgetInstance s e m -> WidgetChildren s e m -> Int -> Maybe (WidgetNode s e m) -> Maybe (WidgetNode s e m)
createNewNodeOldParent widgetInstance children childIdx newChild = case newChild of
  Nothing -> Nothing
  Just tnChild -> replaceChild widgetInstance children childIdx tnChild

createNewNodeMixed :: (Monad m) => WidgetInstance s e m -> WidgetChildren s e m -> Int -> Maybe (WidgetNode s e m) -> Maybe (WidgetNode s e m) -> Maybe (WidgetNode s e m)
createNewNodeMixed oldParentWidgetInstance children childIdx newParent newChild = case (newParent, newChild) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just cn) -> replaceChild oldParentWidgetInstance children childIdx cn
  (Just _, Nothing) -> newParent
  (Just (Node newParentWidgetInstance _), Just newChildTreeNode) -> replaceChild newParentWidgetInstance children childIdx newChildTreeNode

handleEventFromPath :: (Monad m) => s -> Path -> WidgetNode s e m -> SystemEvent -> ChildEventResult s e m
handleEventFromPath app path widgetInstance systemEvent = handleChildEvent app pathSelector path [] widgetInstance systemEvent where
  pathSelector [] _ = ([], Nothing)
  pathSelector (p:ps) children
    | length children > p = (ps, Just p)
    | otherwise = ([], Nothing)

handleEventFromPoint :: (Monad m) => s -> Point -> WidgetNode s e m -> SystemEvent -> ChildEventResult s e m
handleEventFromPoint app cursorPos widgetInstance systemEvent = handleChildEvent app rectSelector cursorPos [] widgetInstance systemEvent where
  rectSelector point children = (point, SQ.lookup 0 inRectList) where
    inRectList = fmap snd $ SQ.filter inNodeRect childrenPair
    inNodeRect = \(Node (WidgetInstance {..}) _, _) -> inRect _widgetInstanceViewport point
    childrenPair = SQ.zip children (SQ.fromList [0..(length children - 1)])

handleCustomCommand :: (Monad m, Typeable i) => s -> Path -> WidgetNode s e m -> i -> ChildEventResult s e m
handleCustomCommand app path treeNode customData = case Monomer.Common.Tree.lookup path treeNode of
  Just (WidgetInstance{ _widgetInstanceWidget = Widget{..}, ..}) ->
    case _widgetHandleCustom app customData of
      Just (WidgetEventResult er ue nw ns) -> ChildEventResult False (fmap (path,) er) ue Nothing [ns]
      Nothing -> ChildEventResult False [] [] Nothing []
  Nothing -> ChildEventResult False [] [] Nothing []

findPathFromPoint :: (Monad m) => Point -> WidgetNode s e m -> Path
findPathFromPoint p widgetInstance = path where
  path = case SQ.lookup 0 inRectList of
    Just (child, idx) -> idx : findPathFromPoint p child
    Nothing -> []
  children = nodeChildren widgetInstance
  inRectList = SQ.filter inNodeRect childrenPair
  inNodeRect = \(Node (WidgetInstance {..}) _, _) -> inRect _widgetInstanceViewport p
  childrenPair = SQ.zip children (SQ.fromList [0..(length children - 1)])

handleRender :: (Monad m) => Renderer m -> s -> Path -> WidgetNode s e m -> Timestamp -> m ()
handleRender renderer app path (Node (widgetInstance@WidgetInstance { _widgetInstanceWidget = Widget{..}, .. }) children) ts = do
  when _widgetInstanceVisible $ do
    renderWidget renderer path RenderNormal $
      _widgetRender renderer app widgetInstance ts

    handleRenderChildren renderer app path children ts

    renderWidget renderer path RenderPost $
      _widgetRenderPost renderer app widgetInstance ts

handleRenderChildren :: (Monad m) => Renderer m -> s -> Path -> WidgetChildren s e m -> Timestamp -> m ()
handleRenderChildren renderer app parentPath children ts = do
  let childrenPair = SQ.zip children (SQ.fromList [0..(length children - 1)])

  mapM_ (\(treeNode, idx) -> handleRender renderer app (idx:parentPath) treeNode ts) childrenPair

renderWidget :: (Monad m) => Renderer m -> Path -> WidgetRenderType -> m () -> m ()
renderWidget renderer path wr renderCalls = do
  beginWidget renderer path wr
  renderCalls
  beginWidget renderer path wr

updateWidgetInstance :: Path -> WidgetNode s e m -> (WidgetInstance s e m -> WidgetInstance s e m) -> Maybe (WidgetNode s e m)
updateWidgetInstance path root updateFn = updateNode path root (\(Node widgetInstance children) -> Node (updateFn widgetInstance) children)

setFocusedStatus :: Path -> Bool -> WidgetNode s e m -> WidgetNode s e m
setFocusedStatus path focused root = case updateWidgetInstance path root updateFn of
    Just newRoot -> newRoot
    Nothing -> root
  where
    updateFn wn@(WidgetInstance {..}) = wn {
      _widgetInstanceFocused = focused
    }

resizeUI :: (Monad m) => Renderer m -> s -> Rect -> WidgetNode s e m -> m (WidgetNode s e m)
resizeUI renderer app assignedRect widgetInstance = do
  preferredSizes <- buildPreferredSizes renderer app True widgetInstance
  resizeNode renderer assignedRect assignedRect preferredSizes widgetInstance

buildPreferredSizes :: (Monad m) => Renderer m -> s -> Bool -> WidgetNode s e m -> m (Tree SizeReq)
buildPreferredSizes renderer app parentVisible (Node (WidgetInstance {..}) children) = do
  let isVisible = parentVisible && _widgetInstanceVisible

  childrenSizes <- mapM (buildPreferredSizes renderer app isVisible) children
  sr <- _widgetPreferredSize _widgetInstanceWidget renderer app _widgetInstanceStyle (seqToList childrenSizes)

  let updatedSr = updateSizeReq sr (_fixedWidth _widgetInstanceStyle) (_fixedHeight _widgetInstanceStyle)

  return $ Node (updatedSr { _srVisible = isVisible}) childrenSizes

updateSizeReq :: SizeReq -> Maybe Double -> Maybe Double -> SizeReq
updateSizeReq sr Nothing Nothing = sr
updateSizeReq sr (Just width) Nothing = sr { _srSize = Size width (_h . _srSize $ sr), _srPolicyWidth = StrictSize }
updateSizeReq sr Nothing (Just height) = sr { _srSize = Size (_w . _srSize $ sr) height, _srPolicyHeight = StrictSize }
updateSizeReq sr (Just width) (Just height) = sr { _srSize = Size width height, _srPolicyWidth = StrictSize, _srPolicyHeight = StrictSize }

resizeNode :: (Monad m) => Renderer m -> Rect -> Rect -> Tree SizeReq -> WidgetNode s e m -> m (WidgetNode s e m)
resizeNode renderer viewport renderArea (Node _ childrenSizes) (Node widgetInstance childrenWns) = do
    newChildren <- mapM childResize childrenPair

    return (Node updatedNode newChildren)
  where
    widget = _widgetInstanceWidget widgetInstance
    style = _widgetInstanceStyle widgetInstance
    (WidgetResizeResult viewports renderAreas newWidget) = case (_widgetResizeChildren widget) viewport renderArea style (seqToList childrenSizes) of
      Nothing -> WidgetResizeResult [] [] Nothing
      Just wrr -> wrr
    updatedNode = widgetInstance {
      _widgetInstanceViewport = viewport,
      _widgetInstanceRenderArea = renderArea,
      _widgetInstanceWidget = fromMaybe widget newWidget
    }
    childrenPair = SQ.zip4 childrenSizes childrenWns (SQ.fromList viewports) (SQ.fromList renderAreas)
    childResize = \(size, node, viewport, renderArea) -> resizeNode renderer viewport renderArea size node
