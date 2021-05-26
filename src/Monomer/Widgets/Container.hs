{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Container (
  module Monomer.Core,
  module Monomer.Core.Combinators,
  module Monomer.Event,
  module Monomer.Graphics,
  module Monomer.Widgets.Util,

  Container(..),
  ContainerGetSizeReqHandler(..),
  ContainerResizeHandler(..),
  createContainer,
  getLayoutDirection,
  getUpdateCWenv,
  updateWenvOffset,
  initWrapper,
  mergeWrapper,
  handleEventWrapper,
  handleMessageWrapper,
  findByPointWrapper,
  findNextFocusWrapper,
  resizeWrapper,
  renderWrapper,
  defaultFindByPoint,
  defaultRender
) where

import Control.Applicative ((<|>))
import Control.Exception (AssertionFailed(..), throw)
import Control.Lens ((&), (^.), (^?), (.~), (%~), (<>~), _Just)
import Control.Monad
import Data.Default
import Data.Foldable (fold, foldl')
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Typeable (Typeable)
import Data.Sequence (Seq(..), (<|), (|>))

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Util

import qualified Monomer.Lens as L

type ContainerGetBaseStyle s e
  = GetBaseStyle s e

type ContainerGetActiveStyle s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> StyleState

type ContainerUpdateCWenvHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> Int
  -> WidgetEnv s e

type ContainerInitHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerInitPostHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
  -> WidgetResult s e

type ContainerMergeChildrenReqHandler s e a
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> a
  -> Bool

type ContainerMergeHandler s e a
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> a
  -> WidgetResult s e

type ContainerMergePostHandler s e a
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> a
  -> WidgetResult s e
  -> WidgetResult s e

type ContainerDisposeHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerFindNextFocusHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> FocusDirection
  -> Path
  -> Seq (WidgetNode s e)

type ContainerFindByPointHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> Point
  -> Maybe Int

type ContainerEventHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> SystemEvent
  -> Maybe (WidgetResult s e)

type ContainerMessageHandler s e
  = forall i . Typeable i
  => WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> i
  -> Maybe (WidgetResult s e)

type ContainerGetSizeReqHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> Seq (WidgetNode s e)
  -> (SizeReq, SizeReq)

type ContainerResizeHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> Rect
  -> Seq (WidgetNode s e)
  -> (WidgetResult s e, Seq Rect)

type ContainerRenderHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> Renderer
  -> IO ()

data Container s e a = Container {
  containerAddStyleReq :: Bool,
  containerChildrenOffset :: Maybe Point,
  containerChildrenScissor :: Maybe Rect,
  containerLayoutDirection :: LayoutDirection,
  containerIgnoreEmptyArea :: Bool,
  containerResizeRequired :: Bool,
  containerUseCustomCursor :: Bool,
  containerUseCustomSize :: Bool,
  containerUseChildrenSizes :: Bool,
  containerUseScissor :: Bool,
  containerGetBaseStyle :: ContainerGetBaseStyle s e,
  containerGetActiveStyle :: ContainerGetActiveStyle s e,
  containerUpdateCWenv :: ContainerUpdateCWenvHandler s e,
  containerInit :: ContainerInitHandler s e,
  containerInitPost :: ContainerInitPostHandler s e,
  containerMergeChildrenReq :: ContainerMergeChildrenReqHandler s e a,
  containerMerge :: ContainerMergeHandler s e a,
  containerMergePost :: ContainerMergePostHandler s e a,
  containerDispose :: ContainerDisposeHandler s e,
  containerFindNextFocus :: ContainerFindNextFocusHandler s e,
  containerFindByPoint :: ContainerFindByPointHandler s e,
  containerHandleEvent :: ContainerEventHandler s e,
  containerHandleMessage :: ContainerMessageHandler s e,
  containerGetSizeReq :: ContainerGetSizeReqHandler s e,
  containerResize :: ContainerResizeHandler s e,
  containerRender :: ContainerRenderHandler s e,
  containerRenderAfter :: ContainerRenderHandler s e
}

instance Default (Container s e a) where
  def = Container {
    containerAddStyleReq = True,
    containerChildrenOffset = Nothing,
    containerChildrenScissor = Nothing,
    containerLayoutDirection = LayoutNone,
    containerIgnoreEmptyArea = False,
    containerResizeRequired = True,
    containerUseCustomCursor = False,
    containerUseCustomSize = False,
    containerUseChildrenSizes = False,
    containerUseScissor = False,
    containerGetBaseStyle = defaultGetBaseStyle,
    containerGetActiveStyle = defaultGetActiveStyle,
    containerUpdateCWenv = defaultUpdateCWenv,
    containerInit = defaultInit,
    containerInitPost = defaultInitPost,
    containerMergeChildrenReq = defaultMergeRequired,
    containerMerge = defaultMerge,
    containerMergePost = defaultMergePost,
    containerDispose = defaultDispose,
    containerFindNextFocus = defaultFindNextFocus,
    containerFindByPoint = defaultFindByPoint,
    containerHandleEvent = defaultHandleEvent,
    containerHandleMessage = defaultHandleMessage,
    containerGetSizeReq = defaultGetSizeReq,
    containerResize = defaultResize,
    containerRender = defaultRender,
    containerRenderAfter = defaultRender
  }

createContainer
  :: WidgetModel a
  => a
  -> Container s e a
  -> Widget s e
createContainer state container = Widget {
  widgetInit = initWrapper container,
  widgetMerge = mergeWrapper container,
  widgetDispose = disposeWrapper container,
  widgetGetState = makeState state,
  widgetGetInstanceTree = getInstanceTreeWrapper container,
  widgetFindNextFocus = findNextFocusWrapper container,
  widgetFindByPoint = findByPointWrapper container,
  widgetFindBranchByPath = containerFindBranchByPath,
  widgetHandleEvent = handleEventWrapper container,
  widgetHandleMessage = handleMessageWrapper container,
  widgetGetSizeReq = getSizeReqWrapper container,
  widgetResize = resizeWrapper container,
  widgetRender = renderWrapper container
}

-- | Get base style for component
defaultGetBaseStyle :: ContainerGetBaseStyle s e
defaultGetBaseStyle wenv node = Nothing

defaultGetActiveStyle :: ContainerGetActiveStyle s e
defaultGetActiveStyle wenv node = activeStyle wenv node

defaultUpdateCWenv :: ContainerUpdateCWenvHandler s e
defaultUpdateCWenv wenv node cnode cidx = wenv

getLayoutDirection :: Bool -> LayoutDirection
getLayoutDirection False = LayoutVertical
getLayoutDirection True = LayoutHorizontal

getUpdateCWenv
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> Int
  -> WidgetEnv s e
getUpdateCWenv container wenv node cnode cidx = newWenv where
  cOffset = containerChildrenOffset container
  updateCWenv = containerUpdateCWenv container
  layoutDirection = containerLayoutDirection container
  offsetWenv wenv
    | isJust cOffset = updateWenvOffset container wenv node
    | otherwise = wenv
  directionWenv = wenv
    & L.layoutDirection .~ layoutDirection
  newWenv = updateCWenv (offsetWenv directionWenv) node cnode cidx

updateWenvOffset
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetEnv s e
updateWenvOffset container wenv node = newWenv where
  cOffset = containerChildrenOffset container
  offset = fromMaybe def cOffset
  accumOffset = addPoint offset (wenv ^. L.offset)
  viewport = node ^. L.info . L.viewport
  updateMain (path, point)
    | isNodeParentOfPath path node = (path, addPoint (negPoint offset) point)
    | otherwise = (path, point)
  newWenv = wenv
    & L.viewport .~ moveRect (negPoint offset) viewport
    & L.inputStatus . L.mousePos %~ addPoint (negPoint offset)
    & L.inputStatus . L.mousePosPrev %~ addPoint (negPoint offset)
    & L.offset %~ addPoint offset
    & L.mainBtnPress %~ fmap updateMain

-- | Init handler
defaultInit :: ContainerInitHandler s e
defaultInit wenv node = resultWidget node

defaultInitPost :: ContainerInitPostHandler s e
defaultInitPost wenv node result = result

initWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
initWrapper container wenv node = initPostHandler wenv node result where
  initHandler = containerInit container
  initPostHandler = containerInitPost container
  getBaseStyle = containerGetBaseStyle container
  updateCWenv = getUpdateCWenv container
  styledNode = initNodeStyle getBaseStyle wenv node
  WidgetResult tempNode reqs = initHandler wenv styledNode
  children = tempNode ^. L.children
  initChild idx child = widgetInit newWidget cwenv newChild where
    newChild = cascadeCtx wenv tempNode child idx
    cwenv = updateCWenv wenv node newChild idx
    newWidget = newChild ^. L.widget
  results = Seq.mapWithIndex initChild children
  newReqs = foldMap _wrRequests results
  newChildren = fmap _wrNode results
  newNode = updateSizeReq wenv $ tempNode
    & L.children .~ newChildren
  result = WidgetResult newNode (reqs <> newReqs)

-- | Merging
defaultMergeRequired :: ContainerMergeChildrenReqHandler s e a
defaultMergeRequired wenv newNode oldNode oldState = True

defaultMerge :: ContainerMergeHandler s e a
defaultMerge wenv newNode oldNode oldState = resultWidget newNode

defaultMergePost :: ContainerMergePostHandler s e a
defaultMergePost wenv newNode oldNode oldState result = result

mergeWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeWrapper container wenv newNode oldNode = newResult where
  getBaseStyle = containerGetBaseStyle container
  updateCWenv = getUpdateCWenv container
  cWenvHelper idx child = updateCWenv wenv newNode child idx
  mergeRequiredHandler = containerMergeChildrenReq container
  mergeHandler = containerMerge container
  mergePostHandler = containerMergePost container
  oldState = widgetGetState (oldNode ^. L.widget) wenv oldNode
  mergeRequired = case useState oldState of
    Just state -> mergeRequiredHandler wenv newNode oldNode state
    Nothing -> True
  styledNode = initNodeStyle getBaseStyle wenv newNode
  pResult = mergeParent mergeHandler wenv styledNode oldNode oldState
  cResult = mergeChildren cWenvHelper wenv newNode oldNode pResult
  vResult = mergeChildrenCheckVisible oldNode cResult
  mResult
    | mergeRequired || nodeFlagsChanged oldNode newNode = vResult
    | otherwise = pResult & L.node . L.children .~ oldNode ^. L.children
  postRes = case useState oldState of
    Just state -> mergePostHandler wenv (mResult^.L.node) oldNode state mResult
    Nothing -> resultWidget (mResult ^. L.node)
  tmpResult
    | isResizeAnyResult (Just postRes) = postRes
        & L.node .~ updateSizeReq wenv (postRes ^. L.node)
    | otherwise = postRes
  newResult = handleWidgetIdChange oldNode tmpResult

mergeParent
  :: WidgetModel a
  => ContainerMergeHandler s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> Maybe WidgetState
  -> WidgetResult s e
mergeParent mergeHandler wenv newNode oldNode oldState = result where
  oldInfo = oldNode ^. L.info
  tempNode = newNode
    & L.info . L.widgetId .~ oldInfo ^. L.widgetId
    & L.info . L.viewport .~ oldInfo ^. L.viewport
    & L.info . L.sizeReqW .~ oldInfo ^. L.sizeReqW
    & L.info . L.sizeReqH .~ oldInfo ^. L.sizeReqH
  result = case useState oldState of
    Just state -> mergeHandler wenv tempNode oldNode state
    Nothing -> resultWidget tempNode

mergeChildren
  :: (Int -> WidgetNode s e -> WidgetEnv s e)
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
  -> WidgetResult s e
mergeChildren updateCWenv wenv newNode oldNode result = newResult where
  WidgetResult uNode uReqs = result
  oldChildren = oldNode ^. L.children
  oldIts = Seq.mapWithIndex (,) oldChildren
  updatedChildren = uNode ^. L.children
  mergeChild idx child = (idx, cascadeCtx wenv uNode child idx)
  newIts = Seq.mapWithIndex mergeChild updatedChildren
  oldKeys = buildLocalMap oldChildren
  newKeys = buildLocalMap (snd <$> newIts)
  mpairs = mergeChildSeq updateCWenv wenv oldKeys newKeys newNode oldIts newIts
  (mergedResults, removedResults) = mpairs
  mergedChildren = fmap _wrNode mergedResults
  mergedReqs = foldMap _wrRequests mergedResults
  removedReqs = foldMap _wrRequests removedResults
  mergedNode = uNode & L.children .~ mergedChildren
  newReqs = uReqs <> mergedReqs <> removedReqs
  newResult = WidgetResult mergedNode newReqs

mergeChildSeq
  :: (Int -> WidgetNode s e -> WidgetEnv s e)
  -> WidgetEnv s e
  -> WidgetKeysMap s e
  -> WidgetKeysMap s e
  -> WidgetNode s e
  -> Seq (Int, WidgetNode s e)
  -> Seq (Int, WidgetNode s e)
  -> (Seq (WidgetResult s e), Seq (WidgetResult s e))
mergeChildSeq updateCWenv wenv oldKeys newKeys newNode oldIts Empty = res where
  dispose (idx, child) = case flip M.member newKeys <$> child^. L.info. L.key of
    Just True -> WidgetResult child Empty
    _ -> widgetDispose (child ^. L.widget) wenv child
  removed = fmap dispose oldIts
  res = (Empty, removed)
mergeChildSeq updateCWenv wenv oldKeys newKeys newNode Empty newIts = res where
  init (idx, child) = widgetInit (child ^. L.widget) wenv child
  merged = fmap init newIts
  res = (merged, Empty)
mergeChildSeq updateCWenv wenv oldKeys newKeys newNode oldIts newIts = res where
  (_, oldChild) :<| oldChildren = oldIts
  (newIdx, newChild) :<| newChildren = newIts
  globalKeys = wenv ^. L.globalKeys
  newWidget = newChild ^. L.widget
  newChildKey = newChild ^. L.info . L.key
  oldKeyMatch = newChildKey >>= \key -> M.lookup key oldKeys
  oldMatch = fromJust oldKeyMatch
  cwenv = updateCWenv newIdx newChild
  mergedOld = widgetMerge newWidget cwenv newChild oldChild
  mergedKey = widgetMerge newWidget cwenv newChild oldMatch
  initNew = widgetInit newWidget cwenv newChild
    & L.requests %~ (|> ResizeWidgets)
  isMergeKey = isJust oldKeyMatch && nodeMatches newChild oldMatch
  (child, oldRest)
    | nodeMatches newChild oldChild = (mergedOld, oldChildren)
    | isMergeKey = (mergedKey, oldIts)
    | otherwise = (initNew, oldIts)
  (cmerged, cremoved)
    = mergeChildSeq updateCWenv wenv oldKeys newKeys newNode oldRest newChildren
  merged = child <| cmerged
  res = (merged, cremoved)

mergeChildrenCheckVisible
  :: WidgetNode s e
  -> WidgetResult s e
  -> WidgetResult s e
mergeChildrenCheckVisible oldNode result = newResult where
  newNode = result ^. L.node
  resizeRequired = childrenVisibleChanged oldNode newNode
  newResult
    | resizeRequired = result & L.requests %~ (|> ResizeWidgets)
    | otherwise = result

getInstanceTreeWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetInstanceNode
getInstanceTreeWrapper container wenv node = instNode where
  updateCWenv = getUpdateCWenv container
  instNode = WidgetInstanceNode {
    _winInfo = node ^. L.info,
    _winState = widgetGetState (node ^. L.widget) wenv node,
    _winChildren = Seq.mapWithIndex getChildTree (node ^. L.children)
  }
  getChildTree idx child = tree where
    cwenv = updateCWenv wenv node child idx
    tree = widgetGetInstanceTree (child ^. L.widget) cwenv child

-- | Dispose handler
defaultDispose :: ContainerInitHandler s e
defaultDispose wenv node = resultWidget node

disposeWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
disposeWrapper container wenv node = result where
  updateCWenv = getUpdateCWenv container
  disposeHandler = containerDispose container
  WidgetResult tempNode reqs = disposeHandler wenv node
  widgetId = node ^. L.info . L.widgetId
  children = tempNode ^. L.children
  dispose idx child = widgetDispose (child ^. L.widget) cwenv child where
    cwenv = updateCWenv wenv node child idx
  results = Seq.mapWithIndex dispose children
  newReqs = foldMap _wrRequests results |> ResetWidgetPath widgetId
  result = WidgetResult node (reqs <> newReqs)

-- | Find next focusable item
defaultFindNextFocus :: ContainerFindNextFocusHandler s e
defaultFindNextFocus wenv node direction start = vchildren where
  vchildren = Seq.filter (^. L.info . L.visible) (node ^. L.children)

findNextFocusWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> FocusDirection
  -> Path
  -> Maybe WidgetNodeInfo
findNextFocusWrapper container wenv node dir start = nextFocus where
  handler = containerFindNextFocus container
  handlerResult = handler wenv node dir start
  children
    | dir == FocusBwd = Seq.reverse handlerResult
    | otherwise = handlerResult
  nextFocus
    | isFocusCandidate dir start node = Just (node ^. L.info)
    | otherwise = findFocusCandidate container wenv dir start node children

findFocusCandidate
  :: Container s e a
  -> WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetNode s e
  -> Seq (WidgetNode s e)
  -> Maybe WidgetNodeInfo
findFocusCandidate _ _ _ _ _ Empty = Nothing
findFocusCandidate container wenv dir start node (ch :<| chs) = result where
  updateCWenv = getUpdateCWenv container
  path = node ^. L.info . L.path
  idx = fromMaybe 0 (Seq.lookup (length path - 1) path)
  cwenv = updateCWenv wenv node ch idx
  isWidgetAfterStart
    | dir == FocusBwd = isNodeBeforePath start ch
    | otherwise = isNodeParentOfPath start ch || isNodeAfterPath start ch
  candidate = widgetFindNextFocus (ch ^. L.widget) cwenv ch dir start
  result
    | isWidgetAfterStart && isJust candidate = candidate
    | otherwise = findFocusCandidate container wenv dir start node chs

-- | Find instance matching point
defaultFindByPoint :: ContainerFindByPointHandler s e
defaultFindByPoint wenv node start point = result where
  children = node ^. L.children
  pointInWidget wi = wi ^. L.visible && pointInRect point (wi ^. L.viewport)
  result = Seq.findIndexL (pointInWidget . _wnInfo) children

findByPointWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> Point
  -> Maybe WidgetNodeInfo
findByPointWrapper container wenv node start point = result where
  offset = fromMaybe def (containerChildrenOffset container)
  updateCWenv = getUpdateCWenv container
  ignoreEmpty = containerIgnoreEmptyArea container
  handler = containerFindByPoint container
  isVisible = node ^. L.info . L.visible
  inVp = isPointInNodeVp point node
  cpoint = addPoint (negPoint offset) point
  path = node ^. L.info . L.path
  children = node ^. L.children
  childIdx = nextTargetStep start node <|> handler wenv node start cpoint
  validateIdx p
    | Seq.length children > p && p >= 0 = Just p
    | otherwise = Nothing
  win = case childIdx >>= validateIdx of
    Just idx -> childWni where
      cwenv = updateCWenv wenv node child idx
      child = Seq.index children idx
      childWidget = child ^. L.widget
      childWni = widgetFindByPoint childWidget cwenv child start cpoint
    Nothing
      | not ignoreEmpty -> Just $ node ^. L.info
      | otherwise -> Nothing
  result
    | isVisible && (inVp || fmap (^. L.path) win /= Just path) = win
    | otherwise = Nothing

containerFindBranchByPath
  :: WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> Seq WidgetNodeInfo
containerFindBranchByPath wenv node path
  | info ^. L.path == path = Seq.singleton info
  | isJust nextStep = info <| childrenInst
  | otherwise = Seq.empty
  where
    children = node ^. L.children
    info = node ^. L.info
    nextStep = nextTargetStep path node
    child = Seq.index children (fromJust nextStep)
    childrenInst = widgetFindBranchByPath (child ^. L.widget) wenv child path

-- | Event Handling
defaultHandleEvent :: ContainerEventHandler s e
defaultHandleEvent wenv node target evt = Nothing

handleEventWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> SystemEvent
  -> Maybe (WidgetResult s e)
handleEventWrapper container wenv node target evt
  | not (node ^. L.info . L.visible) = Nothing
  | targetReached || not targetValid = pResultStyled
  | otherwise = cResultStyled
  where
    -- Having targetValid = False means the next path step is not in
    -- _wiChildren, but may still be valid in the receiving widget
    -- For example, Composite has its own tree of child widgets with (possibly)
    -- different types for Model and Events, and is candidate for the next step
    offset = fromMaybe def (containerChildrenOffset container)
    style = containerGetActiveStyle container wenv node
    handleCursor = not (containerUseCustomCursor container)
    updateCWenv = getUpdateCWenv container
    handler = containerHandleEvent container
    targetReached = isTargetReached target node
    targetValid = isTargetValid target node
    -- Event targeted at parent
    pResponse = handler wenv node target evt
    pResultStyled = handleStyleChange wenv target style handleCursor node evt
      $ handleSizeReqChange container wenv node (Just evt) pResponse
    -- Event targeted at children
    pNode = maybe node (^. L.node) pResponse
    cwenv =  updateCWenv wenv pNode child childIdx
    childIdx = fromJust $ nextTargetStep target pNode
    children = pNode ^. L.children
    child = Seq.index children childIdx
    childWidget = child ^. L.widget
    cevt = translateEvent (negPoint offset) evt
    childrenIgnored = isJust pResponse && ignoreChildren (fromJust pResponse)
    cResponse
      | childrenIgnored || not (child ^. L.info . L.enabled) = Nothing
      | otherwise = widgetHandleEvent childWidget cwenv child target cevt
    cResult = mergeParentChildEvts pNode pResponse cResponse childIdx
    cResultStyled = handleStyleChange cwenv target style handleCursor pNode cevt
      $ handleSizeReqChange container cwenv pNode (Just cevt) cResult

mergeParentChildEvts
  :: WidgetNode s e
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
  -> Int
  -> Maybe (WidgetResult s e)
mergeParentChildEvts _ Nothing Nothing _ = Nothing
mergeParentChildEvts _ pResponse Nothing _ = pResponse
mergeParentChildEvts original Nothing (Just cResponse) idx = Just $ cResponse {
    _wrNode = replaceChild original (_wrNode cResponse) idx
  }
mergeParentChildEvts original (Just pResponse) (Just cResponse) idx
  | ignoreChildren pResponse = Just pResponse
  | ignoreParent cResponse = Just newChildResponse
  | otherwise = Just $ WidgetResult newWidget requests
  where
    pWidget = _wrNode pResponse
    cWidget = _wrNode cResponse
    requests = _wrRequests pResponse <> _wrRequests cResponse
    newWidget = replaceChild pWidget cWidget idx
    newChildResponse = cResponse {
      _wrNode = replaceChild original (_wrNode cResponse) idx
    }

-- | Message Handling
defaultHandleMessage :: ContainerMessageHandler s e
defaultHandleMessage wenv node target message = Nothing

handleMessageWrapper
  :: (WidgetModel a, Typeable i)
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> i
  -> Maybe (WidgetResult s e)
handleMessageWrapper container wenv node target arg
  | not targetReached && not targetValid = Nothing
  | otherwise = handleSizeReqChange container wenv node Nothing result
  where
    updateCWenv = getUpdateCWenv container
    handler = containerHandleMessage container
    targetReached = isTargetReached target node
    targetValid = isTargetValid target node
    childIdx = fromJust $ nextTargetStep target node
    children = node ^. L.children
    child = Seq.index children childIdx
    cwenv = updateCWenv wenv node child childIdx
    message = widgetHandleMessage (child ^. L.widget) cwenv child target arg
    messageResult = updateChild <$> message
    updateChild cr = cr {
      _wrNode = replaceChild node (_wrNode cr) childIdx
    }
    result
      | targetReached = handler wenv node target arg
      | otherwise = messageResult

-- | Preferred size
defaultGetSizeReq :: ContainerGetSizeReqHandler s e
defaultGetSizeReq wenv node children = (newReqW, newReqH) where
  (newReqW, newReqH) = case Seq.lookup 0 children of
    Just child -> (child ^. L.info . L.sizeReqW, child ^. L.info . L.sizeReqH)
    _ -> def

getSizeReqWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> (SizeReq, SizeReq)
getSizeReqWrapper container wenv node = (newReqW, newReqH) where
  addStyleReq = containerAddStyleReq container
  handler = containerGetSizeReq container
  style = containerGetActiveStyle container wenv node
  children = node ^. L.children
  reqs = handler wenv node children
  (tmpReqW, tmpReqH)
    | addStyleReq = sizeReqAddStyle style reqs
    | otherwise = reqs
  -- User settings take precedence
  newReqW = fromMaybe tmpReqW (style ^. L.sizeReqW)
  newReqH = fromMaybe tmpReqH (style ^. L.sizeReqH)

updateSizeReq
  :: WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
updateSizeReq wenv node = newNode where
  (newReqW, newReqH) = widgetGetSizeReq (node ^. L.widget) wenv node
  newNode = node
    & L.info . L.sizeReqW .~ newReqW
    & L.info . L.sizeReqH .~ newReqH

handleSizeReqChange
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Maybe SystemEvent
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
handleSizeReqChange container wenv node evt mResult = result where
  baseResult = fromMaybe (resultWidget node) mResult
  baseNode = baseResult ^. L.node
  resizeReq = isResizeAnyResult mResult
  styleChanged = isJust evt && styleStateChanged wenv baseNode (fromJust evt)
  result
    | styleChanged || resizeReq = Just $ baseResult
      & L.node .~ updateSizeReq wenv baseNode
    | otherwise = mResult

-- | Resize
defaultResize :: ContainerResizeHandler s e
defaultResize wenv node viewport children = resized where
  style = activeStyle wenv node
  contentArea = fromMaybe def (removeOuterBounds style viewport)
  childrenSizes = Seq.replicate (Seq.length children) contentArea
  resized = (resultWidget node, childrenSizes)

resizeWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Rect
  -> WidgetResult s e
resizeWrapper container wenv node viewport = result where
  updateCWenv = getUpdateCWenv container
  resizeRequired = containerResizeRequired container
  useCustomSize = containerUseCustomSize container
  useChildSize = containerUseChildrenSizes container
  handler = containerResize container
  lensVp = L.info . L.viewport
  vpChanged = viewport /= node ^. lensVp
  children = node ^. L.children
  (tempRes, assigned) = handler wenv node viewport children
  resize idx (child, vp) = newChildRes where
    cwenv = updateCWenv wenv node child idx
    tempChildRes = widgetResize (child ^. L.widget) cwenv child vp
    cvp = tempChildRes ^. L.node . L.info . L.viewport
    icvp = fromMaybe vp (intersectRects vp cvp)
    newChildRes = tempChildRes
      & L.node . L.info . L.viewport .~ (if useChildSize then icvp else vp)
  newChildrenRes = Seq.mapWithIndex resize (Seq.zip children assigned)
  newChildren = fmap _wrNode newChildrenRes
  newChildrenReqs = foldMap _wrRequests newChildrenRes
  newVp
    | useCustomSize = tempRes ^. L.node . lensVp
    | otherwise = viewport
  tmpResult
    | resizeRequired || vpChanged = Just $ tempRes
      & L.node . L.info . L.viewport .~ newVp
      & L.node . L.children .~ newChildren
      & L.requests <>~ newChildrenReqs
    | otherwise = Just $ resultWidget node
  result = fromJust $
    handleSizeReqChange container wenv (tempRes ^. L.node) Nothing tmpResult

-- | Rendering
defaultRender :: ContainerRenderHandler s e
defaultRender renderer wenv node = return ()

renderWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Renderer
  -> IO ()
renderWrapper container wenv node renderer =
  drawInScissor renderer useScissor viewport $
    drawStyledAction renderer viewport style $ \_ -> do
      renderBefore wenv node renderer

      drawInScissor renderer useChildrenScissor childrenScissorRect $ do
        when (isJust offset) $ do
          saveContext renderer
          setTranslation renderer (fromJust offset)

        forM_ pairs $ \(idx, child) ->
          when (isWidgetVisible (cwenv child idx) child) $
            widgetRender (child ^. L.widget) (cwenv child idx) child renderer

        when (isJust offset) $
          restoreContext renderer

      -- Outside children scissor
      renderAfter wenv node renderer
  where
    style = containerGetActiveStyle container wenv node
    updateCWenv = getUpdateCWenv container
    useScissor = containerUseScissor container
    childrenScissor = containerChildrenScissor container
    offset = containerChildrenOffset container
    renderBefore = containerRender container
    renderAfter = containerRenderAfter container
    children = node ^. L.children
    viewport = node ^. L.info . L.viewport
    useChildrenScissor = isJust childrenScissor
    childrenScissorRect = fromMaybe def childrenScissor
    pairs = Seq.mapWithIndex (,) children
    cwenv child idx = updateCWenv wenv node child idx

-- | Event Handling Helpers
ignoreChildren :: WidgetResult s e -> Bool
ignoreChildren result = not (Seq.null ignoreReqs) where
  ignoreReqs = Seq.filter isIgnoreChildrenEvents (_wrRequests result)

ignoreParent :: WidgetResult s e -> Bool
ignoreParent result = not (Seq.null ignoreReqs) where
  ignoreReqs = Seq.filter isIgnoreParentEvents (_wrRequests result)

replaceChild
  :: WidgetNode s e -> WidgetNode s e -> Int -> WidgetNode s e
replaceChild parent child idx = parent & L.children .~ newChildren where
  newChildren = Seq.update idx child (parent ^. L.children)

cascadeCtx
  :: WidgetEnv s e -> WidgetNode s e -> WidgetNode s e -> Int -> WidgetNode s e
cascadeCtx wenv parent child idx = newChild where
  pInfo = parent ^. L.info
  cInfo = child ^. L.info
  parentPath = pInfo ^. L.path
  parentOverlay = pInfo ^. L.overlay
  parentVisible = pInfo ^. L.visible
  parentEnabled = pInfo ^. L.enabled
  newPath = parentPath |> idx
  newChild = child
    & L.info . L.widgetId .~ WidgetId (wenv ^. L.timestamp) newPath
    & L.info . L.path .~ newPath
    & L.info . L.overlay .~ (cInfo ^. L.overlay || parentOverlay)
    & L.info . L.visible .~ (cInfo ^. L.visible && parentVisible)
    & L.info . L.enabled .~ (cInfo ^. L.enabled && parentEnabled)

buildLocalMap :: Seq (WidgetNode s e) -> Map WidgetKey (WidgetNode s e)
buildLocalMap widgets = newMap where
  addWidget map widget
    | isJust key = M.insert (fromJust key) widget map
    | otherwise = map
    where
      key = widget ^. L.info . L.key
  newMap = foldl' addWidget M.empty widgets
