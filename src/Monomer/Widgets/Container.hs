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

import Codec.Serialise
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
  -> Int
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetEnv s e

type ContainerInitHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerMergeChildrenReqHandler s e a
  = WidgetEnv s e
  -> a
  -> WidgetNode s e
  -> WidgetNode s e
  -> Bool

type ContainerMergeHandler s e a
  = WidgetEnv s e
  -> a
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerMergePostHandler s e a
  = WidgetEnv s e
  -> WidgetResult s e
  -> a
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerRestoreHandler s e a
  = WidgetEnv s e
  -> a
  -> WidgetNodeInfo
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerRestorePostHandler s e a
  = WidgetEnv s e
  -> WidgetResult s e
  -> a
  -> WidgetNodeInfo
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerDisposeHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerFindNextFocusHandler s e
  = WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetNode s e
  -> Seq (WidgetNode s e)

type ContainerFindByPointHandler s e
  = WidgetEnv s e
  -> Path
  -> Point
  -> WidgetNode s e
  -> Maybe Int

type ContainerEventHandler s e
  = WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)

type ContainerMessageHandler s e
  = forall i . Typeable i
  => WidgetEnv s e
  -> Path
  -> i
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)

type ContainerGetSizeReqHandler s e a
  = WidgetEnv s e
  -> a
  -> WidgetNode s e
  -> Seq (WidgetNode s e)
  -> (SizeReq, SizeReq)

type ContainerResizeHandler s e
  = WidgetEnv s e
  -> Rect
  -> Rect
  -> Seq (WidgetNode s e)
  -> WidgetNode s e
  -> (WidgetResult s e, Seq (Rect, Rect))

type ContainerRenderHandler s e
  = Renderer
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()

data Container s e a = Container {
  containerIgnoreEmptyArea :: Bool,
  containerResizeRequired :: Bool,
  containerStyleChangeCfg :: StyleChangeCfg,
  containerUseCustomSize :: Bool,
  containerUseChildrenSizes :: Bool,
  containerUseScissor :: Bool,
  containerGetBaseStyle :: ContainerGetBaseStyle s e,
  containerGetActiveStyle :: ContainerGetActiveStyle s e,
  containerUpdateCWenv :: ContainerUpdateCWenvHandler s e,
  containerInit :: ContainerInitHandler s e,
  containerMergeChildrenReq :: ContainerMergeChildrenReqHandler s e a,
  containerMerge :: Maybe (ContainerMergeHandler s e a),
  containerMergePost :: ContainerMergePostHandler s e a,
  containerRestore :: ContainerRestoreHandler s e a,
  containerRestorePost :: ContainerRestorePostHandler s e a,
  containerDispose :: ContainerDisposeHandler s e,
  containerFindNextFocus :: ContainerFindNextFocusHandler s e,
  containerFindByPoint :: ContainerFindByPointHandler s e,
  containerHandleEvent :: ContainerEventHandler s e,
  containerHandleMessage :: ContainerMessageHandler s e,
  containerGetSizeReq :: ContainerGetSizeReqHandler s e a,
  containerResize :: ContainerResizeHandler s e,
  containerRender :: ContainerRenderHandler s e,
  containerRenderAfter :: ContainerRenderHandler s e
}

instance Default (Container s e a) where
  def = Container {
    containerIgnoreEmptyArea = False,
    containerResizeRequired = True,
    containerStyleChangeCfg = def,
    containerUseCustomSize = False,
    containerUseChildrenSizes = False,
    containerUseScissor = True,
    containerGetBaseStyle = defaultGetBaseStyle,
    containerGetActiveStyle = defaultGetActiveStyle,
    containerUpdateCWenv = defaultUpdateCWenv,
    containerInit = defaultInit,
    containerMergeChildrenReq = defaultMergeRequired,
    containerMerge = Nothing,
    containerMergePost = defaultMergePost,
    containerRestore = defaultRestore,
    containerRestorePost = defaultRestorePost,
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
  :: (Typeable a, Serialise a)
  => a
  -> Container s e a
  -> Widget s e
createContainer state container = Widget {
  widgetInit = initWrapper container,
  widgetMerge = mergeWrapper container,
  widgetDispose = disposeWrapper container,
  widgetGetState = makeState state,
  widgetRestore = restoreWrapper container,
  widgetSave = saveWrapper container,
  widgetFindNextFocus = findNextFocusWrapper container,
  widgetFindByPoint = findByPointWrapper container,
  widgetHandleEvent = handleEventWrapper container,
  widgetHandleMessage = handleMessageWrapper container,
  widgetResize = resizeWrapper container,
  widgetRender = renderWrapper container
}

-- | Get base style for component
defaultGetBaseStyle :: ContainerGetBaseStyle s e
defaultGetBaseStyle wenv node = Nothing

defaultGetActiveStyle :: ContainerGetActiveStyle s e
defaultGetActiveStyle wenv node = activeStyle wenv node

defaultUpdateCWenv :: ContainerUpdateCWenvHandler s e
defaultUpdateCWenv wenv cidx cnode node = wenv

-- | Init handler
defaultInit :: ContainerInitHandler s e
defaultInit wenv node = resultWidget node

initWrapper
  :: Typeable a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
initWrapper container wenv node = result where
  initHandler = containerInit container
  getBaseStyle = containerGetBaseStyle container
  updateCWenv = containerUpdateCWenv container
  styledNode = initNodeStyle getBaseStyle wenv node
  WidgetResult tempNode reqs events = initHandler wenv styledNode
  children = tempNode ^. L.children
  initChild idx child = widgetInit newWidget cwenv newChild where
    newChild = cascadeCtx wenv tempNode child idx
    cwenv = updateCWenv wenv idx newChild node
    newWidget = newChild ^. L.widget
  results = Seq.mapWithIndex initChild children
  newReqs = foldMap _wrRequests results
  newEvents = foldMap _wrEvents results
  newChildren = fmap _wrNode results
  newNode = updateSizeReq container wenv $ tempNode
    & L.children .~ newChildren
  result = WidgetResult newNode (reqs <> newReqs) (events <> newEvents)

defaultGetState
  :: (Typeable a, Serialise a)
  => a
  -> WidgetEnv s e
  -> Maybe WidgetState
defaultGetState state wenv = Just (WidgetState state)

-- | Merging
defaultMerge :: ContainerMergeHandler s e a
defaultMerge wenv oldState oldNode newNode = resultWidget newNode

defaultMergeRequired :: ContainerMergeChildrenReqHandler s e a
defaultMergeRequired wenv oldState oldNode newNode = True

defaultMergePost :: ContainerMergePostHandler s e a
defaultMergePost wenv result oldState oldNode node = result

mergeWrapper
  :: (Typeable a, Serialise a)
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeWrapper container wenv oldNode newNode = newResult where
  getBaseStyle = containerGetBaseStyle container
  updateCWenv = containerUpdateCWenv container
  cWenvHelper idx child = updateCWenv wenv idx child newNode
  mergeRequiredHandler = containerMergeChildrenReq container
  mergeHandler = case containerMerge container of
    Just handler -> handler
    Nothing -> mergeWithRestore (containerRestore container)
  mergePostHandler = containerMergePost container

  mergeRequired = case useState oldState of
    Just state -> mergeRequiredHandler wenv state oldNode newNode
    Nothing -> True
  oldFlags = [oldNode ^. L.info . L.visible, oldNode ^. L.info . L.enabled]
  newFlags = [newNode ^. L.info . L.visible, newNode ^. L.info . L.enabled]
  oldState = widgetGetState (oldNode ^. L.widget) wenv
  styledNode = initNodeStyle getBaseStyle wenv newNode
  pResult = mergeParent mergeHandler wenv oldState oldNode styledNode
  cResult = mergeChildren cWenvHelper wenv oldNode newNode pResult
  vResult = mergeChildrenCheckVisible oldNode cResult
  mResult
    | mergeRequired || oldFlags /= newFlags = vResult
    | otherwise = pResult & L.node . L.children .~ oldNode ^. L.children
  postRes = case useState oldState of
    Just state -> mergePostHandler wenv mResult state oldNode (mResult^.L.node)
    Nothing -> resultWidget (mResult ^. L.node)
  tmpResult
    | isResizeResult (Just postRes) = postRes
        & L.node .~ updateSizeReq container wenv (postRes ^. L.node)
    | otherwise = postRes
  newResult = handleWidgetIdChange oldNode tmpResult

mergeParent
  :: (Typeable a, Serialise a)
  => ContainerMergeHandler s e a
  -> WidgetEnv s e
  -> Maybe WidgetState
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeParent mergeHandler wenv oldState oldNode newNode = result where
  oldInfo = oldNode ^. L.info
  tempNode = newNode
    & L.info . L.widgetId .~ oldInfo ^. L.widgetId
    & L.info . L.viewport .~ oldInfo ^. L.viewport
    & L.info . L.renderArea .~ oldInfo ^. L.renderArea
    & L.info . L.sizeReqW .~ oldInfo ^. L.sizeReqW
    & L.info . L.sizeReqH .~ oldInfo ^. L.sizeReqH
  result = case useState oldState of
    Just state -> mergeHandler wenv state oldNode tempNode
    Nothing -> resultWidget tempNode

mergeChildren
  :: (Int -> WidgetNode s e -> WidgetEnv s e)
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
  -> WidgetResult s e
mergeChildren updateCWenv wenv oldNode newNode result = newResult where
  WidgetResult uNode uReqs uEvents = result
  oldChildren = oldNode ^. L.children
  oldCsIdx = Seq.mapWithIndex (,) oldChildren
  updatedChildren = uNode ^. L.children
  mergeChild idx child = (idx, cascadeCtx wenv uNode child idx)
  newCsIdx = Seq.mapWithIndex mergeChild updatedChildren
  localKeys = buildLocalMap oldChildren
  mpairs = mergeChildrenSeq updateCWenv wenv localKeys newNode oldCsIdx newCsIdx
  (mergedResults, removedResults) = mpairs
  mergedChildren = fmap _wrNode mergedResults
  mergedReqs = foldMap _wrRequests mergedResults
  mergedEvents = foldMap _wrEvents mergedResults
  removedReqs = foldMap _wrRequests removedResults
  removedEvents = foldMap _wrEvents removedResults
  mergedNode = uNode & L.children .~ mergedChildren
  newReqs = uReqs <> mergedReqs <> removedReqs
  newEvents = uEvents <> mergedEvents <> removedEvents
  newResult = WidgetResult mergedNode newReqs newEvents

mergeChildrenSeq
  :: (Int -> WidgetNode s e -> WidgetEnv s e)
  -> WidgetEnv s e
  -> Map WidgetKey (WidgetNode s e)
  -> WidgetNode s e
  -> Seq (Int, WidgetNode s e)
  -> Seq (Int, WidgetNode s e)
  -> (Seq (WidgetResult s e), Seq (WidgetResult s e))
mergeChildrenSeq updateCWenv wenv localKeys newNode oldItems Empty = res where
  dispose (idx, child) = widgetDispose (child ^. L.widget) wenv child
  removed = fmap dispose oldItems
  res = (Empty, removed)
mergeChildrenSeq updateCWenv wenv localKeys newNode Empty newItems = res where
  init (idx, child) = widgetInit (child ^. L.widget) wenv child
  merged = fmap init newItems
  res = (merged, Empty)
mergeChildrenSeq updateCWenv wenv localKeys newNode oldItems newItems = res where
  (_, oldChild) :<| oldChildren = oldItems
  (newIdx, newChild) :<| newChildren = newItems
  globalKeys = wenv ^. L.globalKeys
  newWidget = newChild ^. L.widget
  newChildKey = newChild ^. L.info . L.key
  oldKeyMatch = newChildKey >>= \key -> findWidgetByKey key localKeys globalKeys
  oldMatch = fromJust oldKeyMatch
  cwenv = updateCWenv newIdx newChild
  mergedOld = widgetMerge newWidget cwenv oldChild newChild
  mergedKey = widgetMerge newWidget cwenv oldMatch newChild
  initNew = widgetInit newWidget cwenv newChild
    & L.requests %~ (|> ResizeWidgets)
  isMergeKey = isJust oldKeyMatch && nodeMatches newChild oldMatch
  (child, oldRest)
    | nodeMatches newChild oldChild = (mergedOld, oldChildren)
    | isMergeKey = (mergedKey, oldItems)
    | otherwise = (initNew, oldItems)
  (cmerged, cremoved)
    = mergeChildrenSeq updateCWenv wenv localKeys newNode oldRest newChildren
  merged = child <| cmerged
  res = (merged, cremoved)

mergeChildrenCheckVisible
  :: WidgetNode s e
  -> WidgetResult s e
  -> WidgetResult s e
mergeChildrenCheckVisible oldNode result = newResult where
  newNode = result ^. L.node
  resizeRequired = visibleChildrenChanged oldNode newNode
  newResult
    | resizeRequired = result & L.requests %~ (|> ResizeWidgets)
    | otherwise = result

mergeWithRestore
  :: ContainerRestoreHandler s e a
  -> WidgetEnv s e
  -> a
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeWithRestore restore wenv oldState oldNode newNode = result where
  info = oldNode ^. L.info
  result = restore wenv oldState info newNode

saveWrapper
  :: (Typeable a, Serialise a)
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetInstanceNode
saveWrapper container wenv node = instNode where
  updateCWenv = containerUpdateCWenv container
  instNode = WidgetInstanceNode {
    _winInfo = node ^. L.info,
    _winState = widgetGetState (node ^. L.widget) wenv,
    _winChildren = Seq.mapWithIndex saveChild (node ^. L.children)
  }
  saveChild idx child = widgetSave (child ^. L.widget) cwenv child where
    cwenv = updateCWenv wenv idx child node

defaultRestore :: ContainerRestoreHandler s e a
defaultRestore wenv oldState oldInfo newNode = resultWidget newNode

defaultRestorePost :: ContainerRestorePostHandler s e a
defaultRestorePost wenv result oldState oldNode node = result

restoreWrapper
  :: (Typeable a, Serialise a)
  => Container s e a
  -> WidgetEnv s e
  -> WidgetInstanceNode
  -> WidgetNode s e
  -> WidgetResult s e
restoreWrapper container wenv win newNode = newResult where
  getBaseStyle = containerGetBaseStyle container
  updateCWenv = containerUpdateCWenv container
  restoreHandler = containerRestore container
  restorePostHandler = containerRestorePost container
  oldInfo = win ^. L.info
  tempNode = newNode
    & L.info . L.widgetId .~ oldInfo ^. L.widgetId
    & L.info . L.viewport .~ oldInfo ^. L.viewport
    & L.info . L.renderArea .~ oldInfo ^. L.renderArea
    & L.info . L.sizeReqW .~ oldInfo ^. L.sizeReqW
    & L.info . L.sizeReqH .~ oldInfo ^. L.sizeReqH
  styledNode = initNodeStyle getBaseStyle wenv tempNode
  WidgetResult pNode pReqs pEvts = case loadState (win ^. L.state) of
    Just state -> restoreHandler wenv state oldInfo styledNode
    _ -> resultWidget styledNode
  -- Process children
  childrenPairs = Seq.zip (win ^. L.children) (pNode ^. L.children)
  restoreChild idx (inst, child) = rchild where
    cnode = cascadeCtx wenv pNode child idx
    cwenv = updateCWenv wenv idx cnode pNode
    rchild = widgetRestore (cnode ^. L.widget) cwenv inst cnode
  restoredChildren = Seq.mapWithIndex restoreChild childrenPairs
  -- Join results
  newChildren = fmap _wrNode restoredChildren
  newParent = pNode
    & L.children .~ newChildren
  cReqs = foldMap _wrRequests restoredChildren
  cEvts = foldMap _wrEvents restoredChildren
  tmpRes = WidgetResult newParent (pReqs <> cReqs) (pEvts <> cEvts)
  postRes = case loadState (win ^. L.state) of
    Just state -> restorePostHandler wenv tmpRes state oldInfo newParent
    Nothing -> resultWidget newParent
  valid = infoMatches (win ^. L.info) (newNode ^. L.info)
  message = matchFailedMsg (win ^. L.info) (newNode ^. L.info)
  newResult
    | not valid = throw (AssertionFailed $ "Restore failed. " ++ message)
    | isResizeResult (Just postRes) = postRes
        & L.node .~ updateSizeReq container wenv (postRes ^. L.node)
    | otherwise = postRes

-- | Dispose handler
defaultDispose :: ContainerInitHandler s e
defaultDispose wenv node = resultWidget node

disposeWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
disposeWrapper container wenv node = result where
  updateCWenv = containerUpdateCWenv container
  disposeHandler = containerDispose container
  WidgetResult tempNode reqs events = disposeHandler wenv node
  children = tempNode ^. L.children
  dispose idx child = widgetDispose (child ^. L.widget) cwenv child where
    cwenv = updateCWenv wenv idx child node
  results = Seq.mapWithIndex dispose children
  newReqs = foldMap _wrRequests results
  newEvents = foldMap _wrEvents results
  result = WidgetResult node (reqs <> newReqs) (events <> newEvents)

-- | Find next focusable item
defaultFindNextFocus :: ContainerFindNextFocusHandler s e
defaultFindNextFocus wenv direction start node = vchildren where
  vchildren = Seq.filter (^. L.info . L.visible) (node ^. L.children)

findNextFocusWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetNode s e
  -> Maybe Path
findNextFocusWrapper container wenv direction start node = nextFocus where
  handler = containerFindNextFocus container
  handlerResult = handler wenv direction start node
  children
    | direction == FocusBwd = Seq.reverse handlerResult
    | otherwise = handlerResult
  nextFocus
    | isFocusCandidate direction start node = Just path
    | otherwise = findFocusCandidate container wenv direction start node children
    where
      path = node ^. L.info . L.path

findFocusCandidate
  :: Container s e a
  -> WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetNode s e
  -> Seq (WidgetNode s e)
  -> Maybe Path
findFocusCandidate _ _ _ _ _ Empty = Nothing
findFocusCandidate container wenv dir start node (ch :<| chs) = result where
  updateCWenv = containerUpdateCWenv container
  path = node ^. L.info . L.path
  idx = fromMaybe 0 (Seq.lookup (length path - 1) path)
  cwenv = updateCWenv wenv idx ch node
  isWidgetAfterStart
    | dir == FocusBwd = isNodeBeforePath start ch
    | otherwise = isNodeParentOfPath start ch || isNodeAfterPath start ch
  candidate = widgetFindNextFocus (ch ^. L.widget) cwenv dir start ch
  result
    | isWidgetAfterStart && isJust candidate = candidate
    | otherwise = findFocusCandidate container wenv dir start node chs

-- | Find instance matching point
defaultFindByPoint :: ContainerFindByPointHandler s e
defaultFindByPoint wenv startPath point node = result where
  children = node ^. L.children
  pointInWidget wi = wi ^. L.visible && pointInRect point (wi ^. L.viewport)
  result = Seq.findIndexL (pointInWidget . _wnInfo) children

findByPointWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> Path
  -> Point
  -> WidgetNode s e
  -> Maybe Path
findByPointWrapper container wenv start point node = result where
  updateCWenv = containerUpdateCWenv container
  ignoreEmpty = containerIgnoreEmptyArea container
  handler = containerFindByPoint container
  isVisible = node ^. L.info . L.visible
  inVp = isPointInNodeVp point node
  path = node ^. L.info . L.path
  children = node ^. L.children
  newStartPath = Seq.drop 1 start
  childIdx = case newStartPath of
    Empty -> handler wenv start point node
    p :<| ps -> Just p
  validateIdx p
    | Seq.length children > p = Just p
    | otherwise = Nothing
  resultPath = case childIdx >>= validateIdx of
    Just idx -> childPath where
      cwenv = updateCWenv wenv idx child node
      childPath = widgetFindByPoint childWidget cwenv newStartPath point child
      child = Seq.index children idx
      childWidget = child ^. L.widget
    Nothing
      | not ignoreEmpty -> Just $ node ^. L.info . L.path
      | otherwise -> Nothing
  result
    | isVisible && (inVp || resultPath /= Just path) = resultPath
    | otherwise = Nothing

-- | Event Handling
defaultHandleEvent :: ContainerEventHandler s e
defaultHandleEvent wenv target evt node = Nothing

handleEventWrapper
  :: Typeable a
  => Container s e a
  -> WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleEventWrapper container wenv target evt node
  | not (node ^. L.info . L.visible) = Nothing
  | targetReached || not targetValid = pResultStyled
  | otherwise = cResultStyled
  where
    -- Having targetValid = False means the next path step is not in
    -- _wiChildren, but may still be valid in the receiving widget
    -- For example, Composite has its own tree of child widgets with (possibly)
    -- different types for Model and Events, and is candidate for the next step
    style = containerGetActiveStyle container wenv node
    styleCfg = containerStyleChangeCfg container
    updateCWenv = containerUpdateCWenv container
    pHandler = containerHandleEvent container
    targetReached = isTargetReached target node
    targetValid = isTargetValid target node
    childIdx = fromJust $ nextTargetStep target node
    children = node ^. L.children
    child = Seq.index children childIdx
    childWidget = child ^. L.widget
    cwenv = updateCWenv wenv childIdx child node
    -- Event targeted at parent
    pResponse = pHandler wenv target evt node
    pResultStyled = handleStyleChange wenv target style styleCfg node evt
      $ handleSizeReqChange container wenv node (Just evt) pResponse
    -- Event targeted at children
    childrenIgnored = isJust pResponse && ignoreChildren (fromJust pResponse)
    cResponse
      | childrenIgnored || not (child ^. L.info . L.enabled) = Nothing
      | otherwise = widgetHandleEvent childWidget cwenv target evt child
    cResult = mergeParentChildEvts node pResponse cResponse childIdx
    cResultStyled = handleStyleChange wenv target style styleCfg node evt
      $ handleSizeReqChange container wenv node (Just evt) cResult

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
  | otherwise = Just $ WidgetResult newWidget requests events
  where
    pWidget = _wrNode pResponse
    cWidget = _wrNode cResponse
    requests = _wrRequests pResponse <> _wrRequests cResponse
    events = _wrEvents pResponse <> _wrEvents cResponse
    newWidget = replaceChild pWidget cWidget idx
    newChildResponse = cResponse {
      _wrNode = replaceChild original (_wrNode cResponse) idx
    }

-- | Message Handling
defaultHandleMessage :: ContainerMessageHandler s e
defaultHandleMessage wenv ctx message node = Nothing

handleMessageWrapper
  :: (Typeable a, Typeable i)
  => Container s e a
  -> WidgetEnv s e
  -> Path
  -> i
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleMessageWrapper container wenv target arg node
  | not targetReached && not targetValid = Nothing
  | otherwise = handleSizeReqChange container wenv node Nothing result
  where
    updateCWenv = containerUpdateCWenv container
    handler = containerHandleMessage container
    targetReached = isTargetReached target node
    targetValid = isTargetValid target node
    childIdx = fromJust $ nextTargetStep target node
    children = node ^. L.children
    child = Seq.index children childIdx
    cwenv = updateCWenv wenv childIdx child node
    message = widgetHandleMessage (child ^. L.widget) cwenv target arg child
    messageResult = updateChild <$> message
    updateChild cr = cr {
      _wrNode = replaceChild node (_wrNode cr) childIdx
    }
    result
      | targetReached = handler wenv target arg node
      | otherwise = messageResult

-- | Preferred size
defaultGetSizeReq :: ContainerGetSizeReqHandler s e a
defaultGetSizeReq wenv node children = def

updateSizeReq
  :: Typeable a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
updateSizeReq container wenv node = newNode where
  psHandler = containerGetSizeReq container
  currState = widgetGetState (node ^. L.widget) wenv
  style = containerGetActiveStyle container wenv node
  children = node ^. L.children
  reqs = case useState currState of
    Just state -> psHandler wenv state node children
    _ -> def
  (newReqW, newReqH) = sizeReqAddStyle style reqs
  newNode = node
    & L.info . L.sizeReqW .~ newReqW
    & L.info . L.sizeReqH .~ newReqH

handleSizeReqChange
  :: Typeable a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Maybe SystemEvent
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
handleSizeReqChange container wenv node evt mResult = result where
  baseResult = fromMaybe (resultWidget node) mResult
  newNode = baseResult ^. L.node
  resizeReq = isResizeResult mResult
  styleChanged = isJust evt && styleStateChanged wenv newNode (fromJust evt)
  result
    | styleChanged || resizeReq = Just $ baseResult
      & L.node .~ updateSizeReq container wenv newNode
    | otherwise = mResult

-- | Resize
defaultResize :: ContainerResizeHandler s e
defaultResize wenv viewport renderArea children node = newSize where
  childrenSizes = Seq.replicate (Seq.length children) def
  newSize = (resultWidget node, childrenSizes)

resizeWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetNode s e
  -> WidgetResult s e
resizeWrapper container wenv viewport renderArea node = result where
  updateCWenv = containerUpdateCWenv container
  resizeRequired = containerResizeRequired container
  useCustomSize = containerUseCustomSize container
  useChildSize = containerUseChildrenSizes container
  handler = containerResize container
  lensVp = L.info . L.viewport
  lensRa = L.info . L.renderArea
  vpChanged = viewport /= node ^. lensVp
  raChanged = renderArea /= node ^. lensRa
  children = node ^. L.children
  (tempRes, assigned) = handler wenv viewport renderArea children node
  resize idx (child, (vp, ra)) = newChildRes where
    cwenv = updateCWenv wenv idx child node
    tempChildRes = widgetResize (child ^. L.widget) cwenv vp ra child
    cvp = tempChildRes ^. L.node . L.info . L.viewport
    cra = tempChildRes ^. L.node . L.info . L.renderArea
    icvp = fromMaybe vp (intersectRects vp cvp)
    icra = fromMaybe ra (intersectRects ra cra)
    newChildRes = tempChildRes
      & L.node . L.info . L.viewport .~ (if useChildSize then icvp else vp)
      & L.node . L.info . L.renderArea .~ (if useChildSize then icra else ra)
  newChildrenRes = Seq.mapWithIndex resize (Seq.zip children assigned)
  newChildren = fmap _wrNode newChildrenRes
  newChildrenReqs = foldMap _wrRequests newChildrenRes
  newChildrenEvts = foldMap _wrEvents newChildrenRes
  newVp
    | useCustomSize = tempRes ^. L.node . lensVp
    | otherwise = viewport
  newRa
    | useCustomSize = tempRes ^. L.node . lensRa
    | otherwise = renderArea
  result
    | resizeRequired || vpChanged || raChanged = tempRes
      & L.node . L.info . L.viewport .~ newVp
      & L.node . L.info . L.renderArea .~ newRa
      & L.node . L.children .~ newChildren
      & L.requests <>~ newChildrenReqs
      & L.events <>~ newChildrenEvts
    | otherwise = resultWidget node

-- | Rendering
defaultRender :: ContainerRenderHandler s e
defaultRender renderer wenv node = return ()

renderWrapper
  :: Container s e a
  -> Renderer
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()
renderWrapper container renderer wenv node =
  drawInScissor renderer useScissor viewport $
    drawStyledAction renderer renderArea style $ \_ -> do
      renderBefore renderer wenv node

      forM_ pairs $ \(idx, child) -> when (isWidgetVisible child viewport) $
        widgetRender (child ^. L.widget) renderer (cwenv idx child) child

      renderAfter renderer wenv node
  where
    style = containerGetActiveStyle container wenv node
    updateCWenv = containerUpdateCWenv container
    useScissor = containerUseScissor container
    renderBefore = containerRender container
    renderAfter = containerRenderAfter container
    children = node ^. L.children
    viewport = node ^. L.info . L.viewport
    renderArea = node ^. L.info . L.renderArea
    pairs = Seq.mapWithIndex (,) children
    cwenv idx child = updateCWenv wenv idx child node

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
  parentVisible = pInfo ^. L.visible
  parentEnabled = pInfo ^. L.enabled
  newPath = parentPath |> idx
  newChild = child
    & L.info . L.widgetId .~ WidgetId (wenv ^. L.timestamp) newPath
    & L.info . L.path .~ newPath
    & L.info . L.visible .~ (cInfo ^. L.visible && parentVisible)
    & L.info . L.enabled .~ (cInfo ^. L.enabled && parentEnabled)

findWidgetByKey
  :: WidgetKey
  -> LocalKeys s e
  -> GlobalKeys s e
  -> Maybe (WidgetNode s e)
findWidgetByKey key localMap globalMap = local <|> global where
  local = M.lookup key localMap
  global = case key of
    WidgetKeyGlobal{} -> M.lookup key globalMap
    _ -> Nothing

buildLocalMap :: Seq (WidgetNode s e) -> Map WidgetKey (WidgetNode s e)
buildLocalMap widgets = newMap where
  addWidget map widget
    | isJust key = M.insert (fromJust key) widget map
    | otherwise = map
    where
      key = widget ^. L.info . L.key
  newMap = foldl' addWidget M.empty widgets
