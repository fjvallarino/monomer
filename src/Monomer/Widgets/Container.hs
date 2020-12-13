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
  ContainerUpdateCWenvHandler(..),
  ContainerGetSizeReqHandler(..),
  ContainerResizeHandler(..),
  createContainer,
  createThemed,
  initWrapper,
  mergeWrapper,
  mergeParent,
  mergeChildren,
  mergeChildrenCheckVisible,
  handleEventWrapper,
  handleMessageWrapper,
  getSizeReqWrapper,
  findByPointWrapper,
  findNextFocusWrapper,
  resizeWrapper,
  renderWrapper,
  renderContainer,
  defaultFindByPoint,
  defaultRender
) where

import Debug.Trace

import Control.Lens ((&), (^.), (.~), (?~), (%~))
import Control.Monad
import Data.Default
import Data.Foldable (fold)
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Typeable (Typeable)
import Data.Sequence (Seq(..), (<|), (|>), (><))

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

type ContainerMergeHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerMergeChildrenRequiredHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState
  -> WidgetNode s e
  -> WidgetNode s e
  -> Bool

type ContainerMergePostHandler s e
  = WidgetEnv s e
  -> WidgetResult s e
  -> Maybe WidgetState
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerDisposeHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e

type ContainerGetStateHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState

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

type ContainerGetSizeReqHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> Seq (WidgetNode s e)
  -> (SizeReq, SizeReq)

type ContainerResizeHandler s e
  = WidgetEnv s e
  -> Rect
  -> Rect
  -> Seq (WidgetNode s e)
  -> WidgetNode s e
  -> (WidgetNode s e, Seq (Rect, Rect))

type ContainerRenderHandler s e
  =  Renderer
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()

data Container s e = Container {
  containerIgnoreEmptyClick :: Bool,
  containerStyleOnMerge :: Bool,
  containerResizeRequired :: Bool,
  containerKeepChildrenSizes :: Bool,
  containerChildrenOffset :: Maybe Point,
  containerChildrenOffsetFwd :: Bool,
  containerGetBaseStyle :: ContainerGetBaseStyle s e,
  containerUpdateCWenv :: ContainerUpdateCWenvHandler s e,
  containerInit :: ContainerInitHandler s e,
  containerMerge :: ContainerMergeHandler s e,
  containerMergeChildrenRequired :: ContainerMergeChildrenRequiredHandler s e,
  containerMergePost :: ContainerMergePostHandler s e,
  containerDispose :: ContainerDisposeHandler s e,
  containerGetState :: ContainerGetStateHandler s e,
  containerFindNextFocus :: ContainerFindNextFocusHandler s e,
  containerFindByPoint :: ContainerFindByPointHandler s e,
  containerHandleEvent :: ContainerEventHandler s e,
  containerHandleMessage :: ContainerMessageHandler s e,
  containerGetSizeReq :: ContainerGetSizeReqHandler s e,
  containerResize :: ContainerResizeHandler s e,
  containerRender :: ContainerRenderHandler s e,
  containerRenderAfter :: ContainerRenderHandler s e
}

instance Default (Container s e) where
  def = Container {
    containerIgnoreEmptyClick = False,
    containerStyleOnMerge = False,
    containerResizeRequired = True,
    containerKeepChildrenSizes = False,
    containerChildrenOffset = Nothing,
    containerChildrenOffsetFwd = False,
    containerGetBaseStyle = defaultGetBaseStyle,
    containerUpdateCWenv = defaultUpdateCWenv,
    containerInit = defaultInit,
    containerMerge = defaultMerge,
    containerMergeChildrenRequired = defaultMergeRequired,
    containerMergePost = defaultMergePost,
    containerDispose = defaultDispose,
    containerGetState = defaultGetState,
    containerFindNextFocus = defaultFindNextFocus,
    containerFindByPoint = defaultFindByPoint,
    containerHandleEvent = defaultHandleEvent,
    containerHandleMessage = defaultHandleMessage,
    containerGetSizeReq = defaultGetSizeReq,
    containerResize = defaultResize,
    containerRender = defaultRender,
    containerRenderAfter = defaultRender
  }

createContainer :: Container s e -> Widget s e
createContainer container = Widget {
  widgetInit = initWrapper container,
  widgetMerge = mergeWrapper container,
  widgetDispose = disposeWrapper container,
  widgetGetState = containerGetState container,
  widgetGetInstanceTree = getInstanceTree,
  widgetFindNextFocus = findNextFocusWrapper container,
  widgetFindByPoint = findByPointWrapper container,
  widgetHandleEvent = handleEventWrapper container,
  widgetHandleMessage = handleMessageWrapper container,
  widgetGetSizeReq = getSizeReqWrapper container,
  widgetResize = resizeWrapper container,
  widgetRender = renderWrapper container
}

createThemed
  :: WidgetType
  -> (WidgetEnv s e -> WidgetNode s e)
  -> WidgetNode s e
createThemed widgetType factory = newNode where
  createNode wenv node = resultWidget themedNode where
    info = node ^. L.info
    tempNode = factory wenv
    tempInfo = tempNode ^. L.info
    themedNode = node
      & L.widget .~ tempNode ^. L.widget
      & L.children .~ tempNode ^. L.children
      & L.info . L.focusable .~ tempInfo ^. L.focusable
      & L.info . L.style .~ (tempInfo ^. L.style <> info ^. L.style)
  init = createNode
  merge wenv oldState oldNode newNode = createNode wenv newNode
  newWidget = createContainer def {
    containerInit = init,
    containerMerge = merge
  }
  newNode = defaultWidgetNode widgetType newWidget

-- | Get base style for component
defaultGetBaseStyle :: ContainerGetBaseStyle s e
defaultGetBaseStyle wenv node = Nothing

defaultUpdateCWenv :: ContainerUpdateCWenvHandler s e
defaultUpdateCWenv wenv cidx cnode node = wenv

updateCWenvWrapper
  :: Container s e
  -> WidgetEnv s e
  -> Int
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetEnv s e
updateCWenvWrapper container wenv cidx cnode node = newWenv where
  handler = containerUpdateCWenv container
  cOffset = containerChildrenOffset container
  cOffsetFwd = containerChildrenOffsetFwd container

  tempWenv = translateWenv wenv cOffset cOffsetFwd
  newWenv = handler tempWenv cidx cnode node

-- | Init handler
defaultInit :: ContainerInitHandler s e
defaultInit wenv node = resultWidget node

initWrapper
  :: Container s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
initWrapper container wenv node = result where
  initHandler = containerInit container
  getBaseStyle = containerGetBaseStyle container
  styledNode = initNodeStyle getBaseStyle wenv node
  WidgetResult tempNode reqs events = initHandler wenv styledNode
  children = tempNode ^. L.children
  initChild idx child = widgetInit newWidget cWenv newChild where
    cWenv = updateCWenvWrapper container wenv idx child node
    newChild = cascadeCtx tempNode child idx
    newWidget = newChild ^. L.widget
  results = Seq.mapWithIndex initChild children
  newReqs = foldMap _wrRequests results
  newEvents = foldMap _wrEvents results
  newChildren = fmap _wrWidget results
  newNode = tempNode & L.children .~ newChildren
  result = WidgetResult newNode (reqs <> newReqs) (events <> newEvents)

-- | Merging
defaultMerge :: ContainerMergeHandler s e
defaultMerge wenv oldState oldNode newNode = resultWidget newNode

defaultMergeRequired :: ContainerMergeChildrenRequiredHandler s e
defaultMergeRequired wenv oldState oldNode newNode = True

defaultMergePost :: ContainerMergePostHandler s e
defaultMergePost wenv result oldState oldNode node = result

mergeWrapper
  :: Container s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeWrapper container wenv oldNode newNode = result where
  getBaseStyle = containerGetBaseStyle container
  cwHandler = containerUpdateCWenv container
  mergeRequiredHandler = containerMergeChildrenRequired container
  mergeHandler = containerMerge container
  mergePostHandler = containerMergePost container

  mergeRequired = mergeRequiredHandler wenv oldState oldNode newNode
  oldState = widgetGetState (oldNode ^. L.widget) wenv
  styledNode = initNodeStyle getBaseStyle wenv newNode
  pResult = mergeParent mergeHandler wenv oldState oldNode styledNode
  cResult
    | mergeRequired = mergeChildren cwHandler wenv oldNode newNode pResult
    | otherwise = pResult & L.widget . L.children .~ oldNode ^. L.children
  vResult = mergeChildrenCheckVisible oldNode cResult
  result = mergePostHandler wenv vResult oldState oldNode (vResult ^. L.widget)

mergeParent
  :: ContainerMergeHandler s e
  -> WidgetEnv s e
  -> Maybe WidgetState
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeParent mergeHandler wenv oldState oldNode newNode = result where
  oldInfo = oldNode ^. L.info
  tempNode = newNode
    & L.info . L.viewport .~ oldInfo ^. L.viewport
    & L.info . L.renderArea .~ oldInfo ^. L.renderArea
    & L.info . L.sizeReqW .~ oldInfo ^. L.sizeReqW
    & L.info . L.sizeReqH .~ oldInfo ^. L.sizeReqH
  result = mergeHandler wenv oldState oldNode tempNode

mergeChildren
  :: ContainerUpdateCWenvHandler s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
  -> WidgetResult s e
mergeChildren cwHandler wenv oldNode newNode result = newResult where
  WidgetResult uNode uReqs uEvents = result
  oldChildren = oldNode ^. L.children
  oldCsIdx = Seq.mapWithIndex (,) oldChildren
  updatedChildren = uNode ^. L.children
  mergeChild idx child = (idx, cascadeCtx uNode child idx)
  newCsIdx = Seq.mapWithIndex mergeChild updatedChildren
  localKeys = buildLocalMap oldChildren
  cResult = mergeChildrenSeq cwHandler wenv localKeys newNode oldCsIdx newCsIdx
  (mergedResults, removedResults) = cResult
  mergedChildren = fmap _wrWidget mergedResults
  concatSeq seqs = fold seqs
  mergedReqs = concatSeq $ fmap _wrRequests mergedResults
  mergedEvents = concatSeq $ fmap _wrEvents mergedResults
  removedReqs = concatSeq $ fmap _wrRequests removedResults
  removedEvents = concatSeq $ fmap _wrEvents removedResults
  mergedNode = uNode & L.children .~ mergedChildren
  newReqs = uReqs <> mergedReqs <> removedReqs
  newEvents = uEvents <> mergedEvents <> removedEvents
  newResult = WidgetResult mergedNode newReqs newEvents

mergeChildrenSeq
  :: ContainerUpdateCWenvHandler s e
  -> WidgetEnv s e
  -> Map WidgetKey (WidgetNode s e)
  -> WidgetNode s e
  -> Seq (Int, WidgetNode s e)
  -> Seq (Int, WidgetNode s e)
  -> (Seq (WidgetResult s e), Seq (WidgetResult s e))
mergeChildrenSeq cwHandler wenv localKeys newNode oldItems Empty = res where
  dispose (idx, child) = widgetDispose (child ^. L.widget) cWenv child where
    cWenv = cwHandler wenv idx child newNode
  removed = fmap dispose oldItems
  res = (Empty, removed)
mergeChildrenSeq cwHandler wenv localKeys newNode Empty newItems = res where
  init (idx, child) = widgetInit (child ^. L.widget) cWenv child where
    cWenv = cwHandler wenv idx child newNode
  merged = fmap init newItems
  res = (merged, Empty)
mergeChildrenSeq cwHandler wenv localKeys newNode oldItems newItems = res where
  (_, oldChild) :<| oldChildren = oldItems
  (newIdx, newChild) :<| newChildren = newItems
  cWenvNew = cwHandler wenv newIdx newChild newNode
  globalKeys = wenv ^. L.globalKeys
  newWidget = newChild ^. L.widget
  newChildKey = newChild ^. L.info . L.key
  oldKeyMatch = newChildKey >>= \key -> findWidgetByKey key localKeys globalKeys
  oldMatch = fromJust oldKeyMatch
  mergedOld = widgetMerge newWidget cWenvNew oldChild newChild
  mergedKey = widgetMerge newWidget cWenvNew oldMatch newChild
  initNew = widgetInit newWidget cWenvNew newChild
  isMergeKey = isJust oldKeyMatch && instanceMatches newChild oldMatch
  (child, oldRest)
    | instanceMatches newChild oldChild = (mergedOld, oldChildren)
    | isMergeKey = (mergedKey, oldItems)
    | otherwise = (initNew, oldItems)
  (cmerged, cremoved)
    = mergeChildrenSeq cwHandler wenv localKeys newNode oldRest newChildren
  merged = child <| cmerged
  res = (merged, cremoved)

mergeChildrenCheckVisible
  :: WidgetNode s e
  -> WidgetResult s e
  -> WidgetResult s e
mergeChildrenCheckVisible oldNode result = newResult where
  newNode = result ^. L.widget
  newVisible = fmap (^. L.info . L.visible) (newNode ^. L.children)
  oldVisible = fmap (^. L.info . L.visible) (oldNode ^. L.children)
  resizeRequired = oldVisible /= newVisible
  newResult
    | resizeRequired = result & L.requests %~ (|> ResizeWidgets)
    | otherwise = result

-- | Dispose handler
defaultDispose :: ContainerInitHandler s e
defaultDispose wenv node = resultWidget node

disposeWrapper
  :: Container s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
disposeWrapper container wenv node = result where
  disposeHandler = containerDispose container
  WidgetResult tempNode reqs events = disposeHandler wenv node
  children = tempNode ^. L.children
  dispose idx child = widgetDispose (child ^. L.widget) cWenv child where
    cWenv = updateCWenvWrapper container wenv idx child node
  results = Seq.mapWithIndex dispose children
  newReqs = foldMap _wrRequests results
  newEvents = foldMap _wrEvents results
  result = WidgetResult node (reqs <> newReqs) (events <> newEvents)

-- | State Handling helpers
defaultGetState :: ContainerGetStateHandler s e
defaultGetState _ = Nothing

-- | Find next focusable item
defaultFindNextFocus :: ContainerFindNextFocusHandler s e
defaultFindNextFocus wenv direction start node = vchildren where
  vchildren = Seq.filter (^. L.info . L.visible) (node ^. L.children)

findNextFocusWrapper
  :: Container s e
  -> WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetNode s e
  -> Maybe Path
findNextFocusWrapper container wenv dir start node = nextFocus where
  handler = containerFindNextFocus container
  handlerResult = handler wenv dir start node
  children
    | dir == FocusBwd = Seq.reverse handlerResult
    | otherwise = handlerResult
  nextFocus
    | isFocusCandidate dir start node = Just path
    | otherwise = findFocusCandidate container wenv node children dir start
    where
      path = node ^. L.info . L.path

findFocusCandidate
  :: Container s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Seq (WidgetNode s e)
  -> FocusDirection
  -> Path
  -> Maybe Path
findFocusCandidate _ _ _ Empty _ _ = Nothing
findFocusCandidate container wenv node (ch :<| chs) dir start = result where
  idx = fromMaybe 0 (lastStep ch)
  cWenv = updateCWenvWrapper container wenv idx ch node
  isWidgetAfterStart
    | dir == FocusBwd = isWidgetBeforePath start ch
    | otherwise = isWidgetParentOfPath start ch || isWidgetAfterPath start ch
  candidate = widgetFindNextFocus (ch ^. L.widget) cWenv dir start ch
  result
    | isWidgetAfterStart && isJust candidate = candidate
    | otherwise = findFocusCandidate container wenv node chs dir start

-- | Find instance matching point
defaultFindByPoint :: ContainerFindByPointHandler s e
defaultFindByPoint wenv startPath point node = result where
  children = node ^. L.children
  pointInWidget wi = wi ^. L.visible && pointInRect point (wi ^. L.viewport)
  result = Seq.findIndexL (pointInWidget . _wnInfo) children

--traceShow ("find", node ^. L.info . L.widgetType, result) 

findByPointWrapper
  :: Container s e
  -> WidgetEnv s e
  -> Path
  -> Point
  -> WidgetNode s e
  -> Maybe Path
findByPointWrapper container wenv start point node = result where
  ignoreEmptyClick = containerIgnoreEmptyClick container
  handler = traceShow (node ^. L.info . L.widgetType, point, node ^. L.info . L.viewport) traceShow (currOff ^. L.y, wenv ^. L.offsetAccum) containerFindByPoint container
  cOffset = containerChildrenOffset container
  --cPoint = addPoint point (fromMaybe def cOffset)
  pPoint = addPoint point (fromMaybe def cOffset)
  currOff = fromMaybe def cOffset
  --cPoint = addPoint point (negPoint (fromMaybe def cOffset))
  children = node ^. L.children
  newStartPath = Seq.drop 1 start
  childIdx = case newStartPath of
--    Empty -> traceShowId $ traceShow ("LALALA", node ^. L.info . L.widgetType) $ handler wenv start cPoint node
    Empty -> handler wenv start point node
    p :<| ps -> Just p
  validateIdx p
    | Seq.length children > p = Just p
    | otherwise = Nothing
  resultPath = case childIdx >>= validateIdx of
    Just idx -> childPath where
      cWenv = updateCWenvWrapper container wenv idx child node
      --cPoint = addPoint point (fromMaybe def cOffset)
      cPoint = addPoint point (cWenv ^. L.offset) -- (fromMaybe def cOffset)
      childPath = widgetFindByPoint childWidget cWenv newStartPath cPoint child
      child = Seq.index children idx
      childWidget = child ^. L.widget
    Nothing
      | ignoreEmptyClick -> Nothing
      | otherwise -> Just $ node ^. L.info . L.path
  result
    | node ^. L.info . L.visible = resultPath
    | otherwise = Nothing

-- | Event Handling
defaultHandleEvent :: ContainerEventHandler s e
defaultHandleEvent wenv target evt node = Nothing

handleEventWrapper
  :: Container s e
  -> WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleEventWrapper container wenv target event node
  | not (node ^. L.info . L.visible) = Nothing
  | targetReached || not targetValid = pResultStyled
  | styleOnMerge = cResultStyled
  | otherwise = cResult
  where
    -- Having targetValid = False means the next path step is not in
    -- _wiChildren, but may still be valid in the receiving widget
    -- For example, Composite has its own tree of child widgets with (possibly)
    -- different types for Model and Events, and is candidate for the next step
    style = activeStyle wenv node
    styleOnMerge = containerStyleOnMerge container
    pHandler = containerHandleEvent container
    cOffset = containerChildrenOffset container
    targetReached = isTargetReached target node
    targetValid = isTargetValid target node
    childIdx = fromJust $ nextTargetStep target node
    cWenv = updateCWenvWrapper container wenv childIdx child node
    cEvent = maybe event (translateEvent event) cOffset
    children = node ^. L.children
    child = Seq.index children childIdx
    childWidget = child ^. L.widget
    pResponse = pHandler wenv target event node
    childrenIgnored = isJust pResponse && ignoreChildren (fromJust pResponse)
    cResponse
      | childrenIgnored || not (child ^. L.info . L.enabled) = Nothing
      | otherwise = widgetHandleEvent childWidget cWenv target cEvent child
    pResultStyled = handleStyleChange wenv target event style pResponse node
    cResult = mergeParentChildEvts node pResponse cResponse childIdx
    cResultStyled = handleStyleChange cWenv target cEvent style cResult node

mergeParentChildEvts
  :: WidgetNode s e
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
  -> Int
  -> Maybe (WidgetResult s e)
mergeParentChildEvts _ Nothing Nothing _ = Nothing
mergeParentChildEvts _ pResponse Nothing _ = pResponse
mergeParentChildEvts original Nothing (Just cResponse) idx = Just $ cResponse {
    _wrWidget = replaceChild original (_wrWidget cResponse) idx
  }
mergeParentChildEvts original (Just pResponse) (Just cResponse) idx
  | ignoreChildren pResponse = Just pResponse
  | ignoreParent cResponse = Just newChildResponse
  | otherwise = Just $ WidgetResult newWidget requests userEvents
  where
    pWidget = _wrWidget pResponse
    cWidget = _wrWidget cResponse
    requests = _wrRequests pResponse >< _wrRequests cResponse
    userEvents = _wrEvents pResponse >< _wrEvents cResponse
    newWidget = replaceChild pWidget cWidget idx
    newChildResponse = cResponse {
      _wrWidget = replaceChild original (_wrWidget cResponse) idx
    }

-- | Message Handling
defaultHandleMessage :: ContainerMessageHandler s e
defaultHandleMessage wenv ctx message node = Nothing

handleMessageWrapper
  :: Typeable i
  => Container s e
  -> WidgetEnv s e
  -> Path
  -> i
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleMessageWrapper container wenv target arg node
  | targetReached = mHandler wenv target arg node
  | not targetValid = Nothing
  | otherwise = messageResult
  where
    mHandler = containerHandleMessage container
    targetReached = isTargetReached target node
    targetValid = isTargetValid target node
    childIdx = fromJust $ nextTargetStep target node
    cWenv = updateCWenvWrapper container wenv childIdx child node
    children = node ^. L.children
    child = Seq.index children childIdx
    message = widgetHandleMessage (child ^. L.widget) cWenv target arg child
    messageResult = updateChild <$> message
    updateChild cr = cr
      & L.widget .~ replaceChild node (cr ^. L.widget) childIdx

-- | Preferred size
defaultGetSizeReq :: ContainerGetSizeReqHandler s e
defaultGetSizeReq wenv node children = def

getSizeReqWrapper
  :: Container s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetSizeReq s e
getSizeReqWrapper container wenv node = newSizeReq & L.widget .~ newNode where
  resizeRequired = containerResizeRequired container
  psHandler = containerGetSizeReq container
  style = activeStyle wenv node
  children = node ^. L.children
  updateChild  idx child = newChild where
    cWenv = updateCWenvWrapper container wenv idx child node
    childReq = widgetGetSizeReq (child ^. L.widget) cWenv child
    WidgetSizeReq cWidget cReqW cReqH = childReq
    newChild = cWidget
      & L.info . L.sizeReqW .~ cReqW
      & L.info . L.sizeReqH .~ cReqH
  newChildren = Seq.mapWithIndex updateChild children
  (sizeReqW, sizeReqH) = psHandler wenv node newChildren
  newSizeReq = sizeReqAddStyle style (WidgetSizeReq node sizeReqW sizeReqH)
  newNode
    | resizeRequired = node & L.children .~ newChildren
    | otherwise = node

-- | Resize
defaultResize :: ContainerResizeHandler s e
defaultResize wenv viewport renderArea children node = newSize where
  childrenSizes = Seq.replicate (Seq.length children) def
  newSize = (node, childrenSizes)

resizeWrapper
  :: Container s e
  -> WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetNode s e
  -> WidgetNode s e
resizeWrapper container wenv viewport renderArea node = newNode where
  resizeRequired = containerResizeRequired container
  --vpChanged = viewport /= node ^. L.info . L.viewport
  raChanged = renderArea /= node ^. L.info . L.renderArea
  keepSizes = containerKeepChildrenSizes container
  handler = containerResize container
  children = node ^. L.children
  (tempNode, assigned) = handler wenv viewport renderArea children node
  resize (idx, child, (vp, ra)) = newChildNode where
    cWenv = updateCWenvWrapper container wenv idx child node
    tempChildNode = widgetResize (child ^. L.widget) cWenv vp ra child
    cvp = tempChildNode ^. L.info . L.viewport
    cra = tempChildNode ^. L.info . L.renderArea
    icvp = fromMaybe vp (intersectRects vp cvp)
    icra = fromMaybe ra (intersectRects ra cra)
    newChildNode = tempChildNode
      & L.info . L.viewport .~ (if keepSizes then icvp else vp)
      & L.info . L.renderArea .~ (if keepSizes then icra else ra)
  idxs = Seq.fromList [0..length children]
  newChildren = resize <$> Seq.zip3 idxs children assigned
  newNode
    | resizeRequired || raChanged = tempNode
      & L.info . L.viewport .~ viewport
      & L.info . L.renderArea .~ renderArea
      & L.children .~ newChildren
    | otherwise = node

-- | Rendering
defaultRender :: ContainerRenderHandler s e
defaultRender renderer wenv node = return ()

renderWrapper
  :: Container s e
  -> Renderer
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()
renderWrapper container renderer wenv node = action where
  cOffset = containerChildrenOffset container
  cOffsetFwd = containerChildrenOffsetFwd container
  before = containerRender container
  after = containerRenderAfter container
  action = renderContainer renderer wenv node cOffset cOffsetFwd before after

renderContainer
  :: Renderer
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Maybe Point
  -> Bool
  -> ContainerRenderHandler s e
  -> ContainerRenderHandler s e
  -> IO ()
renderContainer renderer wenv node cOffset cOffsetFwd renderBefore renderAfter =
  drawInScissor renderer True viewport $
    drawStyledAction renderer renderArea style $ \_ -> do
      renderBefore renderer wenv node

      drawInOffset renderer cOffset $
        forM_ children $ \child -> when (isWidgetVisible wenv child nextVp) $
          widgetRender (child ^. L.widget) renderer cWenv child

      renderAfter renderer wenv node
  where
    style = activeStyle wenv node
    cWenv = translateWenv wenv cOffset cOffsetFwd
    --pOffset = prevOffset wenv cOffsetFwd
    children = node ^. L.children
    viewport = node ^. L.info . L.viewport
    nextVp
      | cOffsetFwd = moveRect (wenv ^. L.offset) viewport
      | otherwise = viewport
    renderArea = node ^. L.info . L.renderArea

prevOffset :: WidgetEnv s e -> Bool -> Point
prevOffset wenv fwdPrevious = offset where
  accum = wenv ^. L.offsetAccum
  offset
    | fwdPrevious = fromMaybe def (Seq.lookup (length accum - 1) accum)
    | otherwise = def

translateWenv :: WidgetEnv s e -> Maybe Point -> Bool -> WidgetEnv s e
translateWenv wenv mOffset fwdPrevious = newWenv where
  offset = fromMaybe def mOffset
  newWenv = wenv
    & L.inputStatus . L.mousePos %~ addPoint (negPoint offset)
    & L.inputStatus . L.mousePosPrev %~ addPoint (negPoint offset)
    & L.offset .~ addPoint (prevOffset wenv fwdPrevious) offset
    & L.offsetAccum %~ (|> offset)

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
  :: WidgetNode s e -> WidgetNode s e -> Int -> WidgetNode s e
cascadeCtx parent child idx = newChild where
  pInfo = parent ^. L.info
  cInfo = child ^. L.info
  parentPath = pInfo ^. L.path
  parentVisible = pInfo ^. L.visible
  parentEnabled = pInfo ^. L.enabled
  newChild = child
    & L.info . L.path .~ parentPath |> idx
    & L.info . L.visible .~ (cInfo ^. L.visible && parentVisible)
    & L.info . L.enabled .~ (cInfo ^. L.enabled && parentEnabled)

isIgnoreChildrenEvents :: WidgetRequest s -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isIgnoreParentEvents :: WidgetRequest s -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False
