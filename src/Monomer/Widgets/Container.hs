{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Container (
  module Monomer.Core,
  module Monomer.Core.Combinators,
  module Monomer.Event,
  module Monomer.Graphics,
  module Monomer.Widgets.Util,

  Container(..),
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

import Control.Lens ((&), (^.), (.~), (%~))
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

type ContainerInitHandler s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e

type ContainerMergeHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState
  -> WidgetInstance s e
  -> WidgetInstance s e
  -> WidgetResult s e

type ContainerDisposeHandler s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e

type ContainerGetStateHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState

type ContainerFindNextFocusHandler s e
  = WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetInstance s e
  -> Seq (WidgetInstance s e)

type ContainerFindByPointHandler s e
  = WidgetEnv s e
  -> Path
  -> Point
  -> WidgetInstance s e
  -> Maybe Int

type ContainerEventHandler s e
  = WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)

type ContainerMessageHandler s e
  = forall i . Typeable i
  => WidgetEnv s e
  -> Path
  -> i
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)

type ContainerGetSizeReqHandler s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> Seq (WidgetInstance s e)
  -> (SizeReq, SizeReq)

type ContainerResizeHandler s e
  = WidgetEnv s e
  -> Rect
  -> Rect
  -> Seq (WidgetInstance s e)
  -> WidgetInstance s e
  -> (WidgetInstance s e, Seq (Rect, Rect))

type ContainerRenderHandler s e
  =  Renderer
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> IO ()

data Container s e = Container {
  containerIgnoreEmptyClick :: Bool,
  containerStyleOnMerge :: Bool,
  containerResizeRequired :: Bool,
  containerKeepChildrenSizes :: Bool,
  containerGetBaseStyle :: ContainerGetBaseStyle s e,
  containerInit :: ContainerInitHandler s e,
  containerMerge :: ContainerMergeHandler s e,
  containerDispose :: ContainerDisposeHandler s e,
  containerGetState :: ContainerGetStateHandler s e,
  containerFindNextFocus :: ContainerFindNextFocusHandler s e,
  containerFindByPoint :: ContainerFindByPointHandler s e,
  containerHandleEvent :: ContainerEventHandler s e,
  containerHandleMessage :: ContainerMessageHandler s e,
  containerGetSizeReq :: ContainerGetSizeReqHandler s e,
  containerResize :: ContainerResizeHandler s e,
  containerRender :: ContainerRenderHandler s e
}

instance Default (Container s e) where
  def = Container {
    containerIgnoreEmptyClick = False,
    containerStyleOnMerge = False,
    containerResizeRequired = True,
    containerKeepChildrenSizes = False,
    containerGetBaseStyle = defaultGetBaseStyle,
    containerInit = defaultInit,
    containerMerge = defaultMerge,
    containerDispose = defaultDispose,
    containerGetState = defaultGetState,
    containerFindNextFocus = defaultFindNextFocus,
    containerFindByPoint = defaultFindByPoint,
    containerHandleEvent = defaultHandleEvent,
    containerHandleMessage = defaultHandleMessage,
    containerGetSizeReq = defaultGetSizeReq,
    containerResize = defaultResize,
    containerRender = defaultRender
  }

createContainer :: Container s e -> Widget s e
createContainer container = Widget {
  widgetInit = initWrapper container,
  widgetMerge = mergeWrapper container,
  widgetDispose = disposeWrapper container,
  widgetGetState = containerGetState container,
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
  -> (WidgetEnv s e -> WidgetInstance s e)
  -> WidgetInstance s e
createThemed widgetType factory = newInst where
  createInst wenv inst = resultWidget themedInst where
    tempInst = factory wenv
    themedInst = inst {
      _wiWidget = _wiWidget tempInst,
      _wiChildren = _wiChildren tempInst,
      _wiFocusable = _wiFocusable tempInst,
      _wiStyle = _wiStyle tempInst <> _wiStyle inst
    }
  init = createInst
  merge wenv oldState oldInst inst = createInst wenv inst
  newWidget = createContainer def {
    containerInit = createInst,
    containerMerge = merge
  }
  newInst = defaultWidgetInstance widgetType newWidget

-- | Get base style for component
defaultGetBaseStyle :: ContainerGetBaseStyle s e
defaultGetBaseStyle wenv inst = Nothing

-- | Init handler
defaultInit :: ContainerInitHandler s e
defaultInit _ inst = resultWidget inst

initWrapper
  :: Container s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e
initWrapper container wenv inst = result where
  initHandler = containerInit container
  getBaseStyle = containerGetBaseStyle container
  styledInst = initInstanceStyle getBaseStyle wenv inst
  WidgetResult tempInst reqs events = initHandler wenv styledInst
  children = _wiChildren tempInst
  initChild idx child = widgetInit newWidget wenv newChild where
    newChild = cascadeCtx tempInst child idx
    newWidget = _wiWidget newChild
  results = Seq.mapWithIndex initChild children
  newReqs = fold $ fmap _wrRequests results
  newEvents = fold $ fmap _wrEvents results
  newChildren = fmap _wrWidget results
  newInst = tempInst {
    _wiChildren = newChildren
  }
  result = WidgetResult newInst (reqs <> newReqs) (events <> newEvents)

-- | Merging
defaultMerge :: ContainerMergeHandler s e
defaultMerge wenv oldState oldInst newInst = resultWidget newInst

mergeWrapper
  :: Container s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e
  -> WidgetResult s e
mergeWrapper container wenv oldInst newInst = result where
  getBaseStyle = containerGetBaseStyle container
  mergeHandler = containerMerge container
  styledInst = initInstanceStyle getBaseStyle wenv newInst
  pResult = mergeParent mergeHandler wenv oldInst styledInst
  cResult = mergeChildren wenv oldInst pResult
  result = mergeChildrenCheckVisible oldInst newInst cResult

mergeParent
  :: ContainerMergeHandler s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e
  -> WidgetResult s e
mergeParent mergeHandler wenv oldInst newInst = result where
  oldState = widgetGetState (_wiWidget oldInst) wenv
  tempInst = newInst {
    _wiViewport = _wiViewport oldInst,
    _wiRenderArea = _wiRenderArea oldInst,
    _wiSizeReqW = _wiSizeReqW oldInst,
    _wiSizeReqH = _wiSizeReqH oldInst
  }
  result = mergeHandler wenv oldState oldInst tempInst

mergeChildren
  :: WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e
  -> WidgetResult s e
mergeChildren wenv oldInst result = newResult where
  WidgetResult uInst uReqs uEvents = result
  oldChildren = _wiChildren oldInst
  updatedChildren = _wiChildren uInst
  mergeChild idx child = cascadeCtx uInst child idx
  newChildren = Seq.mapWithIndex mergeChild updatedChildren
  localKeys = buildLocalMap oldChildren
  cResult = mergeChildrenSeq wenv localKeys oldChildren newChildren
  (mergedResults, removedResults) = cResult
  mergedChildren = fmap _wrWidget mergedResults
  concatSeq seqs = fold seqs
  mergedReqs = concatSeq $ fmap _wrRequests mergedResults
  mergedEvents = concatSeq $ fmap _wrEvents mergedResults
  removedReqs = concatSeq $ fmap _wrRequests removedResults
  removedEvents = concatSeq $ fmap _wrEvents removedResults
  mergedInst = uInst {
    _wiChildren = mergedChildren
  }
  newReqs = uReqs <> mergedReqs <> removedReqs
  newEvents = uEvents <> mergedEvents <> removedEvents
  newResult = WidgetResult mergedInst newReqs newEvents

mergeChildrenSeq
  :: WidgetEnv s e
  -> Map WidgetKey (WidgetInstance s e)
  -> Seq (WidgetInstance s e)
  -> Seq (WidgetInstance s e)
  -> (Seq (WidgetResult s e), Seq (WidgetResult s e))
mergeChildrenSeq wenv localKeys oldItems Empty = (Empty, removed) where
  dispose child = widgetDispose (_wiWidget child) wenv child
  removed = fmap dispose oldItems
mergeChildrenSeq wenv localKeys Empty newItems = (merged, Empty) where
  init child = widgetInit (_wiWidget child) wenv child
  merged = fmap init newItems
mergeChildrenSeq wenv localKeys oldItems newItems = (merged, cremoved) where
  oldChild :<| oldChildren = oldItems
  newChild :<| newChildren = newItems
  newWidget = _wiWidget newChild
  oldKeyMatch = _wiKey newChild >>= flip findWidgetByKey localKeys
  oldMatch = fromJust oldKeyMatch
  mergedOld = widgetMerge newWidget wenv oldChild newChild
  mergedKey = widgetMerge newWidget wenv oldMatch newChild
  initNew = widgetInit newWidget wenv newChild
  isMergeKey = isJust oldKeyMatch && instanceMatches newChild oldMatch
  (child, oldRest)
    | instanceMatches newChild oldChild = (mergedOld, oldChildren)
    | isMergeKey = (mergedKey, oldItems)
    | otherwise = (initNew, oldItems)
  (cmerged, cremoved) = mergeChildrenSeq wenv localKeys oldRest newChildren
  merged = child <| cmerged

mergeChildrenCheckVisible
  :: WidgetInstance s e
  -> WidgetInstance s e
  -> WidgetResult s e
  -> WidgetResult s e
mergeChildrenCheckVisible oldInst newInst result = newResult where
  oldVisible = fmap _wiVisible (_wiChildren oldInst)
  newVisible = fmap _wiVisible (_wiChildren newInst)
  resizeRequired = oldVisible /= newVisible
  newResult
    | resizeRequired = result & L.requests %~ (|> ResizeWidgets)
    | otherwise = result

-- | Dispose handler
defaultDispose :: ContainerInitHandler s e
defaultDispose _ inst = resultWidget inst

disposeWrapper
  :: Container s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e
disposeWrapper container wenv inst = result where
  disposeHandler = containerDispose container
  WidgetResult tempInst reqs events = disposeHandler wenv inst
  children = _wiChildren tempInst
  dispose child = widgetDispose (_wiWidget child) wenv child
  results = fmap dispose children
  newReqs = fold $ fmap _wrRequests results
  newEvents = fold $ fmap _wrEvents results
  result = WidgetResult inst (reqs <> newReqs) (events <> newEvents)

-- | State Handling helpers
defaultGetState :: ContainerGetStateHandler s e
defaultGetState _ = Nothing

-- | Find next focusable item
defaultFindNextFocus :: ContainerFindNextFocusHandler s e
defaultFindNextFocus wenv direction start inst = vchildren where
  vchildren = Seq.filter _wiVisible (_wiChildren inst)

findNextFocusWrapper
  :: Container s e
  -> WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetInstance s e
  -> Maybe Path
findNextFocusWrapper container wenv direction start inst = nextFocus where
  handler = containerFindNextFocus container
  handlerResult = handler wenv direction start inst
  children
    | direction == FocusBwd = Seq.reverse handlerResult
    | otherwise = handlerResult
  nextFocus
    | isFocusCandidate direction start inst = Just (_wiPath inst)
    | otherwise = findFocusCandidate children wenv direction start

findFocusCandidate
  :: Seq (WidgetInstance s e)
  -> WidgetEnv s e
  -> FocusDirection
  -> Path
  -> Maybe Path
findFocusCandidate Empty _ _ _ = Nothing
findFocusCandidate (ch :<| chs) wenv dir start = result where
  isWidgetAfterStart
    | dir == FocusBwd = isWidgetBeforePath start ch
    | otherwise = isWidgetParentOfPath start ch || isWidgetAfterPath start ch
  candidate = widgetFindNextFocus (_wiWidget ch) wenv dir start ch
  result
    | isWidgetAfterStart && isJust candidate = candidate
    | otherwise = findFocusCandidate chs wenv dir start

-- | Find instance matching point
defaultFindByPoint :: ContainerFindByPointHandler s e
defaultFindByPoint wenv startPath point inst = result where
  children = _wiChildren inst
  pointInWidget wi = _wiVisible wi && pointInRect point (_wiViewport wi)
  result = Seq.findIndexL pointInWidget children

findByPointWrapper
  :: Container s e
  -> WidgetEnv s e
  -> Path
  -> Point
  -> WidgetInstance s e
  -> Maybe Path
findByPointWrapper container wenv start point inst = result where
  ignoreEmptyClick = containerIgnoreEmptyClick container
  handler = containerFindByPoint container
  children = _wiChildren inst
  newStartPath = Seq.drop 1 start
  childIdx = case newStartPath of
    Empty -> handler wenv start point inst
    p :<| ps -> Just p
  validateIdx p
    | Seq.length children > p = Just p
    | otherwise = Nothing
  resultPath = case childIdx >>= validateIdx of
    Just idx -> childPath where
      childPath = widgetFindByPoint childWidget wenv newStartPath point child
      child = Seq.index children idx
      childWidget = _wiWidget child
    Nothing
      | ignoreEmptyClick -> Nothing
      | otherwise -> Just $ _wiPath inst
  result
    | _wiVisible inst = resultPath
    | otherwise = Nothing

-- | Event Handling
defaultHandleEvent :: ContainerEventHandler s e
defaultHandleEvent wenv target evt inst = Nothing

handleEventWrapper
  :: Container s e
  -> WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
handleEventWrapper container wenv target event inst
  | not (_wiVisible inst) = Nothing
  | targetReached || not targetValid = pResultStyled
  | styleOnMerge = cResultStyled
  | otherwise = cResult
  where
    -- Having targetValid = False means the next path step is not in
    -- _wiChildren, but may still be valid in the receiving widget
    -- For example, Composite has its own tree of child widgets with (possibly)
    -- different types for Model and Events, and is candidate for the next step
    style = activeStyle wenv inst
    styleOnMerge = containerStyleOnMerge container
    pHandler = containerHandleEvent container
    targetReached = isTargetReached target inst
    targetValid = isTargetValid target inst
    childIdx = fromJust $ nextTargetStep target inst
    children = _wiChildren inst
    child = Seq.index children childIdx
    childWidget = _wiWidget child
    pResponse = pHandler wenv target event inst
    childrenIgnored = isJust pResponse && ignoreChildren (fromJust pResponse)
    cResponse
      | childrenIgnored || not (_wiEnabled child) = Nothing
      | otherwise = widgetHandleEvent childWidget wenv target event child
    pResultStyled = handleStyleChange wenv target event style pResponse inst
    cResult = mergeParentChildEvts inst pResponse cResponse childIdx
    cResultStyled = handleStyleChange wenv target event style cResult inst

mergeParentChildEvts
  :: WidgetInstance s e
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
defaultHandleMessage wenv ctx message inst = Nothing

handleMessageWrapper
  :: Typeable i
  => Container s e
  -> WidgetEnv s e
  -> Path
  -> i
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
handleMessageWrapper container wenv target arg inst
  | targetReached = mHandler wenv target arg inst
  | not targetValid = Nothing
  | otherwise = messageResult
  where
    mHandler = containerHandleMessage container
    targetReached = isTargetReached target inst
    targetValid = isTargetValid target inst
    childIdx = fromJust $ nextTargetStep target inst
    children = _wiChildren inst
    child = Seq.index children childIdx
    message = widgetHandleMessage (_wiWidget child) wenv target arg child
    messageResult = updateChild <$> message
    updateChild cr = cr {
      _wrWidget = replaceChild inst (_wrWidget cr) childIdx
    }

-- | Preferred size
defaultGetSizeReq :: ContainerGetSizeReqHandler s e
defaultGetSizeReq wenv inst children = def

getSizeReqWrapper
  :: Container s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetSizeReq s e
getSizeReqWrapper container wenv inst = newSizeReq & L.widget .~ newInst where
  resizeRequired = containerResizeRequired container
  psHandler = containerGetSizeReq container
  style = activeStyle wenv inst
  children = _wiChildren inst
  updateChild child = newChild where
    childReq = widgetGetSizeReq (_wiWidget child) wenv child
    WidgetSizeReq cWidget cReqW cReqH = childReq
    newChild = cWidget
      & L.sizeReqW .~ cReqW
      & L.sizeReqH .~ cReqH
  newChildren = fmap updateChild children
  (sizeReqW, sizeReqH) = psHandler wenv inst newChildren
  newSizeReq = sizeReqAddStyle style (WidgetSizeReq inst sizeReqW sizeReqH)
  newInst
    | resizeRequired = inst {
        _wiChildren = newChildren
      }
    | otherwise = inst

-- | Resize
defaultResize :: ContainerResizeHandler s e
defaultResize wenv viewport renderArea children inst = newSize where
  childrenSizes = Seq.replicate (Seq.length children) def
  newSize = (inst, childrenSizes)

resizeWrapper
  :: Container s e
  -> WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetInstance s e
  -> WidgetInstance s e
resizeWrapper container wenv viewport renderArea inst = newInst where
  resizeRequired = containerResizeRequired container
  vpChanged = viewport /= _wiViewport inst
  raChanged = renderArea /= _wiRenderArea inst
  keepSizes = containerKeepChildrenSizes container
  handler = containerResize container
  children = _wiChildren inst
  (tempInst, assigned) = handler wenv viewport renderArea children inst
  resize (child, (vp, ra)) = newChildInst where
    tempChildInst = widgetResize (_wiWidget child) wenv vp ra child
    cvp = _wiViewport tempChildInst
    cra = _wiRenderArea tempChildInst
    icvp = fromMaybe vp (intersectRects vp cvp)
    icra = fromMaybe ra (intersectRects ra cra)
    newChildInst = tempChildInst {
      _wiViewport = if keepSizes then icvp else vp,
      _wiRenderArea = if keepSizes then icra else ra
    }
  newChildren = resize <$> Seq.zip children assigned
  newInst
    | resizeRequired || vpChanged || raChanged = tempInst {
        _wiViewport = viewport,
        _wiRenderArea = renderArea,
        _wiChildren = newChildren
      }
    | otherwise = inst

-- | Rendering
defaultRender :: ContainerRenderHandler s e
defaultRender renderer wenv inst = return ()

renderWrapper
  :: Container s e
  -> Renderer
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> IO ()
renderWrapper container renderer wenv inst =
  renderContainer (containerRender container) renderer wenv inst

renderContainer
  :: ContainerRenderHandler s e
  -> Renderer
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> IO ()
renderContainer rHandler renderer wenv inst =
  drawInScissor renderer True viewport $
    drawStyledAction renderer renderArea style $ \_ -> do
      rHandler renderer wenv inst

      forM_ children $ \child -> when (isWidgetVisible child viewport) $
        widgetRender (_wiWidget child) renderer wenv child
  where
    style = activeStyle wenv inst
    children = _wiChildren inst
    viewport = _wiViewport inst
    renderArea = _wiRenderArea inst

-- | Event Handling Helpers
ignoreChildren :: WidgetResult s e -> Bool
ignoreChildren result = not (Seq.null ignoreReqs) where
  ignoreReqs = Seq.filter isIgnoreChildrenEvents (_wrRequests result)

ignoreParent :: WidgetResult s e -> Bool
ignoreParent result = not (Seq.null ignoreReqs) where
  ignoreReqs = Seq.filter isIgnoreParentEvents (_wrRequests result)

replaceChild
  :: WidgetInstance s e -> WidgetInstance s e -> Int -> WidgetInstance s e
replaceChild parent child idx = parent { _wiChildren = newChildren } where
  newChildren = Seq.update idx child (_wiChildren parent)

cascadeCtx
  :: WidgetInstance s e -> WidgetInstance s e -> Int -> WidgetInstance s e
cascadeCtx parent child idx = newChild where
  parentPath = _wiPath parent
  parentVisible = _wiVisible parent
  parentEnabled = _wiEnabled parent
  newChild = child {
    _wiPath = parentPath |> idx,
    _wiVisible = _wiVisible child && parentVisible,
    _wiEnabled = _wiEnabled child && parentEnabled
  }

isIgnoreChildrenEvents :: WidgetRequest s -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isIgnoreParentEvents :: WidgetRequest s -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False
