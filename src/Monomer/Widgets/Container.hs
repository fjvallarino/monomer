{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Container (
  module Monomer.Core,
  module Monomer.Event,
  module Monomer.Graphics,
  module Monomer.Widgets.Util,

  Container(..),
  createContainer,
  initWrapper,
  mergeWrapper,
  handleEventWrapper,
  handleMessageWrapper,
  updateSizeReqWrapper,
  resizeWrapper,
  renderWrapper,
  defaultRender
) where

import Control.Monad
import Data.Default
import Data.Foldable (fold)
import Data.Maybe
import Data.Typeable (Typeable)
import Data.Sequence (Seq(..), (<|), (|>), (><))

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Util

type ContainerInitHandler s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e

type ContainerMergeHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState
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
  -> Maybe Path

type ContainerFindByPointHandler s e
  = WidgetEnv s e
  -> Path
  -> Point
  -> WidgetInstance s e
  -> Maybe Path

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
createContainer Container{..} = Widget {
  widgetInit = initWrapper containerInit,
  widgetMerge = mergeWrapper containerMerge,
  widgetDispose = disposeWrapper containerDispose,
  widgetGetState = containerGetState,
  widgetFindNextFocus = containerFindNextFocus,
  widgetFindByPoint = containerFindByPoint,
  widgetHandleEvent = handleEventWrapper containerHandleEvent,
  widgetHandleMessage = handleMessageWrapper containerHandleMessage,
  widgetUpdateSizeReq = updateSizeReqWrapper containerGetSizeReq,
  widgetResize = resizeWrapper containerResize,
  widgetRender = renderWrapper containerRender
}

-- | Init handler
defaultInit :: ContainerInitHandler s e
defaultInit _ widgetInst = resultWidget widgetInst

initWrapper
  :: ContainerInitHandler s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e
initWrapper initHandler wenv widgetInst = result where
  WidgetResult reqs events tempInstance = initHandler wenv widgetInst
  children = _wiChildren tempInstance
  indexes = Seq.fromList [0..length children]
  zipper idx child = widgetInit newWidget wenv newChild where
    newChild = cascadeCtx widgetInst child idx
    newWidget = _wiWidget newChild
  results = Seq.zipWith zipper indexes children
  newReqs = fold $ fmap _wrRequests results
  newEvents = fold $ fmap _wrEvents results
  newChildren = fmap _wrWidget results
  newInstance = tempInstance {
    _wiChildren = newChildren
  }
  result = WidgetResult (reqs <> newReqs) (events <> newEvents) newInstance

-- | Merging
defaultMerge :: ContainerMergeHandler s e
defaultMerge wenv state newInstance = resultWidget newInstance

mergeWrapper
  :: ContainerMergeHandler s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e
  -> WidgetResult s e
mergeWrapper mergeHandler wenv oldInst newInst = result where
  oldState = widgetGetState (_wiWidget oldInst) wenv
  tempInst = newInst {
    _wiViewport = _wiViewport oldInst,
    _wiRenderArea = _wiRenderArea oldInst
  }
  WidgetResult uReqs uEvents uInstance = mergeHandler wenv oldState tempInst
  oldChildren = _wiChildren oldInst
  updatedChildren = _wiChildren uInstance
  indexes = Seq.fromList [0..length updatedChildren]
  zipper idx child = cascadeCtx tempInst child idx
  newChildren = Seq.zipWith zipper indexes updatedChildren
  (mergedResults, removedResults) = mergeChildren wenv oldChildren newChildren
  mergedChildren = fmap _wrWidget mergedResults
  concatSeq seqs = fold seqs
  mergedReqs = concatSeq $ fmap _wrRequests mergedResults
  mergedEvents = concatSeq $ fmap _wrEvents mergedResults
  removedReqs = concatSeq $ fmap _wrRequests removedResults
  removedEvents = concatSeq $ fmap _wrEvents removedResults
  mergedInstance = uInstance {
    _wiChildren = mergedChildren
  }
  newReqs = uReqs <> mergedReqs <> removedReqs
  newEvents = uEvents <> mergedEvents <> removedEvents
  result = WidgetResult newReqs newEvents mergedInstance

mergeChildren
  :: WidgetEnv s e
  -> Seq (WidgetInstance s e)
  -> Seq (WidgetInstance s e)
  -> (Seq (WidgetResult s e), Seq (WidgetResult s e))
mergeChildren wenv oldItems Empty = (Empty, removed) where
  dispose child = widgetDispose (_wiWidget child) wenv child
  removed = fmap dispose oldItems
mergeChildren wenv Empty newItems = (added, Empty) where
  init child = widgetInit (_wiWidget child) wenv child
  added = fmap init newItems
mergeChildren wenv oldItems newItems = (added, cremoved) where
  oldChild :<| oldChildren = oldItems
  newChild :<| newChildren = newItems
  newWidget = _wiWidget newChild
  oldKeyMatch = _wiKey newChild >>= flip M.lookup (_weGlobalKeys wenv)
  mergedOld = widgetMerge newWidget wenv oldChild newChild
  mergedKey = widgetMerge newWidget wenv (fromJust oldKeyMatch) newChild
  initNew = widgetInit newWidget wenv newChild
  (child, oldRest)
    | instanceMatches newChild oldChild = (mergedOld, oldChildren)
    | isJust oldKeyMatch = (mergedKey, oldItems)
    | otherwise = (initNew, oldItems)
  (cadded, cremoved) = mergeChildren wenv oldRest newChildren
  added = child <| cadded

-- | Dispose handler
defaultDispose :: ContainerInitHandler s e
defaultDispose _ widgetInst = resultWidget widgetInst

disposeWrapper
  :: ContainerDisposeHandler s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e
disposeWrapper disposeHandler wenv widgetInst = result where
  WidgetResult reqs events tempInstance = disposeHandler wenv widgetInst
  children = _wiChildren tempInstance
  dispose child = widgetDispose (_wiWidget child) wenv child
  results = fmap dispose children
  newReqs = fold $ fmap _wrRequests results
  newEvents = fold $ fmap _wrEvents results
  result = WidgetResult (reqs <> newReqs) (events <> newEvents) widgetInst

-- | State Handling helpers
defaultGetState :: ContainerGetStateHandler s e
defaultGetState _ = Nothing

-- | Find next focusable item
defaultFindNextFocus
  :: WidgetEnv s e -> FocusDirection -> Path -> WidgetInstance s e -> Maybe Path
defaultFindNextFocus wenv direction start widgetInst = nextFocus where
  children
    | direction == FocusFwd = _wiChildren widgetInst
    | otherwise = Seq.reverse (_wiChildren widgetInst)
  isBeforeTarget ch
    | direction == FocusFwd = isTargetBeforeCurrent start ch
    | otherwise = isTargetAfterCurrent start ch
  nextCandidate ch = widgetFindNextFocus (_wiWidget ch) wenv direction start ch
  filtered = Seq.filter isBeforeTarget children
  candidates = fmap nextCandidate filtered
  focusedPaths = fmap fromJust (Seq.filter isJust candidates)
  nextFocus
    | isFocusCandidate direction start widgetInst = Just (_wiPath widgetInst)
    | otherwise = Seq.lookup 0 focusedPaths

-- | Find instance matching point
defaultFindByPoint
  :: WidgetEnv s e -> Path -> Point -> WidgetInstance s e -> Maybe Path
defaultFindByPoint wenv startPath point widgetInst = result where
  children = _wiChildren widgetInst
  pointInWidget wi = pointInRect point (_wiViewport wi)
  newStartPath = Seq.drop 1 startPath
  childIdx = case newStartPath of
    Empty -> Seq.findIndexL pointInWidget children
    p :<| ps -> if Seq.length children > p then Just p else Nothing
  result = case childIdx of
    Just idx -> childPath where
      childPath = widgetFindByPoint childWidget wenv newStartPath point child
      child = Seq.index children idx
      childWidget = _wiWidget child
    Nothing -> Just $ _wiPath widgetInst

-- | Event Handling
defaultHandleEvent :: ContainerEventHandler s e
defaultHandleEvent wenv target evt widgetInst = Nothing

handleEventWrapper
  :: ContainerEventHandler s e
  -> WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
handleEventWrapper pHandler wenv target event widgetInst
  | targetReached = handleStyleChange pHandler wenv target event widgetInst
  | not targetValid = Nothing
  | otherwise = mergeParentChildEvts widgetInst pResponse cResponse childIdx
  where
    -- Having targetValid = False means the next path step is not in
    -- _wiChildren, but may still be valid in the receiving widget
    -- For example, Composite has its own tree of child widgets with (possibly)
    -- different types for Model and Events, and is candidate for the next step
    targetReached = isTargetReached target widgetInst
    targetValid = isTargetValid target widgetInst
    childIdx = fromJust $ nextTargetStep target widgetInst
    children = _wiChildren widgetInst
    child = Seq.index children childIdx
    childWidget = _wiWidget child
    pResponse = pHandler wenv target event widgetInst
    childrenIgnored = isJust pResponse && ignoreChildren (fromJust pResponse)
    cResponse
      | childrenIgnored || not (_wiEnabled child) = Nothing
      | otherwise = widgetHandleEvent childWidget wenv target event child

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
  | otherwise = Just $ WidgetResult requests userEvents newWidget
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
defaultHandleMessage wenv ctx message widgetInst = Nothing

handleMessageWrapper
  :: Typeable i
  => ContainerMessageHandler s e
  -> WidgetEnv s e
  -> Path
  -> i
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
handleMessageWrapper mHandler wenv target arg widgetInst
  | targetReached = mHandler wenv target arg widgetInst
  | not targetValid = Nothing
  | otherwise = messageResult
  where
    targetReached = isTargetReached target widgetInst
    targetValid = isTargetValid target widgetInst
    childIdx = fromJust $ nextTargetStep target widgetInst
    children = _wiChildren widgetInst
    child = Seq.index children childIdx
    message = widgetHandleMessage (_wiWidget child) wenv target arg child
    messageResult = updateChild <$> message
    updateChild cr = cr {
      _wrWidget = replaceChild widgetInst (_wrWidget cr) childIdx
    }

-- | Preferred size
defaultGetSizeReq :: ContainerGetSizeReqHandler s e
defaultGetSizeReq wenv inst children = def

updateSizeReqWrapper
  :: ContainerGetSizeReqHandler s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e
updateSizeReqWrapper psHandler wenv widgetInst = newInst where
  style = instanceStyle wenv widgetInst
  children = _wiChildren widgetInst
  updateChild child = widgetUpdateSizeReq (_wiWidget child) wenv child
  newChildren = fmap updateChild children
  reqs = psHandler wenv widgetInst newChildren
  (newReqW, newReqH) = handleSizeReqStyle style reqs
  newInst = widgetInst {
    _wiChildren = newChildren,
    _wiSizeReqW = newReqW,
    _wiSizeReqH = newReqH
  }

-- | Resize
defaultResize :: ContainerResizeHandler s e
defaultResize wenv viewport renderArea children widgetInst = newSize where
  childrenSizes = Seq.replicate (Seq.length children) def
  newSize = (widgetInst, childrenSizes)

resizeWrapper
  :: ContainerResizeHandler s e
  -> WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetInstance s e
  -> WidgetInstance s e
resizeWrapper handler wenv viewport renderArea widgetInst = newSize where
  style = instanceStyle wenv widgetInst
  contentArea = removeOuterBounds style renderArea
  children = _wiChildren widgetInst
  (tempInst, assigned) = handler wenv viewport contentArea children widgetInst
  resize (child, (vp, ra)) = widgetResize (_wiWidget child) wenv vp ra child
  newChildren = resize <$> Seq.zip children assigned
  newSize = tempInst {
    _wiViewport = viewport,
    _wiRenderArea = renderArea,
    _wiChildren = newChildren
  }

-- | Rendering
defaultRender :: ContainerRenderHandler s e
defaultRender renderer wenv widgetInst = return ()

renderWrapper
  :: ContainerRenderHandler s e
  -> Renderer
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> IO ()
renderWrapper rHandler renderer wenv widgetInst =
  drawInScissor renderer True viewport $
    drawStyledAction renderer renderArea style $ \_ -> do
      rHandler renderer wenv widgetInst

      forM_ children $ \child -> when (isVisible child) $
        widgetRender (_wiWidget child) renderer wenv child
  where
    style = instanceStyle wenv widgetInst
    children = _wiChildren widgetInst
    viewport = _wiViewport widgetInst
    renderArea = _wiRenderArea widgetInst
    isVisible c = _wiVisible c && rectsOverlap viewport (_wiViewport c)

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
