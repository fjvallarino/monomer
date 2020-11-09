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
  createThemed,
  initWrapper,
  mergeWrapper,
  handleEventWrapper,
  handleMessageWrapper,
  updateSizeReqWrapper,
  findByPointWrapper,
  findNextFocusWrapper,
  resizeWrapper,
  renderWrapper,
  defaultFindByPoint,
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

type ContainerGetBaseStyle s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> Maybe Style

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
createContainer Container{..} = Widget {
  widgetInit = initWrapper containerInit containerGetBaseStyle,
  widgetMerge = mergeWrapper containerMerge containerGetBaseStyle,
  widgetDispose = disposeWrapper containerDispose,
  widgetGetState = containerGetState,
  widgetFindNextFocus = findNextFocusWrapper containerFindNextFocus,
  widgetFindByPoint = findByPointWrapper False containerFindByPoint,
  widgetHandleEvent = handleEventWrapper False containerHandleEvent,
  widgetHandleMessage = handleMessageWrapper containerHandleMessage,
  widgetUpdateSizeReq = updateSizeReqWrapper containerGetSizeReq,
  widgetResize = resizeWrapper containerResize,
  widgetRender = renderWrapper containerRender
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
  merge wenv oldState inst = createInst wenv inst
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
  :: ContainerInitHandler s e
  -> ContainerGetBaseStyle s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e
initWrapper initHandler getBaseStyle wenv inst = newResult where
  WidgetResult reqs events tempInstance = initHandler wenv inst
  children = _wiChildren tempInstance
  initChild idx child = widgetInit newWidget wenv newChild where
    newChild = cascadeCtx tempInstance child idx
    newWidget = _wiWidget newChild
  results = Seq.mapWithIndex initChild children
  newReqs = fold $ fmap _wrRequests results
  newEvents = fold $ fmap _wrEvents results
  newChildren = fmap _wrWidget results
  newInstance = tempInstance {
    _wiChildren = newChildren
  }
  baseStyle = getBaseStyle wenv newInstance
  styledInst = initInstanceStyle wenv baseStyle newInstance
  newResult = WidgetResult (reqs <> newReqs) (events <> newEvents) styledInst

-- | Merging
defaultMerge :: ContainerMergeHandler s e
defaultMerge wenv state newInstance = resultWidget newInstance

mergeWrapper
  :: ContainerMergeHandler s e
  -> ContainerGetBaseStyle s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e
  -> WidgetResult s e
mergeWrapper mergeHandler getBaseStyle wenv oldInst newInst = newResult where
  oldState = widgetGetState (_wiWidget oldInst) wenv
  tempInst = newInst {
    _wiViewport = _wiViewport oldInst,
    _wiRenderArea = _wiRenderArea oldInst
  }
  WidgetResult uReqs uEvents uInstance = mergeHandler wenv oldState tempInst
  oldChildren = _wiChildren oldInst
  updatedChildren = _wiChildren uInstance
  mergeChild idx child = cascadeCtx tempInst child idx
  newChildren = Seq.mapWithIndex mergeChild updatedChildren
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
  baseStyle = getBaseStyle wenv uInstance
  styledInst = initInstanceStyle wenv baseStyle mergedInstance
  newResult = WidgetResult newReqs newEvents styledInst

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
defaultDispose _ inst = resultWidget inst

disposeWrapper
  :: ContainerDisposeHandler s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e
disposeWrapper disposeHandler wenv inst = result where
  WidgetResult reqs events tempInstance = disposeHandler wenv inst
  children = _wiChildren tempInstance
  dispose child = widgetDispose (_wiWidget child) wenv child
  results = fmap dispose children
  newReqs = fold $ fmap _wrRequests results
  newEvents = fold $ fmap _wrEvents results
  result = WidgetResult (reqs <> newReqs) (events <> newEvents) inst

-- | State Handling helpers
defaultGetState :: ContainerGetStateHandler s e
defaultGetState _ = Nothing

-- | Find next focusable item
defaultFindNextFocus :: ContainerFindNextFocusHandler s e
defaultFindNextFocus wenv direction start inst = vchildren where
  vchildren = Seq.filter _wiVisible (_wiChildren inst)

findNextFocusWrapper
  :: ContainerFindNextFocusHandler s e
  -> WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetInstance s e
  -> Maybe Path
findNextFocusWrapper handler wenv direction start inst = nextFocus where
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
  :: Bool
  -> ContainerFindByPointHandler s e
  -> WidgetEnv s e
  -> Path
  -> Point
  -> WidgetInstance s e
  -> Maybe Path
findByPointWrapper ignoreEmptyClick handler wenv start point inst = result where
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
  :: Bool
  -> ContainerEventHandler s e
  -> WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
handleEventWrapper styleOnMerge pHandler wenv target event inst
  | not (_wiVisible inst) = Nothing
  | targetReached || not targetValid = styledParentResult
  | styleOnMerge = styledChildrenResult
  | otherwise = childrenResult
  where
    -- Having targetValid = False means the next path step is not in
    -- _wiChildren, but may still be valid in the receiving widget
    -- For example, Composite has its own tree of child widgets with (possibly)
    -- different types for Model and Events, and is candidate for the next step
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
    sHandler _ _ _ _ = mergeParentChildEvts inst pResponse cResponse childIdx
    styledParentResult = handleStyleChange pHandler wenv target event inst
    childrenResult = mergeParentChildEvts inst pResponse cResponse childIdx
    styledChildrenResult = handleStyleChange sHandler wenv target event inst

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
defaultHandleMessage wenv ctx message inst = Nothing

handleMessageWrapper
  :: Typeable i
  => ContainerMessageHandler s e
  -> WidgetEnv s e
  -> Path
  -> i
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
handleMessageWrapper mHandler wenv target arg inst
  | targetReached = mHandler wenv target arg inst
  | not targetValid = Nothing
  | otherwise = messageResult
  where
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

updateSizeReqWrapper
  :: ContainerGetSizeReqHandler s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e
updateSizeReqWrapper psHandler wenv inst = newInst where
  style = activeStyle wenv inst
  children = _wiChildren inst
  updateChild child = widgetUpdateSizeReq (_wiWidget child) wenv child
  newChildren = fmap updateChild children
  reqs = psHandler wenv inst newChildren
  (newReqW, newReqH) = handleSizeReqStyle style reqs
  newInst = inst {
    _wiChildren = newChildren,
    _wiSizeReqW = newReqW,
    _wiSizeReqH = newReqH
  }

-- | Resize
defaultResize :: ContainerResizeHandler s e
defaultResize wenv viewport renderArea children inst = newSize where
  childrenSizes = Seq.replicate (Seq.length children) def
  newSize = (inst, childrenSizes)

resizeWrapper
  :: ContainerResizeHandler s e
  -> WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetInstance s e
  -> WidgetInstance s e
resizeWrapper handler wenv viewport renderArea inst = newSize where
  children = _wiChildren inst
  (tempInst, assigned) = handler wenv viewport renderArea children inst
  resize (child, (vp, ra)) = newChildInst where
    tempChildInst = widgetResize (_wiWidget child) wenv vp ra child
    newChildInst = tempChildInst {
      _wiViewport = vp,
      _wiRenderArea = ra
    }
  newChildren = resize <$> Seq.zip children assigned
  newSize = tempInst {
    _wiViewport = viewport,
    _wiRenderArea = renderArea,
    _wiChildren = newChildren
  }

-- | Rendering
defaultRender :: ContainerRenderHandler s e
defaultRender renderer wenv inst = return ()

renderWrapper
  :: ContainerRenderHandler s e
  -> Renderer
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> IO ()
renderWrapper rHandler renderer wenv inst =
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
