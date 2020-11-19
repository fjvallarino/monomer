{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Composite (
  module Monomer.Core,
  module Monomer.Event,
  module Monomer.Graphics,
  module Monomer.Widgets.Util,

  EventResponse(..),
  EventHandler,
  UIBuilder,
  composite
) where

import Data.Default
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Sequence (Seq(..), (|>), (<|), fromList)
import Data.Typeable (Typeable, cast, typeOf)

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Util

type EventHandler s e ep = s -> e -> [EventResponse s e ep]
type UIBuilder s e = s -> WidgetInstance s e
type TaskHandler e = IO (Maybe e)
type ProducerHandler e = (e -> IO ()) -> IO ()

data EventResponse s e ep
  = Model s
  | Event e
  | Report ep
  | Request (WidgetRequest s)
  | forall i . Typeable i => Message WidgetKey i
  | Task (TaskHandler e)
  | Producer (ProducerHandler e)

data Composite s e ep = Composite {
  _widgetType :: WidgetType,
  _eventHandler :: EventHandler s e ep,
  _uiBuilder :: UIBuilder s e
}

data CompositeState s e = CompositeState {
  _cmpModel :: s,
  _cmpRoot :: WidgetInstance s e,
  _cmpInitEvent :: Maybe e,
  _cmpGlobalKeys :: GlobalKeys s e
}

data ReducedEvents s e sp ep = ReducedEvents {
  _reModel :: s,
  _reEvents :: Seq e,
  _reReports :: Seq ep,
  _reRequests :: Seq (WidgetRequest s),
  _reMessages :: Seq (WidgetRequest sp),
  _reTasks :: Seq (TaskHandler e),
  _reProducers :: Seq (ProducerHandler e)
}

composite
  :: (Eq s, Typeable s, Typeable e)
  => WidgetType
  -> s
  -> Maybe e
  -> EventHandler s e ep
  -> UIBuilder s e
  -> WidgetInstance sp ep
composite widgetType model initEvent eventHandler uiBuilder = newInstance where
  widgetRoot = uiBuilder model
  composite = Composite widgetType eventHandler uiBuilder
  state = CompositeState model widgetRoot initEvent M.empty
  widget = createComposite composite state
  newInstance = defaultWidgetInstance widgetType widget

createComposite
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep -> CompositeState s e -> Widget sp ep
createComposite comp state = widget where
  widget = Widget {
    widgetInit = compositeInit comp state,
    widgetMerge = compositeMerge comp state,
    widgetDispose = compositeDispose comp state,
    widgetGetState = makeState state,
    widgetFindNextFocus = compositeFindNextFocus comp state,
    widgetFindByPoint = compositeFindByPoint state,
    widgetHandleEvent = compositeHandleEvent comp state,
    widgetHandleMessage = compositeHandleMessage comp state,
    widgetUpdateSizeReq = compositeUpdateSizeReq comp state,
    widgetResize = compositeResize comp state,
    widgetRender = compositeRender comp state
  }

-- | Init
compositeInit
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetInstance sp ep
  -> WidgetResult sp ep
compositeInit comp state wenv widgetComp = newResult where
  CompositeState{..} = state
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  tempRoot = cascadeCtx widgetComp _cmpRoot
  widget = _wiWidget tempRoot
  WidgetResult reqs evts root = widgetInit widget cwenv tempRoot
  newEvts = maybe evts (evts |>) _cmpInitEvent
  newState = state {
    _cmpRoot = root,
    _cmpGlobalKeys = collectGlobalKeys M.empty root
  }
  tempResult = WidgetResult reqs newEvts root
  styledComp = initInstanceStyle wenv Nothing widgetComp
  newResult = reduceResult comp newState wenv styledComp tempResult


-- | Merge
compositeMerge
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetInstance sp ep
  -> WidgetInstance sp ep
  -> WidgetResult sp ep
compositeMerge comp state wenv oldComposite newComposite = newResult where
  oldState = widgetGetState (_wiWidget oldComposite) wenv
  validState = fromMaybe state (useState oldState)
  CompositeState oldModel oldRoot oldInit oldGlobalKeys = validState
  -- Duplicate widget tree creation is avoided because the widgetRoot created
  -- on _cmp_ has not yet been evaluated
  tempRoot = cascadeCtx newComposite (_uiBuilder comp oldModel)
  tempWidget = _wiWidget tempRoot
  cwenv = convertWidgetEnv wenv oldGlobalKeys oldModel
  mergeRequired = instanceMatches tempRoot oldRoot
  tempResult
    | mergeRequired = widgetMerge tempWidget cwenv oldRoot tempRoot
    | otherwise = widgetInit tempWidget cwenv tempRoot
  newRoot = _wrWidget tempResult
  newState = validState {
    _cmpRoot = newRoot,
    _cmpGlobalKeys = collectGlobalKeys M.empty newRoot
  }
  styledComp = initInstanceStyle wenv Nothing newComposite
  newResult = reduceResult comp newState wenv styledComp tempResult

-- | Dispose
compositeDispose
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetInstance sp ep
  -> WidgetResult sp ep
compositeDispose comp state wenv widgetComp = result where
  CompositeState{..} = state
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  widget = _wiWidget _cmpRoot
  WidgetResult reqs evts _ = widgetDispose widget cwenv _cmpRoot
  tempResult = WidgetResult reqs evts _cmpRoot
  result = reduceResult comp state wenv widgetComp tempResult

-- | Next focusable
compositeFindNextFocus
  :: Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> FocusDirection
  -> Path
  -> WidgetInstance sp ep
  -> Maybe Path
compositeFindNextFocus comp state wenv dir start widgetComp = nextFocus where
  CompositeState{..} = state
  widget = _wiWidget _cmpRoot
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  nextFocus = widgetFindNextFocus widget cwenv dir start _cmpRoot

-- | Find
compositeFindByPoint
  :: CompositeState s e
  -> WidgetEnv sp ep
  -> Path
  -> Point
  -> WidgetInstance sp ep
  -> Maybe Path
compositeFindByPoint CompositeState{..} wenv startPath point widgetComp
  | _wiVisible widgetComp && validStep = resultPath
  | otherwise = Nothing
  where
    widget = _wiWidget _cmpRoot
    cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
    validStep = Seq.null startPath || Seq.index startPath 0 == 0
    newStartPath = Seq.drop 1 startPath
    resultPath = widgetFindByPoint widget cwenv newStartPath point _cmpRoot

-- | Event handling
compositeHandleEvent
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> Path
  -> SystemEvent
  -> WidgetInstance sp ep
  -> Maybe (WidgetResult sp ep)
compositeHandleEvent comp state wenv target evt widgetComp = result where
  CompositeState{..} = state
  widget = _wiWidget _cmpRoot
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  rootEnabled = _wiEnabled _cmpRoot
  processEvent = reduceResult comp state wenv widgetComp
  evtResult
    | not (_wiVisible widgetComp && _wiEnabled widgetComp) = Nothing
    | rootEnabled = widgetHandleEvent widget cwenv target evt _cmpRoot
    | otherwise = Nothing
  result = fmap processEvent evtResult

-- | Message handling
compositeHandleMessage
  :: (Eq s, Typeable i, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> Path
  -> i
  -> WidgetInstance sp ep
  -> Maybe (WidgetResult sp ep)
compositeHandleMessage comp state@CompositeState{..} wenv target arg widgetComp
  | isTargetReached target widgetComp = case cast arg of
      Just (Just evt) -> Just $ reduceResult comp state wenv widgetComp evtResult where
        evtResult = WidgetResult Seq.empty (Seq.singleton evt) _cmpRoot
      _ -> Nothing
  | otherwise = fmap processEvent result where
      processEvent = reduceResult comp state wenv widgetComp
      cmpWidget = _wiWidget _cmpRoot
      cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
      result = widgetHandleMessage cmpWidget cwenv target arg _cmpRoot

-- Preferred size
compositeUpdateSizeReq
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetInstance sp ep
  -> WidgetInstance sp ep
compositeUpdateSizeReq comp state wenv widgetComp = newComp where
  CompositeState{..} = state
  style = activeStyle wenv widgetComp
  widget = _wiWidget _cmpRoot
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  newRoot = widgetUpdateSizeReq widget cwenv _cmpRoot
  currReqW = _wiSizeReqW newRoot
  currReqH = _wiSizeReqH newRoot
  (newReqW, newReqH) = sizeReqAddStyle style (currReqW, currReqH)
  newState = state {
    _cmpRoot = newRoot
  }
  newComp = widgetComp {
    _wiWidget = createComposite comp newState,
    _wiSizeReqW = newReqW,
    _wiSizeReqH = newReqH
  }

-- Resize
compositeResize
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> Rect
  -> Rect
  -> WidgetInstance sp ep
  -> WidgetInstance sp ep
compositeResize comp state wenv viewport renderArea widgetComp = resized where
  CompositeState{..} = state
  style = activeStyle wenv widgetComp
  contentArea = fromMaybe def (removeOuterBounds style renderArea)
  widget = _wiWidget _cmpRoot
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  newRoot = widgetResize widget cwenv viewport contentArea _cmpRoot
  newState = state {
    _cmpRoot = newRoot {
      _wiViewport = viewport,
      _wiRenderArea = contentArea
    }
  }
  resized = widgetComp {
    _wiWidget = createComposite comp newState,
    _wiViewport = viewport,
    _wiRenderArea = renderArea
  }

-- Render
compositeRender
  :: Composite s e ep
  -> CompositeState s e
  -> Renderer
  -> WidgetEnv sp ep
  -> WidgetInstance sp ep
  -> IO ()
compositeRender comp state renderer wenv _ = action where
  CompositeState{..} = state
  widget = _wiWidget _cmpRoot
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  action = widgetRender widget renderer cwenv _cmpRoot

reduceResult
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetInstance sp ep
  -> WidgetResult s e
  -> WidgetResult sp ep
reduceResult comp state wenv widgetComp widgetResult = newResult where
  CompositeState{..} = state
  WidgetResult reqs evts evtsRoot = widgetResult
  evtUpdates = getUpdateModelReqs reqs
  evtModel = foldr (.) id evtUpdates _cmpModel
  evtHandler = _eventHandler comp
  ReducedEvents{..} = reduceCompEvents _cmpGlobalKeys evtHandler evtModel evts
  WidgetResult uReqs uEvts uWidget =
    updateComposite comp state wenv _reModel evtsRoot widgetComp
  currentPath = _wiPath widgetComp
  newReqs = toParentReqs reqs
         <> tasksToRequests currentPath _reTasks
         <> producersToRequests currentPath _reProducers
         <> toParentReqs uReqs
         <> toParentReqs _reRequests
         <> _reMessages
  newEvts = _reReports <> uEvts
  newResult = WidgetResult newReqs newEvts uWidget

updateComposite
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> s
  -> WidgetInstance s e
  -> WidgetInstance sp ep
  -> WidgetResult sp ep
updateComposite comp state wenv newModel widgetRoot widgetComp = result where
  CompositeState{..} = state
  changed = _cmpModel /= newModel
  newState = state {
    _cmpModel = newModel,
    _cmpRoot = widgetRoot
  }
  result
    | changed = rebuildComposite comp state wenv newModel widgetRoot widgetComp
    | otherwise = resultWidget $ widgetComp {
        _wiWidget = createComposite comp newState
      }

rebuildComposite
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> s
  -> WidgetInstance s e
  -> WidgetInstance sp ep
  -> WidgetResult sp ep
rebuildComposite comp state wenv newModel widgetRoot widgetComp = result where
  CompositeState{..} = state
  builtRoot = cascadeCtx widgetComp (_uiBuilder comp newModel)
  builtWidget = _wiWidget builtRoot
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys newModel
  mergedResult = widgetMerge builtWidget cwenv widgetRoot builtRoot
  resizedResult = resizeResult state wenv mergedResult widgetComp
  mergedState = state {
    _cmpModel = newModel,
    _cmpRoot = _wrWidget resizedResult
  }
  result = reduceResult comp mergedState wenv widgetComp resizedResult

resizeResult
  :: (Eq s, Typeable s, Typeable e)
  => CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetResult s e
  -> WidgetInstance sp ep
  -> WidgetResult s e
resizeResult state wenv result widgetComp = resizedResult where
  CompositeState{..} = state
  viewport = _wiViewport widgetComp
  renderArea = _wiRenderArea widgetComp
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  widgetRoot = _wrWidget result
  reqRoot = widgetUpdateSizeReq (_wiWidget widgetRoot) cwenv widgetRoot
  tempRoot = widgetResize (_wiWidget reqRoot) cwenv viewport renderArea reqRoot
  newRoot = tempRoot {
    _wiViewport = viewport,
    _wiRenderArea = renderArea
  }
  resizedResult = result {
    _wrWidget = newRoot
  }

reduceCompEvents
  :: GlobalKeys s e
  -> EventHandler s e ep
  -> s
  -> Seq e
  -> ReducedEvents s e sp ep
reduceCompEvents globalKeys eventHandler model events = result where
  initial = ReducedEvents {
    _reModel = model,
    _reEvents = Seq.empty,
    _reReports = Seq.empty,
    _reRequests = Seq.empty,
    _reMessages = Seq.empty,
    _reTasks = Seq.empty,
    _reProducers = Seq.empty
  }
  reducer current event = foldl' reducer newCurrent newEvents where
    response = eventHandler (_reModel current) event
    processed = foldl' (reduceEvtResponse globalKeys) current response
    newEvents = _reEvents processed
    newCurrent = processed { _reEvents = Seq.empty }
  result = foldl' reducer initial events

reduceEvtResponse
  :: GlobalKeys s e
  -> ReducedEvents s e sp ep
  -> EventResponse s e ep
  -> ReducedEvents s e sp ep
reduceEvtResponse globalKeys curr@ReducedEvents{..} response = case response of
  Model newModel -> curr { _reModel = newModel }
  Event event -> curr { _reEvents = _reEvents |> event }
  Report report -> curr { _reReports = _reReports |> report }
  Request req -> curr { _reRequests = _reRequests |> req }
  Message key message -> case M.lookup key globalKeys of
    Just inst -> curr {
        _reMessages = _reMessages |> SendMessage (_wiPath inst) message
      }
    Nothing -> curr
  Task task -> curr { _reTasks = _reTasks |> task }
  Producer producer -> curr { _reProducers = _reProducers |> producer }

tasksToRequests :: Typeable e => Path -> Seq (IO e) -> Seq (WidgetRequest sp)
tasksToRequests path reqs = RunTask path <$> reqs

producersToRequests
  :: Typeable e => Path -> Seq (ProducerHandler e) -> Seq (WidgetRequest sp)
producersToRequests path reqs = RunProducer path <$> reqs

toParentReqs :: Seq (WidgetRequest s) -> Seq (WidgetRequest sp)
toParentReqs reqs = fmap fromJust $ Seq.filter isJust $ fmap toParentReq reqs

toParentReq :: WidgetRequest s -> Maybe (WidgetRequest sp)
toParentReq IgnoreParentEvents = Just IgnoreParentEvents
toParentReq IgnoreChildrenEvents = Just IgnoreChildrenEvents
toParentReq Resize = Just Resize
toParentReq (MoveFocus dir) = Just (MoveFocus dir)
toParentReq (SetFocus path) = Just (SetFocus path)
toParentReq (GetClipboard path) = Just (GetClipboard path)
toParentReq (SetClipboard clipboard) = Just (SetClipboard clipboard)
toParentReq (StartTextInput rect) = Just (StartTextInput rect)
toParentReq StopTextInput = Just StopTextInput
toParentReq ResetOverlay = Just ResetOverlay
toParentReq (SetOverlay path) = Just (SetOverlay path)
toParentReq (SetCursorIcon icon) = Just (SetCursorIcon icon)
toParentReq (SendMessage path message) = Just (SendMessage path message)
toParentReq (RunTask path action) = Just (RunTask path action)
toParentReq (RunProducer path action) = Just (RunProducer path action)
toParentReq (UpdateWindow req) = Just (UpdateWindow req)
toParentReq (UpdateModel fn) = Nothing

collectGlobalKeys
  :: Map WidgetKey (WidgetInstance s e)
  -> WidgetInstance s e
  -> Map WidgetKey (WidgetInstance s e)
collectGlobalKeys keys inst = foldl' collect updatedMap children where
  children = _wiChildren inst
  collect currKeys child = collectGlobalKeys currKeys child
  updatedMap = case _wiKey inst of
    Just key -> M.insert key inst keys
    _ -> keys

convertWidgetEnv :: WidgetEnv sp ep -> GlobalKeys s e -> s -> WidgetEnv s e
convertWidgetEnv wenv globalKeys model = WidgetEnv {
  _weOS = _weOS wenv,
  _weRenderer = _weRenderer wenv,
  _weTheme = _weTheme wenv,
  _weAppWindowSize = _weAppWindowSize wenv,
  _weGlobalKeys = globalKeys,
  _weCurrentCursor = _weCurrentCursor wenv,
  _weFocusedPath = _weFocusedPath wenv,
  _weOverlayPath = _weOverlayPath wenv,
  _weModel = model,
  _weInputStatus = _weInputStatus wenv,
  _weTimestamp = _weTimestamp wenv,
  _weInTopLayer = _weInTopLayer wenv
}

cascadeCtx :: WidgetInstance sp ep -> WidgetInstance s e -> WidgetInstance s e
cascadeCtx parent child = newChild where
  parentVisible = _wiVisible parent
  parentEnabled = _wiEnabled parent
  newChild = child {
    _wiPath = firstChildPath parent,
    _wiVisible = _wiVisible child && parentVisible,
    _wiEnabled = _wiEnabled child && parentEnabled
  }

getUpdateModelReqs :: (Traversable t) => t (WidgetRequest s) -> Seq (s -> s)
getUpdateModelReqs reqs = foldl' foldHelper Seq.empty reqs where
  foldHelper acc (UpdateModel fn) = acc |> fn
  foldHelper acc _ = acc

firstChildPath :: WidgetInstance s e -> Path
firstChildPath inst = _wiPath inst |> 0
