{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Reduce duplication" -}

module Monomer.Widget.CompositeWidget (
  EventResponse(..),
  EventHandler,
  UIBuilder,
  composite
) where

import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Data.Default
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Sequence (Seq(..), (|>), (<|), fromList)
import Data.Typeable (Typeable, cast, typeOf)

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Widget.Types
import Monomer.Widget.Util

type EventHandler s e ep = s -> e -> EventResponse s e ep
type UIBuilder s e = s -> WidgetInstance s e
type TaskHandler e = IO (Maybe e)
type ProducerHandler e = (e -> IO ()) -> IO ()

data EventResponse s e ep
  = Model s
  | Event e
  | Report ep
  | forall i . Typeable i => Message WidgetKey i
  | Task (TaskHandler e)
  | Producer (ProducerHandler e)
  | Multiple (Seq (EventResponse s e ep))

instance Semigroup (EventResponse s e ep) where
  Multiple seq1 <> Multiple seq2 = Multiple (seq1 <> seq2)
  Multiple seq1 <> er2 = Multiple (seq1 |> er2)
  er1 <> Multiple seq2 = Multiple (er1 <| seq2)
  er1 <> er2 = Multiple (Seq.singleton er1 |> er2)

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
  _reMessages :: Seq (WidgetRequest sp),
  _reReports :: Seq ep,
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
    widgetGetState = makeState state,
    widgetMerge = compositeMerge comp state,
    widgetNextFocusable = compositeNextFocusable comp state,
    widgetFind = compositeFind state,
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
compositeInit comp state wenv widgetComp = result where
  CompositeState{..} = state
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  tempRoot = cascadeCtx widgetComp _cmpRoot
  widget = _wiWidget tempRoot
  WidgetResult reqs evts root = widgetInit widget cwenv tempRoot
  newEvts = maybe evts (evts |>) _cmpInitEvent
  newState = state {
    _cmpGlobalKeys = collectGlobalKeys M.empty root
  }
  tempResult = WidgetResult reqs newEvts root
  result = reduceResult comp newState wenv widgetComp tempResult

-- | Merge
compositeMerge
  :: (Eq s, Typeable s, Typeable e)
  => Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetInstance sp ep
  -> WidgetInstance sp ep
  -> WidgetResult sp ep
compositeMerge comp state wenv oldComposite newComposite = result where
  oldState = widgetGetState (_wiWidget oldComposite) wenv
  validState = fromMaybe state (useState oldState)
  CompositeState oldModel oldRoot oldInit oldGlobalKeys = validState
  -- Duplicate widget tree creation is avoided because the widgetRoot created
  -- on _cmp_ has not yet been evaluated
  newRoot = cascadeCtx newComposite (_uiBuilder comp oldModel)
  newState = validState {
    _cmpRoot = newRoot,
    _cmpGlobalKeys = collectGlobalKeys M.empty newRoot
  }
  newWidget = _wiWidget newRoot
  cwenv = convertWidgetEnv wenv oldGlobalKeys oldModel
  mergeRequired = instanceMatches newRoot oldRoot
  widgetResult
    | mergeRequired = widgetMerge newWidget cwenv oldRoot newRoot
    | otherwise = widgetInit newWidget cwenv newRoot
  result = reduceResult comp newState wenv newComposite widgetResult

-- | Next focusable
compositeNextFocusable
  :: Composite s e ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> Path
  -> WidgetInstance sp ep
  -> Maybe Path
compositeNextFocusable comp state wenv startFrom widgetComp = nextFocus where
  CompositeState{..} = state
  widget = _wiWidget _cmpRoot
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  nextFocus = widgetNextFocusable widget cwenv startFrom _cmpRoot

-- | Find
compositeFind
  :: CompositeState s e
  -> WidgetEnv sp ep
  -> Path
  -> Point
  -> WidgetInstance sp ep
  -> Maybe Path
compositeFind CompositeState{..} wenv startPath point widgetComp
  | validStep = widgetFind widget cwenv newStartPath point _cmpRoot
  | otherwise = Nothing
  where
    widget = _wiWidget _cmpRoot
    cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
    validStep = Seq.null startPath || Seq.index startPath 0 == 0
    newStartPath = Seq.drop 1 startPath

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
      Just evt -> Just $ reduceResult comp state wenv widgetComp evtResult where
        evtResult = WidgetResult Seq.empty (Seq.singleton evt) _cmpRoot
      Nothing -> Nothing
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
  widget = _wiWidget _cmpRoot
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  newRoot = widgetUpdateSizeReq widget cwenv _cmpRoot
  newState = state {
    _cmpRoot = newRoot
  }
  newComp = widgetComp {
    _wiWidget = createComposite comp newState,
    _wiSizeReq = _wiSizeReq newRoot
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
compositeResize comp state wenv newView newArea widgetComp = resized where
  CompositeState{..} = state
  widget = _wiWidget _cmpRoot
  cwenv = convertWidgetEnv wenv _cmpGlobalKeys _cmpModel
  newRoot = widgetResize widget cwenv newView newArea _cmpRoot
  newState = state {
    _cmpRoot = newRoot
  }
  resized = widgetComp {
    _wiWidget = createComposite comp newState,
    _wiViewport = newView,
    _wiRenderArea = newArea
  }

-- Render
compositeRender
  :: (Monad m)
  => Composite s e ep
  -> CompositeState s e
  -> Renderer m
  -> WidgetEnv sp ep
  -> WidgetInstance sp ep
  -> m ()
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
  tempRoot = widgetUpdateSizeReq (_wiWidget widgetRoot) cwenv widgetRoot
  newRoot = widgetResize (_wiWidget tempRoot) cwenv viewport renderArea tempRoot
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
  initial =
    ReducedEvents model Seq.empty Seq.empty Seq.empty Seq.empty Seq.empty
  reducer current event = foldl' reducer newCurrent newEvents where
    response = eventHandler (_reModel current) event
    processed = reduceEvtResponse globalKeys current response
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
  Message key message -> case M.lookup key globalKeys of
    Just inst -> curr {
        _reMessages = _reMessages |> SendMessage (_wiPath inst) message
      }
    Nothing -> curr
  Report report -> curr { _reReports = _reReports |> report }
  Task task -> curr { _reTasks = _reTasks |> task }
  Producer producer -> curr { _reProducers = _reProducers |> producer }
  Multiple ehs -> foldl' (reduceEvtResponse globalKeys) curr ehs

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
toParentReq (SetFocus path) = Just (SetFocus path)
toParentReq (GetClipboard path) = Just (GetClipboard path)
toParentReq (SetClipboard clipboard) = Just (SetClipboard clipboard)
toParentReq ResetOverlay = Just ResetOverlay
toParentReq (SetOverlay path) = Just (SetOverlay path)
toParentReq (SendMessage path message) = Just (SendMessage path message)
toParentReq (RunTask path action) = Just (RunTask path action)
toParentReq (RunProducer path action) = Just (RunProducer path action)
toParentReq (UpdateModel fn) = Nothing

collectGlobalKeys
  :: Map WidgetKey (WidgetInstance s e)
  -> WidgetInstance s e
  -> Map WidgetKey (WidgetInstance s e)
collectGlobalKeys keys widgetInst = foldl' collect updatedMap children where
  children = _wiChildren widgetInst
  collect currKeys child = collectGlobalKeys currKeys child
  updatedMap = case _wiKey widgetInst of
    Just key -> M.insert key widgetInst keys
    _ -> keys

convertWidgetEnv :: WidgetEnv sp ep -> GlobalKeys s e -> s -> WidgetEnv s e
convertWidgetEnv wenv globalKeys model = WidgetEnv {
  _wePlatform = _wePlatform wenv,
  _weScreenSize = _weScreenSize wenv,
  _weGlobalKeys = globalKeys,
  _weFocusedPath = _weFocusedPath wenv,
  _weModel = model,
  _weInputStatus = _weInputStatus wenv,
  _weTimestamp = _weTimestamp wenv
}

cascadeCtx :: WidgetInstance sp ep -> WidgetInstance s e -> WidgetInstance s e
cascadeCtx parent child = newChild where
  parentPath = _wiPath parent
  parentVisible = _wiVisible parent
  parentEnabled = _wiEnabled parent
  newChild = child {
    _wiPath = parentPath |> 0,
    _wiVisible = _wiVisible child && parentVisible,
    _wiEnabled = _wiEnabled child && parentEnabled
  }
