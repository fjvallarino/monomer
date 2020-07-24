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
  _compositeModel :: s,
  _compositeRoot :: WidgetInstance s e,
  _compositeInitEvent :: Maybe e,
  _compositeGlobalKeys :: GlobalKeys s e,
  _compositeSizeReq :: Tree SizeReq
}

data ReducedEvents s e sp ep = ReducedEvents {
  _reModel :: s,
  _reEvents :: Seq e,
  _reMessages :: Seq (WidgetRequest sp),
  _reReports :: Seq ep,
  _reTasks :: Seq (TaskHandler e),
  _reProducers :: Seq (ProducerHandler e)
}

composite :: (Eq s, Typeable s, Typeable e) => WidgetType -> s -> Maybe e -> EventHandler s e ep -> UIBuilder s e -> WidgetInstance sp ep
composite widgetType model initEvent eventHandler uiBuilder = defaultWidgetInstance widgetType widget where
  widgetRoot = uiBuilder model
  composite = Composite widgetType eventHandler uiBuilder
  state = CompositeState model widgetRoot initEvent M.empty (singleNode def)
  widget = createComposite composite state

createComposite :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> Widget sp ep
createComposite comp state = widget where
  widget = Widget {
    _widgetInit = compositeInit comp state,
    _widgetGetState = makeState state,
    _widgetMerge = compositeMerge comp state,
    _widgetNextFocusable = compositeNextFocusable comp state,
    _widgetFind = compositeFind state,
    _widgetHandleEvent = compositeHandleEvent comp state,
    _widgetHandleMessage = compositeHandleMessage comp state,
    _widgetPreferredSize = compositePreferredSize state,
    _widgetResize = compositeResize comp state,
    _widgetRender = compositeRender comp state
  }

compositeInit :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetEnv sp ep -> WidgetInstance sp ep -> WidgetResult sp ep
compositeInit comp state wenv widgetComposite = result where
  CompositeState{..} = state
  cwenv = convertWidgetEnv wenv _compositeGlobalKeys _compositeModel
  tempRoot = cascadeCtx widgetComposite _compositeRoot
  widget = _instanceWidget tempRoot
  WidgetResult reqs evts root = _widgetInit widget cwenv tempRoot
  newEvts = maybe evts (evts |>) _compositeInitEvent
  newState = state {
    _compositeGlobalKeys = collectGlobalKeys M.empty root
  }
  result = processWidgetResult comp newState wenv widgetComposite (WidgetResult reqs newEvts root)

compositeMerge :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetEnv sp ep -> WidgetInstance sp ep -> WidgetInstance sp ep -> WidgetResult sp ep
compositeMerge comp state wenv oldComposite newComposite = result where
  oldState = _widgetGetState (_instanceWidget oldComposite) wenv
  validState = fromMaybe state (useState oldState)
  CompositeState oldModel oldRoot oldInit oldGlobalKeys oldReqs = validState
  -- Duplicate widget tree creation is avoided because the widgetRoot created
  -- on _composite_ has not yet been evaluated
  newRoot = cascadeCtx newComposite (_uiBuilder comp oldModel)
  newState = validState {
    _compositeRoot = newRoot,
    _compositeGlobalKeys = collectGlobalKeys M.empty newRoot
  }
  newWidget = _instanceWidget newRoot
  cwenv = convertWidgetEnv wenv oldGlobalKeys oldModel
  mergeRequired = instanceMatches newRoot oldRoot
  widgetResult
    | mergeRequired = _widgetMerge newWidget cwenv oldRoot newRoot
    | otherwise = _widgetInit newWidget cwenv newRoot
  result = processWidgetResult comp newState wenv newComposite widgetResult

compositeNextFocusable :: Composite s e ep -> CompositeState s e -> WidgetEnv sp ep -> Path -> WidgetInstance sp ep -> Maybe Path
compositeNextFocusable comp state wenv startFrom widgetComposite = nextFocus where
  CompositeState{..} = state
  widget = _instanceWidget _compositeRoot
  cwenv = convertWidgetEnv wenv _compositeGlobalKeys _compositeModel
  isEnabled = _instanceEnabled _compositeRoot
  nextFocus
    | isEnabled = _widgetNextFocusable widget cwenv startFrom _compositeRoot
    | otherwise = Nothing

compositeFind :: CompositeState s e -> WidgetEnv sp ep -> Path -> Point -> WidgetInstance sp ep -> Maybe Path
compositeFind CompositeState{..} wenv startPath point widgetComposite
  | validStep = _widgetFind widget cwenv newStartPath point _compositeRoot
  | otherwise = Nothing
  where
    widget = _instanceWidget _compositeRoot
    cwenv = convertWidgetEnv wenv _compositeGlobalKeys _compositeModel
    validStep = Seq.null startPath || Seq.index startPath 0 == 0
    newStartPath = Seq.drop 1 startPath

compositeHandleEvent :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetEnv sp ep -> Path -> SystemEvent -> WidgetInstance sp ep -> Maybe (WidgetResult sp ep)
compositeHandleEvent comp state wenv target evt widgetComposite = fmap processEvent result where
  CompositeState{..} = state
  widget = _instanceWidget _compositeRoot
  cwenv = convertWidgetEnv wenv _compositeGlobalKeys _compositeModel
  rootEnabled = _instanceEnabled _compositeRoot
  processEvent = processWidgetResult comp state wenv widgetComposite
  result
    | rootEnabled = _widgetHandleEvent widget cwenv target evt _compositeRoot
    | otherwise = Nothing

processWidgetResult :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetEnv sp ep -> WidgetInstance sp ep -> WidgetResult s e -> WidgetResult sp ep
processWidgetResult comp state wenv widgetComposite (WidgetResult reqs evts evtsRoot) = WidgetResult newReqs newEvts uWidget where
  CompositeState{..} = state
  evtUpdates = getUpdateUserStates reqs
  evtModel = foldr (.) id evtUpdates _compositeModel
  ReducedEvents newModel _ messages reports tasks producers = reduceCompositeEvents _compositeGlobalKeys (_eventHandler comp) evtModel evts
  WidgetResult uReqs uEvts uWidget = updateComposite comp state wenv newModel evtsRoot widgetComposite
  currentPath = _instancePath widgetComposite
  newReqs = convertRequests reqs
         <> convertTasksToRequests currentPath tasks
         <> convertProducersToRequests currentPath producers
         <> convertRequests uReqs
         <> messages
  newEvts = reports <> uEvts

updateComposite :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetEnv sp ep -> s -> WidgetInstance s e -> WidgetInstance sp ep -> WidgetResult sp ep
updateComposite comp state wenv newModel oldRoot widgetComposite = if modelChanged then processedResult else updatedResult where
  CompositeState{..} = state
  widget = _instanceWidget _compositeRoot
  modelChanged = _compositeModel /= newModel
  builtRoot = _uiBuilder comp newModel
  cwenv = convertWidgetEnv wenv _compositeGlobalKeys newModel
  mergedResult = _widgetMerge (_instanceWidget builtRoot) cwenv oldRoot builtRoot
  mergedState = state {
    _compositeModel = newModel,
    _compositeRoot = _resultWidget mergedResult
  }
  processedResult = processWidgetResult comp mergedState wenv widgetComposite mergedResult
  updatedResult = updateCompositeSize comp state wenv newModel oldRoot widgetComposite

updateCompositeSize :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetEnv sp ep -> s -> WidgetInstance s e -> WidgetInstance sp ep -> WidgetResult sp ep
updateCompositeSize comp state wenv newModel oldRoot widgetComposite = resultWidget newInstance where
  CompositeState{..} = state
  viewport = _instanceViewport widgetComposite
  renderArea = _instanceRenderArea widgetComposite
  cwenv = convertWidgetEnv wenv _compositeGlobalKeys newModel
  newRoot = _widgetResize (_instanceWidget oldRoot) cwenv viewport renderArea oldRoot _compositeSizeReq
  newState = state {
    _compositeModel = newModel,
    _compositeRoot = newRoot
  }
  newInstance = widgetComposite {
    _instanceWidget = createComposite comp newState
  }

reduceCompositeEvents :: GlobalKeys s e -> EventHandler s e ep -> s -> Seq e -> ReducedEvents s e sp ep
reduceCompositeEvents globalKeys eventHandler model events = foldl' reducer initial events where
  initial = ReducedEvents model Seq.empty Seq.empty Seq.empty Seq.empty Seq.empty
  reducer current event = foldl' reducer newCurrent newEvents where
    processed = convertResponse globalKeys current (eventHandler (_reModel current) event)
    newEvents = _reEvents processed
    newCurrent = processed { _reEvents = Seq.empty }

convertResponse :: GlobalKeys s e -> ReducedEvents s e sp ep -> EventResponse s e ep -> ReducedEvents s e sp ep
convertResponse globalKeys current@ReducedEvents{..} response = case response of
  Model newModel -> current { _reModel = newModel }
  Event event -> current { _reEvents = _reEvents |> event }
  Message key message -> case M.lookup key globalKeys of
    Just inst -> current {
        _reMessages = _reMessages |> SendMessage (_instancePath inst) message
      }
    Nothing -> current
  Report report -> current { _reReports = _reReports |> report }
  Task task -> current { _reTasks = _reTasks |> task }
  Producer producer -> current { _reProducers = _reProducers |> producer }
  Multiple ehs -> foldl' (convertResponse globalKeys) current ehs

convertTasksToRequests :: Typeable e => Path -> Seq (IO e) -> Seq (WidgetRequest sp)
convertTasksToRequests path reqs = RunTask path <$> reqs

convertProducersToRequests :: Typeable e => Path -> Seq (ProducerHandler e) -> Seq (WidgetRequest sp)
convertProducersToRequests path reqs = RunProducer path <$> reqs

convertRequests :: Seq (WidgetRequest s) -> Seq (WidgetRequest sp)
convertRequests reqs = fmap fromJust $ Seq.filter isJust $ fmap convertRequest reqs

convertRequest :: WidgetRequest s -> Maybe (WidgetRequest sp)
convertRequest IgnoreParentEvents = Just IgnoreParentEvents
convertRequest IgnoreChildrenEvents = Just IgnoreChildrenEvents
convertRequest Resize = Just Resize
convertRequest (SetFocus path) = Just (SetFocus path)
convertRequest (GetClipboard path) = Just (GetClipboard path)
convertRequest (SetClipboard clipboard) = Just (SetClipboard clipboard)
convertRequest ResetOverlay = Just ResetOverlay
convertRequest (SetOverlay path) = Just (SetOverlay path)
convertRequest (SendMessage path message) = Just (SendMessage path message)
convertRequest (RunTask path action) = Just (RunTask path action)
convertRequest (RunProducer path action) = Just (RunProducer path action)
convertRequest (UpdateUserState fn) = Nothing

-- | Custom Handling
compositeHandleMessage :: (Eq s, Typeable i, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetEnv sp ep -> Path -> i -> WidgetInstance sp ep -> Maybe (WidgetResult sp ep)
compositeHandleMessage comp state@CompositeState{..} wenv target arg widgetComposite
  | isTargetReached target widgetComposite = case cast arg of
      Just evt -> Just $ processWidgetResult comp state wenv widgetComposite evtResult where
        evtResult = WidgetResult Seq.empty (Seq.singleton evt) _compositeRoot
      Nothing -> Nothing
  | otherwise = fmap processEvent result where
      processEvent = processWidgetResult comp state wenv widgetComposite
      cwenv = convertWidgetEnv wenv _compositeGlobalKeys _compositeModel
      result = _widgetHandleMessage (_instanceWidget _compositeRoot) cwenv target arg _compositeRoot

-- Preferred size
compositePreferredSize :: CompositeState s e -> WidgetEnv sp ep -> WidgetInstance sp ep -> Tree SizeReq
compositePreferredSize state wenv _ = _widgetPreferredSize widget cwenv _compositeRoot where
  CompositeState{..} = state
  widget = _instanceWidget _compositeRoot
  cwenv = convertWidgetEnv wenv _compositeGlobalKeys _compositeModel

-- Resize
compositeResize :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetEnv sp ep -> Rect -> Rect -> WidgetInstance sp ep -> Tree SizeReq -> WidgetInstance sp ep
compositeResize comp state wenv viewport renderArea widgetComposite reqs = newInstance where
  CompositeState{..} = state
  widget = _instanceWidget _compositeRoot
  cwenv = convertWidgetEnv wenv _compositeGlobalKeys _compositeModel
  newRoot = _widgetResize widget cwenv viewport renderArea _compositeRoot reqs
  newState = state {
    _compositeRoot = newRoot,
    _compositeSizeReq = reqs
  }
  newInstance = widgetComposite {
    _instanceWidget = createComposite comp newState,
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea
  }

-- Render
compositeRender :: (Monad m) => Composite s e ep -> CompositeState s e -> Renderer m -> WidgetEnv sp ep -> WidgetInstance sp ep -> m ()
compositeRender comp state renderer wenv _ = _widgetRender widget renderer cwenv _compositeRoot where
  CompositeState{..} = state
  widget = _instanceWidget _compositeRoot
  cwenv = convertWidgetEnv wenv _compositeGlobalKeys _compositeModel

collectGlobalKeys :: Map WidgetKey (WidgetInstance s e) -> WidgetInstance s e -> Map WidgetKey (WidgetInstance s e)
collectGlobalKeys keys widgetInst = foldl' collect updatedMap children where
  children = _instanceChildren widgetInst
  collect currKeys child = collectGlobalKeys currKeys child
  updatedMap = case _instanceKey widgetInst of
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
  parentPath = _instancePath parent
  parentVisible = _instanceVisible parent
  parentEnabled = _instanceEnabled parent
  newChild = child {
    _instancePath = parentPath |> 0,
    _instanceVisible = _instanceVisible child && parentVisible,
    _instanceEnabled = _instanceEnabled child && parentEnabled
  }
