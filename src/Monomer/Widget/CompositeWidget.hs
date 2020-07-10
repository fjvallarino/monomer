{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

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
import Monomer.Widget.PathContext
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
  _compositeApp :: s,
  _compositeRoot :: WidgetInstance s e,
  _compositeInitEvent :: Maybe e,
  _compositeGlobalKeys :: GlobalKeys s e,
  _compositeSizeReq :: Tree SizeReq
}

data ReducedEvents s e sp ep = ReducedEvents {
  _reApp :: s,
  _reEvents :: Seq e,
  _reMessages :: Seq (WidgetRequest sp),
  _reReports :: Seq ep,
  _reTasks :: Seq (TaskHandler e),
  _reProducers :: Seq (ProducerHandler e)
}

composite :: (Eq s, Typeable s, Typeable e) => WidgetType -> s -> Maybe e -> EventHandler s e ep -> UIBuilder s e -> WidgetInstance sp ep
composite widgetType app initEvent eventHandler uiBuilder = defaultWidgetInstance widgetType widget where
  widgetRoot = uiBuilder app
  composite = Composite widgetType eventHandler uiBuilder
  state = CompositeState app widgetRoot initEvent M.empty (singleNode def)
  widget = createComposite composite state

createComposite :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> Widget sp ep
createComposite comp state = widget where
  widget = Widget {
    _widgetInit = compositeInit comp state,
    _widgetGetState = makeState state,
    _widgetMerge = compositeMerge comp state,
    _widgetNextFocusable = compositeNextFocusable state,
    _widgetFind = compositeFind state,
    _widgetHandleEvent = compositeHandleEvent comp state,
    _widgetHandleMessage = compositeHandleMessage comp state,
    _widgetPreferredSize = compositePreferredSize state,
    _widgetResize = compositeResize comp state,
    _widgetRender = compositeRender state
  }

compositeInit :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetContext sp ep -> PathContext -> WidgetInstance sp ep -> WidgetResult sp ep
compositeInit comp state wctx ctx widgetComposite = result where
  CompositeState{..} = state
  widget = _instanceWidget _compositeRoot
  cwctx = convertWidgetContext wctx _compositeGlobalKeys _compositeApp
  cctx = childContext ctx
  WidgetResult reqs evts root = _widgetInit widget cwctx cctx _compositeRoot
  newEvts = maybe evts (evts |>) _compositeInitEvent
  newState = state {
    _compositeGlobalKeys = collectGlobalKeys M.empty cctx _compositeRoot
  }
  result = processWidgetResult comp newState wctx ctx widgetComposite (WidgetResult reqs newEvts root)

compositeMerge :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetContext sp ep -> PathContext -> WidgetInstance sp ep -> WidgetInstance sp ep -> WidgetResult sp ep
compositeMerge comp state wctx ctx newComposite oldComposite = result where
  oldState = _widgetGetState (_instanceWidget oldComposite) wctx
  validState = fromMaybe state (useState oldState)
  CompositeState oldApp oldRoot oldInit oldGlobalKeys oldReqs = validState
  -- Duplicate widget tree creation is avoided because the widgetRoot created on _composite_ has not yet been evaluated
  newRoot = _uiBuilder comp oldApp
  newState = validState {
    _compositeRoot = newRoot,
    _compositeGlobalKeys = collectGlobalKeys M.empty (childContext ctx) newRoot
  }
  newWidget = _instanceWidget newRoot
  cwctx = convertWidgetContext wctx oldGlobalKeys oldApp
  cctx = childContext ctx
  widgetResult = if instanceMatches newRoot oldRoot
                  then _widgetMerge newWidget cwctx cctx newRoot oldRoot
                  else _widgetInit newWidget cwctx cctx newRoot
  result = processWidgetResult comp newState wctx ctx newComposite widgetResult

compositeNextFocusable :: CompositeState s e -> PathContext -> WidgetInstance sp ep -> Maybe Path
compositeNextFocusable CompositeState{..} ctx widgetComposite =
  _widgetNextFocusable (_instanceWidget _compositeRoot) (childContext ctx) _compositeRoot

compositeFind :: CompositeState s e -> Path -> Point -> WidgetInstance sp ep -> Maybe Path
compositeFind CompositeState{..} path point widgetComposite
  | validStep = fmap (0 <|) childPath
  | otherwise = Nothing
  where
    validStep = Seq.null path || Seq.index path 0 == 0
    newPath = Seq.drop 1 path
    childPath = _widgetFind (_instanceWidget _compositeRoot) newPath point _compositeRoot

compositeHandleEvent :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetContext sp ep -> PathContext -> SystemEvent -> WidgetInstance sp ep -> Maybe (WidgetResult sp ep)
compositeHandleEvent comp state wctx ctx evt widgetComposite = fmap processEvent result where
  CompositeState{..} = state
  widget = _instanceWidget _compositeRoot
  cwctx = convertWidgetContext wctx _compositeGlobalKeys _compositeApp
  cctx = childContext ctx
  processEvent = processWidgetResult comp state wctx ctx widgetComposite
  result = _widgetHandleEvent widget cwctx cctx evt _compositeRoot

processWidgetResult :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetContext sp ep -> PathContext -> WidgetInstance sp ep -> WidgetResult s e -> WidgetResult sp ep
processWidgetResult comp state wctx ctx widgetComposite (WidgetResult reqs evts evtsRoot) = WidgetResult newReqs newEvts uWidget where
  CompositeState{..} = state
  evtStates = getUpdateUserStates reqs
  evtApp = foldr (.) id evtStates _compositeApp
  ReducedEvents newApp _ messages reports tasks producers = reduceCompositeEvents _compositeGlobalKeys (_eventHandler comp) evtApp evts
  WidgetResult uReqs uEvts uWidget = updateComposite comp state wctx ctx newApp evtsRoot widgetComposite
  newReqs = convertRequests reqs
         <> convertTasksToRequests ctx tasks
         <> convertProducersToRequests ctx producers
         <> convertRequests uReqs
         <> messages
  newEvts = reports <> uEvts

updateComposite :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetContext sp ep -> PathContext -> s -> WidgetInstance s e -> WidgetInstance sp ep -> WidgetResult sp ep
updateComposite comp state wctx ctx newApp oldRoot widgetComposite = if appChanged then processedResult else updatedResult where
  CompositeState{..} = state
  widget = _instanceWidget _compositeRoot
  cwctx = convertWidgetContext wctx _compositeGlobalKeys newApp
  cctx = childContext ctx
  appChanged = _compositeApp /= newApp
  builtRoot = _uiBuilder comp newApp
  mergedResult = _widgetMerge (_instanceWidget builtRoot) cwctx cctx builtRoot oldRoot
  mergedState = state {
    _compositeApp = newApp,
    _compositeRoot = _resultWidget mergedResult
  }
  processedResult = processWidgetResult comp mergedState wctx ctx widgetComposite mergedResult
  updatedResult = updateCompositeSize comp state wctx ctx newApp oldRoot widgetComposite

updateCompositeSize :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetContext sp ep -> PathContext -> s -> WidgetInstance s e -> WidgetInstance sp ep -> WidgetResult sp ep
updateCompositeSize comp state wctx ctx newApp oldRoot widgetComposite = resultWidget newInstance where
  CompositeState{..} = state
  viewport = _instanceViewport widgetComposite
  renderArea = _instanceRenderArea widgetComposite
  cwctx = convertWidgetContext wctx _compositeGlobalKeys newApp
  cctx = childContext ctx
  newRoot = _widgetResize (_instanceWidget oldRoot) cwctx viewport renderArea oldRoot _compositeSizeReq
  newState = state {
    _compositeApp = newApp,
    _compositeRoot = newRoot
  }
  newInstance = widgetComposite {
    _instanceWidget = createComposite comp newState
  }

reduceCompositeEvents :: GlobalKeys s e -> EventHandler s e ep -> s -> Seq e -> ReducedEvents s e sp ep
reduceCompositeEvents globalKeys appEventHandler app events = foldl' reducer initial events where
  initial = ReducedEvents app Seq.empty Seq.empty Seq.empty Seq.empty Seq.empty
  reducer current event = foldl' reducer newCurrent newEvents where
    processed = convertResponse globalKeys current (appEventHandler (_reApp current) event)
    newEvents = _reEvents processed
    newCurrent = processed { _reEvents = Seq.empty }

convertResponse :: GlobalKeys s e -> ReducedEvents s e sp ep -> EventResponse s e ep -> ReducedEvents s e sp ep
convertResponse globalKeys current@ReducedEvents{..} response = case response of
  Model newApp -> current { _reApp = newApp }
  Event event -> current { _reEvents = _reEvents |> event }
  Message key message -> case M.lookup key globalKeys of
    Just (path, _) -> current { _reMessages = _reMessages |> SendMessage path message }
    Nothing -> current
  Report report -> current { _reReports = _reReports |> report }
  Task task -> current { _reTasks = _reTasks |> task }
  Producer producer -> current { _reProducers = _reProducers |> producer }
  Multiple ehs -> foldl' (convertResponse globalKeys) current ehs

convertTasksToRequests :: Typeable e => PathContext -> Seq (IO e) -> Seq (WidgetRequest sp)
convertTasksToRequests ctx reqs = flip fmap reqs $ \req -> RunTask (_pathCurrent ctx) req

convertProducersToRequests :: Typeable e => PathContext -> Seq ((e -> IO ()) -> IO ()) -> Seq (WidgetRequest sp)
convertProducersToRequests ctx reqs = flip fmap reqs $ \req -> RunProducer (_pathCurrent ctx) req

convertRequests :: Seq (WidgetRequest s) -> Seq (WidgetRequest sp)
convertRequests reqs = fmap fromJust $ Seq.filter isJust $ fmap convertRequest reqs

convertRequest :: WidgetRequest s -> Maybe (WidgetRequest s2)
convertRequest IgnoreParentEvents = Just IgnoreParentEvents
convertRequest IgnoreChildrenEvents = Just IgnoreChildrenEvents
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
compositeHandleMessage :: (Eq s, Typeable i, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetContext sp ep -> PathContext -> i -> WidgetInstance sp ep -> Maybe (WidgetResult sp ep)
compositeHandleMessage comp state@CompositeState{..} wctx ctx arg widgetComposite
  | isTargetReached ctx = case cast arg of
      Just evt -> Just $ processWidgetResult comp state wctx ctx widgetComposite evtResult where
        evtResult = WidgetResult Seq.empty (Seq.singleton evt) _compositeRoot
      Nothing -> Nothing
  | otherwise = fmap processEvent result where
      processEvent = processWidgetResult comp state wctx ctx widgetComposite
      cwctx = convertWidgetContext wctx _compositeGlobalKeys _compositeApp
      nextCtx = fromJust $ moveToTarget ctx
      result = _widgetHandleMessage (_instanceWidget _compositeRoot) cwctx nextCtx arg _compositeRoot

-- Preferred size
compositePreferredSize :: (Monad m) => CompositeState s e -> Renderer m -> WidgetContext sp ep -> WidgetInstance sp ep -> Tree SizeReq
compositePreferredSize CompositeState{..} renderer wctx _ = _widgetPreferredSize widget renderer cwctx _compositeRoot where
  widget = _instanceWidget _compositeRoot
  cwctx = convertWidgetContext wctx _compositeGlobalKeys _compositeApp

-- Resize
compositeResize :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> WidgetContext sp ep -> Rect -> Rect -> WidgetInstance sp ep -> Tree SizeReq -> WidgetInstance sp ep
compositeResize comp state wctx viewport renderArea widgetComposite reqs = newInstance where
  CompositeState{..} = state
  widget = _instanceWidget _compositeRoot
  cwctx = convertWidgetContext wctx _compositeGlobalKeys _compositeApp
  newRoot = _widgetResize widget cwctx viewport renderArea _compositeRoot reqs
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
compositeRender :: (Monad m) => CompositeState s e -> Renderer m -> WidgetContext sp ep -> PathContext -> WidgetInstance sp ep -> m ()
compositeRender CompositeState{..} renderer wctx ctx _ = _widgetRender widget renderer cwctx cctx _compositeRoot where
  widget = _instanceWidget _compositeRoot
  cwctx = convertWidgetContext wctx _compositeGlobalKeys _compositeApp
  cctx = childContext ctx

collectGlobalKeys :: Map WidgetKey (Path, WidgetInstance s e) -> PathContext -> WidgetInstance s e -> Map WidgetKey (Path, WidgetInstance s e)
collectGlobalKeys keys ctx widgetInstance = foldl' collectFn updatedMap pairs where
  children = _instanceChildren widgetInstance
  ctxs = Seq.fromList $ fmap (addToCurrent ctx) [0..length children]
  pairs = Seq.zip ctxs children
  collectFn current (ctxChild, child) = collectGlobalKeys current ctxChild child
  updatedMap = case _instanceKey widgetInstance of
    Just key -> M.insert key (_pathCurrent ctx, widgetInstance) keys
    _ -> keys

convertWidgetContext :: WidgetContext sp ep -> GlobalKeys s e -> s -> WidgetContext s e
convertWidgetContext wctx globalKeys app = WidgetContext {
  _wcScreenSize = _wcScreenSize wctx,
  _wcGlobalKeys = globalKeys,
  _wcApp = app,
  _wcInputStatus = _wcInputStatus wctx,
  _wcTimestamp = _wcTimestamp wctx
}
