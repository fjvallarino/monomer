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
  CompositeState app widgetRoot _ _ _ = state
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

compositeInit :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> sp -> WidgetInstance sp ep -> WidgetResult sp ep
compositeInit comp state ctx pApp widgetComposite = result where
  CompositeState app widgetRoot initEvent _ _ = state
  WidgetResult reqs evts root = _widgetInit (_instanceWidget widgetRoot) (childContext ctx) app widgetRoot
  newEvts = maybe evts (evts |>) initEvent
  newState = state {
    _compositeGlobalKeys = collectGlobalKeys M.empty (childContext ctx) widgetRoot
  }
  result = processWidgetResult comp newState ctx widgetComposite (WidgetResult reqs newEvts root)

compositeMerge :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> GlobalKeys sp ep -> PathContext -> sp -> WidgetInstance sp ep -> WidgetInstance sp ep -> WidgetResult sp ep
compositeMerge comp state _ ctx pApp newComposite oldComposite = result where
  oldState = _widgetGetState (_instanceWidget oldComposite) pApp
  validState = fromMaybe state (useState oldState)
  CompositeState oldApp oldRoot oldInit oldGlobalKeys oldReqs = validState
  -- Duplicate widget tree creation is avoided because the widgetRoot created on _composite_ has not yet been evaluated
  newRoot = _uiBuilder comp oldApp
  newState = validState {
    _compositeRoot = newRoot,
    _compositeGlobalKeys = collectGlobalKeys M.empty (childContext ctx) newRoot
  }
  widgetResult = if instanceMatches newRoot oldRoot
                  then _widgetMerge (_instanceWidget newRoot) oldGlobalKeys (childContext ctx) oldApp newRoot oldRoot
                  else _widgetInit (_instanceWidget newRoot) (childContext ctx) oldApp newRoot
  result = processWidgetResult comp newState ctx newComposite widgetResult

compositeNextFocusable :: CompositeState s e -> PathContext -> WidgetInstance sp ep -> Maybe Path
compositeNextFocusable CompositeState{..} ctx widgetComposite =
  _widgetNextFocusable (_instanceWidget _compositeRoot) (childContext ctx) _compositeRoot

compositeFind :: CompositeState s e -> Point -> WidgetInstance sp ep -> Maybe Path
compositeFind CompositeState{..} point widgetComposite = fmap (0 <|) childPath where
  childPath = _widgetFind (_instanceWidget _compositeRoot) point _compositeRoot

compositeHandleEvent :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> SystemEvent -> sp -> WidgetInstance sp ep -> Maybe (WidgetResult sp ep)
compositeHandleEvent comp state ctx evt pApp widgetComposite = fmap processEvent result where
  CompositeState{..} = state
  processEvent = processWidgetResult comp state ctx widgetComposite
  result = _widgetHandleEvent (_instanceWidget _compositeRoot) (childContext ctx) evt _compositeApp _compositeRoot

processWidgetResult :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> WidgetInstance sp ep -> WidgetResult s e -> WidgetResult sp ep
processWidgetResult comp state ctx widgetComposite (WidgetResult reqs evts evtsRoot) = WidgetResult newReqs newEvts uWidget where
  CompositeState{..} = state
  evtStates = getUpdateUserStates reqs
  evtApp = foldr (.) id evtStates _compositeApp
  ReducedEvents newApp _ messages reports tasks producers = reduceCompositeEvents _compositeGlobalKeys (_eventHandler comp) evtApp evts
  WidgetResult uReqs uEvts uWidget = updateComposite comp state ctx newApp evtsRoot widgetComposite
  newReqs = convertRequests reqs
         <> convertTasksToRequests ctx tasks
         <> convertProducersToRequests ctx producers
         <> convertRequests uReqs
         <> messages
  newEvts = reports <> uEvts

updateComposite :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> s -> WidgetInstance s e -> WidgetInstance sp ep -> WidgetResult sp ep
updateComposite comp state ctx newApp oldRoot widgetComposite = if appChanged then processedResult else updatedResult where
  CompositeState{..} = state
  appChanged = _compositeApp /= newApp
  builtRoot = _uiBuilder comp newApp
  mergedResult = _widgetMerge (_instanceWidget builtRoot) _compositeGlobalKeys (childContext ctx) newApp builtRoot oldRoot
  mergedState = state {
    _compositeApp = newApp,
    _compositeRoot = _resultWidget mergedResult
  }
  processedResult = processWidgetResult comp mergedState ctx widgetComposite mergedResult
  updatedResult = updateCompositeSize comp state ctx newApp oldRoot widgetComposite

updateCompositeSize :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> s -> WidgetInstance s e -> WidgetInstance sp ep -> WidgetResult sp ep
updateCompositeSize comp state ctx newApp oldRoot widgetComposite = resultWidget newInstance where
  CompositeState{..} = state
  viewport = _instanceViewport widgetComposite
  renderArea = _instanceRenderArea widgetComposite
  newRoot = _widgetResize (_instanceWidget oldRoot) newApp viewport renderArea oldRoot _compositeSizeReq
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
convertRequest (ResetOverlay path) = Just (ResetOverlay path)
convertRequest (SetOverlay path) = Just (SetOverlay path)
convertRequest (SendMessage path message) = Just (SendMessage path message)
convertRequest (RunTask path action) = Just (RunTask path action)
convertRequest (RunProducer path action) = Just (RunProducer path action)
convertRequest (UpdateUserState fn) = Nothing

-- | Custom Handling
compositeHandleMessage :: (Eq s, Typeable i, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> i -> sp -> WidgetInstance sp ep -> Maybe (WidgetResult sp ep)
compositeHandleMessage comp state ctx arg app widgetComposite
  | isTargetReached ctx = case cast arg of
      Just evt -> Just $ processWidgetResult comp state ctx widgetComposite evtResult where
        evtResult = WidgetResult Seq.empty (Seq.singleton evt) (_compositeRoot state)
      Nothing -> Nothing
  | otherwise = fmap processEvent result where
      CompositeState app widgetRoot _ _ _ = state
      processEvent = processWidgetResult comp state ctx widgetComposite
      nextCtx = fromJust $ moveToTarget ctx
      result = _widgetHandleMessage (_instanceWidget widgetRoot) nextCtx arg app widgetRoot

-- Preferred size
compositePreferredSize :: (Monad m) => CompositeState s e -> Renderer m -> sp -> WidgetInstance sp ep -> Tree SizeReq
compositePreferredSize CompositeState{..} renderer _ _ =
  _widgetPreferredSize (_instanceWidget _compositeRoot) renderer _compositeApp _compositeRoot

-- Resize
compositeResize :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> sp -> Rect -> Rect -> WidgetInstance sp ep -> Tree SizeReq -> WidgetInstance sp ep
compositeResize comp state _ viewport renderArea widgetComposite reqs = newInstance where
  CompositeState app widgetRoot _ globalKeys _ = state
  newRoot = _widgetResize (_instanceWidget widgetRoot) app viewport renderArea widgetRoot reqs
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
compositeRender :: (Monad m) => CompositeState s e -> Renderer m -> Timestamp -> PathContext -> sp -> WidgetInstance sp ep -> m ()
compositeRender CompositeState{..} renderer ts ctx _ _ =
  _widgetRender (_instanceWidget _compositeRoot) renderer ts (childContext ctx) _compositeApp _compositeRoot

childContext :: PathContext -> PathContext
childContext ctx = addToCurrent ctx 0

collectGlobalKeys :: Map WidgetKey (Path, WidgetInstance s e) -> PathContext -> WidgetInstance s e -> Map WidgetKey (Path, WidgetInstance s e)
collectGlobalKeys keys ctx widgetInstance = foldl' collectFn updatedMap pairs where
  children = _instanceChildren widgetInstance
  ctxs = Seq.fromList $ fmap (addToCurrent ctx) [0..length children]
  pairs = Seq.zip ctxs children
  collectFn current (ctxChild, child) = collectGlobalKeys current ctxChild child
  updatedMap = case _instanceKey widgetInstance of
    Just key -> M.insert key (_pathCurrent ctx, widgetInstance) keys
    _ -> keys
