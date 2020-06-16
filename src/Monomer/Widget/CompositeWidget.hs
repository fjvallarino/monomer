{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.CompositeWidget (
  EventResponseC(..),
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

type UIBuilderC s e = s -> WidgetInstance s e
type EventHandlerC s e ep = s -> e -> EventResponseC s e ep

type TaskHandler e = IO (Maybe e)
type ProducerHandler e = (e -> IO ()) -> IO ()

data ReducedEvents s e ep = ReducedEvents {
  _reApp :: s,
  _reEvents :: Seq e,
  _reMessages :: Seq ep,
  _reTasks :: Seq (TaskHandler e),
  _reProducers :: Seq (ProducerHandler e)
}

data EventResponseC s e ep = StateC s
                           | EventC e
                           | TaskC (TaskHandler e)
                           | ProducerC (ProducerHandler e)
                           | MessageC ep
                           | MultipleC (Seq (EventResponseC s e ep))

instance Semigroup (EventResponseC s e ep) where
  MultipleC seq1 <> MultipleC seq2 = MultipleC (seq1 <> seq2)
  MultipleC seq1 <> er2 = MultipleC (seq1 |> er2)
  er1 <> MultipleC seq2 = MultipleC (er1 <| seq2)
  er1 <> er2 = MultipleC (Seq.empty |> er1 |> er2)

data Composite s e ep = Composite {
  _widgetTypeC :: WidgetType,
  _eventHandlerC :: EventHandlerC s e ep,
  _uiBuilderC :: UIBuilderC s e
}

data CompositeState s e = CompositeState {
  _compositeApp :: s,
  _compositeRoot :: WidgetInstance s e,
  _compositeInitEvent :: Maybe e,
  _compositeGlobalKeys :: GlobalKeys s e,
  _compositeSizeReq :: Tree SizeReq
}

composite :: (Eq s, Typeable s, Typeable e) => WidgetType -> s -> Maybe e -> EventHandlerC s e ep -> UIBuilderC s e -> WidgetInstance sp ep
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
    _widgetHandleCustom = compositeHandleCustom comp state,
    _widgetPreferredSize = compositePreferredSize state,
    _widgetResize = compositeResize comp state,
    _widgetRender = compositeRender state
  }

compositeInit :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> sp -> WidgetInstance sp ep -> EventResult sp ep
compositeInit comp state ctx pApp widgetComposite = result where
  CompositeState app widgetRoot initEvent _ _ = state
  EventResult reqs evts root = _widgetInit (_instanceWidget widgetRoot) (childContext ctx) app widgetRoot
  newEvts = maybe evts (evts |>) initEvent
  newState = state {
    _compositeGlobalKeys = collectGlobalKeys M.empty (childContext ctx) widgetRoot
  }
  result = processEventResult comp newState ctx widgetComposite (EventResult reqs newEvts root)

compositeMerge :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> GlobalKeys sp ep -> PathContext -> sp -> WidgetInstance sp ep -> WidgetInstance sp ep -> EventResult sp ep
compositeMerge comp state _ ctx pApp newComposite oldComposite = result where
  oldState = _widgetGetState (_instanceWidget oldComposite) pApp
  validState = fromMaybe state (useState oldState)
  CompositeState oldApp oldRoot oldInit oldGlobalKeys oldReqs = validState
  -- Duplicate widget tree creation is avoided because the widgetRoot created on _composite_ has not yet been evaluated
  newRoot = _uiBuilderC comp oldApp
  newState = validState {
    _compositeRoot = newRoot,
    _compositeGlobalKeys = collectGlobalKeys M.empty (childContext ctx) newRoot
  }
  eventResult = if instanceMatches newRoot oldRoot
                  then _widgetMerge (_instanceWidget newRoot) oldGlobalKeys (childContext ctx) oldApp newRoot oldRoot
                  else _widgetInit (_instanceWidget newRoot) (childContext ctx) oldApp newRoot
  result = processEventResult comp newState ctx newComposite eventResult

compositeNextFocusable :: CompositeState s e -> PathContext -> WidgetInstance sp ep -> Maybe Path
compositeNextFocusable CompositeState{..} ctx widgetComposite = _widgetNextFocusable (_instanceWidget _compositeRoot) (childContext ctx) _compositeRoot

compositeFind :: CompositeState s e -> Point -> WidgetInstance sp ep -> Maybe Path
compositeFind CompositeState{..} point widgetComposite = fmap (0 <|) childPath where
  childPath = _widgetFind (_instanceWidget _compositeRoot) point _compositeRoot

compositeHandleEvent :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> SystemEvent -> sp -> WidgetInstance sp ep -> Maybe (EventResult sp ep)
compositeHandleEvent comp state ctx evt pApp widgetComposite = fmap processEvent result where
  CompositeState{..} = state
  processEvent = processEventResult comp state ctx widgetComposite
  result = _widgetHandleEvent (_instanceWidget _compositeRoot) (childContext ctx) evt _compositeApp _compositeRoot

processEventResult :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> WidgetInstance sp ep -> EventResult s e -> EventResult sp ep
processEventResult comp state ctx widgetComposite (EventResult reqs evts evtsRoot) = EventResult newReqs newEvts uWidget where
  CompositeState{..} = state
  evtStates = getUpdateUserStates reqs
  evtApp = foldr (.) id evtStates _compositeApp
  ReducedEvents newApp _ messages tasks producers = reduceCompositeEvents (_eventHandlerC comp) evtApp evts
  EventResult uReqs uEvts uWidget = updateComposite comp state ctx newApp evtsRoot widgetComposite
  newReqs = convertRequests reqs
         <> convertTasksToRequests ctx tasks
         <> convertProducersToRequests ctx producers
         <> convertRequests uReqs
  newEvts = messages <> uEvts

updateComposite :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> s -> WidgetInstance s e -> WidgetInstance sp ep -> EventResult sp ep
updateComposite comp state ctx newApp oldRoot widgetComposite = if appChanged then processedResult else updatedResult where
  CompositeState{..} = state
  appChanged = _compositeApp /= newApp
  builtRoot = _uiBuilderC comp newApp
  mergedResult = _widgetMerge (_instanceWidget builtRoot) _compositeGlobalKeys (childContext ctx) newApp builtRoot oldRoot
  mergedState = state {
    _compositeApp = newApp,
    _compositeRoot = _eventResultNewWidget mergedResult
  }
  processedResult = processEventResult comp mergedState ctx widgetComposite mergedResult
  updatedResult = updateCompositeSize comp state ctx newApp oldRoot widgetComposite

updateCompositeSize :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> s -> WidgetInstance s e -> WidgetInstance sp ep -> EventResult sp ep
updateCompositeSize comp state ctx newApp oldRoot widgetComposite = rWidget newInstance where
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

reduceCompositeEvents :: EventHandlerC s e ep -> s -> Seq e -> ReducedEvents s e ep
reduceCompositeEvents appEventHandler app events = foldl' reducer initial events where
  initial = ReducedEvents app Seq.empty Seq.empty Seq.empty Seq.empty
  reducer current event = foldl' reducer newCurrent newEvents where
    processed = convertResponse current (appEventHandler (_reApp current) event)
    newEvents = _reEvents processed
    newCurrent = processed { _reEvents = Seq.empty }

convertResponse :: ReducedEvents s e ep -> EventResponseC s e ep -> ReducedEvents s e ep
convertResponse current@ReducedEvents{..} response = case response of
  StateC newApp -> current { _reApp = newApp }
  EventC event -> current { _reEvents = _reEvents |> event }
  MessageC message -> current { _reMessages = _reMessages |> message }
  TaskC task -> current { _reTasks = _reTasks |> task }
  ProducerC producer -> current { _reProducers = _reProducers |> producer }
  MultipleC ehs -> foldl' convertResponse current ehs

convertTasksToRequests :: Typeable e => PathContext -> Seq (IO e) -> Seq (EventRequest sp)
convertTasksToRequests ctx reqs = flip fmap reqs $ \req -> RunTask (_pathCurrent ctx) req

convertProducersToRequests :: Typeable e => PathContext -> Seq ((e -> IO ()) -> IO ()) -> Seq (EventRequest sp)
convertProducersToRequests ctx reqs = flip fmap reqs $ \req -> RunProducer (_pathCurrent ctx) req

-- | Custom Handling
compositeHandleCustom :: (Eq s, Typeable i, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> i -> sp -> WidgetInstance sp ep -> Maybe (EventResult sp ep)
compositeHandleCustom comp state ctx arg app widgetComposite
  | isTargetReached ctx = case cast arg of
      Just evt -> Just $ processEventResult comp state ctx widgetComposite evtResult where
        evtResult = EventResult Seq.empty (Seq.singleton evt) (_compositeRoot state)
      Nothing -> Nothing
  | otherwise = fmap processEvent result where
      CompositeState app widgetRoot _ _ _ = state
      processEvent = processEventResult comp state ctx widgetComposite
      nextCtx = fromJust $ moveToTarget ctx
      result = _widgetHandleCustom (_instanceWidget widgetRoot) nextCtx arg app widgetRoot

-- Preferred size
compositePreferredSize :: (Monad m) => CompositeState s e -> Renderer m -> sp -> WidgetInstance sp ep -> Tree SizeReq
compositePreferredSize CompositeState{..} renderer _ _ = _widgetPreferredSize (_instanceWidget _compositeRoot) renderer _compositeApp _compositeRoot

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
compositeRender CompositeState{..} renderer ts ctx _ _ = _widgetRender (_instanceWidget _compositeRoot) renderer ts (childContext ctx) _compositeApp _compositeRoot

childContext :: PathContext -> PathContext
childContext ctx = addToCurrent ctx 0
--
collectGlobalKeys :: Map WidgetKey (Path, WidgetInstance s e) -> PathContext -> WidgetInstance s e -> Map WidgetKey (Path, WidgetInstance s e)
collectGlobalKeys keys ctx widgetInstance = foldl' collectFn updatedMap pairs where
  children = _instanceChildren widgetInstance
  ctxs = Seq.fromList $ fmap (addToCurrent ctx) [0..length children]
  pairs = Seq.zip ctxs children
  collectFn current (ctx, child) = collectGlobalKeys current ctx child
  updatedMap = case _instanceKey widgetInstance of
    Just key -> M.insert key (rootPath, widgetInstance) keys
    _ -> keys
