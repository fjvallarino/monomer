{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.CompositeWidget (
  EventResponseC(..),
  composite
) where

import Debug.Trace

import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (|>), (<|), fromList)
import Data.Typeable (Typeable, cast, typeOf)

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
  er1 <> er2 = MultipleC (fromList [er1, er2])

data Composite s e ep = Composite {
  _widgetTypeC :: WidgetType,
  _eventHandlerC :: EventHandlerC s e ep,
  _uiBuilderC :: UIBuilderC s e,
  _sizeReqC :: Tree SizeReq
}

data CompositeState s e = CompositeState {
  _compositeApp :: s,
  _compositeRoot :: WidgetInstance s e
}

composite :: (Eq s, Typeable s, Typeable e) => WidgetType -> s -> EventHandlerC s e ep -> UIBuilderC s e -> WidgetInstance sp ep
composite widgetType app eventHandler uiBuilder = defaultWidgetInstance widgetType widget where
  widgetRoot = uiBuilder app
  composite = Composite widgetType eventHandler uiBuilder (singleNode def)
  state = CompositeState app widgetRoot
  widget = createComposite composite state

createComposite :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> Widget sp ep
createComposite comp state = widget where
  CompositeState app widgetRoot = state
  widget = Widget {
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

compositeMerge :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> sp -> WidgetInstance sp ep -> WidgetInstance sp ep -> WidgetInstance sp ep
compositeMerge comp state pApp newComposite oldComposite = newInstance where
  oldState = _widgetGetState (_instanceWidget oldComposite) pApp
  CompositeState oldApp oldRoot = fromMaybe state (useState oldState)
  -- Duplicate widget tree creation is avoided because the widgetRoot created on _composite_ has not yet been evaluated
  newRoot = _uiBuilderC comp oldApp
  widgetRoot = _widgetMerge (_instanceWidget newRoot) oldApp newRoot oldRoot
  newState = CompositeState oldApp widgetRoot
  newInstance = newComposite {
    _instanceWidget = createComposite comp newState
  }

compositeNextFocusable :: CompositeState s e -> PathContext -> WidgetInstance sp ep -> Maybe Path
compositeNextFocusable (CompositeState app widgetRoot) ctx widgetComposite = _widgetNextFocusable (_instanceWidget widgetRoot) ctx widgetRoot

compositeFind :: CompositeState s e -> Point -> WidgetInstance sp ep -> Maybe Path
compositeFind (CompositeState app widgetRoot) point widgetComposite = _widgetFind (_instanceWidget widgetRoot) point widgetRoot

compositeHandleEvent :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> SystemEvent -> sp -> WidgetInstance sp ep -> Maybe (EventResult sp ep)
compositeHandleEvent comp state ctx evt pApp widgetComposite = fmap processEvent result where
  CompositeState app widgetRoot = state
  processEvent = processEventResult comp state ctx widgetComposite
  result = _widgetHandleEvent (_instanceWidget widgetRoot) ctx evt app widgetRoot

processEventResult :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> WidgetInstance sp ep -> EventResult s e -> EventResult sp ep
processEventResult comp state ctx widgetComposite (EventResult reqs evts evtsRoot) = EventResult newReqs messages newInstance where
  CompositeState app widgetRoot = state
  evtStates = getUpdateUserStates reqs
  evtApp = foldr (.) id evtStates app
  ReducedEvents newApp _ messages tasks producers = reduceCompositeEvents (_eventHandlerC comp) evtApp evts
  newReqs = convertRequests reqs
         <> convertTasksToRequests ctx tasks
         <> convertProducersToRequests ctx producers
  newInstance = updateComposite comp app newApp evtsRoot widgetComposite

updateComposite :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> s -> s -> WidgetInstance s e -> WidgetInstance sp ep -> WidgetInstance sp ep
updateComposite comp app newApp oldRoot widgetComposite = newInstance where
  builtRoot = _uiBuilderC comp newApp
  tempRoot = if | app /= newApp -> _widgetMerge (_instanceWidget builtRoot) newApp builtRoot oldRoot
                | otherwise -> oldRoot
  viewport = _instanceViewport widgetComposite
  renderArea = _instanceRenderArea widgetComposite
  newRoot = _widgetResize (_instanceWidget tempRoot) newApp viewport renderArea tempRoot (_sizeReqC comp)
  newState = CompositeState newApp newRoot
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
convertTasksToRequests ctx reqs = flip fmap reqs $ \req -> RunCustom (_pathCurrent ctx) req

convertProducersToRequests :: Typeable e => PathContext -> Seq ((e -> IO ()) -> IO ()) -> Seq (EventRequest sp)
convertProducersToRequests ctx reqs = flip fmap reqs $ \req -> RunProducer (_pathCurrent ctx) req

-- | Custom Handling
compositeHandleCustom :: (Eq s, Typeable i, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> i -> sp -> WidgetInstance sp ep -> Maybe (EventResult sp ep)
compositeHandleCustom comp state ctx arg app widgetComposite
  | isTargetReached ctx = case cast arg of
      Just evt -> handleCustomHelper comp state ctx widgetComposite evt
      Nothing -> Nothing
  | otherwise = fmap processEvent result where
      CompositeState app widgetRoot = state
      processEvent = processEventResult comp state ctx widgetComposite
      result = _widgetHandleCustom (_instanceWidget widgetRoot) ctx arg app widgetRoot

handleCustomHelper :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> PathContext -> WidgetInstance sp ep -> e -> Maybe (EventResult sp ep)
handleCustomHelper comp state ctx widgetComposite res = Just $ processEventResult comp state ctx widgetComposite evtResult where
  evtResult = EventResult Seq.empty (Seq.singleton res) (_compositeRoot state)

-- Preferred size
compositePreferredSize :: (Monad m) => CompositeState s e -> Renderer m -> sp -> WidgetInstance sp ep -> Tree SizeReq
compositePreferredSize (CompositeState app widgetRoot) renderer _ _ = _widgetPreferredSize (_instanceWidget widgetRoot) renderer app widgetRoot

-- Resize
compositeResize :: (Eq s, Typeable s, Typeable e) => Composite s e ep -> CompositeState s e -> sp -> Rect -> Rect -> WidgetInstance sp ep -> Tree SizeReq -> WidgetInstance sp ep
compositeResize comp state _ viewport renderArea widgetComposite reqs = newInstance where
  CompositeState app widgetRoot = state
  newComp = comp { _sizeReqC = reqs }
  newRoot = _widgetResize (_instanceWidget widgetRoot) app viewport renderArea widgetRoot reqs
  newState = CompositeState app newRoot
  newInstance = widgetComposite {
    _instanceWidget = createComposite newComp newState,
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea
  }

-- Render
compositeRender :: (Monad m) => CompositeState s e -> Renderer m -> Timestamp -> PathContext -> sp -> WidgetInstance sp ep -> m ()
compositeRender (CompositeState app widgetRoot) renderer ts ctx _ _ = _widgetRender (_instanceWidget widgetRoot) renderer ts ctx app widgetRoot
