{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  composite,
  composite_,
  compositeV,
  compositeV_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (%~), (<>~))
import Data.Default
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Sequence (Seq(..), (|>), (<|), fromList)
import Data.Typeable (Typeable, cast, typeOf)

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Spacer
import Monomer.Widgets.Util

import qualified Monomer.Lens as L

type ParentModel sp = Typeable sp
type CompositeModel s = (Eq s, Typeable s)
type CompositeEvent e = Typeable e

type EventHandler s e ep = s -> e -> [EventResponse s e ep]
type UIBuilder s e = s -> WidgetNode s e
type MergeRequired s = s -> s -> Bool
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

data CompositeCfg s ep sp = CompositeCfg {
  _cmcMergeRequired :: Maybe (MergeRequired s),
  _cmcOnChange :: [s -> ep],
  _cmcOnChangeReq :: [WidgetRequest sp]
}

instance Default (CompositeCfg s ep sp) where
  def = CompositeCfg {
    _cmcMergeRequired = Nothing,
    _cmcOnChange = [],
    _cmcOnChangeReq = []
  }

instance Semigroup (CompositeCfg s ep sp) where
  (<>) c1 c2 = CompositeCfg {
    _cmcMergeRequired = _cmcMergeRequired c2 <|> _cmcMergeRequired c1,
    _cmcOnChange = _cmcOnChange c2 <|> _cmcOnChange c1,
    _cmcOnChangeReq = _cmcOnChangeReq c2 <|> _cmcOnChangeReq c1
  }

instance Monoid (CompositeCfg s ep sp) where
  mempty = def

instance CmbMergeRequired (CompositeCfg s ep sp) s where
  mergeRequired fn = def {
    _cmcMergeRequired = Just fn
  }

instance CmbOnChange (CompositeCfg s ep sp) s ep where
  onChange fn = def {
    _cmcOnChange = [fn]
  }

instance CmbOnChangeReq (CompositeCfg s ep sp) sp where
  onChangeReq req = def {
    _cmcOnChangeReq = [req]
  }

data Composite s e sp ep = Composite {
  _cmpWidgetData :: WidgetData sp s,
  _cmpEventHandler :: EventHandler s e ep,
  _cmpUiBuilder :: UIBuilder s e,
  _cmpMergeRequired :: MergeRequired s,
  _cmpInitEvent :: Maybe e,
  _cmpOnChange :: [s -> ep],
  _cmpOnChangeReq :: [WidgetRequest sp]
}

data CompositeState s e sp = CompositeState {
  _cpsModel :: Maybe s,
  _cpsRoot :: WidgetNode s e,
  _cpsGlobalKeys :: GlobalKeys s e
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
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> ALens' sp s
  -> Maybe e
  -> EventHandler s e ep
  -> UIBuilder s e
  -> WidgetNode sp ep
composite widgetType field initEvt evtHandler uiBuilder = newNode where
  newNode = composite_ widgetType field initEvt evtHandler uiBuilder def

composite_
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> ALens' sp s
  -> Maybe e
  -> EventHandler s e ep
  -> UIBuilder s e
  -> [CompositeCfg s ep sp]
  -> WidgetNode sp ep
composite_ widgetType field initEvt evtHandler uiBuilder cfgs = newNode where
  widgetData = WidgetLens field
  newNode = compositeD_ widgetType widgetData initEvt evtHandler uiBuilder cfgs

compositeV
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> s
  -> (s -> ep)
  -> Maybe e
  -> EventHandler s e ep
  -> UIBuilder s e
  -> WidgetNode sp ep
compositeV wType val handler initEvt evtHandler uiBuilder = newNode where
  newNode = compositeV_ wType val handler initEvt evtHandler uiBuilder def

compositeV_
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> s
  -> (s -> ep)
  -> Maybe e
  -> EventHandler s e ep
  -> UIBuilder s e
  -> [CompositeCfg s ep sp]
  -> WidgetNode sp ep
compositeV_ wType val handler initEvt evtHandler uiBuilder cfgs = newNode where
  widgetData = WidgetValue val
  newCfgs = onChange handler : cfgs
  newNode = compositeD_ wType widgetData initEvt evtHandler uiBuilder newCfgs

compositeD_
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> WidgetData sp s
  -> Maybe e
  -> EventHandler s e ep
  -> UIBuilder s e
  -> [CompositeCfg s ep sp]
  -> WidgetNode sp ep
compositeD_ wType wData initEvt evtHandler uiBuilder configs = newNode where
  config = mconcat configs
  mergeReq = fromMaybe (/=) (_cmcMergeRequired config)
  widgetRoot = spacer
  composite = Composite {
    _cmpWidgetData = wData,
    _cmpEventHandler = evtHandler,
    _cmpUiBuilder = uiBuilder,
    _cmpMergeRequired = mergeReq,
    _cmpInitEvent = initEvt,
    _cmpOnChange = _cmcOnChange config,
    _cmpOnChangeReq = _cmcOnChangeReq config
  }
  state = CompositeState Nothing widgetRoot M.empty
  widget = createComposite composite state
  newNode = defaultWidgetNode wType widget

createComposite
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> Widget sp ep
createComposite comp state = widget where
  widget = Widget {
    widgetInit = compositeInit comp state,
    widgetMerge = compositeMerge comp state,
    widgetDispose = compositeDispose comp state,
    widgetGetState = makeState state,
    widgetGetInstanceTree = getInstanceTree,
    widgetFindNextFocus = compositeFindNextFocus comp state,
    widgetFindByPoint = compositeFindByPoint comp state,
    widgetHandleEvent = compositeHandleEvent comp state,
    widgetHandleMessage = compositeHandleMessage comp state,
    widgetGetSizeReq = compositeGetSizeReq comp state,
    widgetResize = compositeResize comp state,
    widgetRender = compositeRender comp state
  }

-- | Init
compositeInit
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetResult sp ep
compositeInit comp state wenv widgetComp = newResult where
  CompositeState{..} = state
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  -- Creates UI using provided function
  builtRoot = _cmpUiBuilder comp model
  tempRoot = cascadeCtx widgetComp builtRoot
  WidgetResult root reqs evts = widgetInit (tempRoot ^. L.widget) cwenv tempRoot
  newEvts = maybe evts (evts |>) (_cmpInitEvent comp)
  newState = state {
    _cpsModel = Just model,
    _cpsRoot = root,
    _cpsGlobalKeys = collectGlobalKeys M.empty root
  }
  tempResult = WidgetResult root reqs newEvts
  getBaseStyle wenv node = Nothing
  styledComp = initNodeStyle getBaseStyle wenv widgetComp
  newResult = reduceResult comp newState wenv styledComp tempResult

-- | Merge
compositeMerge
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetNode sp ep
  -> WidgetResult sp ep
compositeMerge comp state wenv oldComp newComp = newResult where
  oldState = widgetGetState (oldComp ^. L.widget) wenv
  validState = fromMaybe state (useState oldState)
  CompositeState oldModel oldRoot oldGlobalKeys = validState
  model = getModel comp wenv
  -- Creates new UI using provided function
  tempRoot = cascadeCtx newComp (_cmpUiBuilder comp model)
  tempWidget = tempRoot ^. L.widget
  cwenv = convertWidgetEnv wenv oldGlobalKeys model
  -- Needed in case the user references something outside model when building UI
  -- The same model is provided as old since nothing else is available, but
  -- mergeRequired may be using data from a closure
  mergeRequired
    | isJust oldModel = _cmpMergeRequired comp (fromJust oldModel) model
    | otherwise = True
  initRequired = not (instanceMatches tempRoot oldRoot)
  tempResult
    | initRequired = widgetInit tempWidget cwenv tempRoot
    | otherwise = widgetMerge tempWidget cwenv oldRoot tempRoot
  newRoot = _wrWidget tempResult
  newState = validState {
    _cpsModel = Just model,
    _cpsRoot = newRoot,
    _cpsGlobalKeys = collectGlobalKeys M.empty newRoot
  }
  getBaseStyle wenv node = Nothing
  styledComp = initNodeStyle getBaseStyle wenv newComp
  newResult
    | mergeRequired = reduceResult comp newState wenv styledComp tempResult
    | otherwise = resultWidget $ styledComp
        & L.widget .~ oldComp ^. L.widget

-- | Dispose
compositeDispose
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetResult sp ep
compositeDispose comp state wenv widgetComp = result where
  CompositeState{..} = state
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  widget = _cpsRoot ^. L.widget
  WidgetResult _ reqs evts = widgetDispose widget cwenv _cpsRoot
  tempResult = WidgetResult _cpsRoot reqs evts
  result = reduceResult comp state wenv widgetComp tempResult

-- | Next focusable
compositeFindNextFocus
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> FocusDirection
  -> Path
  -> WidgetNode sp ep
  -> Maybe Path
compositeFindNextFocus comp state wenv dir start widgetComp = nextFocus where
  CompositeState{..} = state
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  nextFocus = widgetFindNextFocus widget cwenv dir start _cpsRoot

-- | Find
compositeFindByPoint
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> Path
  -> Point
  -> WidgetNode sp ep
  -> Maybe Path
compositeFindByPoint comp state wenv startPath point widgetComp
  | widgetComp ^. L.info . L.visible && validStep = resultPath
  | otherwise = Nothing
  where
    CompositeState{..} = state
    widget = _cpsRoot ^. L.widget
    model = getModel comp wenv
    cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
    validStep = Seq.null startPath || Seq.index startPath 0 == 0
    newStartPath = Seq.drop 1 startPath
    resultPath = widgetFindByPoint widget cwenv newStartPath point _cpsRoot

-- | Event handling
compositeHandleEvent
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> Path
  -> SystemEvent
  -> WidgetNode sp ep
  -> Maybe (WidgetResult sp ep)
compositeHandleEvent comp state wenv target evt widgetComp = result where
  CompositeState{..} = state
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  rootEnabled = _cpsRoot ^. L.info . L.enabled
  compVisible = widgetComp ^. L.info . L.visible
  compEnabled = widgetComp ^. L.info . L.enabled
  processEvent = reduceResult comp state wenv widgetComp
  evtResult
    | not (compVisible && compEnabled) = Nothing
    | rootEnabled = widgetHandleEvent widget cwenv target evt _cpsRoot
    | otherwise = Nothing
  result = fmap processEvent evtResult

-- | Message handling
compositeHandleMessage
  :: (CompositeModel s, CompositeEvent e, ParentModel sp, Typeable i)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> Path
  -> i
  -> WidgetNode sp ep
  -> Maybe (WidgetResult sp ep)
compositeHandleMessage comp state@CompositeState{..} wenv target arg widgetComp
  | isTargetReached target widgetComp = case cast arg of
      Just (Just evt) -> reducedResult where
        evtResult = WidgetResult _cpsRoot Seq.empty (Seq.singleton evt)
        reducedResult = Just $ reduceResult comp state wenv widgetComp evtResult
      _ -> Nothing
  | otherwise = fmap processEvent result where
      processEvent = reduceResult comp state wenv widgetComp
      cmpWidget = _cpsRoot ^. L.widget
      model = getModel comp wenv
      cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
      result = widgetHandleMessage cmpWidget cwenv target arg _cpsRoot

-- Preferred size
compositeGetSizeReq
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetSizeReq sp ep
compositeGetSizeReq comp state wenv widgetComp = newSizeReq where
  CompositeState{..} = state
  style = activeStyle wenv widgetComp
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  tempChildReq = widgetGetSizeReq widget cwenv _cpsRoot
  newChildReq = sizeReqAddStyle style tempChildReq
  childRoot = newChildReq ^. L.widget
  childReqW = newChildReq ^. L.sizeReqW
  childReqH = newChildReq ^. L.sizeReqH
  newRoot = childRoot
    & L.info . L.sizeReqW .~ childReqW
    & L.info . L.sizeReqH .~ childReqH
  newState = state {
    _cpsRoot = newRoot
  }
  newComp = widgetComp
    & L.widget .~ createComposite comp newState
  newSizeReq = WidgetSizeReq newComp childReqW childReqH

-- Resize
compositeResize
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> Rect
  -> Rect
  -> WidgetNode sp ep
  -> WidgetNode sp ep
compositeResize comp state wenv viewport renderArea widgetComp = resized where
  CompositeState{..} = state
  style = activeStyle wenv widgetComp
  contentArea = fromMaybe def (removeOuterBounds style renderArea)
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  newRoot = widgetResize widget cwenv viewport contentArea _cpsRoot
  newState = state {
    _cpsRoot = newRoot
      & L.info . L.viewport .~ viewport
      & L.info . L.renderArea .~ contentArea
  }
  resized = widgetComp
    & L.widget .~ createComposite comp newState
    & L.info . L.viewport .~ viewport
    & L.info . L.renderArea .~ renderArea

-- Render
compositeRender
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> Renderer
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> IO ()
compositeRender comp state renderer wenv _ = action where
  CompositeState{..} = state
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  action = widgetRender widget renderer cwenv _cpsRoot

reduceResult
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetResult s e
  -> WidgetResult sp ep
reduceResult comp state wenv widgetComp widgetResult = newResult where
  CompositeState{..} = state
  WidgetResult evtsRoot reqs evts = widgetResult
  -- Since composite may reduce several times before giving control back, its
  -- copy of _cpsModel may be more up to date than WidgetEnv's model
  model
    | isJust _cpsModel = fromJust _cpsModel
    | otherwise = getModel comp wenv
  evtUpdates = getUpdateModelReqs reqs
  evtModel = foldr (.) id evtUpdates model
  evtHandler = _cmpEventHandler comp
  ReducedEvents{..} = reduceCompEvents _cpsGlobalKeys evtHandler evtModel evts
  WidgetResult uWidget uReqs uEvts =
    updateComposite comp state wenv _reModel evtsRoot widgetComp
  currentPath = widgetComp ^. L.info . L.path
  newReqs = toParentReqs reqs
         <> tasksToRequests currentPath _reTasks
         <> producersToRequests currentPath _reProducers
         <> uReqs
         <> toParentReqs _reRequests
         <> _reMessages
  newEvts = _reReports <> uEvts
  newResult = WidgetResult uWidget newReqs newEvts

updateComposite
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> s
  -> WidgetNode s e
  -> WidgetNode sp ep
  -> WidgetResult sp ep
updateComposite comp state wenv newModel widgetRoot widgetComp = result where
  CompositeState{..} = state
  model
    | isJust _cpsModel = fromJust _cpsModel
    | otherwise = getModel comp wenv
  -- _cmpMergeRequired is not used here for two reasons
  --   - 1) This function is called on events internal to the composite,
  --        so external changes do not affect it (those are handled in merge)
  --   - 2) It could cause an infinite loop
  mergeRequired = model /= newModel
  newState = state {
    _cpsRoot = widgetRoot,
    _cpsGlobalKeys = collectGlobalKeys M.empty widgetRoot
  }
  result
    | mergeRequired = mergeChild comp state wenv newModel widgetRoot widgetComp
    | otherwise = resultWidget $ widgetComp
      & L.widget .~ createComposite comp newState

mergeChild
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e sp
  -> WidgetEnv sp ep
  -> s
  -> WidgetNode s e
  -> WidgetNode sp ep
  -> WidgetResult sp ep
mergeChild comp state wenv newModel widgetRoot widgetComp = newResult where
  CompositeState{..} = state
  builtRoot = cascadeCtx widgetComp (_cmpUiBuilder comp newModel)
  builtWidget = builtRoot ^. L.widget
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys newModel
  mergedResult = widgetMerge builtWidget cwenv widgetRoot builtRoot
  mergedState = state {
    _cpsModel = Just newModel,
    _cpsRoot = mergedResult ^. L.widget,
    _cpsGlobalKeys = collectGlobalKeys M.empty (mergedResult ^. L.widget)
  }
  result = reduceResult comp mergedState wenv widgetComp mergedResult
  newEvents = fmap ($ newModel) (_cmpOnChange comp)
  newReqs = widgetDataSet (_cmpWidgetData comp) newModel ++ _cmpOnChangeReq comp
  newResult = result
    & L.requests <>~ Seq.fromList newReqs
    & L.events <>~ Seq.fromList newEvents

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
    Just node -> curr {
        _reMessages = _reMessages |> SendMessage (node ^. L.info . L.path) message
      }
    Nothing -> curr
  Task task -> curr { _reTasks = _reTasks |> task }
  Producer producer -> curr { _reProducers = _reProducers |> producer }

getModel
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> WidgetEnv sp ep
  -> s
getModel comp wenv = widgetDataGet (_weModel wenv) (_cmpWidgetData comp)

tasksToRequests :: CompositeEvent e => Path -> Seq (IO e) -> Seq (WidgetRequest sp)
tasksToRequests path reqs = RunTask path <$> reqs

producersToRequests
  :: CompositeEvent e => Path -> Seq (ProducerHandler e) -> Seq (WidgetRequest sp)
producersToRequests path reqs = RunProducer path <$> reqs

toParentReqs :: Seq (WidgetRequest s) -> Seq (WidgetRequest sp)
toParentReqs reqs = fmap fromJust $ Seq.filter isJust $ fmap toParentReq reqs

toParentReq :: WidgetRequest s -> Maybe (WidgetRequest sp)
toParentReq IgnoreParentEvents = Just IgnoreParentEvents
toParentReq IgnoreChildrenEvents = Just IgnoreChildrenEvents
toParentReq ResizeWidgets = Just ResizeWidgets
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
toParentReq RenderOnce = Just RenderOnce
toParentReq (RenderEvery path ms) = Just (RenderEvery path ms)
toParentReq (RenderStop path) = Just (RenderStop path)
toParentReq (ExitApplication exit) = Just (ExitApplication exit)
toParentReq (UpdateWindow req) = Just (UpdateWindow req)
toParentReq (UpdateModel fn) = Nothing

collectGlobalKeys
  :: Map WidgetKey (WidgetNode s e)
  -> WidgetNode s e
  -> Map WidgetKey (WidgetNode s e)
collectGlobalKeys keys node = newMap where
  children = node ^. L.children
  collect currKeys child = collectGlobalKeys currKeys child
  updatedMap = case node ^. L.info . L.key of
    Just (WidgetKeyGlobal key) -> M.insert (WidgetKeyGlobal key) node keys
    _ -> keys
  newMap = foldl' collect updatedMap children

convertWidgetEnv :: WidgetEnv sp ep -> GlobalKeys s e -> s -> WidgetEnv s e
convertWidgetEnv wenv globalKeys model = WidgetEnv {
  _weOS = _weOS wenv,
  _weRenderer = _weRenderer wenv,
  _weTheme = _weTheme wenv,
  _weAppWindowSize = _weAppWindowSize wenv,
  _weGlobalKeys = globalKeys,
  _weCurrentCursor = _weCurrentCursor wenv,
  _weFocusedPath = _weFocusedPath wenv,
  _wePressedPath = _wePressedPath wenv,
  _weOverlayPath = _weOverlayPath wenv,
  _weModel = model,
  _weInputStatus = _weInputStatus wenv,
  _weTimestamp = _weTimestamp wenv,
  _weInTopLayer = _weInTopLayer wenv
}

cascadeCtx :: WidgetNode sp ep -> WidgetNode s e -> WidgetNode s e
cascadeCtx parent child = newChild where
  pVisible = parent ^. L.info . L.visible
  pEnabled = parent ^. L.info . L.enabled
  cVisible = child ^. L.info . L.visible
  cEnabled = child ^. L.info . L.enabled
  newChild = child
    & L.info . L.path .~ firstChildPath parent
    & L.info . L.visible .~ (cVisible && pVisible)
    & L.info . L.enabled .~ (cEnabled && pEnabled)

getUpdateModelReqs :: (Traversable t) => t (WidgetRequest s) -> Seq (s -> s)
getUpdateModelReqs reqs = foldl' foldHelper Seq.empty reqs where
  foldHelper acc (UpdateModel fn) = acc |> fn
  foldHelper acc _ = acc

firstChildPath :: WidgetNode s e -> Path
firstChildPath node = node ^. L.info . L.path |> 0
