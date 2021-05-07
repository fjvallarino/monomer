{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Composite (
  module Monomer.Core,
  module Monomer.Event,
  module Monomer.Graphics,
  module Monomer.Widgets.Util,

  CompositeCfg,
  EventResponse(..),
  MergeReqsHandler,
  EventHandler,
  UIBuilder,
  compositeMergeReqs,
  composite,
  composite_,
  compositeV,
  compositeV_,
  compositeExt,
  compositeExt_,
  compositeD_
) where

import Debug.Trace

import Control.Applicative ((<|>))
import Control.Exception (AssertionFailed(..), throw)
import Control.Lens (ALens', (&), (^.), (^?), (.~), (%~), (<>~), at, ix, non)
import Data.Default
import Data.Either
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Sequence (Seq(..), (|>), (<|), fromList)
import Data.Typeable (Typeable, cast, typeOf)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Singles.Spacer
import Monomer.Widgets.Util

import qualified Monomer.Lens as L

type ParentModel sp = Typeable sp
type CompositeModel s = (Eq s, Typeable s, WidgetModel s)
type CompositeEvent e = WidgetEvent e

type MergeReqsHandler s e
  = WidgetEnv s e -> WidgetNode s e -> WidgetNode s e -> s -> [WidgetRequest s e]
type EventHandler s e sp ep
  = WidgetEnv s e -> WidgetNode s e -> s -> e -> [EventResponse s e sp ep]
type UIBuilder s e = WidgetEnv s e -> s -> WidgetNode s e
type MergeRequired s = s -> s -> Bool
type TaskHandler e = IO e
type ProducerHandler e = (e -> IO ()) -> IO ()

data CompMsgUpdate
  = forall s . Typeable s => CompMsgUpdate (s -> s)

data EventResponse s e sp ep
  = Model s
  | Event e
  | Report ep
  | Request (WidgetRequest s e)
  | RequestParent (WidgetRequest sp ep)
  | forall i . Typeable i => Message WidgetKey i
  | Task (TaskHandler e)
  | Producer (ProducerHandler e)

data CompositeCfg s e sp ep = CompositeCfg {
  _cmcMergeRequired :: Maybe (MergeRequired s),
  _cmcMergeReqs :: [MergeReqsHandler s e],
  _cmcOnInit :: [e],
  _cmcOnDispose :: [e],
  _cmcOnResize :: [Rect -> e],
  _cmcOnChange :: [s -> ep],
  _cmcOnChangeReq :: [s -> WidgetRequest sp ep],
  _cmcOnEnabledChange :: [e],
  _cmcOnVisibleChange :: [e]
}

instance Default (CompositeCfg s e sp ep) where
  def = CompositeCfg {
    _cmcMergeRequired = Nothing,
    _cmcMergeReqs = [],
    _cmcOnInit = [],
    _cmcOnDispose = [],
    _cmcOnResize = [],
    _cmcOnChange = [],
    _cmcOnChangeReq = [],
    _cmcOnEnabledChange = [],
    _cmcOnVisibleChange = []
  }

instance Semigroup (CompositeCfg s e sp ep) where
  (<>) c1 c2 = CompositeCfg {
    _cmcMergeRequired = _cmcMergeRequired c2 <|> _cmcMergeRequired c1,
    _cmcMergeReqs = _cmcMergeReqs c1 <> _cmcMergeReqs c2,
    _cmcOnInit = _cmcOnInit c1 <> _cmcOnInit c2,
    _cmcOnDispose = _cmcOnDispose c1 <> _cmcOnDispose c2,
    _cmcOnResize = _cmcOnResize c1 <> _cmcOnResize c2,
    _cmcOnChange = _cmcOnChange c1 <> _cmcOnChange c2,
    _cmcOnChangeReq = _cmcOnChangeReq c1 <> _cmcOnChangeReq c2,
    _cmcOnEnabledChange = _cmcOnEnabledChange c1 <> _cmcOnEnabledChange c2,
    _cmcOnVisibleChange = _cmcOnVisibleChange c1 <> _cmcOnVisibleChange c2
  }

instance Monoid (CompositeCfg s e sp ep) where
  mempty = def

instance CmbMergeRequired (CompositeCfg s e sp ep) s where
  mergeRequired fn = def {
    _cmcMergeRequired = Just fn
  }

instance CmbOnInit (CompositeCfg s e sp ep) e where
  onInit fn = def {
    _cmcOnInit = [fn]
  }

instance CmbOnDispose (CompositeCfg s e sp ep) e where
  onDispose fn = def {
    _cmcOnDispose = [fn]
  }

instance CmbOnResize (CompositeCfg s e sp ep) e Rect where
  onResize fn = def {
    _cmcOnResize = [fn]
  }

instance CmbOnChange (CompositeCfg s e sp ep) s ep where
  onChange fn = def {
    _cmcOnChange = [fn]
  }

instance CmbOnChangeReq (CompositeCfg s e sp ep) sp ep s where
  onChangeReq req = def {
    _cmcOnChangeReq = [req]
  }

instance CmbOnEnabledChange (CompositeCfg s e sp ep) e where
  onEnabledChange fn = def {
    _cmcOnEnabledChange = [fn]
  }

instance CmbOnVisibleChange (CompositeCfg s e sp ep) e where
  onVisibleChange fn = def {
    _cmcOnVisibleChange = [fn]
  }

compositeMergeReqs :: MergeReqsHandler s e -> CompositeCfg s e sp ep
compositeMergeReqs fn = def {
  _cmcMergeReqs = [fn]
}

data Composite s e sp ep = Composite {
  _cmpWidgetData :: WidgetData sp s,
  _cmpEventHandler :: EventHandler s e sp ep,
  _cmpUiBuilder :: UIBuilder s e,
  _cmpMergeRequired :: MergeRequired s,
  _cmpMergeReqs :: [MergeReqsHandler s e],
  _cmpOnInit :: [e],
  _cmpOnDispose :: [e],
  _cmpOnResize :: [Rect -> e],
  _cmpOnChange :: [s -> ep],
  _cmpOnChangeReq :: [s -> WidgetRequest sp ep],
  _cmpOnEnabledChange :: [e],
  _cmpOnVisibleChange :: [e]
}

data CompositeState s e = CompositeState {
  _cpsModel :: Maybe s,
  _cpsRoot :: WidgetNode s e,
  _cpsGlobalKeys :: WidgetKeysMap s e
}

data ReducedEvents s e sp ep = ReducedEvents {
  _reModel :: s,
  _reEvents :: Seq e,
  _reReports :: Seq ep,
  _reRequests :: Seq (WidgetRequest s e),
  _reMessages :: Seq (WidgetRequest sp ep),
  _reTasks :: Seq (TaskHandler e),
  _reProducers :: Seq (ProducerHandler e)
}

composite
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => WidgetType
  -> ALens' sp s
  -> UIBuilder s e
  -> EventHandler s e sp ep
  -> WidgetNode sp ep
composite widgetType field uiBuilder evtHandler = newNode where
  newNode = composite_ widgetType field uiBuilder evtHandler def

composite_
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => WidgetType
  -> ALens' sp s
  -> UIBuilder s e
  -> EventHandler s e sp ep
  -> [CompositeCfg s e sp ep]
  -> WidgetNode sp ep
composite_ widgetType field uiBuilder evtHandler cfgs = newNode where
  widgetData = WidgetLens field
  newNode = compositeD_ widgetType widgetData uiBuilder evtHandler cfgs

compositeV
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => WidgetType
  -> s
  -> (s -> ep)
  -> UIBuilder s e
  -> EventHandler s e sp ep
  -> WidgetNode sp ep
compositeV wType val handler uiBuilder evtHandler = newNode where
  newNode = compositeV_ wType val handler uiBuilder evtHandler def

compositeV_
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => WidgetType
  -> s
  -> (s -> ep)
  -> UIBuilder s e
  -> EventHandler s e sp ep
  -> [CompositeCfg s e sp ep]
  -> WidgetNode sp ep
compositeV_ wType val handler uiBuilder evtHandler cfgs = newNode where
  widgetData = WidgetValue val
  newCfgs = onChange handler : cfgs
  newNode = compositeD_ wType widgetData uiBuilder evtHandler newCfgs

compositeExt
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => WidgetType
  -> s
  -> UIBuilder s e
  -> EventHandler s e sp ep
  -> WidgetNode sp ep
compositeExt wType val uiBuilder evtHandler = newNode where
  newNode = compositeExt_ wType val uiBuilder evtHandler []

compositeExt_
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => WidgetType
  -> s
  -> UIBuilder s e
  -> EventHandler s e sp ep
  -> [CompositeCfg s e sp ep]
  -> WidgetNode sp ep
compositeExt_ wType val uiBuilder evtHandler cfgs = newNode where
  widgetData = WidgetValue val
  newNode = compositeD_ wType widgetData uiBuilder evtHandler cfgs

compositeD_
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => WidgetType
  -> WidgetData sp s
  -> UIBuilder s e
  -> EventHandler s e sp ep
  -> [CompositeCfg s e sp ep]
  -> WidgetNode sp ep
compositeD_ wType wData uiBuilder evtHandler configs = newNode where
  config = mconcat configs
  mergeReq = fromMaybe (/=) (_cmcMergeRequired config)
  widgetRoot = spacer
  composite = Composite {
    _cmpWidgetData = wData,
    _cmpEventHandler = evtHandler,
    _cmpUiBuilder = uiBuilder,
    _cmpMergeRequired = mergeReq,
    _cmpMergeReqs = _cmcMergeReqs config,
    _cmpOnInit = _cmcOnInit config,
    _cmpOnDispose = _cmcOnDispose config,
    _cmpOnResize = _cmcOnResize config,
    _cmpOnChange = _cmcOnChange config,
    _cmpOnChangeReq = _cmcOnChangeReq config,
    _cmpOnEnabledChange = _cmcOnEnabledChange config,
    _cmpOnVisibleChange = _cmcOnVisibleChange config
  }
  state = CompositeState Nothing widgetRoot M.empty
  widget = createComposite composite state
  newNode = defaultWidgetNode wType widget

createComposite
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> Widget sp ep
createComposite comp state = widget where
  widget = Widget {
    widgetInit = compositeInit comp state,
    widgetMerge = compositeMerge comp state,
    widgetDispose = compositeDispose comp state,
    widgetGetState = makeState state,
    widgetGetInstanceTree = compositeGetInstanceTree comp state,
    widgetFindNextFocus = compositeFindNextFocus comp state,
    widgetFindByPoint = compositeFindByPoint comp state,
    widgetFindByPath = compositeFindByPath comp state,
    widgetHandleEvent = compositeHandleEvent comp state,
    widgetHandleMessage = compositeHandleMessage comp state,
    widgetGetSizeReq = compositeGetSizeReq comp state,
    widgetResize = compositeResize comp state,
    widgetRender = compositeRender comp state
  }

-- | Init
compositeInit
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetResult sp ep
compositeInit comp state wenv widgetComp = newResult where
  CompositeState{..} = state
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  -- Creates UI using provided function
  builtRoot = _cmpUiBuilder comp cwenv model
  tempRoot = cascadeCtx wenv widgetComp builtRoot
  WidgetResult root reqs = widgetInit (tempRoot ^. L.widget) cwenv tempRoot
  newEvts = RaiseEvent <$> Seq.fromList (_cmpOnInit comp)
  newState = state {
    _cpsModel = Just model,
    _cpsRoot = root,
    _cpsGlobalKeys = collectGlobalKeys M.empty root
  }
  tempResult = WidgetResult root (reqs <> newEvts)
  getBaseStyle wenv node = Nothing
  styledComp = initNodeStyle getBaseStyle wenv widgetComp
  newResult = toParentResult comp newState wenv styledComp tempResult

-- | Merge
compositeMerge
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetNode sp ep
  -> WidgetResult sp ep
compositeMerge comp state wenv newComp oldComp = newResult where
  widgetId = oldComp ^. L.info . L.widgetId
  oldState = widgetGetState (oldComp ^. L.widget) wenv oldComp
  validState = fromMaybe state (useState oldState)
  CompositeState oldModel oldRoot oldGlobalKeys = validState
  model = getModel comp wenv
  -- Creates new UI using provided function
  cwenv = convertWidgetEnv wenv oldGlobalKeys model
  tempRoot = cascadeCtx wenv newComp (_cmpUiBuilder comp cwenv model)
  tempWidget = tempRoot ^. L.widget
  -- Needed in case the user references something outside model when building UI
  -- The same model is provided as old since nothing else is available, but
  -- mergeRequired may be using data from a closure
  visibleChg = oldComp ^. L.info . L.visible /= newComp ^. L.info . L.visible
  enabledChg = oldComp ^. L.info . L.enabled /= newComp ^. L.info . L.enabled
  modelChanged = _cmpMergeRequired comp (fromJust oldModel) model
  mergeRequired
    | isJust oldModel = modelChanged || visibleChg || enabledChg
    | otherwise = True
  initRequired = not (nodeMatches tempRoot oldRoot)
  WidgetResult newRoot tmpReqs
    | initRequired = widgetInit tempWidget cwenv tempRoot
    | mergeRequired = widgetMerge tempWidget cwenv tempRoot oldRoot
    | otherwise = resultWidget oldRoot
  newState = validState {
    _cpsModel = Just model,
    _cpsRoot = newRoot,
    _cpsGlobalKeys = collectGlobalKeys M.empty newRoot
  }
  getBaseStyle wenv node = Nothing
  styledComp = initNodeStyle getBaseStyle wenv newComp
    & L.info . L.widgetId .~ oldComp ^. L.info . L.widgetId
    & L.info . L.viewport .~ oldComp ^. L.info . L.viewport
    & L.info . L.sizeReqW .~ oldComp ^. L.info . L.sizeReqW
    & L.info . L.sizeReqH .~ oldComp ^. L.info . L.sizeReqH
  visibleEvts = if visibleChg then _cmpOnVisibleChange comp else []
  enabledEvts = if enabledChg then _cmpOnEnabledChange comp else []
  mergeReqsFns = _cmpMergeReqs comp
  mergeReqs = concatMap (\fn -> fn cwenv newRoot oldRoot model) mergeReqsFns
  extraReqs = seqCatMaybes (toParentReq widgetId <$> Seq.fromList mergeReqs)
  evts = RaiseEvent <$> Seq.fromList (visibleEvts ++ enabledEvts)
  tmpResult = WidgetResult newRoot (tmpReqs <> extraReqs <> evts)
  reducedResult = toParentResult comp newState wenv styledComp tmpResult
  newResult = handleWidgetIdChange oldComp reducedResult

-- | Dispose
compositeDispose
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetResult sp ep
compositeDispose comp state wenv widgetComp = result where
  CompositeState{..} = state
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  widget = _cpsRoot ^. L.widget
  newEvts = RaiseEvent <$> Seq.fromList (_cmpOnDispose comp)
  WidgetResult _ reqs = widgetDispose widget cwenv _cpsRoot
  tempResult = WidgetResult _cpsRoot (reqs <> newEvts)
  result = toParentResult comp state wenv widgetComp tempResult

compositeGetInstanceTree
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetInstanceNode
compositeGetInstanceTree comp state wenv node = instTree where
  CompositeState{..} = state
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  cInstTree = widgetGetInstanceTree widget cwenv _cpsRoot
  instTree = WidgetInstanceNode {
    _winInfo = node ^. L.info,
    _winState = Just (WidgetState state),
    _winChildren = Seq.singleton cInstTree
  }

-- | Next focusable
compositeFindNextFocus
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> FocusDirection
  -> Path
  -> Maybe WidgetNodeInfo
compositeFindNextFocus comp state wenv widgetComp dir start = nextFocus where
  CompositeState{..} = state
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  nextFocus = widgetFindNextFocus widget cwenv _cpsRoot dir start

-- | Find
compositeFindByPoint
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> Path
  -> Point
  -> Maybe WidgetNodeInfo
compositeFindByPoint comp state wenv widgetComp start point
  | widgetComp ^. L.info . L.visible && validStep = resultInfo
  | otherwise = Nothing
  where
    CompositeState{..} = state
    widget = _cpsRoot ^. L.widget
    model = getModel comp wenv
    cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
    next = nextTargetStep start widgetComp
    validStep = isNothing next || next == Just 0
    resultInfo = widgetFindByPoint widget cwenv _cpsRoot start point

compositeFindByPath
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> Path
  -> Maybe WidgetNodeInfo
compositeFindByPath comp state wenv widgetComp path
  | info ^. L.path == path = Just info
  | nextStep == Just 0 = widgetFindByPath (child ^. L.widget) cwenv child path
  | otherwise = Nothing
  where
    CompositeState{..} = state
    model = getModel comp wenv
    cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
    info = widgetComp ^. L.info
    nextStep = nextTargetStep path widgetComp
    child = _cpsRoot

-- | Event handling
compositeHandleEvent
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> Path
  -> SystemEvent
  -> Maybe (WidgetResult sp ep)
compositeHandleEvent comp state wenv widgetComp target evt = result where
  CompositeState{..} = state
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  rootEnabled = _cpsRoot ^. L.info . L.enabled
  compVisible = widgetComp ^. L.info . L.visible
  compEnabled = widgetComp ^. L.info . L.enabled
  processEvent = toParentResult comp state wenv widgetComp
  evtResult
    | not (compVisible && compEnabled) = Nothing
    | rootEnabled = widgetHandleEvent widget cwenv _cpsRoot target evt
    | otherwise = Nothing
  result = fmap processEvent evtResult

-- | Message handling
compositeHandleMessage
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp, Typeable i)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> Path
  -> i
  -> Maybe (WidgetResult sp ep)
compositeHandleMessage comp state@CompositeState{..} wenv widgetComp target arg
  | isTargetReached target widgetComp = case cast arg of
      Just evt -> Just $ handleMsgEvent comp state wenv widgetComp evt
      Nothing -> case cast arg of
        Just (CompMsgUpdate msg) -> handleMsgUpdate comp state wenv widgetComp <$> cast msg
        _ -> traceShow ("Failed match on Composite handleEvent", typeOf arg) Nothing
  | otherwise = fmap processEvent result where
      processEvent = toParentResult comp state wenv widgetComp
      cmpWidget = _cpsRoot ^. L.widget
      model = getModel comp wenv
      cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
      result = widgetHandleMessage cmpWidget cwenv _cpsRoot target arg

-- Preferred size
compositeGetSizeReq
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> (SizeReq, SizeReq)
compositeGetSizeReq comp state wenv widgetComp = (newReqW, newReqH) where
  CompositeState{..} = state
  style = activeStyle wenv widgetComp
  widget = _cpsRoot ^. L.widget
  currReqW = _cpsRoot ^. L.info . L.sizeReqW
  currReqH = _cpsRoot ^. L.info . L.sizeReqH
  (tmpReqW, tmpReqH) = sizeReqAddStyle style (currReqW, currReqH)
  -- User settings take precedence
  newReqW = fromMaybe tmpReqW (style ^. L.sizeReqW)
  newReqH = fromMaybe tmpReqH (style ^. L.sizeReqH)

-- Preferred size
updateSizeReq
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetNode sp ep
updateSizeReq comp state wenv widgetComp = newComp where
  (newReqW, newReqH) = compositeGetSizeReq comp state wenv widgetComp
  newComp = widgetComp
    & L.info . L.sizeReqW .~ newReqW
    & L.info . L.sizeReqH .~ newReqH

-- Resize
compositeResize
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> Rect
  -> WidgetResult sp ep
compositeResize comp state wenv widgetComp viewport = resizedRes where
  CompositeState{..} = state
  style = activeStyle wenv widgetComp
  contentArea = fromMaybe def (removeOuterBounds style viewport)
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  WidgetResult newRoot newReqs = widgetResize widget cwenv _cpsRoot contentArea
  oldVp = widgetComp ^. L.info . L.viewport
  sizeChanged = viewport /= oldVp
  resizeEvts = fmap ($ viewport) (_cmpOnResize comp)
  resizeReqs
    | sizeChanged = RaiseEvent <$> Seq.fromList resizeEvts
    | otherwise = Empty
  childRes = WidgetResult newRoot (newReqs <> resizeReqs)
    & L.node . L.info . L.viewport .~ contentArea
  resizedRes = toParentResult comp state wenv widgetComp childRes
    & L.node . L.info . L.viewport .~ viewport

-- Render
compositeRender
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> Renderer
  -> IO ()
compositeRender comp state wenv widgetComp renderer = action where
  CompositeState{..} = state
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  action = widgetRender widget cwenv _cpsRoot renderer

handleMsgEvent
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> e
  -> WidgetResult sp ep
handleMsgEvent comp state wenv widgetComp event = newResult where
  CompositeState{..} = state
  model
    | isJust _cpsModel = fromJust _cpsModel
    | otherwise = getModel comp wenv
  evtHandler = _cmpEventHandler comp
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  response = evtHandler cwenv _cpsRoot model event
  newReqs = evtResponseToRequest widgetComp _cpsGlobalKeys <$> response
  newResult = WidgetResult widgetComp (Seq.fromList (catMaybes newReqs))

handleMsgUpdate
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> (s -> s)
  -> WidgetResult sp ep
handleMsgUpdate comp state wenv widgetComp fnUpdate = result where
  CompositeState{..} = state
  model
    | isJust _cpsModel = fromJust _cpsModel
    | otherwise = getModel comp wenv
  newModel = fnUpdate model
  result
    | model == newModel = resultWidget widgetComp
    | otherwise = mergeChild comp state wenv newModel _cpsRoot widgetComp

toParentResult
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetResult s e
  -> WidgetResult sp ep
toParentResult comp state wenv widgetComp result = newResult where
  WidgetResult newRoot reqs = result
  widgetId = widgetComp ^. L.info . L.widgetId
  newState = state {
    _cpsRoot = newRoot
  }
  newComp = widgetComp
    & L.widget .~ createComposite comp newState
  newNode = updateSizeReq comp newState wenv newComp
  newReqs = seqCatMaybes (toParentReq widgetId <$> reqs)
  newResult = WidgetResult newNode newReqs

evtResponseToRequest
  :: (Typeable s, Typeable sp, WidgetEvent e, WidgetEvent ep)
  => WidgetNode sp ep
  -> WidgetKeysMap s e
  -> EventResponse s e sp ep
  -> Maybe (WidgetRequest sp ep)
evtResponseToRequest widgetComp globalKeys response = case response of
  Model newModel -> Just $ sendTo widgetComp (CompMsgUpdate $ const newModel)
  Event event -> Just $ sendTo widgetComp event
  Report report -> Just (RaiseEvent report)
  Request req -> toParentReq widgetId req
  RequestParent req -> Just req
  Message key msg -> (`sendTo` msg) <$> M.lookup key globalKeys
  Task task -> Just $ RunTask widgetId path task
  Producer producer -> Just $ RunProducer widgetId path producer
  where
    sendTo node msg = SendMessage (node ^. L.info . L.widgetId) msg
    widgetId = widgetComp ^. L.info . L.widgetId
    path = widgetComp ^. L.info . L.path

mergeChild
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> s
  -> WidgetNode s e
  -> WidgetNode sp ep
  -> WidgetResult sp ep
mergeChild comp state wenv newModel widgetRoot widgetComp = newResult where
  CompositeState{..} = state
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys newModel
  builtRoot = cascadeCtx wenv widgetComp (_cmpUiBuilder comp cwenv newModel)
      & L.info . L.widgetId .~ _cpsRoot ^. L.info . L.widgetId
  builtWidget = builtRoot ^. L.widget
  mergedResult = widgetMerge builtWidget cwenv builtRoot widgetRoot
  mergedState = state {
    _cpsModel = Just newModel,
    _cpsRoot = mergedResult ^. L.node,
    _cpsGlobalKeys = collectGlobalKeys M.empty (mergedResult ^. L.node)
  }
  result = toParentResult comp mergedState wenv widgetComp mergedResult
  newEvents = RaiseEvent <$> fmap ($ newModel) (_cmpOnChange comp)
  newReqs = widgetDataSet (_cmpWidgetData comp) newModel
    ++ fmap ($ newModel) (_cmpOnChangeReq comp)
  newResult = result
    & L.requests <>~ Seq.fromList newReqs <> Seq.fromList newEvents

getModel
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, ParentModel sp)
  => Composite s e sp ep
  -> WidgetEnv sp ep
  -> s
getModel comp wenv = widgetDataGet (_weModel wenv) (_cmpWidgetData comp)

toParentReq
  :: (Typeable s, Typeable sp)
  => WidgetId
  -> WidgetRequest s e
  -> Maybe (WidgetRequest sp ep)
toParentReq _ IgnoreParentEvents = Just IgnoreParentEvents
toParentReq _ IgnoreChildrenEvents = Just IgnoreChildrenEvents
toParentReq _ ResizeWidgets = Just ResizeWidgets
toParentReq _ (MoveFocus start dir) = Just (MoveFocus start dir)
toParentReq _ (SetFocus path) = Just (SetFocus path)
toParentReq _ (GetClipboard path) = Just (GetClipboard path)
toParentReq _ (SetClipboard clipboard) = Just (SetClipboard clipboard)
toParentReq _ (StartTextInput rect) = Just (StartTextInput rect)
toParentReq _ StopTextInput = Just StopTextInput
toParentReq _ (ResetOverlay wid) = Just (ResetOverlay wid)
toParentReq _ (SetOverlay wid path) = Just (SetOverlay wid path)
toParentReq _ (SetCursorIcon wid icon) = Just (SetCursorIcon wid icon)
toParentReq _ (ResetCursorIcon wid) = Just (ResetCursorIcon wid)
toParentReq _ (StartDrag wid path info) = Just (StartDrag wid path info)
toParentReq _ (StopDrag wid) = Just (StopDrag wid)
toParentReq _ RenderOnce = Just RenderOnce
toParentReq _ (RenderEvery path ms repeat) = Just (RenderEvery path ms repeat)
toParentReq _ (RenderStop path) = Just (RenderStop path)
toParentReq _ (ExitApplication exit) = Just (ExitApplication exit)
toParentReq _ (UpdateWindow req) = Just (UpdateWindow req)
toParentReq _ (SetWidgetPath wid path) = Just (SetWidgetPath wid path)
toParentReq _ (ResetWidgetPath wid) = Just (ResetWidgetPath wid)
toParentReq wid (UpdateModel fn) = Just (SendMessage wid (CompMsgUpdate fn))
toParentReq wid (RaiseEvent message) = Just (SendMessage wid message)
toParentReq _ (SendMessage wid message) = Just (SendMessage wid message)
toParentReq _ (RunTask wid path action) = Just (RunTask wid path action)
toParentReq _ (RunProducer wid path action) = Just (RunProducer wid path action)

collectGlobalKeys
  :: Map WidgetKey (WidgetNode s e)
  -> WidgetNode s e
  -> Map WidgetKey (WidgetNode s e)
collectGlobalKeys keys node = newMap where
  children = node ^. L.children
  collect currKeys child = collectGlobalKeys currKeys child
  updatedMap = case node ^. L.info . L.key of
    Just key -> M.insert key node keys
    _ -> keys
  newMap = foldl' collect updatedMap children

convertWidgetEnv :: WidgetEnv sp ep -> WidgetKeysMap s e -> s -> WidgetEnv s e
convertWidgetEnv wenv globalKeys model = WidgetEnv {
  _weOs = _weOs wenv,
  _weRenderer = _weRenderer wenv,
  _weFindByPath = _weFindByPath wenv,
  _weMainButton = _weMainButton wenv,
  _weTheme = _weTheme wenv,
  _weWindowSize = _weWindowSize wenv,
  _weGlobalKeys = globalKeys,
  _weCursor = _weCursor wenv,
  _weHoveredPath = _weHoveredPath wenv,
  _weFocusedPath = _weFocusedPath wenv,
  _weDragStatus = _weDragStatus wenv,
  _weMainBtnPress = _weMainBtnPress wenv,
  _weOverlayPath = _weOverlayPath wenv,
  _weModel = model,
  _weInputStatus = _weInputStatus wenv,
  _weTimestamp = _weTimestamp wenv,
  _weInTopLayer = _weInTopLayer wenv,
  _weLayoutDirection = LayoutNone,
  _weViewport = _weViewport wenv,
  _weOffset = _weOffset wenv
}

cascadeCtx
  :: WidgetEnv sp ep -> WidgetNode sp ep -> WidgetNode s e -> WidgetNode s e
cascadeCtx wenv parent child = newChild where
  pOverlay = parent ^. L.info . L.overlay
  pVisible = parent ^. L.info . L.visible
  pEnabled = parent ^. L.info . L.enabled
  cOverlay = child ^. L.info . L.overlay
  cVisible = child ^. L.info . L.visible
  cEnabled = child ^. L.info . L.enabled
  newPath = parent ^. L.info . L.path |> 0
  newChild = child
    & L.info . L.widgetId .~ WidgetId (wenv ^. L.timestamp) newPath
    & L.info . L.path .~ newPath
    & L.info . L.overlay .~ (cOverlay || pOverlay)
    & L.info . L.visible .~ (cVisible && pVisible)
    & L.info . L.enabled .~ (cEnabled && pEnabled)
