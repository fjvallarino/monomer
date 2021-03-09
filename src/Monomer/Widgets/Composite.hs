{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  compositeV_,
  compositeExt,
  compositeExt_
) where

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Codec.Serialise
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
type CompositeModel s = (Eq s, WidgetModel s)
type CompositeEvent e = WidgetEvent e

type EventHandler s e ep
  = WidgetEnv s e -> WidgetNode s e -> s -> e -> [EventResponse s e ep]
type UIBuilder s e = WidgetEnv s e -> s -> WidgetNode s e
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

data CompositeCfg s e sp ep = CompositeCfg {
  _cmcMergeRequired :: Maybe (MergeRequired s),
  _cmcOnInit :: [e],
  _cmcOnDispose :: [e],
  _cmcOnResize :: [Rect -> e],
  _cmcOnChange :: [s -> ep],
  _cmcOnChangeReq :: [WidgetRequest sp],
  _cmcOnEnabledChange :: [e],
  _cmcOnVisibleChange :: [e]
}

instance Default (CompositeCfg s e sp ep) where
  def = CompositeCfg {
    _cmcMergeRequired = Nothing,
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

instance CmbOnResize (CompositeCfg s e sp ep) Rect e where
  onResize fn = def {
    _cmcOnResize = [fn]
  }

instance CmbOnChange (CompositeCfg s e sp ep) s ep where
  onChange fn = def {
    _cmcOnChange = [fn]
  }

instance CmbOnChangeReq (CompositeCfg s e sp ep) sp where
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

data Composite s e sp ep = Composite {
  _cmpWidgetData :: WidgetData sp s,
  _cmpEventHandler :: EventHandler s e ep,
  _cmpUiBuilder :: UIBuilder s e,
  _cmpMergeRequired :: MergeRequired s,
  _cmpOnInit :: [e],
  _cmpOnDispose :: [e],
  _cmpOnResize :: [Rect -> e],
  _cmpOnChange :: [s -> ep],
  _cmpOnChangeReq :: [WidgetRequest sp],
  _cmpOnEnabledChange :: [e],
  _cmpOnVisibleChange :: [e]
}

data CompositeState s e = CompositeState {
  _cpsModel :: Maybe s,
  _cpsRoot :: WidgetNode s e,
  _cpsGlobalKeys :: GlobalKeys s e
}

instance WidgetModel s => Serialise (CompositeState s e) where
  encode state = encodeListLen 2 <> encodeWord 0 <> encode modelBS where
    modelBS = modelToByteString (_cpsModel state)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    modelBS <- decode
    let model = fromRight Nothing (byteStringToModel modelBS)
    case (len, tag) of
      (2, 0) -> return $ CompositeState model spacer M.empty
      _ -> fail "Invalid Composite state"

instance (WidgetModel s, Typeable e) => WidgetModel (CompositeState s e) where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

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
  -> UIBuilder s e
  -> EventHandler s e ep
  -> WidgetNode sp ep
composite widgetType field uiBuilder evtHandler = newNode where
  newNode = composite_ widgetType field uiBuilder evtHandler def

composite_
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> ALens' sp s
  -> UIBuilder s e
  -> EventHandler s e ep
  -> [CompositeCfg s e sp ep]
  -> WidgetNode sp ep
composite_ widgetType field uiBuilder evtHandler cfgs = newNode where
  widgetData = WidgetLens field
  newNode = compositeD_ widgetType widgetData uiBuilder evtHandler cfgs

compositeV
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> s
  -> (s -> ep)
  -> UIBuilder s e
  -> EventHandler s e ep
  -> WidgetNode sp ep
compositeV wType val handler uiBuilder evtHandler = newNode where
  newNode = compositeV_ wType val handler uiBuilder evtHandler def

compositeV_
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> s
  -> (s -> ep)
  -> UIBuilder s e
  -> EventHandler s e ep
  -> [CompositeCfg s e sp ep]
  -> WidgetNode sp ep
compositeV_ wType val handler uiBuilder evtHandler cfgs = newNode where
  widgetData = WidgetValue val
  newCfgs = onChange handler : cfgs
  newNode = compositeD_ wType widgetData uiBuilder evtHandler newCfgs

compositeExt
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> s
  -> UIBuilder s e
  -> EventHandler s e ep
  -> WidgetNode sp ep
compositeExt wType val uiBuilder evtHandler = newNode where
  newNode = compositeExt_ wType val uiBuilder evtHandler []

compositeExt_
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> s
  -> UIBuilder s e
  -> EventHandler s e ep
  -> [CompositeCfg s e sp ep]
  -> WidgetNode sp ep
compositeExt_ wType val uiBuilder evtHandler cfgs = newNode where
  widgetData = WidgetValue val
  newNode = compositeD_ wType widgetData uiBuilder evtHandler cfgs

compositeD_
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => WidgetType
  -> WidgetData sp s
  -> UIBuilder s e
  -> EventHandler s e ep
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
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> Widget sp ep
createComposite comp state = widget where
  widget = Widget {
    widgetInit = compositeInit comp state,
    widgetMerge = compositeMerge comp state,
    widgetDispose = compositeDispose comp state,
    widgetGetState = makeState state,
    widgetSave = compositeSave comp state,
    widgetRestore = compositeRestore comp state,
    widgetFindNextFocus = compositeFindNextFocus comp state,
    widgetFindByPoint = compositeFindByPoint comp state,
    widgetFindByPath = compositeFindByPath comp state,
    widgetHandleEvent = compositeHandleEvent comp state,
    widgetHandleMessage = compositeHandleMessage comp state,
    widgetResize = compositeResize comp state,
    widgetRender = compositeRender comp state
  }

-- | Init
compositeInit
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
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
  WidgetResult root reqs evts = widgetInit (tempRoot ^. L.widget) cwenv tempRoot
  newEvts = Seq.fromList (_cmpOnInit comp)
  newState = state {
    _cpsModel = Just model,
    _cpsRoot = root,
    _cpsGlobalKeys = collectGlobalKeys M.empty root
  }
  tempResult = WidgetResult root reqs (evts <> newEvts)
  getBaseStyle wenv node = Nothing
  styledComp = initNodeStyle getBaseStyle wenv widgetComp
  newResult = reduceResult comp newState wenv styledComp tempResult

-- | Merge
compositeMerge
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
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
  WidgetResult newRoot tmpReqs tmpEvts
    | initRequired = widgetInit tempWidget cwenv tempRoot
    | mergeRequired = widgetMerge tempWidget cwenv oldRoot tempRoot
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
  evts = Seq.fromList (visibleEvts ++ enabledEvts)
  tmpResult = WidgetResult newRoot tmpReqs (tmpEvts <> evts)
  reducedResult = reduceResult comp newState wenv styledComp tmpResult
  newResult = handleWidgetIdChange oldComp reducedResult

-- | Dispose
compositeDispose
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
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
  newEvts = Seq.fromList (_cmpOnDispose comp)
  WidgetResult _ reqs evts = widgetDispose widget cwenv _cpsRoot
  tempResult = WidgetResult _cpsRoot reqs (evts <> newEvts)
  result = reduceResult comp state wenv widgetComp tempResult

compositeSave
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetInstanceNode
compositeSave comp state wenv node = instTree where
  CompositeState{..} = state
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  cInstTree = widgetSave widget cwenv _cpsRoot
  instTree = WidgetInstanceNode {
    _winInfo = node ^. L.info,
    _winState = Just (WidgetState state),
    _winChildren = Seq.singleton cInstTree
  }

compositeRestore
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetInstanceNode
  -> WidgetNode sp ep
  -> WidgetResult sp ep
compositeRestore comp state wenv win newComp = result where
  oldState = loadState (win ^. L.state)
  validState = fromMaybe state oldState
  oldGlobalKeys = M.empty
  oldModel = _cpsModel validState
  oldInfo = win ^. L.info
  model = fromMaybe (getModel comp wenv) oldModel
  cwenv = convertWidgetEnv wenv oldGlobalKeys model
  tempRoot = cascadeCtx wenv newComp (_cmpUiBuilder comp cwenv model)
  tempWidget = tempRoot ^. L.widget
  tempResult = case Seq.lookup 0 (win ^. L.children) of
    Just cwin -> widgetRestore tempWidget cwenv cwin tempRoot
    _ -> resultWidget tempRoot
  newRoot = tempResult ^. L.node
  newState = validState {
    _cpsModel = Just model,
    _cpsRoot = newRoot,
    _cpsGlobalKeys = collectGlobalKeys M.empty newRoot
  }
  getBaseStyle wenv node = Nothing
  styledComp = initNodeStyle getBaseStyle wenv newComp
  reducedResult = reduceResult comp newState wenv styledComp tempResult
  widgetId = newComp ^. L.info . L.widgetId
  valid = infoMatches (win ^. L.info) (newComp ^. L.info)
  message = matchFailedMsg (win ^. L.info) (newComp ^. L.info)
  result
    | valid = reducedResult
        & L.node . L.info . L.widgetId .~ oldInfo ^. L.widgetId
        & L.node . L.info . L.viewport .~ oldInfo ^. L.viewport
        & L.node . L.info . L.sizeReqW .~ oldInfo ^. L.sizeReqW
        & L.node . L.info . L.sizeReqH .~ oldInfo ^. L.sizeReqH
    | otherwise = throw (AssertionFailed $ "Restore failed. " ++ message)

-- | Next focusable
compositeFindNextFocus
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> FocusDirection
  -> Path
  -> WidgetNode sp ep
  -> Maybe WidgetNodeInfo
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
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> Path
  -> Point
  -> WidgetNode sp ep
  -> Maybe WidgetNodeInfo
compositeFindByPoint comp state wenv startPath point widgetComp
  | widgetComp ^. L.info . L.visible && validStep = resultInfo
  | otherwise = Nothing
  where
    CompositeState{..} = state
    widget = _cpsRoot ^. L.widget
    model = getModel comp wenv
    cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
    next = nextTargetStep startPath widgetComp
    validStep = isNothing next || next == Just 0
    resultInfo = widgetFindByPoint widget cwenv startPath point _cpsRoot

compositeFindByPath
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> Path
  -> WidgetNode sp ep
  -> Maybe WidgetNodeInfo
compositeFindByPath comp state wenv path widgetComp
  | info ^. L.path == path = Just info
  | nextStep == Just 0 = widgetFindByPath (child ^. L.widget) cwenv path child
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
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
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
  -> CompositeState s e
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
updateSizeReq
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetNode sp ep
updateSizeReq state wenv widgetComp = newComp where
  CompositeState{..} = state
  style = activeStyle wenv widgetComp
  widget = _cpsRoot ^. L.widget
  currReqW = _cpsRoot ^. L.info . L.sizeReqW
  currReqH = _cpsRoot ^. L.info . L.sizeReqH
  (tmpReqW, tmpReqH) = sizeReqAddStyle style (currReqW, currReqH)
  -- User settings take precedence
  newReqW = fromMaybe tmpReqW (style ^. L.sizeReqW)
  newReqH = fromMaybe tmpReqH (style ^. L.sizeReqH)
  newComp = widgetComp
    & L.info . L.sizeReqW .~ newReqW
    & L.info . L.sizeReqH .~ newReqH

-- Resize
compositeResize
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> Rect
  -> WidgetNode sp ep
  -> WidgetResult sp ep
compositeResize comp state wenv viewport widgetComp = resized where
  CompositeState{..} = state
  style = activeStyle wenv widgetComp
  contentArea = fromMaybe def (removeOuterBounds style viewport)
  widget = _cpsRoot ^. L.widget
  model = getModel comp wenv
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys model
  childRes = widgetResize widget cwenv contentArea _cpsRoot
  oldRa = widgetComp ^. L.info . L.viewport
  sizeChanged = viewport /= oldRa
  resizeEvts = fmap ($ viewport) (_cmpOnResize comp)
  newEvts
    | sizeChanged = Seq.fromList resizeEvts
    | otherwise = Empty
  compRes = reduceResult comp state wenv widgetComp $ childRes
    & L.node . L.info . L.viewport .~ contentArea
    & L.events .~ childRes ^. L.events <> newEvts
  resized = compRes
    & L.node . L.info . L.viewport .~ viewport

-- Render
compositeRender
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
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
  -> CompositeState s e
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
  evtModel = foldr ($) model evtUpdates
  evtHandler = _cmpEventHandler comp
  cwenv = convertWidgetEnv wenv _cpsGlobalKeys evtModel
  ReducedEvents{..} =
      reduceCompEvents _cpsGlobalKeys evtHandler cwenv evtsRoot evtModel evts
  WidgetResult uWidget uReqs uEvts =
    updateComposite comp state wenv _reModel evtsRoot widgetComp
  widgetId = widgetComp ^. L.info . L.widgetId
  path = widgetComp ^. L.info . L.path
  newReqs = toParentReqs reqs
         <> tasksToRequests widgetId path _reTasks
         <> producersToRequests widgetId path _reProducers
         <> uReqs
         <> toParentReqs _reRequests
         <> _reMessages
  newEvts = _reReports <> uEvts
  newResult = WidgetResult uWidget newReqs newEvts

updateComposite
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
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
    | otherwise = resultWidget $ updateSizeReq newState wenv widgetComp
      & L.widget .~ createComposite comp newState

mergeChild
  :: (CompositeModel s, CompositeEvent e, ParentModel sp)
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
  builtWidget = builtRoot ^. L.widget
  mergedResult = widgetMerge builtWidget cwenv widgetRoot $ builtRoot
      & L.info . L.widgetId .~ _cpsRoot ^. L.info . L.widgetId
  mergedState = state {
    _cpsModel = Just newModel,
    _cpsRoot = mergedResult ^. L.node,
    _cpsGlobalKeys = collectGlobalKeys M.empty (mergedResult ^. L.node)
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
  -> WidgetEnv s e
  -> WidgetNode s e
  -> s
  -> Seq e
  -> ReducedEvents s e sp ep
reduceCompEvents globalKeys eventHandler cwenv node model events = result where
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
    response = eventHandler cwenv node (_reModel current) event
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
      _reMessages = _reMessages |> SendMessage (node^.L.info.L.widgetId) message
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

tasksToRequests
  :: CompositeEvent e
  => WidgetId
  -> Path
  -> Seq (IO e)
  -> Seq (WidgetRequest sp)
tasksToRequests widgetId path reqs = RunTask widgetId path <$> reqs

producersToRequests
  :: CompositeEvent e
  => WidgetId
  -> Path
  -> Seq (ProducerHandler e)
  -> Seq (WidgetRequest sp)
producersToRequests widgetId path reqs = RunProducer widgetId path <$> reqs

toParentReqs :: Seq (WidgetRequest s) -> Seq (WidgetRequest sp)
toParentReqs reqs = fmap fromJust $ Seq.filter isJust $ fmap toParentReq reqs

toParentReq :: WidgetRequest s -> Maybe (WidgetRequest sp)
toParentReq IgnoreParentEvents = Just IgnoreParentEvents
toParentReq IgnoreChildrenEvents = Just IgnoreChildrenEvents
toParentReq ResizeWidgets = Just ResizeWidgets
toParentReq (MoveFocus start dir) = Just (MoveFocus start dir)
toParentReq (SetFocus path) = Just (SetFocus path)
toParentReq (GetClipboard path) = Just (GetClipboard path)
toParentReq (SetClipboard clipboard) = Just (SetClipboard clipboard)
toParentReq (StartTextInput rect) = Just (StartTextInput rect)
toParentReq StopTextInput = Just StopTextInput
toParentReq (ResetOverlay wid) = Just (ResetOverlay wid)
toParentReq (SetOverlay wid path) = Just (SetOverlay wid path)
toParentReq (SetCursorIcon wid icon) = Just (SetCursorIcon wid icon)
toParentReq (ResetCursorIcon wid) = Just (ResetCursorIcon wid)
toParentReq (StartDrag wid path info) = Just (StartDrag wid path info)
toParentReq (CancelDrag wid) = Just (CancelDrag wid)
toParentReq RenderOnce = Just RenderOnce
toParentReq (RenderEvery path ms repeat) = Just (RenderEvery path ms repeat)
toParentReq (RenderStop path) = Just (RenderStop path)
toParentReq (ExitApplication exit) = Just (ExitApplication exit)
toParentReq (UpdateWindow req) = Just (UpdateWindow req)
toParentReq (SetWidgetPath wid path) = Just (SetWidgetPath wid path)
toParentReq (ResetWidgetPath wid) = Just (ResetWidgetPath wid)
toParentReq (UpdateModel fn) = Nothing
toParentReq (SendMessage path message) = Just (SendMessage path message)
toParentReq (RunTask wid path action) = Just (RunTask wid path action)
toParentReq (RunProducer wid path action) = Just (RunProducer wid path action)

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
  newPath = firstChildPath parent
  newChild = child
    & L.info . L.widgetId .~ WidgetId (wenv ^. L.timestamp) newPath
    & L.info . L.path .~ newPath
    & L.info . L.overlay .~ (cOverlay || pOverlay)
    & L.info . L.visible .~ (cVisible && pVisible)
    & L.info . L.enabled .~ (cEnabled && pEnabled)

getUpdateModelReqs :: (Traversable t) => t (WidgetRequest s) -> Seq (s -> s)
getUpdateModelReqs reqs = foldl' foldHelper Seq.empty reqs where
  foldHelper acc (UpdateModel fn) = acc |> fn
  foldHelper acc _ = acc

firstChildPath :: WidgetNode s e -> Path
firstChildPath node = node ^. L.info . L.path |> 0
