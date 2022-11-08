{-|
Module      : Monomer.Widgets.Composite
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Composite widget. Main glue between all the other widgets, also acts as the main
app widget. Composite allows to split an application into reusable parts without
the need to implement a lower level widget. It can communicate with its parent
component by reporting events.

Requires two functions:

- UI Builder: creates the widget tree based on the provided Widget Environment
and model. This widget tree is made of other widgets, in general combinations of
containers and singles.
- Event Handler: processes user defined events which are raised by the widgets
created when building the UI.

Composite is discussed in detail in the tutorials.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Composite (
  -- * Re-exported modules
  module Monomer.Core,
  module Monomer.Event,
  module Monomer.Widgets.Util,

  -- * Configuration
  CompositeCfg,
  EventResponse(..),
  CompParentModel,
  CompositeModel,
  CompositeEvent,
  MergeRequired,
  MergeReqsHandler,
  MergeEventsHandler,
  MergeModelHandler,
  EventHandler,
  UIBuilder,
  TaskHandler,
  ProducerHandler,
  CompMsgUpdate,
  compositeMergeReqs,
  compositeMergeEvents,
  compositeMergeModel,

  -- * Constructors
  composite,
  composite_,
  compositeV,
  compositeV_,
  compositeD_
) where

import Debug.Trace

import Control.Applicative ((<|>))
import Control.Exception (AssertionFailed(..), throw)
import Control.Lens (ALens', (&), (^.), (^?), (.~), (%~), (<>~), at, ix, non)
import Control.Monad (when)
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
import Monomer.Graphics.Types
import Monomer.Helper
import Monomer.Widgets.Singles.Spacer
import Monomer.Widgets.Util

import qualified Monomer.Core.Lens as L

-- | Type of the parent's model
type CompParentModel sp = Typeable sp
-- | Type of the composite's model
type CompositeModel s = (Eq s, WidgetModel s)
-- | Type of the composite's event
type CompositeEvent e = WidgetEvent e

-- | Checks if merging the composite is required.
type MergeRequired s e
  = WidgetEnv s e  -- ^ Widget environment.
  -> s             -- ^ Old composite model.
  -> s             -- ^ New composite model
  -> Bool          -- ^ True if merge is required.

-- | Generates requests during the merge process.
type MergeReqsHandler s e sp
  = WidgetEnv s e         -- ^ Widget environment.
  -> WidgetNode s e       -- ^ New widget node.
  -> WidgetNode s e       -- ^ Old widget node.
  -> sp                   -- ^ Parent model.
  -> s                    -- ^ Old composite model.
  -> s                    -- ^ New composite model.
  -> [WidgetRequest s e]  -- ^ The list of requests.

-- | Generates events during the merge process.
type MergeEventsHandler s e sp
  = WidgetEnv s e         -- ^ Widget environment.
  -> WidgetNode s e       -- ^ New widget node.
  -> WidgetNode s e       -- ^ Old widget node.
  -> sp                   -- ^ Parent model.
  -> s                    -- ^ Old composite model.
  -> s                    -- ^ New composite model.
  -> [e]                  -- ^ The list of events.

-- | Allows updating the composite model with information from the parent model.
type MergeModelHandler s e sp
  = WidgetEnv s e         -- ^ Widget environment.
  -> sp                   -- ^ Parent model.
  -> s                    -- ^ Old composite model.
  -> s                    -- ^ New composite model.
  -> s                    -- ^ Updated composite model.

-- | Handles a composite event and returns a set of responses.
type EventHandler s e sp ep
  = WidgetEnv s e
  -> WidgetNode s e
  -> s
  -> e
  -> [EventResponse s e sp ep]

-- | Creates the widget tree based on the given model.
type UIBuilder s e = WidgetEnv s e -> s -> WidgetNode s e

-- | Asynchronous task generating a single event.
type TaskHandler e = IO e

-- | Asynchronous task generating multiple events.
type ProducerHandler e = (e -> IO ()) -> IO ()

-- | Model update function wrapped as a message.
data CompMsgUpdate
  = forall s . CompositeModel s => CompMsgUpdate (s -> s)

{-|
Delayed request. Used to account for widget tree changes in previous steps. When
processing EventResponses that depend on WidgetKeys, resolving the key at the
time the response is created may result in missing/no longer valid keys. The
delayed message allows resolving the key right before the WidgetRequest is
processed.
-}
data CompMsgDelayedRequest
  = CompMsgSetFocus WidgetKey
  | CompMsgMoveFocus (Maybe WidgetKey) FocusDirection
  | forall i . Typeable i => CompMsgMessage WidgetKey i

-- | Response options for an event handler.
data EventResponse s e sp ep
  -- | Modifies the current model, prompting a merge.
  = Model s
  -- | Raises a new event, which will be handled in the same cycle.
  | Event e
  -- | Raises an event that will be handled by the parent Composite.
  | Report ep
  -- | Generates a 'WidgetRequest'.
  | Request (WidgetRequest s e)
  {-|
  Generates a 'WidgetRequest' matching the parent Composite's type. Useful when
  receiving requests as configuration from the parent, since the types will not
  match otherwise.
  -}
  | RequestParent (WidgetRequest sp ep)
  {-|
  Generates a request to set focus on the widget with the matching key. If the
  key does not exist, focus remains on the currently focused widget.
  -}
  | SetFocusOnKey WidgetKey
  {-|
  Generates a request to move focus forward/backward, optionally indicating the
  key of the starting widget.
  -}
  | MoveFocusFromKey (Maybe WidgetKey) FocusDirection
  {-|
  Sends a message to the given key. If the key does not exist, the message will
  not be delivered.
  -}
  | forall i . Typeable i => Message WidgetKey i
  {-|
  Runs an asynchronous task that will return a single result. The task is
  responsible for reporting errors using the expected event type. If the task
  crashes without returning a value, the composite will not know about it.
  -}
  | Task (TaskHandler e)
  {-|
  Runs an asynchronous task that will produce unlimited result. The producer is
  responsible for reporting errors using the expected event type. If the
  producer crashes without sending a value, composite will not know about it.
  -}
  | Producer (ProducerHandler e)

{-|
Configuration options for composite:

- 'onInit': event to raise when the widget is created. Useful for initializing
  required resources.
- 'onDispose': event to raise when the widget is disposed. Useful for freeing
  acquired resources.
- 'onResize': event to raise when the size of the widget changes.
- 'onChange': event to raise when the model changes. The value passed to the
  provided event is the previous version of the model. The current version of
  the model is always available as a parameter in the _handleEvent_ function.
- 'onChangeReq': 'WidgetRequest' to generate when the model changes.
- 'onEnabledChange': event to raise when the enabled status changes.
- 'onVisibleChange': event to raise when the visibility changes.
- 'mergeRequired': indicates if merging is necessary for this widget. In case
  the UI build process references information outside the model, it can be used
  to signal that merging is required even if the model has not changed. It can
  also be used as a performance tweak if the changes do not require rebuilding
  the UI.
- 'compositeMergeReqs': functions to generate WidgetRequests during the merge
  process. Since merge is already handled by Composite (by merging its tree),
  this is complementary for the cases when more control, and the previous
  version of the widget tree, is required.  For example, it is used in
  'Monomer.Widgets.Containers.Confirm' to set the focus on its Accept button
  when visibility is restored (this usually means it was brought to the front in
  a zstack, and the visibility flag of the previous version needs to be
  checked).
- 'compositeMergeModel': Allows updating the composite model with information
  from the parent model. Useful when the composite needs a more complex model
  than what the user is binding.
-}
data CompositeCfg s e sp ep = CompositeCfg {
  _cmcMergeRequired :: Maybe (MergeRequired s e),
  _cmcMergeReqs :: [MergeReqsHandler s e sp],
  _cmcMergeModel :: Maybe (MergeModelHandler s e sp),
  _cmcOnInitReq :: [WidgetRequest s e],
  _cmcOnDisposeReq :: [WidgetRequest s e],
  _cmcOnResize :: [Rect -> e],
  _cmcOnChangeReq :: [s -> WidgetRequest s e],
  _cmcOnEnabledChange :: [e],
  _cmcOnVisibleChange :: [e]
}

instance Default (CompositeCfg s e sp ep) where
  def = CompositeCfg {
    _cmcMergeModel = Nothing,
    _cmcMergeRequired = Nothing,
    _cmcMergeReqs = [],
    _cmcOnInitReq = [],
    _cmcOnDisposeReq = [],
    _cmcOnResize = [],
    _cmcOnChangeReq = [],
    _cmcOnEnabledChange = [],
    _cmcOnVisibleChange = []
  }

instance Semigroup (CompositeCfg s e sp ep) where
  (<>) c1 c2 = CompositeCfg {
    _cmcMergeModel = _cmcMergeModel c2 <|> _cmcMergeModel c1,
    _cmcMergeRequired = _cmcMergeRequired c2 <|> _cmcMergeRequired c1,
    _cmcMergeReqs = _cmcMergeReqs c1 <> _cmcMergeReqs c2,
    _cmcOnInitReq = _cmcOnInitReq c1 <> _cmcOnInitReq c2,
    _cmcOnDisposeReq = _cmcOnDisposeReq c1 <> _cmcOnDisposeReq c2,
    _cmcOnResize = _cmcOnResize c1 <> _cmcOnResize c2,
    _cmcOnChangeReq = _cmcOnChangeReq c1 <> _cmcOnChangeReq c2,
    _cmcOnEnabledChange = _cmcOnEnabledChange c1 <> _cmcOnEnabledChange c2,
    _cmcOnVisibleChange = _cmcOnVisibleChange c1 <> _cmcOnVisibleChange c2
  }

instance Monoid (CompositeCfg s e sp ep) where
  mempty = def

instance CmbMergeRequired (CompositeCfg s e sp ep) (WidgetEnv s e) s where
  mergeRequired fn = def {
    _cmcMergeRequired = Just fn
  }

instance WidgetEvent e => CmbOnInit (CompositeCfg s e sp ep) e where
  onInit fn = def {
    _cmcOnInitReq = [RaiseEvent fn]
  }

instance CmbOnInitReq (CompositeCfg s e sp ep) s e where
  onInitReq req = def {
    _cmcOnInitReq = [req]
  }

instance WidgetEvent e => CmbOnDispose (CompositeCfg s e sp ep) e where
  onDispose fn = def {
    _cmcOnDisposeReq = [RaiseEvent fn]
  }

instance CmbOnDisposeReq (CompositeCfg s e sp ep) s e where
  onDisposeReq req = def {
    _cmcOnDisposeReq = [req]
  }

instance CmbOnResize (CompositeCfg s e sp ep) e Rect where
  onResize fn = def {
    _cmcOnResize = [fn]
  }

instance WidgetEvent e => CmbOnChange (CompositeCfg s e sp ep) s e where
  onChange fn = def {
    _cmcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (CompositeCfg s e sp ep) s e s where
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

{-|
Generate WidgetRequests during the merge process.

This function is not called during initialization; 'onInitReq' can be used.
-}
compositeMergeReqs :: MergeReqsHandler s e sp -> CompositeCfg s e sp ep
compositeMergeReqs fn = def {
  _cmcMergeReqs = [fn]
}

{-|
Generate events during the merge process.

This function is not called during initialization; 'onInit' can be used.
-}
compositeMergeEvents
  :: WidgetEvent e => MergeEventsHandler s e sp -> CompositeCfg s e sp ep
compositeMergeEvents fn = cfg where
  cfg = def {
    _cmcMergeReqs = [wrapper]
  }
  wrapper wenv node oldNode parentModel oldModel newModel
    = RaiseEvent <$> fn wenv node oldNode parentModel oldModel newModel

{-|
Allows updating the composite model with information from the parent model.
Useful when the composite needs a more complex model than what the user is
binding.

For example, a database record may be binded as the model from the parent, but
the composite needs its own boolean flags to toggle visibility on different
sections.

This function is called during both merge and init. On init, the oldModel will
be equal to the current model.
-}
compositeMergeModel :: MergeModelHandler s e sp -> CompositeCfg s e sp ep
compositeMergeModel fn = def {
  _cmcMergeModel = Just fn
}

data Composite s e sp ep = Composite {
  _cmpWidgetData :: !(WidgetData sp s),
  _cmpEventHandler :: !(EventHandler s e sp ep),
  _cmpUiBuilder :: !(UIBuilder s e),
  _cmpMergeRequired :: MergeRequired s e,
  _cmpMergeReqs :: [MergeReqsHandler s e sp],
  _cmpMergeModel :: Maybe (MergeModelHandler s e sp),
  _cmpOnInitReq :: [WidgetRequest s e],
  _cmpOnDisposeReq :: [WidgetRequest s e],
  _cmpOnResize :: [Rect -> e],
  _cmpOnChangeReq :: [s -> WidgetRequest s e],
  _cmpOnEnabledChange :: [e],
  _cmpOnVisibleChange :: [e]
}

data CompositeState s e = CompositeState {
  _cpsModel :: !(Maybe s),
  _cpsRoot :: !(WidgetNode s e),
  _cpsWidgetKeyMap :: WidgetKeyMap s e
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

{-|
Creates a composite taking its model from a lens into the parent model.
-}
composite
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => WidgetType              -- ^ The name of the composite.
  -> ALens' sp s             -- ^ The lens into the parent's model.
  -> UIBuilder s e           -- ^ The UI builder function.
  -> EventHandler s e sp ep  -- ^ The event handler.
  -> WidgetNode sp ep        -- ^ The resulting widget.
composite widgetType field uiBuilder evtHandler = newNode where
  newNode = composite_ widgetType field uiBuilder evtHandler def

{-|
Creates a composite taking its model from a lens into the parent model. Accepts
config.
-}
composite_
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => WidgetType                -- ^ The name of the composite.
  -> ALens' sp s               -- ^ The lens into the parent's model.
  -> UIBuilder s e             -- ^ The UI builder function.
  -> EventHandler s e sp ep    -- ^ The event handler.
  -> [CompositeCfg s e sp ep]  -- ^ The config options.
  -> WidgetNode sp ep          -- ^ The resulting widget.
composite_ widgetType field uiBuilder evtHandler cfgs = newNode where
  widgetData = WidgetLens field
  newNode = compositeD_ widgetType widgetData uiBuilder evtHandler cfgs

-- | Creates a composite using the given model and onChange event handler.
compositeV
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => WidgetType              -- ^ The name of the composite.
  -> s                       -- ^ The model.
  -> (s -> e)                -- ^ The event to report when model changes.
  -> UIBuilder s e           -- ^ The UI builder function.
  -> EventHandler s e sp ep  -- ^ The event handler.
  -> WidgetNode sp ep        -- ^ The resulting widget.
compositeV wType val handler uiBuilder evtHandler = newNode where
  newNode = compositeV_ wType val handler uiBuilder evtHandler def

{-|
Creates a composite using the given model and onChange event handler. Accepts
config.
-}
compositeV_
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => WidgetType                -- ^ The name of the composite.
  -> s                         -- ^ The model.
  -> (s -> e)                  -- ^ The event to report when model changes.
  -> UIBuilder s e             -- ^ The UI builder function.
  -> EventHandler s e sp ep    -- ^ The event handler.
  -> [CompositeCfg s e sp ep]  -- ^ The config options.
  -> WidgetNode sp ep          -- ^ The resulting widget.
compositeV_ wType val handler uiBuilder evtHandler cfgs = newNode where
  widgetData = WidgetValue val
  newCfgs = onChange handler : cfgs
  newNode = compositeD_ wType widgetData uiBuilder evtHandler newCfgs

-- | Creates a composite providing a WidgetData instance and config.
compositeD_
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => WidgetType                -- ^ The name of the composite.
  -> WidgetData sp s           -- ^ The model.
  -> UIBuilder s e             -- ^ The UI builder function.
  -> EventHandler s e sp ep    -- ^ The event handler.
  -> [CompositeCfg s e sp ep]  -- ^ The config options.
  -> WidgetNode sp ep          -- ^ The resulting widget.
compositeD_ wType wData uiBuilder evtHandler configs = newNode where
  config = mconcat configs
  mergeReq = fromMaybe (const (/=)) (_cmcMergeRequired config)
  !widgetRoot = spacer
  composite = Composite {
    _cmpWidgetData = wData,
    _cmpEventHandler = evtHandler,
    _cmpUiBuilder = uiBuilder,
    _cmpMergeRequired = mergeReq,
    _cmpMergeReqs = _cmcMergeReqs config,
    _cmpMergeModel = _cmcMergeModel config,
    _cmpOnInitReq = _cmcOnInitReq config,
    _cmpOnDisposeReq = _cmcOnDisposeReq config,
    _cmpOnResize = _cmcOnResize config,
    _cmpOnChangeReq = _cmcOnChangeReq config,
    _cmpOnEnabledChange = _cmcOnEnabledChange config,
    _cmpOnVisibleChange = _cmcOnVisibleChange config
  }
  state = CompositeState Nothing widgetRoot M.empty
  widget = createComposite composite state
  !newNode = defaultWidgetNode wType widget

createComposite
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> Widget sp ep
createComposite !comp !state = widget where
  widget = Widget {
    widgetInit = compositeInit comp state,
    widgetMerge = compositeMerge comp state,
    widgetDispose = compositeDispose comp state,
    widgetGetState = makeState state,
    widgetGetInstanceTree = compositeGetInstanceTree comp state,
    widgetFindNextFocus = compositeFindNextFocus comp state,
    widgetFindByPoint = compositeFindByPoint comp state,
    widgetFindBranchByPath = compositeFindBranchByPath comp state,
    widgetHandleEvent = compositeHandleEvent comp state,
    widgetHandleMessage = compositeHandleMessage comp state,
    widgetGetSizeReq = compositeGetSizeReq comp state,
    widgetResize = compositeResize comp state,
    widgetRender = compositeRender comp state
  }

-- | Init
compositeInit
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetResult sp ep
compositeInit comp state wenv widgetComp = newResult where
  CompositeState{..} = state

  !mergeModel = _cmpMergeModel comp
  !parentModel = wenv ^. L.model
  !userModel = getUserModel comp wenv
  !model = case mergeModel of
    Just merge -> merge cwenv parentModel userModel userModel where
      !cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap userModel
    _ -> userModel

  -- Creates UI using provided function
  !cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model
  !builtRoot = _cmpUiBuilder comp cwenv model
  !tempRoot = cascadeCtx wenv widgetComp builtRoot

  WidgetResult root reqs = widgetInit (tempRoot ^. L.widget) cwenv tempRoot
  !newState = state {
    _cpsModel = Just model,
    _cpsRoot = root,
    _cpsWidgetKeyMap = collectWidgetKeys M.empty root
  }

  getBaseStyle wenv node = Nothing
  styledComp = initNodeStyle getBaseStyle wenv widgetComp

  initReqs = Seq.fromList (_cmpOnInitReq comp)
  tempResult = WidgetResult root (RenderOnce <| reqs <> initReqs)
  !newResult = toParentResult comp newState wenv styledComp tempResult

-- | Merge
compositeMerge
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
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
  CompositeState oldModel oldRoot oldWidgetKeys = validState

  !mergeModel = _cmpMergeModel comp
  !parentModel = wenv ^. L.model
  !userModel = getUserModel comp wenv
  !model = case mergeModel of
    Just merge -> merge cwenv parentModel (fromJust oldModel) userModel where
      cwenv = convertWidgetEnv wenv oldWidgetKeys userModel
    _ -> userModel

  -- Creates new UI using provided function
  cwenv = convertWidgetEnv wenv oldWidgetKeys model
  tempRoot = cascadeCtx wenv newComp (_cmpUiBuilder comp cwenv model)
  tempWidget = tempRoot ^. L.widget
  -- Needed in case the user references something outside model when building UI
  -- The same model is provided as old since nothing else is available, but
  -- mergeRequired may be using data from a closure
  modelChanged = _cmpMergeRequired comp cwenv (fromJust oldModel) model
  visibleChg = nodeVisibleChanged oldComp newComp
  enabledChg = nodeEnabledChanged oldComp newComp
  flagsChanged = visibleChg || enabledChg
  themeChanged = wenv ^. L.themeChanged
  mergeRequired
    | isJust oldModel = modelChanged || flagsChanged || themeChanged
    | otherwise = True
  initRequired = not (nodeMatches tempRoot oldRoot)
  useNewRoot = initRequired || mergeRequired

  WidgetResult !newRoot !tmpReqs
    | initRequired = widgetInit tempWidget cwenv tempRoot
    | mergeRequired = widgetMerge tempWidget cwenv tempRoot oldRoot
    | otherwise = resultNode oldRoot
  !newState = validState {
    _cpsModel = Just model,
    _cpsRoot = newRoot,
    _cpsWidgetKeyMap = collectWidgetKeys M.empty newRoot
  }
  getBaseStyle wenv node = Nothing
  styledComp = initNodeStyle getBaseStyle wenv newComp
    & L.info . L.widgetId .~ oldComp ^. L.info . L.widgetId
    & L.info . L.viewport .~ oldComp ^. L.info . L.viewport
    & L.info . L.sizeReqW .~ oldComp ^. L.info . L.sizeReqW
    & L.info . L.sizeReqH .~ oldComp ^. L.info . L.sizeReqH

  visibleEvts
    | useNewRoot && visibleChg = _cmpOnVisibleChange comp
    | otherwise = []
  enabledEvts
    | useNewRoot && enabledChg = _cmpOnEnabledChange comp
    | otherwise = []
  evts = RaiseEvent <$> Seq.fromList (visibleEvts ++ enabledEvts)

  mergeReqsFns = _cmpMergeReqs comp
  mergeHelper f = f cwenv newRoot oldRoot parentModel (fromJust oldModel) model
  mergeReqs = concatMap mergeHelper mergeReqsFns
  extraReqs = seqCatMaybes (toParentReq widgetId <$> Seq.fromList mergeReqs)

  tmpResult = WidgetResult newRoot (RenderOnce <| tmpReqs <> extraReqs <> evts)
  reducedResult
    | useNewRoot = toParentResult comp newState wenv styledComp tmpResult
    | otherwise = resultNode oldComp
  !newResult = handleWidgetIdChange oldComp reducedResult

-- | Dispose
compositeDispose
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetResult sp ep
compositeDispose comp state wenv widgetComp = result where
  CompositeState{..} = state

  model = getCompositeModel state
  cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model
  widget = _cpsRoot ^. L.widget
  widgetId = widgetComp ^. L.info . L.widgetId

  handleReq (RaiseEvent evt) = reqs where
    WidgetResult _ reqs = handleMsgEvent comp state wenv widgetComp evt
  handleReq req = maybe Seq.empty Seq.singleton (toParentReq widgetId req)

  parentReqs = mconcat (handleReq <$> _cmpOnDisposeReq comp)

  WidgetResult _ childReqs = widgetDispose widget cwenv _cpsRoot
  tempResult = WidgetResult _cpsRoot childReqs
  result = toParentResult comp state wenv widgetComp tempResult
    & L.requests %~ (parentReqs <>)

compositeGetInstanceTree
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetInstanceNode
compositeGetInstanceTree comp state wenv node = instTree where
  CompositeState{..} = state
  widget = _cpsRoot ^. L.widget
  model = getCompositeModel state
  cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model
  cInstTree = widgetGetInstanceTree widget cwenv _cpsRoot
  instTree = WidgetInstanceNode {
    _winInfo = node ^. L.info,
    _winState = Just (WidgetState state),
    _winChildren = Seq.singleton cInstTree
  }

-- | Next focusable
compositeFindNextFocus
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
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
  model = getCompositeModel state
  cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model
  nextFocus = widgetFindNextFocus widget cwenv _cpsRoot dir start

-- | Find
compositeFindByPoint
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
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
    model = getCompositeModel state
    cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model
    next = nextTargetStep widgetComp start
    validStep = isNothing next || next == Just 0
    resultInfo = widgetFindByPoint widget cwenv _cpsRoot start point

compositeFindBranchByPath
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> Path
  -> Seq WidgetNodeInfo
compositeFindBranchByPath comp state wenv widgetComp path
  | info ^. L.path == path = Seq.singleton info
  | nextStep == Just 0 = info <| childrenInst
  | otherwise = Seq.empty
  where
    CompositeState{..} = state
    model = getCompositeModel state
    cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model
    info = widgetComp ^. L.info
    nextStep = nextTargetStep widgetComp path
    child = _cpsRoot
    childrenInst = widgetFindBranchByPath (child ^. L.widget) cwenv child path

-- | Event handling
compositeHandleEvent
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
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
  !model = getCompositeModel state
  !cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model
  rootEnabled = _cpsRoot ^. L.info . L.enabled
  compVisible = widgetComp ^. L.info . L.visible
  compEnabled = widgetComp ^. L.info . L.enabled

  processEvent = toParentResult comp state wenv widgetComp
  !evtResult
    | not (compVisible && compEnabled) = Nothing
    | rootEnabled = widgetHandleEvent widget cwenv _cpsRoot target evt
    | otherwise = Nothing
  !result = fmap processEvent evtResult

-- | Message handling
compositeHandleMessage
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp, Typeable i)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> Path
  -> i
  -> Maybe (WidgetResult sp ep)
compositeHandleMessage comp state@CompositeState{..} !wenv !widgetComp !target arg
  | isTargetReached widgetComp target = case cast arg of
      Just evt -> Just $ handleMsgEvent comp state wenv widgetComp evt
      Nothing -> case cast arg of
        Just (CompMsgUpdate msg) -> handleMsgUpdate comp state wenv widgetComp <$> cast msg
        Nothing -> case cast arg of
          Just req -> handleDelayedRequest comp state wenv widgetComp req
          _ -> traceShow ("Failed match on Composite handleMessage", typeOf arg) Nothing
  | otherwise = fmap processEvent result where
      processEvent = toParentResult comp state wenv widgetComp
      cmpWidget = _cpsRoot ^. L.widget
      !model = getCompositeModel state
      !cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model
      result = widgetHandleMessage cmpWidget cwenv _cpsRoot target arg

-- Preferred size
compositeGetSizeReq
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> (SizeReq, SizeReq)
compositeGetSizeReq comp state wenv widgetComp = (newReqW, newReqH) where
  CompositeState{..} = state
  style = currentStyle wenv widgetComp
  widget = _cpsRoot ^. L.widget
  currReqW = _cpsRoot ^. L.info . L.sizeReqW
  currReqH = _cpsRoot ^. L.info . L.sizeReqH
  (tmpReqW, tmpReqH) = sizeReqAddStyle style (currReqW, currReqH)
  -- User settings take precedence
  newReqW = fromMaybe tmpReqW (style ^. L.sizeReqW)
  newReqH = fromMaybe tmpReqH (style ^. L.sizeReqH)

-- Preferred size
updateSizeReq
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
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
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> Rect
  -> (Path -> Bool)
  -> WidgetResult sp ep
compositeResize comp state wenv widgetComp viewport rszReq = resizedRes where
  CompositeState{..} = state
  style = currentStyle wenv widgetComp
  carea = fromMaybe def (removeOuterBounds style viewport)
  widget = _cpsRoot ^. L.widget
  model = getCompositeModel state
  cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model

  WidgetResult newRoot newReqs = widgetResize widget cwenv _cpsRoot carea rszReq
  oldVp = widgetComp ^. L.info . L.viewport
  sizeChanged = viewport /= oldVp
  resizeEvts = fmap ($ viewport) (_cmpOnResize comp)
  resizeReqs
    | sizeChanged = RaiseEvent <$> Seq.fromList resizeEvts
    | otherwise = Empty

  childRes = WidgetResult newRoot (newReqs <> resizeReqs)
    & L.node . L.info . L.viewport .~ carea
  resizedRes = toParentResult comp state wenv widgetComp childRes
    & L.node . L.info . L.viewport .~ viewport

-- Render
compositeRender
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> Renderer
  -> IO ()
compositeRender comp state wenv widgetComp renderer =
  when (isWidgetVisible wenv widgetComp) $
    drawStyledAction renderer viewport style $ \_ ->
      widgetRender widget cwenv _cpsRoot renderer
  where
    CompositeState{..} = state
    widget = _cpsRoot ^. L.widget
    viewport = widgetComp ^. L.info . L.viewport
    style = currentStyle wenv widgetComp
    !model = getCompositeModel state
    !cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model

handleMsgEvent
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> e
  -> WidgetResult sp ep
handleMsgEvent comp state wenv widgetComp event = newResult where
  CompositeState{..} = state
  evtHandler = _cmpEventHandler comp
  !model = getCompositeModel state
  !cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap model
  !response = evtHandler cwenv _cpsRoot model event
  !newReqs = evtResponseToRequest widgetComp _cpsWidgetKeyMap <$> response
  !newResult = WidgetResult widgetComp (Seq.fromList (catMaybes newReqs))

handleMsgUpdate
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> (s -> s)
  -> WidgetResult sp ep
handleMsgUpdate comp state wenv widgetComp fnUpdate = result where
  CompositeState{..} = state
  !model = getCompositeModel state
  !newModel = fnUpdate model
  !result
    | model == newModel = resultNode widgetComp
    | otherwise = mergeChild comp state wenv newModel _cpsRoot widgetComp

toParentResult
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> WidgetResult s e
  -> WidgetResult sp ep
toParentResult comp state !wenv !widgetComp !result = newResult where
  WidgetResult newRoot reqs = result
  widgetId = widgetComp ^. L.info . L.widgetId
  newState = state {
    _cpsRoot = newRoot
  }
  newComp = widgetComp
    & L.widget .~ createComposite comp newState
  newNode = updateSizeReq comp newState wenv newComp
  newReqs = seqCatMaybes (toParentReq widgetId <$> reqs)
  !newResult = WidgetResult newNode newReqs

evtResponseToRequest
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => WidgetNode sp ep
  -> WidgetKeyMap s e
  -> EventResponse s e sp ep
  -> Maybe (WidgetRequest sp ep)
evtResponseToRequest widgetComp widgetKeys response = case response of
  Model newModel -> Just $ sendMsgTo widgetComp (CompMsgUpdate $ const newModel)
  Event event -> Just $ sendMsgTo widgetComp event
  Report report -> Just (RaiseEvent report)
  Request req -> toParentReq widgetId req
  RequestParent req -> Just req
  SetFocusOnKey key -> Just $ sendMsgTo widgetComp (CompMsgSetFocus key)
  MoveFocusFromKey key dir -> Just $ sendMsgTo widgetComp (CompMsgMoveFocus key dir)
  Message key msg -> Just $ sendMsgTo widgetComp (CompMsgMessage key msg)
  Task task -> Just $ RunTask widgetId path task
  Producer producer -> Just $ RunProducer widgetId path producer
  where
    widgetId = widgetComp ^. L.info . L.widgetId
    path = widgetComp ^. L.info . L.path

handleDelayedRequest
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> WidgetNode sp ep
  -> CompMsgDelayedRequest
  -> Maybe (WidgetResult sp ep)
handleDelayedRequest comp state wenv node req = result where
    widgetKeys = _cpsWidgetKeyMap state
    newReq = case req of
      CompMsgSetFocus key -> setFocus <$> lookupNode widgetKeys "SetFocusOnKey" key
      CompMsgMoveFocus (Just key) dir -> moveFocusFrom key dir
      CompMsgMoveFocus _ dir -> Just $ MoveFocus Nothing dir
      CompMsgMessage key msg -> (`sendMsgTo` msg) <$> lookupNode widgetKeys "Message" key
    result = resultReqs node . (: []) <$> newReq

    setFocus node = SetFocus (node ^. L.info . L.widgetId)
    moveFocusFrom key dir = mwid >> Just (MoveFocus mwid dir) where
      mnode = lookupNode widgetKeys "MoveFocusFromKey" key
      mwid = (^. L.info . L.widgetId) <$> mnode

mergeChild
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> CompositeState s e
  -> WidgetEnv sp ep
  -> s
  -> WidgetNode s e
  -> WidgetNode sp ep
  -> WidgetResult sp ep
mergeChild comp state wenv newModel widgetRoot widgetComp = parentResult where
  CompositeState{..} = state
  oldModel = getCompositeModel state
  cwenv = convertWidgetEnv wenv _cpsWidgetKeyMap newModel
  widgetId = _cpsRoot ^. L.info . L.widgetId
  builtRoot = cascadeCtx wenv widgetComp (_cmpUiBuilder comp cwenv newModel)
      & L.info . L.widgetId .~ widgetId
  builtWidget = builtRoot ^. L.widget
  initRequired = not (nodeMatches widgetRoot builtRoot)
  mergedResult
    | initRequired = widgetInit builtWidget cwenv builtRoot
    | otherwise = widgetMerge builtWidget cwenv builtRoot widgetRoot
  !mergedState = state {
    _cpsModel = Just newModel,
    _cpsRoot = mergedResult ^. L.node,
    _cpsWidgetKeyMap = collectWidgetKeys M.empty (mergedResult ^. L.node)
  }
  childReqs = fmap ($ oldModel) (_cmpOnChangeReq comp)
  parentReqs = widgetDataSet (_cmpWidgetData comp) newModel
    ++ [ResizeWidgets widgetId | initRequired]
  childResult = mergedResult
    & L.requests <>~ Seq.fromList childReqs
  result = toParentResult comp mergedState wenv widgetComp childResult
  parentResult = result
    & L.requests .~ Seq.fromList parentReqs <> result ^. L.requests

getUserModel
  :: (CompositeModel s, CompositeEvent e, CompositeEvent ep, CompParentModel sp)
  => Composite s e sp ep
  -> WidgetEnv sp ep
  -> s
getUserModel comp wenv = widgetDataGet (_weModel wenv) (_cmpWidgetData comp)

getCompositeModel
  :: (CompositeModel s, CompositeEvent e)
  => CompositeState s e
  -> s
getCompositeModel state = case _cpsModel state of
  Just model -> model
  _ -> error "Error calling getCompositeModel: widgetInit has not been invoked."

toParentReq
  :: (CompositeModel s, CompParentModel sp)
  => WidgetId
  -> WidgetRequest s e
  -> Maybe (WidgetRequest sp ep)
toParentReq _ IgnoreParentEvents = Just IgnoreParentEvents
toParentReq _ IgnoreChildrenEvents = Just IgnoreChildrenEvents
toParentReq _ (ResizeWidgets wid) = Just (ResizeWidgets wid)
toParentReq _ (ResizeWidgetsImmediate wid) = Just (ResizeWidgetsImmediate wid)
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
toParentReq _ (RemoveRendererImage name) = Just (RemoveRendererImage name)
toParentReq _ (ExitApplication exit) = Just (ExitApplication exit)
toParentReq _ (UpdateWindow req) = Just (UpdateWindow req)
toParentReq _ (SetWidgetPath wid path) = Just (SetWidgetPath wid path)
toParentReq _ (ResetWidgetPath wid) = Just (ResetWidgetPath wid)
toParentReq wid (UpdateModel fn) = Just (SendMessage wid (CompMsgUpdate fn))
toParentReq wid (RaiseEvent message) = Just (SendMessage wid message)
toParentReq _ (SendMessage wid message) = Just (SendMessage wid message)
toParentReq _ (RunTask wid path action) = Just (RunTask wid path action)
toParentReq _ (RunProducer wid path action) = Just (RunProducer wid path action)
toParentReq _ (RunInRenderThread wid path action) = Just (RunInRenderThread wid path action)

collectWidgetKeys
  :: Map WidgetKey (WidgetNode s e)
  -> WidgetNode s e
  -> Map WidgetKey (WidgetNode s e)
collectWidgetKeys keys node = newMap where
  children = node ^. L.children
  collect currKeys child = collectWidgetKeys currKeys child
  updatedMap = case node ^. L.info . L.key of
    Just key -> M.insert key node keys
    _ -> keys
  newMap = foldl' collect updatedMap children

convertWidgetEnv :: WidgetEnv sp ep -> WidgetKeyMap s e -> s -> WidgetEnv s e
convertWidgetEnv wenv widgetKeyMap model = WidgetEnv {
  _weOs = _weOs wenv,
  _weDpr = _weDpr wenv,
  _weAppStartTs = _weAppStartTs wenv,
  _weFontManager = _weFontManager wenv,
  _weFindBranchByPath = _weFindBranchByPath wenv,
  _weMainButton = _weMainButton wenv,
  _weContextButton = _weContextButton wenv,
  _weTheme = _weTheme wenv,
  _weWindowSize = _weWindowSize wenv,
  _weWidgetShared = _weWidgetShared wenv,
  _weWidgetKeyMap = widgetKeyMap,
  _weCursor = _weCursor wenv,
  _weHoveredPath = _weHoveredPath wenv,
  _weFocusedPath = _weFocusedPath wenv,
  _weDragStatus = _weDragStatus wenv,
  _weMainBtnPress = _weMainBtnPress wenv,
  _weOverlayPath = _weOverlayPath wenv,
  _weModel = model,
  _weInputStatus = _weInputStatus wenv,
  _weTimestamp = _weTimestamp wenv,
  _weThemeChanged = _weThemeChanged wenv,
  _weInTopLayer = _weInTopLayer wenv,
  _weLayoutDirection = LayoutNone,
  _weViewport = _weViewport wenv,
  _weOffset = _weOffset wenv
}

cascadeCtx
  :: WidgetEnv sp ep -> WidgetNode sp ep -> WidgetNode s e -> WidgetNode s e
cascadeCtx wenv parent child = newChild where
  pVisible = parent ^. L.info . L.visible
  pEnabled = parent ^. L.info . L.enabled
  cVisible = child ^. L.info . L.visible
  cEnabled = child ^. L.info . L.enabled
  newPath = parent ^. L.info . L.path |> 0
  newChild = child
    & L.info . L.widgetId .~ WidgetId (wenv ^. L.timestamp) newPath
    & L.info . L.path .~ newPath
    & L.info . L.visible .~ (cVisible && pVisible)
    & L.info . L.enabled .~ (cEnabled && pEnabled)

lookupNode :: WidgetKeyMap s e -> String -> WidgetKey -> Maybe (WidgetNode s e)
lookupNode widgetKeys desc key = case M.lookup key widgetKeys of
  Nothing -> trace ("Key " ++ show key ++ " not found for " ++ desc) Nothing
  res -> res

sendMsgTo :: Typeable i => WidgetNode s e -> i -> WidgetRequest sp ep
sendMsgTo node msg = SendMessage (node ^. L.info . L.widgetId) msg
