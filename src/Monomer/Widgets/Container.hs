{-|
Module      : Monomer.Widgets.Container
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper for creating widgets with children elements.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Container (
  -- * Re-exported modules
  module Monomer.Core,
  module Monomer.Core.Combinators,
  module Monomer.Event,
  module Monomer.Graphics,
  module Monomer.Widgets.Util,

  -- * Configuration
  ContainerGetBaseStyle,
  ContainerGetCurrentStyle,
  ContainerCreateContainerFromModel,
  ContainerUpdateCWenvHandler,
  ContainerInitHandler,
  ContainerInitPostHandler,
  ContainerMergeChildrenReqHandler,
  ContainerMergeHandler,
  ContainerMergePostHandler,
  ContainerDisposeHandler,
  ContainerFindNextFocusHandler,
  ContainerFindByPointHandler,
  ContainerFilterHandler,
  ContainerEventHandler,
  ContainerMessageHandler,
  ContainerGetSizeReqHandler,
  ContainerResizeHandler,
  ContainerRenderHandler,
  Container(..),
  updateWenvOffset,

  -- * Constructors
  createContainer
) where

import Control.Applicative ((<|>))
import Control.Exception (AssertionFailed(..), throw)
import Control.Lens ((&), (^.), (^?), (.~), (%~), (<>~), _Just)
import Control.Monad
import Data.Default
import Data.Foldable (fold, foldl')
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Typeable (Typeable)
import Data.Sequence (Seq(..), (<|), (|>))

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Util

import qualified Monomer.Lens as L

{-|
Returns the base style for this type of widget.

Usually this style comes from the active theme.
-}
type ContainerGetBaseStyle s e
  = GetBaseStyle s e  -- ^ The base style for a new node.

{-|
Returns the active style for this type of widget. It depends on the state of
the widget, which can be:

- Basic
- Hovered
- Focused
- Hovered and Focused
- Active
- Disabled

In general there's no needed to override it, except when the widget does not use
the full content rect.

An example can be found in "Monomer.Widgets.Containers.Tooltip".
-}
type ContainerGetCurrentStyle s e
  = WidgetEnv s e      -- ^ The widget environment.
  -> WidgetNode s e    -- ^ The widget node.
  -> StyleState        -- ^ The active style for the node.

{-|
Returns an updated version of the Container instance. Currently only used
during merge. Not needed in general, except for widgets that modify offset
or layout direction.

An example can be found in "Monomer.Widgets.Containers.Scroll".

Note:

Some widgets, scroll for example, provide offset/layout direction to the
Container instance; this information is passed down to children widgets in all
the lifecycle methods. During merge a problem arises: the new node has not yet
processed the old state and, since it is the Container whose merge function is
being invoked, it is not able to provide the correct offset/layout direction.
It is also not possible to directly extract this information from the old node,
because we have a Widget instance and not its Container instance. This function
provides a workaround for this.

This is a hacky solution; a more flexible dispatcher for Composite could avoid it.
-}
type ContainerCreateContainerFromModel s e a
  = WidgetEnv s e            -- ^ The widget environment.
  -> WidgetNode s e          -- ^ The widget node.
  -> a                       -- ^ The previous model.
  -> Maybe (Container s e a) -- ^ An updated Container instance.

{-|
Updates the widget environment before passing it down to children. This function
is called during the execution of all the widget functions. Useful for
restricting viewport or modifying other kind of contextual information.

An example can be found in "Monomer.Widgets.Containers.ThemeSwitch".
-}
type ContainerUpdateCWenvHandler s e
  = WidgetEnv s e    -- ^ The widget environment.
  -> WidgetNode s e  -- ^ The widget node.
  -> WidgetNode s e  -- ^ The child node.
  -> Int             -- ^ The index of the node.
  -> WidgetEnv s e   -- ^ The updated widget environment.

{-|
Initializes the given node. This could include rebuilding the widget in case
internal state needs to use model/environment information, generate user
events or make requests to the runtime.

An example can be found in "Monomer.Widgets.Containers.SelectList".

Most of the current containers serve layout purposes and don't need a custom
/init/.
-}
type ContainerInitHandler s e
  = WidgetEnv s e      -- ^ The widget environment.
  -> WidgetNode s e    -- ^ The widget node.
  -> WidgetResult s e  -- ^ The result of the init operation.

{-|
Allows making further operations after children have been initialized.

Note: if state was modified on 'containerInit', you should use the new state
provided as an argument, since the state referenced in the closure will be
outdated.
-}
type ContainerInitPostHandler s e a
  = WidgetEnv s e      -- ^ The widget environment.
  -> WidgetNode s e    -- ^ The widget node.
  -> a                 -- ^ The current state of the widget node.
  -> WidgetResult s e  -- ^ The result after children have been initialized.
  -> WidgetResult s e  -- ^ The result of the init post operation.

{-|
Returns whether merge is required for children. It's mostly used for performance
optimization.

An example can be found in "Monomer.Widgets.Containers.SelectList".
-}
type ContainerMergeChildrenReqHandler s e a
  = WidgetEnv s e    -- ^ The widget environment.
  -> WidgetNode s e  -- ^ The widget node.
  -> WidgetNode s e  -- ^ The previous widget node.
  -> a               -- ^ The state of the previous widget node.
  -> Bool            -- ^ True if widget is needed.

{-|
Merges the current node with the node it matched with during the merge process.
Receives the newly created node (whose *init* function is not called), the
previous node and the state extracted from that node. This process is widget
dependent, and may use or ignore the previous state depending on newly available
information.

In general, you want to at least keep the previous state unless the widget is
stateless or only consumes model/environment information.

Examples can be found in "Monomer.Widgets.Containers.Fade" and
"Monomer.Widgets.Containers.Tooltip". On the other hand,
"Monomer.Widgets.Containers.Grid" does not need to override merge since it's
stateless.
-}
type ContainerMergeHandler s e a
  = WidgetEnv s e      -- ^ The widget environment.
  -> WidgetNode s e    -- ^ The widget node.
  -> WidgetNode s e    -- ^ The previous widget node.
  -> a                 -- ^ The state of the previous widget node.
  -> WidgetResult s e  -- ^ The result of the merge operation.

{-|
Allows making further operations after children have been merged.

Examples can be found in "Monomer.Widgets.Containers.SelectList" and
"Monomer.Widgets.Containers.ZStack".

Note: if state was modified during merge, you should use the new state provided
as an argument, since the state referenced in the closure will be outdated.
-}
type ContainerMergePostHandler s e a
  = WidgetEnv s e      -- ^ The widget environment.
  -> WidgetNode s e    -- ^ The widget node.
  -> WidgetNode s e    -- ^ The previous widget node.
  -> a                 -- ^ The state of the previous widget node.
  -> a                 -- ^ The current state of the widget node.
  -> WidgetResult s e  -- ^ The result after children have been merged.
  -> WidgetResult s e  -- ^ The result of the merge post operation.

{-|
Disposes the current node. Only used by widgets which allocate resources during
/init/ or /merge/, and will usually involve requests to the runtime.

An example can be found "Monomer.Widgets.Containers.Dropdown".
-}
type ContainerDisposeHandler s e
  = WidgetEnv s e      -- ^ The widget environment.
  -> WidgetNode s e    -- ^ The widget node.
  -> WidgetResult s e  -- ^ The result of the dispose operation.

{-|
Returns the next focusable node. What next/previous is, depends on how the
container works. Moving right -> bottom is usually considered forward.
-}
type ContainerFindNextFocusHandler s e
  = WidgetEnv s e          -- ^ The widget environment.
  -> WidgetNode s e        -- ^ The widget node.
  -> FocusDirection        -- ^ The direction in which focus is moving.
  -> Path                  -- ^ The start path from which to search.
  -> Seq (WidgetNode s e)  -- ^ The next focusable node info.

{-|
Returns the currently hovered widget, if any. If the widget is rectangular and
uses the full content area, there is not need to override this function.

An example can be found "Monomer.Widgets.Containers.Dropdown".
-}
type ContainerFindByPointHandler s e
  = WidgetEnv s e    -- ^ The widget environment.
  -> WidgetNode s e  -- ^ The widget node.
  -> Path            -- ^ The start path from which to search.
  -> Point           -- ^ The point to test for.
  -> Maybe Int       -- ^ The hovered child index, if any.

{-|
Receives a System event and, optionally, modifies the event, its target, or
cancels the event propagation by returning null.

Examples can be found in "Monomer.Widgets.Containers.Base.LabeledItem".
-}
type ContainerFilterHandler s e
  = WidgetEnv s e               -- ^ The widget environment.
  -> WidgetNode s e             -- ^ The widget node.
  -> Path                       -- ^ The target path of the event.
  -> SystemEvent                -- ^ The SystemEvent to handle.
  -> Maybe (Path, SystemEvent)  -- ^ The optional modified event/target.

{-|
Receives a System event and, optionally, returns a result. This can include an
updated version of the widget (in case it has internal state), user events or
requests to the runtime.

Examples can be found in "Monomer.Widgets.Containers.Draggable" and
"Monomer.Widgets.Containers.Keystroke".
-}
type ContainerEventHandler s e
  = WidgetEnv s e              -- ^ The widget environment.
  -> WidgetNode s e            -- ^ The widget node.
  -> Path                      -- ^ The target path of the event.
  -> SystemEvent               -- ^ The SystemEvent to handle.
  -> Maybe (WidgetResult s e)  -- ^ The result of handling the event, if any.

{-|
Receives a message and, optionally, returns a result. This can include an
updated version of the widget (in case it has internal state), user events or
requests to the runtime. There is no validation regarding the message type, and
the widget should take care of _casting_ to the correct type using
"Data.Typeable.cast"

Examples can be found in "Monomer.Widgets.Containers.Fade" and
"Monomer.Widgets.Containers.Scroll".
-}
type ContainerMessageHandler s e
  = forall i . Typeable i
  => WidgetEnv s e             -- ^ The widget environment.
  -> WidgetNode s e            -- ^ The widget node.
  -> Path                      -- ^ The target path of the message.
  -> i                         -- ^ The message to handle.
  -> Maybe (WidgetResult s e)  -- ^ The result of handling the message, if any.

{-|
Returns the preferred size for the widget. This size should not include border
and padding; those are added automatically by Container.

This is called to update WidgetNodeInfo only at specific times.

Examples can be found in "Monomer.Widgets.Containers.Grid" and
"Monomer.Widgets.Containers.Stack".
-}
type ContainerGetSizeReqHandler s e
  = WidgetEnv s e          -- ^ The widget environment.
  -> WidgetNode s e        -- ^ The widget node.
  -> Seq (WidgetNode s e)  -- ^ The children widgets
  -> (SizeReq, SizeReq)    -- ^ The horizontal and vertical requirements.

{-|
Assigns space to children according to the Container's logic, returning a 'Seq'
of 'Rect's in the same order of the child widgets. If the container has a single
child it is not generally necessary to override this function. A new version of
the Container may be returned if state needs to be updated.

Examples can be found in "Monomer.Widgets.Containers.Grid" and
"Monomer.Widgets.Containers.Stack".

Note: if the Container's state depends on the current size, visibility should be
considered; since invisible nodes receive zero space, it may affect calculations
and cause unexpected behavior when the widget is made visible again. Examples
can be found in "Monomer.Widgets.Containers.Scroll" and
"Monomer.Widgets.Containers.Split".
-}
type ContainerResizeHandler s e
  = WidgetEnv s e                  -- ^ The widget environment.
  -> WidgetNode s e                -- ^ The widget node.
  -> Rect                          -- ^ The new viewport.
  -> Seq (WidgetNode s e)          -- ^ The children widgets
  -> (WidgetResult s e, Seq Rect)  -- ^ The result of resizing the widget.

{-|
Renders the widget's content using the given Renderer. In general, this method
needs to be overriden. There are two render methods: one runs before children,
the other one after.

Examples can be found in "Monomer.Widgets.Containers.Draggable" and
"Monomer.Widgets.Containers.Scroll".
-}
type ContainerRenderHandler s e
  = WidgetEnv s e      -- ^ The widget environment.
  -> WidgetNode s e    -- ^ The widget node.
  -> Renderer          -- ^ The renderer, providing low level drawing functions.
  -> IO ()             -- ^ The IO action with rendering instructions.

-- | Interface for Container widgets.
data Container s e a = Container {
  -- | True if border and padding should be added to size requirement. Defaults
  --   to True.
  containerAddStyleReq :: Bool,
  -- | Offset to apply to children. This not only includes rendering, but also
  --   updating SystemEvents and all coordinate related functions.
  containerChildrenOffset :: Maybe Point,
  -- | Scissor to apply to child widgets. This is not the same as the widget
  --   enabled by containerUseScissor
  containerChildrenScissor :: Maybe Rect,
  -- | If True, the container will render its background and border. In some
  --   cases passing rendering control to children is useful. Defaults to True.
  containerDrawDecorations :: Bool,
  -- | The layout direction generated by this widget. If one is indicated, it
  --   can be used by widgets such as "Monomer.Widgets.Singles.Spacer"
  containerLayoutDirection :: LayoutDirection,
  -- | If True, when none of the children is found under the pointer, indicates
  --   an event will not be handled. If False, the parent (i.e., current) widget
  --   will be returned. This is useful when using zstack and wanting for events
  --   to be handled in lower layers.
  containerIgnoreEmptyArea :: Bool,
  -- | True if style cursor should be ignored. If it's False, cursor changes need
  --   to be handled in custom code. Defaults to False.
  containerUseCustomCursor :: Bool,
  -- | If true, it will ignore extra space assigned by the parent container, but
  --   it will not use more space than assigned. Defaults to False.
  containerUseCustomSize :: Bool,
  -- | If true, it will accept the size requested by children, restricted to the
  --   space already assigned.
  containerUseChildrenSizes :: Bool,
  -- | True if automatic scissoring needs to be applied. Defaults to False.
  containerUseScissor :: Bool,
  -- | Returns the base style for this type of widget.
  containerGetBaseStyle :: ContainerGetBaseStyle s e,
  -- | Returns an updated version of the Container instance.
  containerCreateContainerFromModel :: ContainerCreateContainerFromModel s e a,
  -- | Returns the current style, depending on the status of the widget.
  containerGetCurrentStyle :: ContainerGetCurrentStyle s e,
  -- | Updates the widget environment before passing it down to children.
  containerUpdateCWenv :: ContainerUpdateCWenvHandler s e,
  -- | Initializes the given node.
  containerInit :: ContainerInitHandler s e,
  -- | Allow for extra steps after children are initialized.
  containerInitPost :: ContainerInitPostHandler s e a,
  -- | Returns whether merge is required for children.
  containerMergeChildrenReq :: ContainerMergeChildrenReqHandler s e a,
  -- | Merges the node with the node it matched with during the merge process.
  containerMerge :: ContainerMergeHandler s e a,
  -- | Allow for extra steps after children are merged.
  containerMergePost :: ContainerMergePostHandler s e a,
  -- | Disposes the current node.
  containerDispose :: ContainerDisposeHandler s e,
  -- | Returns the next focusable node.
  containerFindNextFocus :: ContainerFindNextFocusHandler s e,
  -- | Returns the currently hovered widget, if any.
  containerFindByPoint :: ContainerFindByPointHandler s e,
  -- | Receives a System event and, optionally, filters/modifies it.
  containerFilterEvent :: ContainerFilterHandler s e,
  -- | Receives a System event and, optionally, returns a result.
  containerHandleEvent :: ContainerEventHandler s e,
  -- | Receives a message and, optionally, returns a result.
  containerHandleMessage :: ContainerMessageHandler s e,
  -- | Returns the preferred size for the widget.
  containerGetSizeReq :: ContainerGetSizeReqHandler s e,
  -- | Resizes the widget to the provided size.
  containerResize :: ContainerResizeHandler s e,
  -- | Renders the widget's content. This runs before childrens' render.
  containerRender :: ContainerRenderHandler s e,
  -- | Renders the widget's content. This runs after childrens' render.
  containerRenderAfter :: ContainerRenderHandler s e
}

instance Default (Container s e a) where
  def = Container {
    containerAddStyleReq = True,
    containerChildrenOffset = Nothing,
    containerChildrenScissor = Nothing,
    containerDrawDecorations = True,
    containerLayoutDirection = LayoutNone,
    containerIgnoreEmptyArea = False,
    containerUseCustomCursor = False,
    containerUseCustomSize = False,
    containerUseChildrenSizes = False,
    containerUseScissor = False,
    containerGetBaseStyle = defaultGetBaseStyle,
    containerGetCurrentStyle = defaultGetCurrentStyle,
    containerCreateContainerFromModel = defaultCreateContainerFromModel,
    containerUpdateCWenv = defaultUpdateCWenv,
    containerInit = defaultInit,
    containerInitPost = defaultInitPost,
    containerMergeChildrenReq = defaultMergeRequired,
    containerMerge = defaultMerge,
    containerMergePost = defaultMergePost,
    containerDispose = defaultDispose,
    containerFindNextFocus = defaultFindNextFocus,
    containerFindByPoint = defaultFindByPoint,
    containerFilterEvent = defaultFilterEvent,
    containerHandleEvent = defaultHandleEvent,
    containerHandleMessage = defaultHandleMessage,
    containerGetSizeReq = defaultGetSizeReq,
    containerResize = defaultResize,
    containerRender = defaultRender,
    containerRenderAfter = defaultRender
  }

{-|
Creates a widget based on the Container infrastructure. An initial state and the
Container definition need to be provided. In case internal state is not needed,
__()__ can be provided. Using the __def__ instance as a starting point is
recommended to focus on overriding only what is needed:

@
widget = createContainer () def {
  containerRender = ...
}
@
-}
createContainer
  :: WidgetModel a
  => a
  -> Container s e a
  -> Widget s e
createContainer !state !container = Widget {
  widgetInit = initWrapper container,
  widgetMerge = mergeWrapper container,
  widgetDispose = disposeWrapper container,
  widgetGetState = makeState state,
  widgetGetInstanceTree = getInstanceTreeWrapper container,
  widgetFindNextFocus = findNextFocusWrapper container,
  widgetFindByPoint = findByPointWrapper container,
  widgetFindBranchByPath = containerFindBranchByPath,
  widgetHandleEvent = handleEventWrapper container,
  widgetHandleMessage = handleMessageWrapper container,
  widgetGetSizeReq = getSizeReqWrapper container,
  widgetResize = resizeWrapper container,
  widgetRender = renderWrapper container
}

-- | Get base style for component
defaultGetBaseStyle :: ContainerGetBaseStyle s e
defaultGetBaseStyle wenv node = Nothing

defaultGetCurrentStyle :: ContainerGetCurrentStyle s e
defaultGetCurrentStyle wenv node = currentStyle wenv node

defaultCreateContainerFromModel :: ContainerCreateContainerFromModel s e a
defaultCreateContainerFromModel wenv node state = Nothing

defaultUpdateCWenv :: ContainerUpdateCWenvHandler s e
defaultUpdateCWenv wenv node cnode cidx = wenv

getUpdateCWenv
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> Int
  -> WidgetEnv s e
getUpdateCWenv container !wenv !node !cnode !cidx = newWenv where
  cOffset = containerChildrenOffset container
  updateCWenv = containerUpdateCWenv container
  layoutDirection = containerLayoutDirection container

  pViewport = node ^. L.info . L.viewport
  cViewport = cnode ^. L.info . L.viewport
  newViewport = fromMaybe def (intersectRects pViewport cViewport)

  offsetWenv !wenv
    | isJust cOffset = updateWenvOffset container wenv node newViewport
    | otherwise = wenv
  !directionWenv = wenv
    & L.layoutDirection .~ layoutDirection

  !newWenv = updateCWenv (offsetWenv directionWenv) node cnode cidx

{-|
Helper function that updates widget environment based on current container
information. In case the created container needs to pass information down using
wenv, it should call this function first and update the resulting wenv.
-}
updateWenvOffset
  :: Container s e a  -- ^ The container config
  -> WidgetEnv s e    -- ^ The widget environment.
  -> WidgetNode s e   -- ^ The widget node.
  -> Rect             -- ^ The target viewport.
  -> WidgetEnv s e    -- ^ THe updated widget environment.
updateWenvOffset container wenv node viewport = newWenv where
  cOffset = containerChildrenOffset container
  offset = fromMaybe def cOffset

  updateMain (path, point)
    | isNodeParentOfPath node path = (path, addPoint (negPoint offset) point)
    | otherwise = (path, point)

  newWenv = wenv
    & L.viewport .~ moveRect (negPoint offset) viewport
    & L.inputStatus . L.mousePos %~ addPoint (negPoint offset)
    & L.inputStatus . L.mousePosPrev %~ addPoint (negPoint offset)
    & L.offset %~ addPoint offset
    & L.mainBtnPress %~ fmap updateMain

-- | Init handler
defaultInit :: ContainerInitHandler s e
defaultInit wenv node = resultNode node

defaultInitPost :: ContainerInitPostHandler s e a
defaultInitPost wenv node state result = result

initWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
initWrapper container wenv node = result where
  initHandler = containerInit container
  initPostHandler = containerInitPost container
  getBaseStyle = containerGetBaseStyle container
  updateCWenv = getUpdateCWenv container

  styledNode = initNodeStyle getBaseStyle wenv node
  WidgetResult tempNode reqs = initHandler wenv styledNode
  children = tempNode ^. L.children
  initChild idx child = widgetInit newWidget cwenv newChild where
    newChild = cascadeCtx wenv tempNode child idx
    cwenv = updateCWenv wenv node newChild idx
    newWidget = newChild ^. L.widget
  results = Seq.mapWithIndex initChild children
  newReqs = foldMap _wrRequests results
  newChildren = fmap _wrNode results
  newNode = updateSizeReq wenv $ tempNode
    & L.children .~ newChildren

  tmpResult = WidgetResult newNode (reqs <> newReqs)
  newState = widgetGetState (newNode ^. L.widget) wenv newNode
  result = case useState newState of
    Just st -> initPostHandler wenv newNode st tmpResult
    Nothing -> tmpResult

-- | Merging
defaultMergeRequired :: ContainerMergeChildrenReqHandler s e a
defaultMergeRequired wenv newNode oldNode oldState = True

defaultMerge :: ContainerMergeHandler s e a
defaultMerge wenv newNode oldNode oldState = resultNode newNode

defaultMergePost :: ContainerMergePostHandler s e a
defaultMergePost wenv newNode oldNode oldState newState result = result

mergeWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeWrapper container wenv newNode oldNode = newResult where
  getBaseStyle = containerGetBaseStyle container
  createContainerFromModel = containerCreateContainerFromModel container

  mergeRequiredHandler = containerMergeChildrenReq container
  mergeHandler = containerMerge container
  mergePostHandler = containerMergePost container

  oldState = widgetGetState (oldNode ^. L.widget) wenv oldNode
  mergeRequired = case useState oldState of
    Just ostate -> mergeRequiredHandler wenv newNode oldNode ostate
    Nothing -> True

  styledNode = initNodeStyle getBaseStyle wenv newNode

  -- Check if an updated container can be used for offset/layout direction.
  pNode = pResult ^. L.node
  updateCWenv = case useState oldState >>= createContainerFromModel wenv pNode of
    Just newContainer -> getUpdateCWenv newContainer
    _ -> getUpdateCWenv container
  cWenvHelper idx child = cwenv where
    cwenv = updateCWenv wenv pNode child idx

  pResult = mergeParent mergeHandler wenv styledNode oldNode oldState
  cResult = mergeChildren cWenvHelper wenv newNode oldNode pResult
  vResult = mergeChildrenCheckVisible oldNode cResult

  flagsChanged = nodeFlagsChanged oldNode newNode
  themeChanged = wenv ^. L.themeChanged
  mResult
    | mergeRequired || flagsChanged || themeChanged = vResult
    | otherwise = pResult & L.node . L.children .~ oldNode ^. L.children

  mNode = mResult ^. L.node
  mState = widgetGetState (mNode ^. L.widget) wenv mNode
  postRes = case (,) <$> useState oldState <*> useState mState of
    Just (ost, st) -> mergePostHandler wenv mNode oldNode ost st mResult
    Nothing -> resultNode (mResult ^. L.node)

  tmpResult
    | isResizeAnyResult (Just postRes) = postRes
        & L.node .~ updateSizeReq wenv (postRes ^. L.node)
    | otherwise = postRes
  newResult = handleWidgetIdChange oldNode tmpResult

mergeParent
  :: WidgetModel a
  => ContainerMergeHandler s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> Maybe WidgetState
  -> WidgetResult s e
mergeParent mergeHandler wenv newNode oldNode oldState = result where
  oldInfo = oldNode ^. L.info
  tempNode = newNode
    & L.info . L.widgetId .~ oldInfo ^. L.widgetId
    & L.info . L.viewport .~ oldInfo ^. L.viewport
    & L.info . L.sizeReqW .~ oldInfo ^. L.sizeReqW
    & L.info . L.sizeReqH .~ oldInfo ^. L.sizeReqH
  result = case useState oldState of
    Just ostate -> mergeHandler wenv tempNode oldNode ostate
    Nothing -> resultNode tempNode

mergeChildren
  :: (Int -> WidgetNode s e -> WidgetEnv s e)
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
  -> WidgetResult s e
mergeChildren updateCWenv !wenv !newNode !oldNode !pResult = newResult where
  WidgetResult pNode pReqs = pResult
  oldChildren = oldNode ^. L.children
  oldIts = Seq.mapWithIndex (,) oldChildren
  updatedChildren = pNode ^. L.children

  mergeChild idx child = (idx, cascadeCtx wenv pNode child idx)
  newIts = Seq.mapWithIndex mergeChild updatedChildren
  oldKeys = buildLocalMap oldChildren
  newKeys = buildLocalMap (snd <$> newIts)

  mpairs = mergeChildSeq updateCWenv wenv oldKeys newKeys newNode oldIts newIts
  (mergedResults, removedResults) = mpairs
  mergedChildren = fmap _wrNode mergedResults
  mergedReqs = foldMap _wrRequests mergedResults
  removedReqs = foldMap _wrRequests removedResults
  mergedNode = pNode & L.children .~ mergedChildren
  newReqs = pReqs <> mergedReqs <> removedReqs
  !newResult = WidgetResult mergedNode newReqs

mergeChildSeq
  :: (Int -> WidgetNode s e -> WidgetEnv s e)
  -> WidgetEnv s e
  -> WidgetKeyMap s e
  -> WidgetKeyMap s e
  -> WidgetNode s e
  -> Seq (Int, WidgetNode s e)
  -> Seq (Int, WidgetNode s e)
  -> (Seq (WidgetResult s e), Seq (WidgetResult s e))
mergeChildSeq updateCWenv wenv oldKeys newKeys newNode oldIts Empty = res where
  isMember = flip M.member newKeys
  dispose (!idx, !child) = case isMember <$> child ^. L.info . L.key of
    Just True -> WidgetResult child Empty
    _ -> widgetDispose (child ^. L.widget) wenv child
  !removed = fmap dispose oldIts
  !res = (Empty, removed)
mergeChildSeq updateCWenv wenv oldKeys newKeys newNode Empty newIts = res where
  init (idx, !child) = widgetInit (child ^. L.widget) wenv child
  !merged = fmap init newIts
  !res = (merged, Empty)
mergeChildSeq updateCWenv wenv oldKeys newKeys newNode oldIts newIts = res where
  (_, !oldChild) :<| oldChildren = oldIts
  (!newIdx, !newChild) :<| newChildren = newIts
  !newWidget = newChild ^. L.widget
  !newWidgetId = newChild ^. L.info . L.widgetId
  !newChildKey = newChild ^. L.info . L.key
  !cwenv = updateCWenv newIdx newChild

  oldKeyMatch = newChildKey >>= \key -> M.lookup key oldKeys
  oldMatch = fromMaybe newNode oldKeyMatch
  isMergeKey = isJust oldKeyMatch && nodeMatches newChild oldMatch

  mergedOld = widgetMerge newWidget cwenv newChild oldChild
  mergedKey = widgetMerge newWidget cwenv newChild oldMatch
  initNew = widgetInit newWidget cwenv newChild
    & L.requests %~ (|> ResizeWidgets newWidgetId)

  (!child, !oldRest)
    | nodeMatches newChild oldChild = (mergedOld, oldChildren)
    | isMergeKey = (mergedKey, oldIts)
    | otherwise = (initNew, oldIts)

  (!cmerged, !cremoved)
    = mergeChildSeq updateCWenv wenv oldKeys newKeys newNode oldRest newChildren
  !merged = child <| cmerged
  !res = (merged, cremoved)

mergeChildrenCheckVisible
  :: WidgetNode s e
  -> WidgetResult s e
  -> WidgetResult s e
mergeChildrenCheckVisible oldNode result = newResult where
  newNode = result ^. L.node
  widgetId = newNode ^. L.info . L.widgetId
  resizeRequired = childrenVisibleChanged oldNode newNode
  !newResult
    | resizeRequired = result & L.requests %~ (|> ResizeWidgets widgetId)
    | otherwise = result

getInstanceTreeWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetInstanceNode
getInstanceTreeWrapper container wenv node = instNode where
  updateCWenv = getUpdateCWenv container
  instNode = WidgetInstanceNode {
    _winInfo = node ^. L.info,
    _winState = widgetGetState (node ^. L.widget) wenv node,
    _winChildren = Seq.mapWithIndex getChildTree (node ^. L.children)
  }
  getChildTree idx child = tree where
    cwenv = updateCWenv wenv node child idx
    tree = widgetGetInstanceTree (child ^. L.widget) cwenv child

-- | Dispose handler
defaultDispose :: ContainerInitHandler s e
defaultDispose wenv node = resultNode node

disposeWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
disposeWrapper container wenv node = result where
  updateCWenv = getUpdateCWenv container
  disposeHandler = containerDispose container

  WidgetResult tempNode reqs = disposeHandler wenv node
  widgetId = node ^. L.info . L.widgetId
  children = tempNode ^. L.children

  dispose !idx !child = widgetDispose (child ^. L.widget) cwenv child where
    cwenv = updateCWenv wenv node child idx
  results = Seq.mapWithIndex dispose children
  newReqs = foldMap _wrRequests results |> ResetWidgetPath widgetId
  !result = WidgetResult node (reqs <> newReqs)

-- | Find next focusable item
defaultFindNextFocus :: ContainerFindNextFocusHandler s e
defaultFindNextFocus !wenv !node !direction !start = vchildren where
  vchildren = Seq.filter (^. L.info . L.visible) (node ^. L.children)

findNextFocusWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> FocusDirection
  -> Path
  -> Maybe WidgetNodeInfo
findNextFocusWrapper container !wenv !node !dir !start = nextFocus where
  handler = containerFindNextFocus container
  handlerResult = handler wenv node dir start
  children
    | dir == FocusBwd = Seq.reverse handlerResult
    | otherwise = handlerResult
  !nextFocus
    | isFocusCandidate node start dir = Just (node ^. L.info)
    | otherwise = findFocusCandidate container wenv dir start node children

findFocusCandidate
  :: Container s e a
  -> WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetNode s e
  -> Seq (WidgetNode s e)
  -> Maybe WidgetNodeInfo
findFocusCandidate _ _ _ _ _ Empty = Nothing
findFocusCandidate container !wenv !dir !start !node (ch :<| chs) = result where
  updateCWenv = getUpdateCWenv container
  !path = node ^. L.info . L.path
  !idx = fromMaybe 0 (Seq.lookup (length path - 1) path)
  !cwenv = updateCWenv wenv node ch idx
  !isWidgetAfterStart
    | dir == FocusBwd = isNodeBeforePath ch start
    | otherwise = isNodeParentOfPath ch start || isNodeAfterPath ch start

  candidate = widgetFindNextFocus (ch ^. L.widget) cwenv ch dir start
  result
    | isWidgetAfterStart && isJust candidate = candidate
    | otherwise = findFocusCandidate container wenv dir start node chs

-- | Find instance matching point
defaultFindByPoint :: ContainerFindByPointHandler s e
defaultFindByPoint !wenv !node !start !point = result where
  children = node ^. L.children
  pointInWidget wi = wi ^. L.visible && pointInRect point (wi ^. L.viewport)
  result = Seq.findIndexL (pointInWidget . _wnInfo) children

findByPointWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> Point
  -> Maybe WidgetNodeInfo
findByPointWrapper !container !wenv !node !start !point = result where
  offset = fromMaybe def (containerChildrenOffset container)
  updateCWenv = getUpdateCWenv container
  ignoreEmpty = containerIgnoreEmptyArea container
  handler = containerFindByPoint container

  isVisible = node ^. L.info . L.visible
  inVp = isPointInNodeVp node point
  cpoint = addPoint (negPoint offset) point
  path = node ^. L.info . L.path
  children = node ^. L.children
  childIdx = nextTargetStep node start <|> handler wenv node start cpoint
  validateIdx p
    | Seq.length children > p && p >= 0 = Just p
    | otherwise = Nothing

  win = case childIdx >>= validateIdx of
    Just idx -> childWni where
      cwenv = updateCWenv wenv node child idx
      child = Seq.index children idx
      childWidget = child ^. L.widget
      childWni = widgetFindByPoint childWidget cwenv child start cpoint
    Nothing
      | not ignoreEmpty -> Just $ node ^. L.info
      | otherwise -> Nothing
  result
    | isVisible && (inVp || fmap (^. L.path) win /= Just path) = win
    | otherwise = Nothing

containerFindBranchByPath
  :: WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> Seq WidgetNodeInfo
containerFindBranchByPath !wenv !node !path
  | info ^. L.path == path = Seq.singleton info
  | isJust nextChild = info <| nextInst (fromJust nextChild)
  | otherwise = Seq.empty
  where
    children = node ^. L.children
    info = node ^. L.info
    nextStep = nextTargetStep node path
    nextChild = nextStep >>= flip Seq.lookup children
    nextInst child = widgetFindBranchByPath (child ^. L.widget) wenv child path

-- | Event Handling
defaultFilterEvent :: ContainerFilterHandler s e
defaultFilterEvent wenv node target evt = Just (target, evt)

defaultHandleEvent :: ContainerEventHandler s e
defaultHandleEvent wenv node target evt = Nothing

handleEventWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> SystemEvent
  -> Maybe (WidgetResult s e)
handleEventWrapper container !wenv !node !baseTarget !baseEvt
  | not (node ^. L.info . L.visible) || isNothing filteredEvt = Nothing
  | targetReached || not targetValid = pResultStyled
  | otherwise = nextTargetStep pNode target >>= cResultStyled
  where
    -- Having targetValid = False means the next path step is not in
    -- _wiChildren, but may still be valid in the receiving widget
    -- For example, Composite has its own tree of child widgets with (possibly)
    -- different types for Model and Events, and is candidate for the next step
    !offset = fromMaybe def (containerChildrenOffset container)
    !style = containerGetCurrentStyle container wenv node
    !doCursor = not (containerUseCustomCursor container)
    updateCWenv = getUpdateCWenv container
    filterHandler = containerFilterEvent container
    eventHandler = containerHandleEvent container

    !targetReached = isTargetReached node target
    !targetValid = isTargetValid node target
    !filteredEvt = filterHandler wenv node baseTarget baseEvt
    (!target, !evt) = fromMaybe (baseTarget, baseEvt) filteredEvt
    -- Event targeted at parent
    !pResult = eventHandler wenv node target evt
    pResultStyled = handleStyleChange wenv target style doCursor node evt
      $ handleSizeReqChange container wenv node (Just evt) pResult
    pNode = maybe node (^. L.node) pResult

    -- Event targeted at children
    cResultStyled childIdx = result where
      --childIdx = fromJust $ nextTargetStep pNode target
      children = pNode ^. L.children
      child = Seq.index children childIdx
      childWidget = child ^. L.widget
      cevt = translateEvent (negPoint offset) evt
      cwenv = updateCWenv wenv pNode child childIdx

      childrenIgnored = isJust pResult && ignoreChildren (fromJust pResult)
      parentIgnored = isJust cResult && ignoreParent (fromJust cResult)

      cResult
        | childrenIgnored || not (child ^. L.info . L.enabled) = Nothing
        | otherwise = widgetHandleEvent childWidget cwenv child target cevt
      cResultMerged
        | parentIgnored = mergeParentChildEvts node Nothing cResult childIdx
        | otherwise = mergeParentChildEvts pNode pResult cResult childIdx

      cpNode
        | parentIgnored = node
        | otherwise = pNode
      !result = handleStyleChange cwenv target style doCursor cpNode cevt
        $ handleSizeReqChange container cwenv cpNode (Just cevt) cResultMerged

mergeParentChildEvts
  :: WidgetNode s e
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
  -> Int
  -> Maybe (WidgetResult s e)
mergeParentChildEvts _ Nothing Nothing _ = Nothing
mergeParentChildEvts _ pResponse Nothing _ = pResponse
mergeParentChildEvts original Nothing (Just cResponse) idx = Just $ cResponse {
    _wrNode = replaceChild original (_wrNode cResponse) idx
  }
mergeParentChildEvts original (Just pResponse) (Just cResponse) idx
  | ignoreChildren pResponse = Just pResponse
  | ignoreParent cResponse = Just newChildResponse
  | otherwise = Just $ WidgetResult newWidget requests
  where
    pWidget = _wrNode pResponse
    cWidget = _wrNode cResponse
    requests = _wrRequests pResponse <> _wrRequests cResponse
    newWidget = replaceChild pWidget cWidget idx
    newChildResponse = cResponse {
      _wrNode = replaceChild original (_wrNode cResponse) idx
    }

-- | Message Handling
defaultHandleMessage :: ContainerMessageHandler s e
defaultHandleMessage wenv node target message = Nothing

handleMessageWrapper
  :: (WidgetModel a, Typeable i)
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> i
  -> Maybe (WidgetResult s e)
handleMessageWrapper container !wenv !node !target arg
  | not targetReached && not targetValid = Nothing
  | otherwise = handleSizeReqChange container wenv node Nothing result
  where
    updateCWenv = getUpdateCWenv container
    handler = containerHandleMessage container

    targetReached = isTargetReached node target
    targetValid = isTargetValid node target

    messageResult childIdx = updateChild <$> message where
      children = node ^. L.children
      child = Seq.index children childIdx
      cwenv = updateCWenv wenv node child childIdx

      message = widgetHandleMessage (child ^. L.widget) cwenv child target arg
      updateChild !cr = cr {
        _wrNode = replaceChild node (_wrNode cr) childIdx
      }

    result
      | targetReached = handler wenv node target arg
      | otherwise = nextTargetStep node target >>= messageResult

-- | Preferred size
defaultGetSizeReq :: ContainerGetSizeReqHandler s e
defaultGetSizeReq wenv node children = (newReqW, newReqH) where
  (newReqW, newReqH) = case Seq.lookup 0 children of
    Just child -> (child ^. L.info . L.sizeReqW, child ^. L.info . L.sizeReqH)
    _ -> def

getSizeReqWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> (SizeReq, SizeReq)
getSizeReqWrapper container wenv node = (newReqW, newReqH) where
  addStyleReq = containerAddStyleReq container
  handler = containerGetSizeReq container
  style = containerGetCurrentStyle container wenv node

  children = node ^. L.children
  reqs = handler wenv node children
  (tmpReqW, tmpReqH)
    | addStyleReq = sizeReqAddStyle style reqs
    | otherwise = reqs
  -- User settings take precedence
  newReqW = fromMaybe tmpReqW (style ^. L.sizeReqW)
  newReqH = fromMaybe tmpReqH (style ^. L.sizeReqH)

updateSizeReq
  :: WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
updateSizeReq wenv node = newNode where
  (newReqW, newReqH) = widgetGetSizeReq (node ^. L.widget) wenv node
  newNode = node
    & L.info . L.sizeReqW .~ newReqW
    & L.info . L.sizeReqH .~ newReqH

handleSizeReqChange
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Maybe SystemEvent
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
handleSizeReqChange container wenv node evt mResult = result where
  baseResult = fromMaybe (resultNode node) mResult
  baseNode = baseResult ^. L.node
  resizeReq = isResizeAnyResult mResult
  styleChanged = isJust evt && styleStateChanged wenv baseNode (fromJust evt)
  result
    | styleChanged || resizeReq = Just $ baseResult
      & L.node .~ updateSizeReq wenv baseNode
    | otherwise = mResult

-- | Resize
defaultResize :: ContainerResizeHandler s e
defaultResize wenv node viewport children = resized where
  style = currentStyle wenv node
  contentArea = fromMaybe def (removeOuterBounds style viewport)
  childrenSizes = Seq.replicate (Seq.length children) contentArea
  resized = (resultNode node, childrenSizes)

resizeWrapper
  :: WidgetModel a
  => Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Rect
  -> (Path -> Bool)
  -> WidgetResult s e
resizeWrapper container wenv node viewport resizeReq = result where
  updateCWenv = getUpdateCWenv container
  useCustomSize = containerUseCustomSize container
  useChildSize = containerUseChildrenSizes container
  handler = containerResize container

  lensVp = L.info . L.viewport
  vpChanged = viewport /= node ^. lensVp
  path = node ^. L.info . L.path
  children = node ^. L.children

  (tempRes, assigned) = handler wenv node viewport children
  resize idx (!child, !vp) = newChildRes where
    !cwenv = updateCWenv wenv node child idx
    tempChildRes = widgetResize (child ^. L.widget) cwenv child vp resizeReq
    cvp = tempChildRes ^. L.node . L.info . L.viewport
    icvp = fromMaybe vp (intersectRects vp cvp)
    !newChildRes = tempChildRes
      & L.node . L.info . L.viewport .~ (if useChildSize then icvp else vp)

  newChildrenRes = Seq.mapWithIndex resize (Seq.zip children assigned)
  newChildren = fmap _wrNode newChildrenRes
  newChildrenReqs = foldMap _wrRequests newChildrenRes
  newVp
    | useCustomSize = tempRes ^. L.node . lensVp
    | otherwise = viewport
  tmpResult
    | vpChanged || resizeReq path = Just $ tempRes
      & L.node . L.info . L.viewport .~ newVp
      & L.node . L.children .~ newChildren
      & L.requests <>~ newChildrenReqs
    | otherwise = Just $ resultNode node
  result = fromJust $
    handleSizeReqChange container wenv (tempRes ^. L.node) Nothing tmpResult

-- | Rendering
defaultRender :: ContainerRenderHandler s e
defaultRender renderer wenv node = return ()

renderWrapper
  :: Container s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Renderer
  -> IO ()
renderWrapper container wenv !node !renderer =
  drawInScissor renderer useScissor viewport $
    drawStyledAction_ renderer drawDecorations viewport style $ \_ -> do
      renderBefore wenv node renderer

      drawInScissor renderer useChildrenScissor childrenScissorRect $ do
        when (isJust offset) $ do
          saveContext renderer
          setTranslation renderer (fromJust offset)

        forM_ pairs $ \(idx, child) ->
          when (isWidgetVisible (cwenv child idx) child) $
            widgetRender (child ^. L.widget) (cwenv child idx) child renderer

        when (isJust offset) $
          restoreContext renderer

      -- Outside children scissor
      renderAfter wenv node renderer
  where
    style = containerGetCurrentStyle container wenv node
    updateCWenv = getUpdateCWenv container
    drawDecorations = containerDrawDecorations container
    useScissor = containerUseScissor container
    childrenScissor = containerChildrenScissor container
    offset = containerChildrenOffset container
    renderBefore = containerRender container
    renderAfter = containerRenderAfter container

    children = node ^. L.children
    viewport = node ^. L.info . L.viewport
    useChildrenScissor = isJust childrenScissor
    childrenScissorRect = fromMaybe def childrenScissor
    pairs = Seq.mapWithIndex (,) children
    cwenv !child !idx = updateCWenv wenv node child idx

-- | Event Handling Helpers
ignoreChildren :: WidgetResult s e -> Bool
ignoreChildren result = not (Seq.null ignoreReqs) where
  ignoreReqs = Seq.filter isIgnoreChildrenEvents (_wrRequests result)

ignoreParent :: WidgetResult s e -> Bool
ignoreParent result = not (Seq.null ignoreReqs) where
  ignoreReqs = Seq.filter isIgnoreParentEvents (_wrRequests result)

replaceChild
  :: WidgetNode s e -> WidgetNode s e -> Int -> WidgetNode s e
replaceChild !parent !child !idx = parent & L.children .~ newChildren where
  newChildren = Seq.update idx child (parent ^. L.children)

cascadeCtx
  :: WidgetEnv s e -> WidgetNode s e -> WidgetNode s e -> Int -> WidgetNode s e
cascadeCtx !wenv !parent !child !idx = newChild where
  pInfo = parent ^. L.info
  cInfo = child ^. L.info
  parentPath = pInfo ^. L.path
  parentVisible = pInfo ^. L.visible
  parentEnabled = pInfo ^. L.enabled
  !newPath = parentPath |> idx
  !newChild = child
    & L.info . L.widgetId .~ WidgetId (wenv ^. L.timestamp) newPath
    & L.info . L.path .~ newPath
    & L.info . L.visible .~ (cInfo ^. L.visible && parentVisible)
    & L.info . L.enabled .~ (cInfo ^. L.enabled && parentEnabled)

buildLocalMap :: Seq (WidgetNode s e) -> Map WidgetKey (WidgetNode s e)
buildLocalMap widgets = newMap where
  addWidget map widget
    | isJust key = M.insert (fromJust key) widget map
    | otherwise = map
    where
      key = widget ^. L.info . L.key
  newMap = foldl' addWidget M.empty widgets
