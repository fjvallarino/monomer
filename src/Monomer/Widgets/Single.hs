{-|
Module      : Monomer.Widgets.Single
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper for creating widgets without children elements
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Single (
  module Monomer.Core,
  module Monomer.Core.Combinators,
  module Monomer.Event,
  module Monomer.Graphics,
  module Monomer.Widgets.Util,

  SingleGetBaseStyle,
  SingleGetActiveStyle,
  SingleInitHandler,
  SingleMergeHandler,
  SingleDisposeHandler,
  SingleFindNextFocusHandler,
  SingleFindByPointHandler,
  SingleEventHandler,
  SingleMessageHandler,
  SingleGetSizeReqHandler,
  SingleResizeHandler,
  SingleRenderHandler,

  Single(..),
  createSingle
) where

import Control.Exception (AssertionFailed(..), throw)
import Control.Lens ((&), (^.), (^?), (.~), (%~), _Just)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..), (|>))
import Data.Typeable (Typeable, cast)

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
type SingleGetBaseStyle s e
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
the full content rect. An example can be found in "Monomer.Widgets.Singles.Radio".
-}
type SingleGetActiveStyle s e
  = WidgetEnv s e      -- ^ The widget environment.
  -> WidgetNode s e    -- ^ The widget node.
  -> StyleState        -- ^ The active style for the node.

{-|
Initializes the given node. This could include rebuilding the widget in case
internal state needs to use model/environment information, generate user
events or make requests to the runtime.

An examples can be found in "Monomer.Widgets.Singles.Label" and
"Monomer.Widgets.Singles.Image". On the other hand, "Monomer.Widgets.Radio" does
not need to override /init/.
-}
type SingleInitHandler s e
  = WidgetEnv s e        -- ^ The widget environment.
  -> WidgetNode s e      -- ^ The widget node.
  -> WidgetResult s e    -- ^ The result of the init operation.

{-|
Merges the current node with the node it matched with during the merge process.
Receives the newly created node (whose *init* function is not called), the
previous node and the state extracted from that node. This process is widget
dependent, and may use or ignore the previous state depending on newly available
information.

In general, you want to at least keep the previous state unless the widget is
stateless or only consumes model/environment information.

Examples can be found in "Monomer.Widgets.Singles.Label" and
"Monomer.Widgets.Singles.Image". On the other hand,
"Monomer.Widgets.Singles.Radio" does not need to override merge since it's
stateless.
-}
type SingleMergeHandler s e a
  = WidgetEnv s e        -- ^ The widget environment.
  -> WidgetNode s e      -- ^ The widget node.
  -> WidgetNode s e      -- ^ The previous widget node.
  -> a                   -- ^ The state of the previous widget node.
  -> WidgetResult s e    -- ^ The result of the merge operation.

{-|
Disposes the current node. Only used by widgets which allocate resources during
/init/ or /merge/, and will usually involve requests to the runtime.

An example can be found "Monomer.Widgets.Singles.Image".
-}
type SingleDisposeHandler s e
  = WidgetEnv s e        -- ^ The widget environment.
  -> WidgetNode s e      -- ^ The widget node.
  -> WidgetResult s e    -- ^ The result of the dispose operation.

{-|
Returns the next focusable node. Since this type of widget does not have
children, there is not need to override this function, as there are only
two options:

- The node is focusable and target is valid: the node is returned
- The node is not focusable: Nothing is returned
-}
type SingleFindNextFocusHandler s e
  = WidgetEnv s e            -- ^ The widget environment.
  -> WidgetNode s e          -- ^ The widget node.
  -> FocusDirection          -- ^ The direction in which focus is moving.
  -> Path                    -- ^ The start path from which to search.
  -> Maybe WidgetNodeInfo    -- ^ The next focusable node info.

{-|
Returns the currently hovered widget, if any. If the widget is rectangular and
uses the full content area, there is not need to override this function.

An example can be found "Monomer.Widgets.Singles.Radio".
-}
type SingleFindByPointHandler s e
  = WidgetEnv s e           -- ^ The widget environment.
  -> WidgetNode s e         -- ^ The widget node.
  -> Path                   -- ^ The start path from which to search.
  -> Point                  -- ^ The point to test for.
  -> Maybe WidgetNodeInfo   -- ^ The hovered node info, if any.

{-|
Receives a System event and, optionally, returns a result. This can include an
updated version of the widget (in case it has internal state), user events or
requests to the runtime.

Examples can be found in "Monomer.Widgets.Singles.Button" and
"Monomer.Widgets.Singles.Slider".
-}
type SingleEventHandler s e
  = WidgetEnv s e                -- ^ The widget environment.
  -> WidgetNode s e              -- ^ The widget node.
  -> Path                        -- ^ The target path of the event.
  -> SystemEvent                 -- ^ The SystemEvent to handle.
  -> Maybe (WidgetResult s e)    -- ^ The result of handling the event, if any.

{-|
Receives a message and, optionally, returns a result. This can include an
updated version of the widget (in case it has internal state), user events or
requests to the runtime. There is no validation regarding the message type, and
the widget should take care of _casting_ to the correct type using
"Data.Typeable.cast"

Examples can be found in "Monomer.Widgets.Singles.Button" and
"Monomer.Widgets.Singles.Slider".
-}
type SingleMessageHandler s e
  = forall i . Typeable i
  => WidgetEnv s e              -- ^ The widget environment.
  -> WidgetNode s e             -- ^ The widget node.
  -> Path                       -- ^ The target path of the message.
  -> i                          -- ^ The message to handle.
  -> Maybe (WidgetResult s e)   -- ^ The result of handling the message, if any.

{-|
Returns the preferred size for the widget. This size should not include border
and padding; those are added automatically by Single.

This is called to update WidgetNodeInfo only at specific times.

Examples can be found in "Monomer.Widgets.Singles.Checkbox" and
"Monomer.Widgets.Singles.Label".
-}
type SingleGetSizeReqHandler s e
  = WidgetEnv s e          -- ^ The widget environment.
  -> WidgetNode s e        -- ^ The widget node.
  -> (SizeReq, SizeReq)    -- ^ The horizontal and vertical requirements.

{-|
Resizes the widget to the provided size. If the widget state does not depend
on the viewport size, this function does not need to be overriden.

Examples can be found in "Monomer.Widgets.Singles.Label".
-}
type SingleResizeHandler s e
  = WidgetEnv s e        -- ^ The widget environment.
  -> WidgetNode s e      -- ^ The widget node.
  -> Rect                -- ^ The new viewport.
  -> WidgetResult s e    -- ^ The result of resizing the widget.

{-|
Renders the widget's content using the given Renderer. In general, this method
needs to be overriden.

Examples can be found in "Monomer.Widgets.Singles.Checkbox" and
"Monomer.Widgets.Singles.Slider".
-}
type SingleRenderHandler s e
  = WidgetEnv s e      -- ^ The widget environment.
  -> WidgetNode s e    -- ^ The widget node.
  -> Renderer          -- ^ The renderer, providing low level drawing functions.
  -> IO ()             -- ^ The IO action with rendering instructions.

data Single s e a = Single {
  -- | True if border and padding should be added to size requirement. Defaults
  -- | to True.
  singleAddStyleReq :: Bool,
  -- | True if focus should be requested when mouse button is pressed (before
  -- | click). Defaults to True.
  singleFocusOnBtnPressed :: Bool,
  -- | True if style cursor should be ignored. If it's False, cursor changes need
  -- | to be handled in custom code. Defaults to False.
  singleUseCustomCursor :: Bool,
  -- | If true, it will ignore extra space assigned by the parent container, but
  -- | it will not use more space than assigned. Defaults to False.
  singleUseCustomSize :: Bool,
  -- | True if automatic scissoring needs to be applied. Defaults to False.
  singleUseScissor :: Bool,
  -- | Returns the base style for this type of widget.
  singleGetBaseStyle :: SingleGetBaseStyle s e,
  -- | Returns the active style, depending on the status of the widget.
  singleGetActiveStyle :: SingleGetActiveStyle s e,
  -- | Initializes the given node.
  singleInit :: SingleInitHandler s e,
  -- | Merges the node with the node it matched with during the merge process.
  singleMerge :: SingleMergeHandler s e a,
  -- | Disposes the current node.
  singleDispose :: SingleDisposeHandler s e,
  -- | Returns the next focusable node.
  singleFindNextFocus :: SingleFindNextFocusHandler s e,
  -- | Returns the currently hovered widget, if any.
  singleFindByPoint :: SingleFindByPointHandler s e,
  -- | Receives a System event and, optionally, returns a result.
  singleHandleEvent :: SingleEventHandler s e,
  -- | Receives a message and, optionally, returns a result.
  singleHandleMessage :: SingleMessageHandler s e,
  -- | Returns the preferred size for the widget.
  singleGetSizeReq :: SingleGetSizeReqHandler s e,
  -- | Resizes the widget to the provided size.
  singleResize :: SingleResizeHandler s e,
  -- | Renders the widget's content.
  singleRender :: SingleRenderHandler s e
}

instance Default (Single s e a) where
  def = Single {
    singleAddStyleReq = True,
    singleFocusOnBtnPressed = True,
    singleUseCustomCursor = False,
    singleUseCustomSize = False,
    singleUseScissor = False,
    singleGetBaseStyle = defaultGetBaseStyle,
    singleGetActiveStyle = defaultGetActiveStyle,
    singleInit = defaultInit,
    singleMerge = defaultMerge,
    singleDispose = defaultDispose,
    singleFindNextFocus = defaultFindNextFocus,
    singleFindByPoint = defaultFindByPoint,
    singleHandleEvent = defaultHandleEvent,
    singleHandleMessage = defaultHandleMessage,
    singleGetSizeReq = defaultGetSizeReq,
    singleResize = defaultResize,
    singleRender = defaultRender
  }

{-|
Creates a widget based on the Single infrastructure. An initial state and the
Single definition need to be provided. In case internal state is not needed,
__()__ can be provided. Using the __def__ instance as a starting point is
recommended to focus on overriding only what is needed:

@
widget = createSingle () def {
  singleRender = ...
}
@
-}
createSingle :: WidgetModel a => a -> Single s e a -> Widget s e
createSingle state single = Widget {
  widgetInit = initWrapper single,
  widgetMerge = mergeWrapper single,
  widgetDispose = disposeWrapper single,
  widgetGetState = makeState state,
  widgetGetInstanceTree = getInstanceTreeWrapper single,
  widgetFindNextFocus = singleFindNextFocus single,
  widgetFindByPoint = singleFindByPoint single,
  widgetFindBranchByPath = singleFindBranchByPath,
  widgetHandleEvent = handleEventWrapper single,
  widgetHandleMessage = handleMessageWrapper single,
  widgetGetSizeReq = getSizeReqWrapper single,
  widgetResize = resizeHandlerWrapper single,
  widgetRender = renderWrapper single
}

defaultGetBaseStyle :: SingleGetBaseStyle s e
defaultGetBaseStyle wenv node = Nothing

defaultGetActiveStyle :: SingleGetActiveStyle s e
defaultGetActiveStyle wenv node = activeStyle wenv node

defaultInit :: SingleInitHandler s e
defaultInit wenv node = resultNode node

initWrapper
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
initWrapper single wenv node = newResult where
  initHandler = singleInit single
  getBaseStyle = singleGetBaseStyle single
  styledNode = initNodeStyle getBaseStyle wenv node
  tmpResult = initHandler wenv styledNode
  newResult = tmpResult
    & L.node .~ updateSizeReq wenv (tmpResult ^. L.node)

defaultMerge :: SingleMergeHandler s e a
defaultMerge wenv newNode oldState oldNode = resultNode newNode

mergeWrapper
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeWrapper single wenv newNode oldNode = newResult where
  mergeHandler = singleMerge single
  oldState = widgetGetState (oldNode ^. L.widget) wenv oldNode
  oldInfo = oldNode ^. L.info
  nodeHandler wenv styledNode = case useState oldState of
    Just state -> mergeHandler wenv styledNode oldNode state
    _ -> resultNode styledNode
  tmpResult = runNodeHandler single wenv newNode oldInfo nodeHandler
  newResult = handleWidgetIdChange oldNode tmpResult

runNodeHandler
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNodeInfo
  -> (WidgetEnv s e -> WidgetNode s e -> WidgetResult s e)
  -> WidgetResult s e
runNodeHandler single wenv newNode oldInfo nodeHandler = newResult where
  getBaseStyle = singleGetBaseStyle single
  tempNode = newNode
    & L.info . L.widgetId .~ oldInfo ^. L.widgetId
    & L.info . L.viewport .~ oldInfo ^. L.viewport
    & L.info . L.sizeReqW .~ oldInfo ^. L.sizeReqW
    & L.info . L.sizeReqH .~ oldInfo ^. L.sizeReqH
  styledNode = initNodeStyle getBaseStyle wenv tempNode
  tmpResult = nodeHandler wenv styledNode
  newResult
    | isResizeAnyResult (Just tmpResult) = tmpResult
        & L.node .~ updateSizeReq wenv (tmpResult ^. L.node)
    | otherwise = tmpResult

getInstanceTreeWrapper
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetInstanceNode
getInstanceTreeWrapper container wenv node = instNode where
  instNode = WidgetInstanceNode {
    _winInfo = node ^. L.info,
    _winState = widgetGetState (node ^. L.widget) wenv node,
    _winChildren = fmap (getChildTree wenv) (node ^. L.children)
  }
  getChildTree wenv child = widgetGetInstanceTree (child ^. L.widget) wenv child

defaultDispose :: SingleDisposeHandler s e
defaultDispose wenv node = resultNode node

disposeWrapper
  :: Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
disposeWrapper single wenv node = result where
  disposeHandler = singleDispose single
  WidgetResult newNode reqs = disposeHandler wenv node
  widgetId = node ^. L.info . L.widgetId
  newReqs = reqs |> ResetWidgetPath widgetId
  result = WidgetResult newNode newReqs

defaultFindNextFocus :: SingleFindNextFocusHandler s e
defaultFindNextFocus wenv node direction startFrom
  | isFocusCandidate direction startFrom node = Just (node ^. L.info)
  | otherwise = Nothing

defaultFindByPoint :: SingleFindByPointHandler s e
defaultFindByPoint wenv node start point
  | visible && validPath && isPointInNodeVp point node = Just info
  | otherwise = Nothing
  where
    info = node ^. L.info
    visible = info ^. L.visible
    path = node ^. L.info . L.path
    validPath = seqStartsWith start path

singleFindBranchByPath
  :: WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> Seq WidgetNodeInfo
singleFindBranchByPath wenv node path
  | info ^. L.path == path = Seq.singleton info
  | otherwise = Seq.empty
  where
    info = node ^. L.info

defaultHandleEvent :: SingleEventHandler s e
defaultHandleEvent wenv node target evt = Nothing

handleEventWrapper
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> SystemEvent
  -> Maybe (WidgetResult s e)
handleEventWrapper single wenv node target evt
  | not (node ^. L.info . L.visible) = Nothing
  | otherwise = handleStyleChange wenv target style handleCursor node evt result
  where
    style = singleGetActiveStyle single wenv node
    handleCursor = not (singleUseCustomCursor single)
    focusOnPressed = singleFocusOnBtnPressed single
    handler = singleHandleEvent single
    handlerRes = handler wenv node target evt
    sizeResult = handleSizeReqChange single wenv node (Just evt) handlerRes
    result
      | focusOnPressed = handleFocusRequest wenv node evt sizeResult
      | otherwise = sizeResult

handleFocusRequest
  :: WidgetEnv s e
  -> WidgetNode s e
  -> SystemEvent
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
handleFocusRequest wenv oldNode evt mResult = newResult where
  node = maybe oldNode (^. L.node) mResult
  prevReqs = maybe Empty (^. L.requests) mResult
  isFocusable = node ^. L.info . L.focusable
  btnPressed = case evt of
    ButtonAction _ btn BtnPressed _ -> Just btn
    _ -> Nothing
  isFocusReq = btnPressed == Just (wenv ^. L.mainButton)
    && isFocusable
    && not (isNodeFocused wenv node)
    && isNodeTopLevel wenv node
    && isNothing (Seq.findIndexL isFocusRequest prevReqs)
  focusReq = SetFocus (node ^. L.info . L.widgetId)
  newResult
    | isFocusReq && isJust mResult = (& L.requests %~ (|> focusReq)) <$> mResult
    | isFocusReq = Just $ resultReqs node [focusReq]
    | otherwise = mResult

defaultHandleMessage :: SingleMessageHandler s e
defaultHandleMessage wenv node target message = Nothing

handleMessageWrapper :: forall s e a i . (WidgetModel a, Typeable i)
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Path
  -> i
  -> Maybe (WidgetResult s e)
handleMessageWrapper single wenv node target msg = result where
  handler = singleHandleMessage single
  result = handleSizeReqChange single wenv node Nothing
    $ handler wenv node target msg

defaultGetSizeReq :: SingleGetSizeReqHandler s e
defaultGetSizeReq wenv node = def

getSizeReqWrapper
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> (SizeReq, SizeReq)
getSizeReqWrapper single wenv node = (newReqW, newReqH) where
  addStyleReq = singleAddStyleReq single
  handler = singleGetSizeReq single
  style = singleGetActiveStyle single wenv node
  reqs = handler wenv node
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
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Maybe SystemEvent
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
handleSizeReqChange single wenv node evt mResult = result where
  baseResult = fromMaybe (resultNode node) mResult
  newNode = baseResult ^. L.node
  resizeReq = isResizeAnyResult mResult
  styleChanged = isJust evt && styleStateChanged wenv newNode (fromJust evt)
  result
    | styleChanged || resizeReq = Just $ baseResult
      & L.node .~ updateSizeReq wenv newNode
    | otherwise = mResult

defaultResize :: SingleResizeHandler s e
defaultResize wenv node viewport = resultNode node

resizeHandlerWrapper
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Rect
  -> WidgetResult s e
resizeHandlerWrapper single wenv node viewport = result where
  useCustomSize = singleUseCustomSize single
  handler = singleResize single
  tmpRes = handler wenv node viewport
  lensVp = L.info . L.viewport
  newVp
    | useCustomSize = tmpRes ^. L.node . lensVp
    | otherwise = viewport
  tmpResult = Just $ tmpRes
    & L.node . L.info . L.viewport .~ newVp
  newNode = tmpRes ^. L.node
  result = fromJust $ handleSizeReqChange single wenv newNode Nothing tmpResult

defaultRender :: SingleRenderHandler s e
defaultRender wenv node renderer = return ()

renderWrapper
  :: Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Renderer
  -> IO ()
renderWrapper single wenv node renderer =
  drawInScissor renderer useScissor viewport $
    drawStyledAction renderer viewport style $ \_ ->
      handler wenv node renderer
  where
    handler = singleRender single
    useScissor = singleUseScissor single
    style = singleGetActiveStyle single wenv node
    viewport = node ^. L.info . L.viewport
