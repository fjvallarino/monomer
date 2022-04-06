{-|
Module      : Monomer.Core.WidgetTypes
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Basic types and definitions for Widgets.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

module Monomer.Core.WidgetTypes where

import Control.Concurrent (MVar)
import Control.Lens (ALens')
import Data.Default
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import Data.Word (Word64)
import GHC.Generics
import TextShow

import qualified Data.Text as T

import Monomer.Common
import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

{-|
Time expressed in milliseconds. Useful for representing the time of events,
length of intervals, start time of the application and ellapsed time since its
start.

It can be converted from/to other numeric types using the standard functions.
-}
newtype Millisecond = Millisecond {
  unMilliseconds :: Word64
} deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral, Read, Show, Default, TextShow)
  deriving (Generic)

-- | Type constraints for a valid model
type WidgetModel s = Typeable s
-- | Type constraints for a valid event
type WidgetEvent e = Typeable e

{-|
Map of WidgetKeys to WidgetNodes. This association is valid only in the context
of a Composite, with visibility of keys restricted to its scope. WidgetKeys
inside nested Composites are /not/ visible.
-}
type WidgetKeyMap s e = Map WidgetKey (WidgetNode s e)

-- | Direction of focus movement.
data FocusDirection
  = FocusFwd  -- ^ Focus moving forward (usually left to right, top to bottom).
  | FocusBwd  -- ^ Focus moving backward (usually right to left, top to bottom).
  deriving (Eq, Show)

-- | "WidgetRequest" specific for window related operations.
data WindowRequest
  = WindowSetTitle Text  -- ^ Sets the title of the window to the given text.
  | WindowSetFullScreen  -- ^ Switches to fullscreen mode.
  | WindowMaximize       -- ^ Maximizes the window.
  | WindowMinimize       -- ^ Minimizes the window.
  | WindowRestore        -- ^ Restores the window to its previous normal size.
  | WindowBringToFront   -- ^ Brings the window to the foreground.
  deriving (Eq, Show)

-- | Type of a widget. Used during the merge process.
newtype WidgetType
  = WidgetType Text
  deriving (Eq, Show, Generic)

instance IsString WidgetType where
  fromString = WidgetType . T.pack

{-|
Widgets can receive/report data using lenses or direct values. Reporting
WidgetValue requires user events (in general, an onChange event).
-}
data WidgetData s a
  = WidgetValue a            -- ^ A direct value.
  | WidgetLens (ALens' s a)  -- ^ A lens into the parent model.

{-|
Widgets instances have an associated path from the root, which is unique at a
specific point in time. This path may change, since widgets could be added
before or after it (for example, a widget is added to the beginning of a list).
WidgetIds are used by the runtime to create an association from a unique
identifier to the current valid path of an instance; this unique identifier, the
WidgetId, is the result of combining the timestamp when the instance was created
and its path at that time.

Several WidgetRequests rely on this to find the destination of asynchronous
requests (tasks, clipboard, etc).
-}
data WidgetId = WidgetId {
  _widTs :: Millisecond,  -- ^ The timestamp when the instance was created.
  _widPath :: Path      -- ^ The path at creation time.
} deriving (Eq, Show, Ord, Generic)

instance Default WidgetId where
  def = WidgetId 0 emptyPath

{-|
During the merge process, widgets are matched based on WidgetType and WidgetKey.
By default an instance's key is null, which means any matching type will be
valid for merging. If you have items that can be reordered, using a key makes
sure merge picks the correct instance for merging. Keys should be unique within
the context of a Composite. Duplicate key behavior is undefined.
-}
newtype WidgetKey
  = WidgetKey Text
  deriving (Eq, Show, Ord, Generic)

instance IsString WidgetKey where
  fromString = WidgetKey . T.pack

{-|
Wrapper of a Typeable instance representing the state of a widget. The widget is
in charge of casting to the correct type.
-}
data WidgetState
  = forall i . WidgetModel i => WidgetState i

instance Show WidgetState where
  show (WidgetState state) = "WidgetState: " ++ show (typeOf state)

{-|
Wrapper of a Typeable instance representing shared data between widgets. Used,
for example, by image widget to avoid loading the same image multiple times. The
widget is in charge of casting to the correct type.
-}
data WidgetShared
  = forall i . Typeable i => WidgetShared i

instance Show WidgetShared where
  show (WidgetShared shared) = "WidgetShared: " ++ show (typeOf shared)

{-|
WidgetRequests are the way a widget can perform side effects, such as changing
cursor icons, get/set the clipboard and perform asynchronous tasks. These
requests are included as part of a WidgetResult in different points in the
lifecycle of a widget.
-}
data WidgetRequest s e
  -- | Ignore events generated by the parent. Could be used to consume the tab
  --   key and avoid having the focus move to the next widget.
  = IgnoreParentEvents
  -- | Ignore children events. Scroll relies on this to handle click/wheel.
  | IgnoreChildrenEvents
  -- | The widget content changed and requires a different size. Processed at
  --   the end of the cycle, since several widgets may request it.
  | ResizeWidgets WidgetId
  -- | The widget content changed and requires a different size. Processed
  --   immediately. Avoid if possible, since it can affect performance.
  | ResizeWidgetsImmediate WidgetId
  -- | Moves the focus, optionally indicating a starting widgetId.
  | MoveFocus (Maybe WidgetId) FocusDirection
  -- | Sets the focus to the given widgetId.
  | SetFocus WidgetId
  -- | Requests the clipboard contents. It will be received as a SystemEvent.
  | GetClipboard WidgetId
  -- | Sets the clipboard to the given ClipboardData.
  | SetClipboard ClipboardData
  -- | Sets the viewport that should be remain visible when an on-screen
  --   keyboard is displayed. Required for mobile.
  | StartTextInput Rect
  -- | Resets the keyboard viewport,
  | StopTextInput
  -- | Sets a widget as the base target of future events. This is used by the
  --   dropdown component to handle list events; this list, acting as an
  --   overlay, is displayed on top of all other widgets. Tooltip uses it too.
  --   every other widget).
  | SetOverlay WidgetId Path
  -- | Removes the existing overlay.
  | ResetOverlay WidgetId
  -- | Sets the current active cursor icon. This acts as a stack, and resetting
  --   a widgetId means going back to the cursor set immediately before.
  | SetCursorIcon WidgetId CursorIcon
  -- | Removes a cursor icon from the stack. Duplicate requests are ignored.
  | ResetCursorIcon WidgetId
  -- | Sets the current item being dragged and the message it carries. This
  --   message can be used by targets to check if they accept it or not.
  | StartDrag WidgetId Path WidgetDragMsg
  -- | Cancels the current dragging process.
  | StopDrag WidgetId
  -- | Requests rendering a single frame. Rendering is not done at a fixed rate,
  --   in order to reduce CPU usage. Widgets are responsible for requesting
  --   rendering at points of interest. Mouse (except mouse move) and keyboard
  --   events automatically generate render requests, but the result of a
  --   WidgetTask or WidgetProducer does not.
  | RenderOnce
  -- | Useful if a widget requires periodic rendering. An optional maximum
  --   number of frames can be provided.
  | RenderEvery WidgetId Millisecond (Maybe Int)
  -- | Stops a previous periodic rendering request.
  | RenderStop WidgetId
  {-|
  Requests an image to be removed from the Renderer. In general, used by the
  dispose function.
  -}
  | RemoveRendererImage Text
  -- | Requests to exit the application. Can also be used to cancel a previous
  --   request (or a window close).
  | ExitApplication Bool
  -- | Performs a "WindowRequest".
  | UpdateWindow WindowRequest
  -- | Request a model update. This usually involves lenses and "widgetDataSet".
  | UpdateModel (s -> s)
  -- | Updates the path of a given widget. Both "Monomer.Widgets.Single" and
  --   "Monomer.Widgets.Container" handle this automatically.
  | SetWidgetPath WidgetId Path
  -- | Clears an association between widgetId and path.
  | ResetWidgetPath WidgetId
  -- | Raises a user event, which usually will be processed in handleEvent by a
  --   "Monomer.Widgets.Composite" instance.
  | WidgetEvent e => RaiseEvent e
  -- | Sends a message to the given widgetId. If the target does not expect the
  --   message's type, it will be ignored.
  | forall i . Typeable i => SendMessage WidgetId i
  -- | Runs an asynchronous tasks. It is mandatory to return a message that will
  --   be sent to the task owner (this is the only way to feed data back).
  | forall i . Typeable i => RunTask WidgetId Path (IO i)
  -- | Similar to RunTask, but can generate unlimited messages. This is useful
  --   for WebSockets and similar data sources. It receives a function that
  --   can be used to send messages back to the producer owner.
  | forall i . Typeable i => RunProducer WidgetId Path ((i -> IO ()) -> IO ())
  -- | Runs an asynchronous tasks in the render thread. It is mandatory to
  --   return a message that will be sent to the task owner (this is the only
  --   way to feed data back). This should only be used when implementing low
  --   level rendering widgets that need to create API specific resources.
  | forall i . Typeable i => RunInRenderThread WidgetId Path (IO i)

instance Eq e => Eq (WidgetRequest s e) where
  IgnoreParentEvents == IgnoreParentEvents = True
  IgnoreChildrenEvents == IgnoreChildrenEvents = True
  ResizeWidgets w1 == ResizeWidgets w2 = w1 == w2
  ResizeWidgetsImmediate w1 == ResizeWidgetsImmediate w2 = w1 == w2
  MoveFocus w1 fd1 == MoveFocus w2 fd2 = (w1, fd1) == (w2, fd2)
  SetFocus w1 == SetFocus w2 = w1 == w2
  GetClipboard w1 == GetClipboard w2 = w1 == w2
  SetClipboard c1 == SetClipboard c2 = c1 == c2
  StartTextInput r1 == StartTextInput r2 = r1 == r2
  StopTextInput == StopTextInput = True
  SetOverlay w1 p1 == SetOverlay w2 p2 = (w1, p1) == (w2, p2)
  ResetOverlay w1 == ResetOverlay w2 = w1 == w2
  SetCursorIcon w1 c1 == SetCursorIcon w2 c2 = (w1, c1) == (w2, c2)
  ResetCursorIcon w1 == ResetCursorIcon w2 = w1 == w2
  StartDrag w1 p1 m1 == StartDrag w2 p2 m2 = (w1, p1, m1) == (w2, p2, m2)
  StopDrag w1 == StopDrag w2 = w1 == w2
  RenderOnce == RenderOnce = True
  RenderEvery p1 c1 r1 == RenderEvery p2 c2 r2 = (p1, c1, r1) == (p2, c2, r2)
  RenderStop p1 == RenderStop p2 = p1 == p2
  ExitApplication e1 == ExitApplication e2 = e1 == e2
  UpdateWindow w1 == UpdateWindow w2 = w1 == w2
  SetWidgetPath w1 p1 == SetWidgetPath w2 p2 = (w1, p1) == (w2, p2)
  ResetWidgetPath w1 == ResetWidgetPath w2 = w1 == w2
  RaiseEvent e1 == RaiseEvent e2 = e1 == e2
  _ == _ = False

{-|
Result of widget operations (init, merge, handleEvent, etc). The node is
mandatory. The "resultNode", "resultEvts", "resultReqs" and "resultReqsEvts"
helper functions can also be used.

In general a result starts in a child widget, but parent widgets can append
requets or new versions of themselves.
-}
data WidgetResult s e = WidgetResult {
  _wrNode :: WidgetNode s e,              -- ^ The updated widget node.
  _wrRequests :: Seq (WidgetRequest s e)  -- ^ The widget requests.
}

-- This instance is lawless (there is not an empty widget): use with caution
instance Semigroup (WidgetResult s e) where
  er1 <> er2 = WidgetResult {
    _wrNode = _wrNode er2,
    _wrRequests = _wrRequests er1 <> _wrRequests er2
  }

-- | Used to indicate active layout direction. Some widgets, such as spacer,
--   can use it to adapt themselves.
data LayoutDirection
  = LayoutNone
  | LayoutHorizontal
  | LayoutVertical
  deriving (Eq, Show, Generic)

-- | The widget environment. This includes system information, active viewport,
--   and input status among other things.
data WidgetEnv s e = WidgetEnv {
  -- | The OS of the host.
  _weOs :: Text,
  -- | Device pixel rate.
  _weDpr :: Double,
  -- | The timestamp in milliseconds when the application started.
  _weAppStartTs :: Millisecond,
  -- | Provides helper funtions for calculating text size.
  _weFontManager :: FontManager,
  -- | Returns the node info, and its parents', given a path from root.
  _weFindBranchByPath :: Path -> Seq WidgetNodeInfo,
  -- | The mouse button that is considered main.
  _weMainButton :: Button,
  -- | The mouse button that is considered as secondary or context button.
  _weContextButton :: Button,
  -- | The active theme. Some widgets derive their base style from this.
  _weTheme :: Theme,
  -- | The main window size.
  _weWindowSize :: Size,
  -- | The active map of shared data.
  _weWidgetShared :: MVar (Map Text WidgetShared),
  {-
  The active map of WidgetKey -> WidgetNode, if any. This map is restricted to
  to the parent 'Composite'. Do not use this map directly, rely instead on the
  'widgetIdFromKey', 'nodeInfoFromKey' and 'nodeInfoFromPath' utility functions.
  -}
  _weWidgetKeyMap :: WidgetKeyMap s e,
  -- | The currently hovered path, if any.
  _weHoveredPath :: Maybe Path,
  -- | The currently focused path. There's always one, even if it's empty.
  _weFocusedPath :: Path,
  -- | The current overlay path, if any.
  _weOverlayPath :: Maybe Path,
  -- | The current drag message and source path, if any is active.
  _weDragStatus :: Maybe (Path, WidgetDragMsg),
  -- | Indicates the path and position where the main button was pressed.
  _weMainBtnPress :: Maybe (Path, Point),
  -- | The current active cursor, and the path that set it.
  _weCursor :: Maybe (Path, CursorIcon),
  -- | The current user model.
  _weModel :: s,
  -- | The input status, mainly mouse and keyboard.
  _weInputStatus :: InputStatus,
  {-|
  The timestamp in milliseconds when this event/message cycle started. This
  value starts from zero each time the application is run.
  -}
  _weTimestamp :: Millisecond,
  {-|
  Whether the theme changed in this cycle. Should be considered when a widget
  avoids merging as optimization, as the styles may have changed.
  -}
  _weThemeChanged :: Bool,
  -- | Indicates whether the current widget is in a top layer (zstack).
  _weInTopLayer :: Point -> Bool,
  -- | The current layout direction.
  _weLayoutDirection :: LayoutDirection,
  {-|
  The active viewport.  This may be smaller than the widget's viewport, if it's
  currently inside a scroll or similar.
  -}
  _weViewport :: Rect,
  -- | The current accumulated offset. This can be affected by scroll.
  _weOffset :: Point
}

-- | Complementary information to a Widget, forming a node in the widget tree.
data WidgetNodeInfo =
  WidgetNodeInfo {
    -- | Type of the widget.
    _wniWidgetType :: WidgetType,
    -- | The identifier at creation time of the widget (runtime generated).
    _wniWidgetId :: WidgetId,
    -- | Key/Identifier of the widget (user provided). Used for merging.
    _wniKey :: Maybe WidgetKey,
    -- | The path of the instance in the widget tree, as a set of indexes.
    _wniPath :: Path,
    -- | The requested width for the widget. The one in style takes precedence.
    _wniSizeReqW :: SizeReq,
    -- | The requested height for the widget. The one in style takes precedence.
    _wniSizeReqH :: SizeReq,
    -- | Indicates if the widget is enabled for user interaction.
    _wniEnabled :: Bool,
    -- | Indicates if the widget is visible.
    _wniVisible :: Bool,
    -- | Indicates whether the widget can receive focus.
    _wniFocusable :: Bool,
    {-|
    The area of the window where the widget can draw. Could be out of bounds or
    partially invisible if inside a scroll. The viewport on 'WidgetEnv' defines
    what is currently visible.
    -}
    _wniViewport :: Rect,
    -- | Style attributes of the widget instance.
    _wniStyle :: Style
  } deriving (Eq, Show, Generic)

instance Default WidgetNodeInfo where
  def = WidgetNodeInfo {
    _wniWidgetType = WidgetType (T.pack ""),
    _wniWidgetId = def,
    _wniKey = Nothing,
    _wniPath = emptyPath,
    _wniSizeReqW = def,
    _wniSizeReqH = def,
    _wniEnabled = True,
    _wniVisible = True,
    _wniFocusable = False,
    _wniViewport = def,
    _wniStyle = def
  }

-- | An instance of the widget in the widget tree.
data WidgetNode s e = WidgetNode {
  -- | The actual widget.
  _wnWidget :: Widget s e,
  -- | Information about the instance.
  _wnInfo :: WidgetNodeInfo,
  -- | The children widgets, if any.
  _wnChildren :: Seq (WidgetNode s e)
}

{-|
An instance of the widget in the widget tree, without specific type information.
This allows querying for widgets that may be nested in Composites, which are not
visible as a regular "WidgetNode" because of possible type mismatches (see
"WidgetKeyMap").
-}
data WidgetInstanceNode = WidgetInstanceNode {
  -- | Information about the instance.
  _winInfo :: WidgetNodeInfo,
  -- | The widget state, if any.
  _winState :: Maybe WidgetState,
  -- | The children widget, if any.
  _winChildren :: Seq WidgetInstanceNode
} deriving (Show, Generic)

{-|
Main widget type. This is the type all widgets implement. In general it's not
needed to implement this type directly, and it's easier to use
"Monomer.Widgets.Container" for widgets with children elements and
"Monomer.Widgets.Single" for widgets without children.
-}
data Widget s e =
  Widget {
    {-|
    Initializes the given node. This could include rebuilding the widget in
    case internal state needs to use model/environment information, generate
    user events or make requests to the runtime.

    Arguments:

    - The widget environment.
    - The widget node.

    Returns:

    - The result of the init operation.
    -}
    widgetInit
      :: WidgetEnv s e
      -> WidgetNode s e
      -> WidgetResult s e,
    {-|
    Merges the current node with the node it matched with during the merge
    process. Receives the newly created node (whose *init* function is not
    called), the previous node and the state extracted from that node. This
    process is widget dependent, and may use or ignore the previous state
    depending on newly available information.

    In general, you want to at least keep the previous state unless the widget
    is stateless or only consumes model/environment information.

    Arguments:

    - The widget environment.
    - The widget node.
    - The previous widget node.

    Returns:

    - The result of the merge operation.
    -}
    widgetMerge
      :: WidgetEnv s e
      -> WidgetNode s e
      -> WidgetNode s e
      -> WidgetResult s e,
    {-|
    Disposes the current node. Only used by widgets which allocate resources
    during /init/ or /merge/, and will usually involve requests to the runtime.

    Arguments:

    - The widget environment.
    - The widget node.

    Returns:

    - The result of the dispose operation.
    -}
    widgetDispose
      :: WidgetEnv s e
      -> WidgetNode s e
      -> WidgetResult s e,
    {-|
    Returns the current internal state, which can later be used when during the
    merge process.

    Arguments:

    - The widget environment.
    - The widget node.

    Returns:

    - The internal state, if any.
    -}
    widgetGetState
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Maybe WidgetState,
    {-|
    Returns information about the instance and its children.

    Arguments:

    - The widget environment.
    - The widget node.

    Returns:

    - The untyped node information.
    -}
    widgetGetInstanceTree
      :: WidgetEnv s e
      -> WidgetNode s e
      -> WidgetInstanceNode,
    {-|
    Returns the next focusable node. What next/previous is, depends on how the
    widget works. Moving left to right, top to bottom is usually considered
    forward.

    Arguments:

    - The widget environment.
    - The widget node.
    - The direction in which focus is moving.
    - The path to start the search from.

    Returns:

    - The next focusable node info.
    -}
    widgetFindNextFocus
      :: WidgetEnv s e
      -> WidgetNode s e
      -> FocusDirection
      -> Path
      -> Maybe WidgetNodeInfo,
    {-|
    Returns the currently hovered widget, if any.

    Arguments:

    - The widget environment.
    - The widget node.
    - The path to start the search from.
    - The point to test for.

    Returns:

    - The hovered child index, if any.
    -}
    widgetFindByPoint
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Path
      -> Point
      -> Maybe WidgetNodeInfo,
    {-|
    Returns the widget matching the given path, plus all its parents.

    Arguments:

    - The widget environment.
    - The widget node.
    - The path to search for.

    Returns:

    - The sequence of widgets up to path, ordered from root to target.
    -}
    widgetFindBranchByPath
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Path
      -> Seq WidgetNodeInfo,
    {-|
    Receives a System event and, optionally, returns a result. This can include
    an updated version of the widget (in case it has internal state), user
    events or requests to the runtime.

    Arguments:

    - The widget environment.
    - The widget node.
    - The target path of the event.
    - The SystemEvent to handle.

    Returns:

    - The result of handling the event, if any.
    -}
    widgetHandleEvent
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Path
      -> SystemEvent
      -> Maybe (WidgetResult s e),
    {-|
    Receives a message and, optionally, returns a result. This can include an
    updated version of the widget (in case it has internal state), user events
    or requests to the runtime. There is no validation regarding the message
    type, and the widget should take care of _casting_ to the correct type using
    "Data.Typeable.cast"

    Arguments:

    - The widget environment.
    - The widget node.
    - The target path of the message.
    - The message to handle.

    Returns:

    - The result of handling the message, if any.
    -}
    widgetHandleMessage
      :: forall i . Typeable i
      => WidgetEnv s e
      -> WidgetNode s e
      -> Path
      -> i
      -> Maybe (WidgetResult s e),
    {-|
    Returns the preferred size for the widget.

    Arguments:

    - The widget environment.
    - The widget node.

    Returns:

    - The horizontal and vertical requirements.
    -}
    widgetGetSizeReq
      :: WidgetEnv s e
      -> WidgetNode s e
      -> (SizeReq, SizeReq),
    {-|
    Resizes the widget to the provided size.

    Arguments:

    - The widget environment.
    - The widget node.
    - The new viewport.
    - Helper to checks if a given path, or its children, requested resize.

    Returns:

    - The result of resizing the widget.
    -}
    widgetResize
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Rect
      -> (Path -> Bool)
      -> WidgetResult s e,
    {-|
    Renders the widget's content using the given Renderer.

    Arguments:

    - The widget environment.
    - The widget node.
    - The renderer, providing low level drawing functions.

    Returns:

    - The IO action with rendering instructions.
    -}
    widgetRender
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Renderer
      -> IO ()
  }

instance Show (WidgetRequest s e) where
  show IgnoreParentEvents = "IgnoreParentEvents"
  show IgnoreChildrenEvents = "IgnoreChildrenEvents"
  show (ResizeWidgets wid) = "ResizeWidgets: " ++ show wid
  show (ResizeWidgetsImmediate wid) = "ResizeWidgetsImmediate: " ++ show wid
  show (MoveFocus start dir) = "MoveFocus: " ++ show (start, dir)
  show (SetFocus path) = "SetFocus: " ++ show path
  show (GetClipboard wid) = "GetClipboard: " ++ show wid
  show (SetClipboard _) = "SetClipboard"
  show (StartTextInput rect) = "StartTextInput: " ++ show rect
  show StopTextInput = "StopTextInput"
  show (SetOverlay wid path) = "SetOverlay: " ++ show (wid, path)
  show (ResetOverlay wid) = "ResetOverlay: " ++ show wid
  show (SetCursorIcon wid icon) = "SetCursorIcon: " ++ show (wid, icon)
  show (ResetCursorIcon wid) = "ResetCursorIcon: " ++ show wid
  show (StartDrag wid path info) = "StartDrag: " ++ show (wid, path, info)
  show (StopDrag wid) = "StopDrag: " ++ show wid
  show RenderOnce = "RenderOnce"
  show (RenderEvery wid ms repeat) = "RenderEvery: " ++ show (wid, ms, repeat)
  show (RenderStop wid) = "RenderStop: " ++ show wid
  show (RemoveRendererImage name) = "RemoveRendererImage: " ++ show name
  show ExitApplication{} = "ExitApplication"
  show (UpdateWindow req) = "UpdateWindow: " ++ show req
  show UpdateModel{} = "UpdateModel"
  show (SetWidgetPath wid path) = "SetWidgetPath: " ++ show (wid, path)
  show (ResetWidgetPath wid) = "ResetWidgetPath: " ++ show wid
  show RaiseEvent{} = "RaiseEvent"
  show SendMessage{} = "SendMessage"
  show RunTask{} = "RunTask"
  show RunProducer{} = "RunProducer"
  show RunInRenderThread{} = "RunInRenderThread"

instance Show (WidgetResult s e) where
  show result = "WidgetResult "
    ++ "{ _wrRequests: " ++ show (_wrRequests result)
    ++ ", _wrNode: " ++ show (_wrNode result)
    ++ " }"

instance Show (WidgetEnv s e) where
  show wenv = "WidgetEnv "
    ++ "{ _weOs: " ++ show (_weOs wenv)
    ++ ", _weWindowSize: " ++ show (_weWindowSize wenv)
    ++ ", _weFocusedPath: " ++ show (_weFocusedPath wenv)
    ++ ", _weTimestamp: " ++ show (_weTimestamp wenv)
    ++ " }"

instance Show (WidgetNode s e) where
  show node = "WidgetNode "
    ++ "{ _wnInfo: " ++ show (_wnInfo node)
    ++ ", _wnChildren: " ++ show (_wnChildren node)
    ++ " }"
