{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Core.WidgetTypes where

import Control.Lens (ALens')
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import GHC.Generics

import qualified Data.Text as T

import Monomer.Core.BasicTypes
import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

type Timestamp = Int

type WidgetModel s = Typeable s
type WidgetEvent e = Typeable e

type WidgetKeysMap s e = Map WidgetKey (WidgetNode s e)

data FocusDirection
  = FocusFwd
  | FocusBwd
  deriving (Eq, Show)

data WindowRequest
  = WindowSetTitle Text
  | WindowSetFullScreen
  | WindowMaximize
  | WindowMinimize
  | WindowRestore
  | WindowBringToFront
  deriving (Eq, Show)

newtype WidgetType
  = WidgetType Text
  deriving (Eq, Show, Generic)

instance IsString WidgetType where
  fromString = WidgetType . T.pack

data WidgetData s a
  = WidgetValue a
  | WidgetLens (ALens' s a)

data WidgetId = WidgetId {
  _widTs :: Int,
  _widPath :: Path
} deriving (Eq, Show, Ord, Generic)

instance Default WidgetId where
  def = WidgetId 0 emptyPath

newtype WidgetKey
  = WidgetKey Text
  deriving (Eq, Show, Ord, Generic)

instance IsString WidgetKey where
  fromString = WidgetKey . T.pack

data WidgetState
  = forall i . WidgetModel i => WidgetState i

instance Show WidgetState where
  show (WidgetState state) = "WidgetState: " ++ show (typeOf state)

data WidgetRequest s e
  = IgnoreParentEvents
  | IgnoreChildrenEvents
  | ResizeWidgets
  | MoveFocus (Maybe WidgetId) FocusDirection
  | SetFocus WidgetId
  | GetClipboard WidgetId
  | SetClipboard ClipboardData
  | StartTextInput Rect
  | StopTextInput
  | SetOverlay WidgetId Path
  | ResetOverlay WidgetId
  | SetCursorIcon WidgetId CursorIcon
  | ResetCursorIcon WidgetId
  | StartDrag WidgetId Path WidgetDragMsg
  | StopDrag WidgetId
  | RenderOnce
  | RenderEvery WidgetId Int (Maybe Int)
  | RenderStop WidgetId
  | ExitApplication Bool
  | UpdateWindow WindowRequest
  | UpdateModel (s -> s)
  | SetWidgetPath WidgetId Path
  | ResetWidgetPath WidgetId
  | WidgetEvent e => RaiseEvent e
  | forall i . Typeable i => SendMessage WidgetId i
  | forall i . Typeable i => RunTask WidgetId Path (IO i)
  | forall i . Typeable i => RunProducer WidgetId Path ((i -> IO ()) -> IO ())

instance Eq e => Eq (WidgetRequest s e) where
  IgnoreParentEvents == IgnoreParentEvents = True
  IgnoreChildrenEvents == IgnoreChildrenEvents = True
  ResizeWidgets == ResizeWidgets = True
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

data WidgetResult s e = WidgetResult {
  _wrNode :: WidgetNode s e,
  _wrRequests :: Seq (WidgetRequest s e)
}

-- This instance is lawless (there is not an empty widget): use with caution
instance Semigroup (WidgetResult s e) where
  er1 <> er2 = WidgetResult {
    _wrNode = _wrNode er2,
    _wrRequests = _wrRequests er1 <> _wrRequests er2
  }

data LayoutDirection
  = LayoutNone
  | LayoutHorizontal
  | LayoutVertical
  deriving (Eq, Show, Generic)

data WidgetEnv s e = WidgetEnv {
  _weOS :: Text,
  _weRenderer :: Renderer,
  _weFindByPath :: Path -> Maybe WidgetNodeInfo,
  _weMainButton :: Button,
  _weTheme :: Theme,
  _weWindowSize :: Size,
  _weGlobalKeys :: WidgetKeysMap s e,
  _weHoveredPath :: Maybe Path,
  _weFocusedPath :: Path,
  _weOverlayPath :: Maybe Path,
  _weDragStatus :: Maybe (Path, WidgetDragMsg),
  _weMainBtnPress :: Maybe (Path, Point),
  _weCursor :: Maybe (Path, CursorIcon),
  _weModel :: s,
  _weInputStatus :: InputStatus,
  _weTimestamp :: Timestamp,
  _weInTopLayer :: Point -> Bool,
  _weLayoutDirection :: LayoutDirection,
  _weViewport :: Rect,
  _weOffset :: Point
}

-- | Complementary information to a Widget, forming a node in the view tree
data WidgetNodeInfo =
  WidgetNodeInfo {
    -- | Type of the widget
    _wniWidgetType :: !WidgetType,
    -- | The identifier at creation time of the widget
    _wniWidgetId :: !WidgetId,
    -- | Key/Identifier of the widget
    _wniKey :: Maybe WidgetKey,
    -- | The path of the instance in the widget tree
    _wniPath :: !Path,
    -- | The preferred size for the widget
    _wniSizeReqW :: !SizeReq,
    _wniSizeReqH :: !SizeReq,
    -- | Indicates whether the widget is displayed as overlay
    _wniOverlay :: !Bool,
    -- | Indicates if the widget is enabled for user interaction
    _wniEnabled :: !Bool,
    -- | Indicates if the widget is visible
    _wniVisible :: !Bool,
    -- | Indicates whether the widget can receive focus
    _wniFocusable :: !Bool,
    -- | The area of the screen where the widget can draw
    _wniViewport :: !Rect,
    -- | Style attributes of the widget instance
    _wniStyle :: Style
  } deriving (Eq, Show, Generic)

instance Default WidgetNodeInfo where
  def = WidgetNodeInfo {
    _wniWidgetType = "",
    _wniWidgetId = def,
    _wniKey = Nothing,
    _wniPath = emptyPath,
    _wniSizeReqW = def,
    _wniSizeReqH = def,
    _wniOverlay = False,
    _wniEnabled = True,
    _wniVisible = True,
    _wniFocusable = False,
    _wniViewport = def,
    _wniStyle = def
  }

data WidgetNode s e = WidgetNode {
  -- | The actual widget
  _wnWidget :: Widget s e,
  -- | Common information about the instance
  _wnInfo :: WidgetNodeInfo,
  -- | The children widget, if any
  _wnChildren :: Seq (WidgetNode s e)
}

data WidgetInstanceNode = WidgetInstanceNode {
  -- | The instance
  _winInfo :: WidgetNodeInfo,
  _winState :: Maybe WidgetState,
  -- | The children widget, if any
  _winChildren :: Seq WidgetInstanceNode
} deriving (Show, Generic)

data Widget s e =
  Widget {
    -- | Performs widget initialization
    widgetInit
      :: WidgetEnv s e
      -> WidgetNode s e
      -> WidgetResult s e,
    -- | Merges the current widget tree with the old one
    --
    -- Current state
    -- Old instance
    -- New instance
    widgetMerge
      :: WidgetEnv s e
      -> WidgetNode s e
      -> WidgetNode s e
      -> WidgetResult s e,
    -- | Performs widget release
    widgetDispose
      :: WidgetEnv s e
      -> WidgetNode s e
      -> WidgetResult s e,
    -- | Returns the current internal state, which can later be used when
    -- | merging widget trees
    widgetGetState
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Maybe WidgetState,
    -- | Returns information about the instance and its children
    widgetGetInstanceTree
      :: WidgetEnv s e
      -> WidgetNode s e
      -> WidgetInstanceNode,
    -- | Returns the list of focusable paths, if any
    --
    widgetFindNextFocus
      :: WidgetEnv s e
      -> WidgetNode s e
      -> FocusDirection
      -> Path
      -> Maybe WidgetNodeInfo,
    -- | Returns the path of the child item with the given coordinates, starting
    -- | on the given path
    widgetFindByPoint
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Path
      -> Point
      -> Maybe WidgetNodeInfo,
    -- | Returns the path of the child item with the given path
    widgetFindByPath
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Path
      -> Maybe WidgetNodeInfo,
    -- | Handles an event
    --
    -- Current user state
    -- Path of focused widget
    -- Current widget path
    -- Event to handle
    --
    -- Returns: the list of generated events and, maybe, a new version of the
    -- widget if internal state changed
    widgetHandleEvent
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Path
      -> SystemEvent
      -> Maybe (WidgetResult s e),
    -- | Handles a custom message
    --
    -- Result of asynchronous computation
    --
    -- Returns: the list of generated events and a new version of the widget if
    -- internal state changed
    widgetHandleMessage
      :: forall i . Typeable i
      => WidgetEnv s e
      -> WidgetNode s e
      -> Path
      -> i
      -> Maybe (WidgetResult s e),
    -- | Returns the size requirements for the widget. This is called to update
    -- WidgetNodeInfo only at specific times.
    widgetGetSizeReq
      :: WidgetEnv s e
      -> WidgetNode s e
      -> (SizeReq, SizeReq),
    -- | Resizes the children of this widget
    --
    -- Vieport assigned to the widget
    -- Region assigned to the widget
    -- Style options
    -- Preferred size for each of the children widgets
    --
    -- Returns: the size assigned to each of the children
    widgetResize
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Rect
      -> WidgetResult s e,
    -- | Renders the widget
    --
    -- Renderer
    -- The widget instance to render
    -- The current time in milliseconds
    --
    -- Returns: unit
    widgetRender
      :: WidgetEnv s e
      -> WidgetNode s e
      -> Renderer
      -> IO ()
  }

instance Show (WidgetRequest s e) where
  show IgnoreParentEvents = "IgnoreParentEvents"
  show IgnoreChildrenEvents = "IgnoreChildrenEvents"
  show ResizeWidgets = "ResizeWidgets"
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
  show ExitApplication{} = "ExitApplication"
  show (UpdateWindow req) = "UpdateWindow: " ++ show req
  show UpdateModel{} = "UpdateModel"
  show (SetWidgetPath wid path) = "SetWidgetPath: " ++ show (wid, path)
  show (ResetWidgetPath wid) = "ResetWidgetPath: " ++ show wid
  show RaiseEvent{} = "RaiseEvent"
  show SendMessage{} = "SendMessage"
  show RunTask{} = "RunTask"
  show RunProducer{} = "RunProducer"

instance Show (WidgetResult s e) where
  show result = "WidgetResult "
    ++ "{ _wrRequests: " ++ show (_wrRequests result)
    ++ ", _wrNode: " ++ show (_wrNode result)
    ++ " }"

instance Show (WidgetEnv s e) where
  show wenv = "WidgetEnv "
    ++ "{ _weOS: " ++ show (_weOS wenv)
    ++ ", _weWindowSize: " ++ show (_weWindowSize wenv)
    ++ ", _weFocusedPath: " ++ show (_weFocusedPath wenv)
    ++ ", _weTimestamp: " ++ show (_weTimestamp wenv)
    ++ " }"

instance Show (WidgetNode s e) where
  show node = "WidgetNode "
    ++ "{ _wnInfo: " ++ show (_wnInfo node)
    ++ ", _wnChildren: " ++ show (_wnChildren node)
    ++ " }"
