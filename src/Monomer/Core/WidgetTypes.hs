{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Core.WidgetTypes where

import Control.Lens (ALens')
import Data.Default
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Typeable (Typeable)

import Monomer.Core.BasicTypes
import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

type Timestamp = Int
type LocalKeys s e = Map WidgetKey (WidgetNode s e)
type GlobalKeys s e = Map WidgetKey (WidgetNode s e)

data FocusDirection
  = FocusFwd
  | FocusBwd
  deriving (Eq, Show)

data TextOverflow
  = Ellipsis
  | ClipText
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
  = WidgetType { unWidgetType :: String }
  deriving (Eq)

instance Show WidgetType where
  show (WidgetType t) = t

instance IsString WidgetType where
  fromString = WidgetType

data WidgetData s a
  = WidgetValue a
  | WidgetLens (ALens' s a)

data WidgetKey
  = WidgetKeyLocal Text
  | WidgetKeyGlobal Text
  deriving (Eq, Ord, Show)

data WidgetState
  = forall i . Typeable i => WidgetState i

data WidgetRequest s
  = IgnoreParentEvents
  | IgnoreChildrenEvents
  | ResizeWidgets
  | MoveFocus FocusDirection
  | SetFocus Path
  | GetClipboard Path
  | SetClipboard ClipboardData
  | StartTextInput Rect
  | StopTextInput
  | SetOverlay Path
  | ResetOverlay
  | SetCursorIcon CursorIcon
  | RenderOnce
  | RenderEvery Path Int
  | RenderStop Path
  | ExitApplication Bool
  | UpdateWindow WindowRequest
  | UpdateModel (s -> s)
  | forall i . Typeable i => SendMessage Path i
  | forall i . Typeable i => RunTask Path (IO i)
  | forall i . Typeable i => RunProducer Path ((i -> IO ()) -> IO ())

data WidgetResult s e = WidgetResult {
  _wrNode :: WidgetNode s e,
  _wrRequests :: Seq (WidgetRequest s),
  _wrEvents :: Seq e
}

-- This instance is lawless (there is not an empty widget): use with caution
instance Semigroup (WidgetResult s e) where
  er1 <> er2 = WidgetResult {
    _wrNode = _wrNode er2,
    _wrRequests = _wrRequests er1 <> _wrRequests er2,
    _wrEvents = _wrEvents er1 <> _wrEvents er2
  }

data WidgetEnv s e = WidgetEnv {
  _weOS :: Text,
  _weRenderer :: Renderer,
  _weTheme :: Theme,
  _weWindowSize :: Size,
  _weGlobalKeys :: GlobalKeys s e,
  _weFocusedPath :: Path,
  _wePressedPath :: Maybe Path,
  _weOverlayPath :: Maybe Path,
  _weCurrentCursor :: CursorIcon,
  _weModel :: s,
  _weInputStatus :: InputStatus,
  _weTimestamp :: Timestamp,
  _weInTopLayer :: Point -> Bool
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
  -- | The children widget, if any
  _winChildren :: Seq WidgetInstanceNode
}

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
      -> FocusDirection
      -> Path
      -> WidgetNode s e
      -> Maybe Path,
    -- | Returns the path of the child item with the given coordinates, starting
    -- | on the given path
    widgetFindByPoint
      :: WidgetEnv s e
      -> Path
      -> Point
      -> WidgetNode s e
      -> Maybe Path,
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
      -> Path
      -> SystemEvent
      -> WidgetNode s e
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
      -> Path
      -> i
      -> WidgetNode s e
      -> Maybe (WidgetResult s e),
    -- | Updates the sizeReq field for the widget
    widgetUpdateSizeReq
      :: WidgetEnv s e
      -> WidgetNode s e
      -> WidgetNode s e,
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
      -> Rect
      -> Rect
      -> WidgetNode s e
      -> WidgetNode s e,
    -- | Renders the widget
    --
    -- Renderer
    -- The widget instance to render
    -- The current time in milliseconds
    --
    -- Returns: unit
    widgetRender
      :: Renderer
      -> WidgetEnv s e
      -> WidgetNode s e
      -> IO ()
  }

-- | Complementary information to a Widget, forming a node in the view tree
data WidgetNodeInfo =
  WidgetNodeInfo {
    -- | Type of the widget
    _wniWidgetType :: !WidgetType,
    -- | Key/Identifier of the widget
    _wniKey :: Maybe WidgetKey,
    -- | The path of the instance in the widget tree
    _wniPath :: !Path,
    -- | The preferred size for the widget
    _wniSizeReqW :: !SizeReq,
    _wniSizeReqH :: !SizeReq,
    -- | Indicates if the widget is enabled for user interaction
    _wniEnabled :: !Bool,
    -- | Indicates if the widget is visible
    _wniVisible :: !Bool,
    -- | Indicates whether the widget can receive focus
    _wniFocusable :: !Bool,
    -- | The visible area of the screen assigned to the widget
    _wniViewport :: !Rect,
    -- | The area of the screen where the widget can draw
    -- | Usually equal to _wniViewport, but may be larger if the widget is
    -- | wrapped in a scrollable container
    _wniRenderArea :: !Rect,
    -- | Style attributes of the widget instance
    _wniStyle :: Style
  } deriving (Eq, Show)

instance Default WidgetNodeInfo where
  def = WidgetNodeInfo {
    _wniWidgetType = "",
    _wniKey = Nothing,
    _wniPath = rootPath,
    _wniSizeReqW = def,
    _wniSizeReqH = def,
    _wniEnabled = True,
    _wniVisible = True,
    _wniFocusable = False,
    _wniViewport = def,
    _wniRenderArea = def,
    _wniStyle = def
  }

instance Show (WidgetRequest s) where
  show IgnoreParentEvents = "IgnoreParentEvents"
  show IgnoreChildrenEvents = "IgnoreChildrenEvents"
  show ResizeWidgets = "ResizeWidgets"
  show (MoveFocus dir) = "MoveFocus: " ++ show dir
  show (SetFocus path) = "SetFocus: " ++ show path
  show (GetClipboard path) = "GetClipboard: " ++ show path
  show (SetClipboard _) = "SetClipboard"
  show (StartTextInput rect) = "StartTextInput: " ++ show rect
  show StopTextInput = "StopTextInput"
  show ResetOverlay = "ResetOverlay"
  show (SetOverlay path) = "SetOverlay: " ++ show path
  show (SetCursorIcon icon) = "SetCursorIcon: " ++ show icon
  show RenderOnce = "RenderOnce"
  show (RenderEvery path ms) = "RenderEvery: " ++ show path ++ " - " ++ show ms
  show (RenderStop path) = "RenderStop: " ++ show path
  show ExitApplication{} = "ExitApplication"
  show (UpdateWindow req) = "UpdateWindow: " ++ show req
  show UpdateModel{} = "UpdateModel"
  show SendMessage{} = "SendMessage"
  show RunTask{} = "RunTask"
  show RunProducer{} = "RunProducer"

instance Show (WidgetResult s e) where
  show result = "WidgetResult "
    ++ "{ _wrRequests: " ++ show (_wrRequests result)
    ++ ", _wrEvents: " ++ show (length (_wrEvents result))
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
