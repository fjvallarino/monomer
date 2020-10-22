{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Core.WidgetTypes where

import Control.Lens (ALens')
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Typeable (Typeable)

import Monomer.Core.BasicTypes
import Monomer.Core.StyleTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

type Timestamp = Int
type GlobalKeys s e = Map WidgetKey (WidgetInstance s e)

data FocusDirection
  = FocusFwd
  | FocusBwd
  deriving (Eq, Show)

data TextOverflow
  = Ellipsis
  | ClipText
  deriving (Eq, Show)

newtype WidgetType
  = WidgetType { unWidgetType :: String }
  deriving (Eq, Show)

instance IsString WidgetType where
  fromString = WidgetType

data WidgetData s a
  = WidgetValue a
  | WidgetLens (ALens' s a)

newtype WidgetKey
  = WidgetKey Text
  deriving (Show, Eq, Ord)

data WidgetState
  = forall i . Typeable i => WidgetState i

data WidgetRequest s
  = IgnoreParentEvents
  | IgnoreChildrenEvents
  | Resize
  | MoveFocus FocusDirection
  | SetFocus Path
  | GetClipboard Path
  | SetClipboard ClipboardData
  | StartTextInput Rect
  | StopTextInput
  | ResetOverlay
  | SetOverlay Path
  | SetCursorIcon CursorIcon
  | UpdateModel (s -> s)
  | forall i . Typeable i => SendMessage Path i
  | forall i . Typeable i => RunTask Path (IO i)
  | forall i . Typeable i => RunProducer Path ((i -> IO ()) -> IO ())

data WidgetResult s e = WidgetResult {
  _wrRequests :: Seq (WidgetRequest s),
  _wrEvents :: Seq e,
  _wrWidget :: WidgetInstance s e
}

instance Semigroup (WidgetResult s e) where
  er1 <> er2 = WidgetResult reqs evts widget where
    reqs = _wrRequests er1 <> _wrRequests er2
    evts = _wrEvents er1 <> _wrEvents er2
    widget = _wrWidget er2

data WidgetEnv s e = WidgetEnv {
  _weOS :: Text,
  _weRenderer :: Renderer,
  _weTheme :: Theme,
  _weAppWindowSize :: Size,
  _weGlobalKeys :: GlobalKeys s e,
  _weFocusedPath :: Path,
  _weModel :: s,
  _weInputStatus :: InputStatus,
  _weTimestamp :: Timestamp
}

data Widget s e =
  Widget {
    -- | Performs widget initialization
    widgetInit
      :: WidgetEnv s e
      -> WidgetInstance s e
      -> WidgetResult s e,
    -- | Merges the current widget tree with the old one
    --
    -- Current state
    -- Old instance
    -- New instance
    widgetMerge
      :: WidgetEnv s e
      -> WidgetInstance s e
      -> WidgetInstance s e
      -> WidgetResult s e,
    -- | Performs widget release
    widgetDispose
      :: WidgetEnv s e
      -> WidgetInstance s e
      -> WidgetResult s e,
    -- | Returns the current internal state, which can later be used when
    -- | merging widget trees
    widgetGetState
      :: WidgetEnv s e
      -> Maybe WidgetState,
    -- | Returns the list of focusable paths, if any
    --
    widgetFindNextFocus
      :: WidgetEnv s e
      -> FocusDirection
      -> Path
      -> WidgetInstance s e
      -> Maybe Path,
    -- | Returns the path of the child item with the given coordinates, starting
    -- | on the given path
    widgetFindByPoint
      :: WidgetEnv s e
      -> Path
      -> Point
      -> WidgetInstance s e
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
      -> WidgetInstance s e
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
      -> WidgetInstance s e
      -> Maybe (WidgetResult s e),
    -- | Updates the sizeReq field for the widget
    widgetUpdateSizeReq
      :: WidgetEnv s e
      -> WidgetInstance s e
      -> WidgetInstance s e,
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
      -> WidgetInstance s e
      -> WidgetInstance s e,
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
      -> WidgetInstance s e
      -> IO ()
  }

-- | Complementary information to a Widget, forming a node in the view tree
data WidgetInstance s e =
  WidgetInstance {
    -- | Type of the widget
    _wiWidgetType :: !WidgetType,
    -- | Key/Identifier of the widget
    _wiKey :: Maybe WidgetKey,
    -- | The path of the instance in the widget tree
    _wiPath :: !Path,
    -- | The actual widget
    _wiWidget :: Widget s e,
    -- | The children widget, if any
    _wiChildren :: Seq (WidgetInstance s e),
    -- | The preferred size for the widget
    _wiSizeReqW :: SizeReq,
    _wiSizeReqH :: SizeReq,
    -- | Indicates if the widget is enabled for user interaction
    _wiEnabled :: !Bool,
    -- | Indicates if the widget is visible
    _wiVisible :: !Bool,
    -- | Indicates whether the widget can receive focus
    _wiFocusable :: !Bool,
    -- | The visible area of the screen assigned to the widget
    _wiViewport :: !Rect,
    -- | The area of the screen where the widget can draw
    -- | Usually equal to _wiViewport, but may be larger if the widget is
    -- | wrapped in a scrollable container
    _wiRenderArea :: !Rect,
    -- | Style attributes of the widget instance
    _wiStyle :: Style
  }

instance Show (WidgetRequest s) where
  show IgnoreParentEvents = "IgnoreParentEvents"
  show IgnoreChildrenEvents = "IgnoreChildrenEvents"
  show Resize = "Resize"
  show (MoveFocus dir) = "MoveFocus: " ++ show dir
  show (SetFocus path) = "SetFocus: " ++ show path
  show (GetClipboard path) = "GetClipboard: " ++ show path
  show (SetClipboard _) = "SetClipboard"
  show (StartTextInput rect) = "StartTextInput: " ++ show rect
  show StopTextInput = "StopTextInput"
  show ResetOverlay = "ResetOverlay"
  show (SetOverlay path) = "SetOverlay: " ++ show path
  show (SetCursorIcon icon) = "SetCursorIcon: " ++ show icon
  show UpdateModel{} = "UpdateModel"
  show SendMessage{} = "SendMessage"
  show RunTask{} = "RunTask"
  show RunProducer{} = "RunProducer"

instance Show (WidgetEnv s e) where
  show wenv = "WidgetEnv "
    ++ "{ _weOS: " ++ show (_weOS wenv)
    ++ ", _weAppWindowSize: " ++ show (_weAppWindowSize wenv)
    ++ ", _weFocusedPath: " ++ show (_weFocusedPath wenv)
    ++ ", _weTimestamp: " ++ show (_weTimestamp wenv)
    ++ " }"

instance Show (WidgetInstance s e) where
  show inst = "WidgetInstance "
    ++ "{ _wiWidgetType: " ++ show (_wiWidgetType inst)
    ++ ", _wiKey: " ++ show (_wiKey inst)
    ++ ", _wiPath: " ++ show (_wiPath inst)
    ++ ", _wiSizeReqW: " ++ show (_wiSizeReqW inst)
    ++ ", _wiSizeReqH: " ++ show (_wiSizeReqH inst)
    ++ ", _wiEnabled: " ++ show (_wiEnabled inst)
    ++ ", _wiVisible: " ++ show (_wiVisible inst)
    ++ ", _wiFocusable: " ++ show (_wiFocusable inst)
    ++ ", _wiViewport: " ++ show (_wiViewport inst)
    ++ ", _wiRenderArea: " ++ show (_wiRenderArea inst)
    ++ " }"
