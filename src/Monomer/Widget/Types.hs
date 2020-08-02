{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widget.Types where

import Control.Lens (ALens')
import Data.Default
import Data.Map.Strict (Map)
import Data.Sequence (Seq, (<|), (|>))
import Data.Text (Text)
import Data.Typeable (cast, Typeable)

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types

type Timestamp = Int
type WidgetType = String
type GlobalKeys s e = Map WidgetKey (WidgetInstance s e)

data WidgetValue s a
  = WidgetValue a
  | WidgetLens (ALens' s a)

newtype WidgetKey
  = WidgetKey Text deriving (Show, Eq, Ord)

data WidgetState
  = forall i . Typeable i => WidgetState i

data SizePolicy
  = StrictSize
  | FlexibleSize
  | RemainderSize
  deriving (Show, Eq)

data SizeReq = SizeReq {
  _sizeRequested :: Size,
  _sizePolicyWidth :: SizePolicy,
  _sizePolicyHeight :: SizePolicy
} deriving (Show, Eq)

instance Default SizeReq where
  def = SizeReq def FlexibleSize FlexibleSize

data WidgetRequest s
  = IgnoreParentEvents
  | IgnoreChildrenEvents
  | Resize
  | SetFocus Path
  | GetClipboard Path
  | SetClipboard ClipboardData
  | ResetOverlay
  | SetOverlay Path
  | UpdateModel (s -> s)
  | forall i . Typeable i => SendMessage Path i
  | forall i . Typeable i => RunTask Path (IO i)
  | forall i . Typeable i => RunProducer Path ((i -> IO ()) -> IO ())

data WidgetResult s e = WidgetResult {
  _resultRequests :: Seq (WidgetRequest s),
  _resultEvents :: Seq e,
  _resultWidget :: WidgetInstance s e
}

instance Semigroup (WidgetResult s e) where
  er1 <> er2 = WidgetResult reqs evts widget where
    reqs = _resultRequests er1 <> _resultRequests er2
    evts = _resultEvents er1 <> _resultEvents er2
    widget = _resultWidget er2

data WidgetPlatform = WidgetPlatform {
  _wpOS :: Text,
  _wpGetKeyCode :: String -> Maybe KeyCode,
  _wpGetTextSize :: Font -> FontSize -> Text -> Size
}

data WidgetEnv s e = WidgetEnv {
  _wePlatform :: WidgetPlatform,
  _weScreenSize :: Size,
  _weGlobalKeys :: GlobalKeys s e,
  _weFocusedPath :: Path,
  _weModel :: s,
  _weInputStatus :: InputStatus,
  _weTimestamp :: Int
}

data Widget s e =
  Widget {
    -- | Performs widget initialization
    _widgetInit
      :: WidgetEnv s e
      -> WidgetInstance s e
      -> WidgetResult s e,
    -- | Returns the current internal state, which can later be used when
    -- | merging widget trees
    _widgetGetState
      :: WidgetEnv s e
      -> Maybe WidgetState,
    -- | Merges the current widget tree with the old one
    --
    -- Current state
    -- Old instance
    -- New instance
    _widgetMerge
      :: WidgetEnv s e
      -> WidgetInstance s e
      -> WidgetInstance s e
      -> WidgetResult s e,
    -- | Returns the list of focusable paths, if any
    --
    _widgetNextFocusable
      :: WidgetEnv s e
      -> Path
      -> WidgetInstance s e
      -> Maybe Path,
    -- | Returns the path of the child item with the given coordinates, starting
    -- | on the given path
    _widgetFind
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
    _widgetHandleEvent
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
    _widgetHandleMessage
      :: forall i . Typeable i
      => WidgetEnv s e
      -> Path
      -> i
      -> WidgetInstance s e
      -> Maybe (WidgetResult s e),
    -- | Minimum size desired by the widget
    --
    -- Style options
    -- Preferred size for each of the children widgets
    -- Renderer (mainly for text sizing functions)
    --
    -- Returns: the minimum size desired by the widget
    _widgetPreferredSize
      :: WidgetEnv s e
      -> WidgetInstance s e
      -> Tree SizeReq,
    -- | Resizes the children of this widget
    --
    -- Vieport assigned to the widget
    -- Region assigned to the widget
    -- Style options
    -- Preferred size for each of the children widgets
    --
    -- Returns: the size assigned to each of the children
    _widgetResize
      :: WidgetEnv s e
      -> Rect
      -> Rect
      -> Tree SizeReq
      -> WidgetInstance s e
      -> WidgetInstance s e,
    -- | Renders the widget
    --
    -- Renderer
    -- The widget instance to render
    -- The current time in milliseconds
    --
    -- Returns: unit
    _widgetRender
      :: forall m . Monad m
      => Renderer m
      -> WidgetEnv s e
      -> WidgetInstance s e
      -> m ()
  }

-- | Complementary information to a Widget, forming a node in the view tree
data WidgetInstance s e =
  WidgetInstance {
    -- | Type of the widget
    _instanceType :: !WidgetType,
    -- | Key/Identifier of the widget
    _instanceKey :: Maybe WidgetKey,
    -- | The path of the instance in the widget tree
    _instancePath :: !Path,
    -- | The actual widget
    _instanceWidget :: Widget s e,
    -- | The children widget, if any
    _instanceChildren :: Seq (WidgetInstance s e),
    -- | Indicates if the widget is enabled for user interaction
    _instanceEnabled :: !Bool,
    -- | Indicates if the widget is visible
    _instanceVisible :: !Bool,
    -- | Indicates whether the widget can receive focus
    _instanceFocusable :: !Bool,
    -- | The visible area of the screen assigned to the widget
    _instanceViewport :: !Rect,
    -- | The area of the screen where the widget can draw
    -- | Usually equal to _instanceViewport, but may be larger if the widget is
    -- | wrapped in a scrollable container
    _instanceRenderArea :: !Rect,
    -- | Style attributes of the widget instance
    _instanceStyle :: Style
  }
