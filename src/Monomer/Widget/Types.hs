{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widget.Types where

import Control.Lens (ALens')
import Data.ByteString (ByteString)
import Data.Default
import Data.Map.Strict (Map)
import Data.Sequence (Seq, (<|), (|>))
import Data.String (IsString(..))
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
type GlobalKeys s e = Map WidgetKey (WidgetInstance s e)

newtype WidgetType
  = WidgetType { unWidgetType :: String }
  deriving (Eq, Show)

instance IsString WidgetType where
  fromString str = WidgetType str

data WidgetValue s a
  = WidgetValue a
  | WidgetLens (ALens' s a)

newtype WidgetKey
  = WidgetKey Text
  deriving (Show, Eq, Ord)

data WidgetState
  = forall i . Typeable i => WidgetState i

data SizePolicy
  = StrictSize
  | FlexibleSize
  | RemainderSize
  deriving (Show, Eq)

data SizeReq = SizeReq {
  _srSize :: Size,
  _srPolicyW :: SizePolicy,
  _srPolicyH :: SizePolicy
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
  _wrRequests :: Seq (WidgetRequest s),
  _wrEvents :: Seq e,
  _wrWidget :: WidgetInstance s e
}

instance Semigroup (WidgetResult s e) where
  er1 <> er2 = WidgetResult reqs evts widget where
    reqs = _wrRequests er1 <> _wrRequests er2
    evts = _wrEvents er1 <> _wrEvents er2
    widget = _wrWidget er2

data WidgetPlatform = WidgetPlatform {
  _wpOS :: Text,
  _wpGetKeyCode :: String -> Maybe KeyCode,
  _wpComputeTextSize :: Font -> FontSize -> Text -> Size
}

data WidgetEnv s e = WidgetEnv {
  _wePlatform :: WidgetPlatform,
  _weRenderer :: Renderer,
  _weTheme :: Theme,
  _weScreenSize :: Size,
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
    _wiSizeReq :: SizeReq,
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

instance Show (WidgetInstance s e) where
  show inst = desc where
    desc = wtype ++ ", " ++ wkey ++ ", " ++ wchildren
    wtype = "Type = " ++ unWidgetType (_wiWidgetType inst)
    wkey = "Key = " ++ show (_wiKey inst)
    wchildren = "Children = " ++ show (length $ _wiChildren inst)
