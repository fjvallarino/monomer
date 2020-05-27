{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widget.Types where

import Data.Typeable (cast, Typeable)
import GHC.Generics

import qualified Data.Sequence as SQ

import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Widget.PathContext

type Timestamp = Int
type WidgetType = String
type WidgetKey = String
type WidgetChildren s e m = SQ.Seq (WidgetInstance s e m)

data WidgetState = forall i . (Typeable i, Generic i) => WidgetState i

data SizePolicy = StrictSize | FlexibleSize | RemainderSize deriving (Show, Eq)

data SizeReq = SizeReq {
  _sizeRequested :: Size,
  _sizePolicyWidth :: SizePolicy,
  _sizePolicyHeight :: SizePolicy
} deriving (Show, Eq)

data EventResult s e m = EventResult {
  _eventResultRequest :: SQ.Seq (EventRequest s),
  _eventResultUserEvents :: SQ.Seq e,
  _eventResultNewWidget :: WidgetInstance s e m
}

data Widget s e m =
  (Monad m) => Widget {
    -- | Returns the current internal state, which can later be used when merging widget trees
    _widgetGetState :: s -> Maybe WidgetState,
    -- | Merges the current widget tree with the old one
    --
    -- Current app state
    -- Old instance
    -- New instance
    _widgetMerge :: s -> WidgetInstance s e m -> WidgetInstance s e m -> WidgetInstance s e m,
    -- | Returns the list of focusable paths, if any
    --
    _widgetNextFocusable :: PathContext -> WidgetInstance s e m -> Maybe Path,
    -- | Returns the path of the child item with the given coordinates
    _widgetFind :: Point -> WidgetInstance s e m -> Maybe Path,
    -- | Handles an event
    --
    -- Current user state
    -- Path of focused widget
    -- Current widget path
    -- Event to handle
    --
    -- Returns: the list of generated events and, maybe, a new version of the widget if internal state changed
    _widgetHandleEvent :: PathContext -> SystemEvent -> s -> WidgetInstance s e m -> Maybe (EventResult s e m),
    -- | Handles an custom asynchronous event
    --
    -- Result of asynchronous computation
    --
    -- Returns: the list of generated events and, maybe, a new version of the widget if internal state changed
    _widgetHandleCustom :: forall i . Typeable i => PathContext -> i -> s -> WidgetInstance s e m -> Maybe (EventResult s e m),
    -- | Minimum size desired by the widget
    --
    -- Style options
    -- Preferred size for each of the children widgets
    -- Renderer (mainly for text sizing functions)
    --
    -- Returns: the minimum size desired by the widget
    _widgetPreferredSize :: Renderer m -> s -> WidgetInstance s e m -> m (Tree SizeReq),
    -- | Resizes the children of this widget
    --
    -- Vieport assigned to the widget
    -- Region assigned to the widget
    -- Style options
    -- Preferred size for each of the children widgets
    --
    -- Returns: the size assigned to each of the children
    _widgetResize :: s -> Rect -> Rect -> WidgetInstance s e m -> Tree SizeReq -> WidgetInstance s e m,
    -- | Renders the widget
    --
    -- Renderer
    -- The widget instance to render
    -- The current time in milliseconds
    --
    -- Returns: unit
    _widgetRender :: Renderer m -> Timestamp -> PathContext -> s -> WidgetInstance s e m -> m ()
  }

-- | Complementary information to a Widget, forming a node in the view tree
--
-- Type variables:
-- * n: Identifier for a node
data WidgetInstance s e m =
  (Monad m) => WidgetInstance {
    -- | Type of the widget
    _instanceType :: WidgetType,
    -- | Key/Identifier of the widget. If provided, it needs to be unique in the same hierarchy level (not globally)
    _instanceKey :: Maybe WidgetKey,
    -- | The actual widget
    _instanceWidget :: Widget s e m,
    -- | The children widget, if any
    _instanceChildren :: WidgetChildren s e m,
    -- | Indicates if the widget is enabled for user interaction
    _instanceEnabled :: Bool,
    -- | Indicates if the widget is visible
    _instanceVisible :: Bool,
    -- | Indicates whether the widget can receive focus
    _instanceFocusable :: Bool,
    -- | The visible area of the screen assigned to the widget
    _instanceViewport :: Rect,
    -- | The area of the screen where the widget can draw
    -- | Usually equal to _instanceViewport, but may be larger if the widget is wrapped in a scrollable container
    _instanceRenderArea :: Rect,
    -- | Style attributes of the widget instance
    _instanceStyle :: Style
  }
