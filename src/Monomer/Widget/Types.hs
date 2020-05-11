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

type Timestamp = Int
type WidgetType = String
type WidgetKey = String
type WidgetNode s e m = Tree (WidgetInstance s e m)
type WidgetChildren s e m = SQ.Seq (WidgetNode s e m)

data WidgetState = forall i . (Typeable i, Generic i) => WidgetState i

data SizePolicy = StrictSize | FlexibleSize | RemainderSize deriving (Show, Eq)

data SizeReq = SizeReq {
  _srSize :: Size,
  _srPolicyWidth :: SizePolicy,
  _srPolicyHeight :: SizePolicy,
  _srVisible :: Bool
} deriving (Show, Eq)

data WidgetEventResult s e m = WidgetEventResult {
  _eventResultRequest :: [EventRequest],
  _eventResultUserEvents :: [e],
  _eventResultNewWidget :: Maybe (Widget s e m),
  _eventResultNewState :: s -> s
}

data WidgetResizeResult s e m = WidgetResizeResult {
  _resizeResultRenderAreas :: [Rect],
  _resizeResultViewports :: [Rect],
  _resizeResultWidget :: Maybe (Widget s e m)
}

data ChildEventResult s e m = ChildEventResult {
  cerIgnoreParentEvents :: Bool,
  cerEventRequests :: [(Path, EventRequest)],
  cerUserEvents :: [e],
  cerNewTreeNode :: Maybe (WidgetNode s e m),
  cerNewState :: [s -> s]
}

data Widget s e m =
  (Monad m) => Widget {
    -- | Type of the widget
    _widgetType :: WidgetType,
    -- | Indicates whether the widget can receive focus
    _widgetFocusable :: Bool,
    -- | Provides the previous internal state to the new widget, which can choose to ignore it or update itself
    _widgetRestoreState :: s -> Maybe WidgetState -> Maybe (Widget s e m),
    -- | Returns the current internal state, which can later be used to restore the widget
    _widgetSaveState :: s -> Maybe WidgetState,
    -- | Handles an event
    --
    -- Region assigned to the widget
    -- Event to handle
    --
    -- Returns: the list of generated events and, maybe, a new version of the widget if internal state changed
    _widgetHandleEvent :: s -> Rect -> SystemEvent -> Maybe (WidgetEventResult s e m),
    -- | Handles an custom asynchronous event
    --
    -- Result of asynchronous computation
    --
    -- Returns: the list of generated events and, maybe, a new version of the widget if internal state changed
    _widgetHandleCustom :: forall i . Typeable i => s -> i -> Maybe (WidgetEventResult s e m),
    -- | Minimum size desired by the widget
    --
    -- Style options
    -- Preferred size for each of the children widgets
    -- Renderer (mainly for text sizing functions)
    --
    -- Returns: the minimum size desired by the widget
    _widgetPreferredSize :: Renderer m -> s -> Style -> [SizeReq] -> m SizeReq,
    -- | Resizes the children of this widget
    --
    -- Vieport assigned to the widget
    -- Region assigned to the widget
    -- Style options
    -- Preferred size for each of the children widgets
    --
    -- Returns: the size assigned to each of the children
    _widgetResizeChildren :: Rect -> Rect -> Style -> [SizeReq] -> Maybe (WidgetResizeResult s e m),
    -- | Renders the widget
    --
    -- Renderer
    -- The widget instance to render
    -- The current time in milliseconds
    --
    -- Returns: unit
    _widgetRender :: Renderer m -> s -> WidgetInstance s e m -> Timestamp -> m (),
    _widgetRenderPost :: Renderer m -> s  -> WidgetInstance s e m -> Timestamp -> m ()
  }

-- | Complementary information to a Widget, forming a node in the view tree
--
-- Type variables:
-- * n: Identifier for a node
data WidgetInstance s e m =
  (Monad m) => WidgetInstance {
    -- | Key/Identifier of the widget. If provided, it needs to be unique in the same hierarchy level (not globally)
    _widgetInstanceKey :: Maybe WidgetKey,
    -- | The actual widget
    _widgetInstanceWidget :: Widget s e m,
    -- | Indicates if the widget is enabled for user interaction
    _widgetInstanceEnabled :: Bool,
    -- | Indicates if the widget is visible
    _widgetInstanceVisible :: Bool,
    -- | Indicates if the widget is focused
    _widgetInstanceFocused :: Bool,
    -- | The visible area of the screen assigned to the widget
    _widgetInstanceViewport :: Rect,
    -- | The area of the screen where the widget can draw
    -- | Usually equal to _widgetInstanceViewport, but may be larger if the widget is wrapped in a scrollable container
    _widgetInstanceRenderArea :: Rect,
    -- | Style attributes of the widget instance
    _widgetInstanceStyle :: Style
  }
