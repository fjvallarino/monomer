{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Core where

import Control.Monad
import Control.Monad.State

import Data.Default
import Data.String

import GUI.Core
import GUI.Data.Tree
import GUI.Widget.Style

import qualified Data.Text as T
import qualified Data.Sequence as SQ

type Timestamp = Int
type Enabled = Bool
type Focused = Bool

data Direction = Horizontal | Vertical deriving (Show, Eq)

data Button = LeftBtn | RightBtn deriving (Show, Eq)
data ButtonState = PressedBtn | ReleasedBtn deriving (Show, Eq)

type KeyCode = Int
data KeyMotion = KeyPressed | KeyReleased deriving (Show, Eq)

data SystemEvent = Update Timestamp |
                   Click Point Button ButtonState |
                   KeyAction KeyCode KeyMotion deriving (Show, Eq)

data EventResult s e m = NoEvents | Events [e] | EventsState [e] (Widget s e m)
newtype WidgetType = WidgetType String deriving Eq
newtype WidgetKey = WidgetKey String deriving Eq

instance IsString WidgetType where
  fromString string = WidgetType string

instance IsString WidgetKey where
  fromString string = WidgetKey string

newtype NodePath = NodePath [Int]
data NodeInfo = NodeInfo WidgetType (Maybe WidgetKey)

instance Semigroup (EventResult s e m) where
  (<>) NoEvents er2 = er2
  (<>) er1 NoEvents = er1
  (<>) (Events e1) (Events e2) = Events (e1 ++ e2)
  (<>) (EventsState e1 s1) (Events e2) = EventsState (e1 ++ e2) s1
  (<>) (Events e1) (EventsState e2 s2) = EventsState (e1 ++ e2) s2
  (<>) (EventsState e1 s1) (EventsState e2 s2) = EventsState (e1 ++ e2) s2

data GUIContext app = GUIContext {
  _appContext :: app,
  _focusRing :: [Path]
} deriving (Show, Eq)

initGUIContext :: app -> GUIContext app
initGUIContext app = GUIContext {
  _appContext = app,
  _focusRing = []
}

isFocused :: GUIContext app -> Path -> Focused
isFocused (GUIContext _ []) _ = False
isFocused (GUIContext _ (x:xs)) path = x == path

data Widget s e m =
  (MonadState s m) => Widget {
    -- | Type of the widget
    _widgetType :: WidgetType,
    -- | Indicates whether the widget can receive focus
    _widgetFocusable :: Bool,
    -- | Handles an event
    --
    -- Region assigned to the widget
    -- Indicates if the widget has focus
    -- Event to handle
    --
    -- Returns: the list of generated events and, maybe, a new version of the widget if internal state changed
    _handleEvent :: Rect -> Bool -> SystemEvent -> EventResult s e m,
    -- | Minimum size desired by the widget
    --
    -- Style options
    -- Preferred size for each of the children widgets
    -- Renderer (mainly for text sizing functions)
    --
    -- Returns: the minimum size desired by the widget
    _preferredSize :: Renderer m -> Style -> [Size] -> m Size,
    -- | Resizes the children of this widget
    --
    -- Region assigned to the widget
    -- Style options
    -- Preferred size for each of the children widgets
    --
    -- Returns: the size assigned to each of the children
    _resizeChildren :: Rect -> Style -> [Size] -> [Rect],
    -- | Renders the widget
    --
    -- Renderer
    -- Region assigned to the widget
    -- Style options
    -- Indicates if the widget (and its children) are enabled
    -- Indicates if the widget has focus
    -- The current time in milliseconds
    --
    -- Returns: unit
    _render :: Renderer m -> Rect -> Style -> Enabled -> Focused -> Timestamp -> m ()
  }

-- | Complementary information to a Widget, forming a node in the view tree
--
-- Type variables:
-- * n: Identifier for a node
data WidgetNode s e m =
  (MonadState s m) => WidgetNode {
    -- | Key/Identifier of the widget. If provided, it needs to be unique in the same hierarchy level (not globally)
    _widgetKey :: Maybe WidgetKey,
    _widget :: Widget s e m,
    _widgetEnabled :: Enabled,
    _viewport :: Rect,
    _style :: Style,
    _calculatedStyle :: Style,
    _calculatedSize :: Size
  }

key :: (MonadState s m) => WidgetKey -> WidgetNode s e m -> WidgetNode s e m
key key wn = wn { _widgetKey = Just key }

style :: (MonadState s m) => Tree (WidgetNode s e m) -> Style -> Tree (WidgetNode s e m)
style (Node value children) newStyle = Node (value { _style = newStyle }) children

children :: (MonadState s m) => Tree (WidgetNode s e m) -> [Tree (WidgetNode s e m)] -> Tree (WidgetNode s e m)
children (Node value _) newChildren = fromList value newChildren

cascadeStyle :: (MonadState s m) => Style -> Tree (WidgetNode s e m) -> Tree (WidgetNode s e m)
cascadeStyle parentStyle (Node (wn@WidgetNode{..}) children) = newNode where
  newNode = Node (wn { _calculatedStyle = newStyle }) newChildren
  newStyle = _style <> parentStyle
  newChildren = fmap (cascadeStyle newStyle) children

defaultWidgetNode :: (MonadState s m) => Widget s e m -> WidgetNode s e m
defaultWidgetNode widget = WidgetNode {
  _widgetKey = Nothing,
  _widget = widget,
  _widgetEnabled = True,
  _viewport = def,
  _style = mempty,
  _calculatedStyle = mempty,
  _calculatedSize = def
}

singleWidget :: (MonadState s m) => Widget s e m -> Tree (WidgetNode s e m)
singleWidget widget = singleton (defaultWidgetNode widget)

parentWidget :: (MonadState s m) => Widget s e m -> [Tree (WidgetNode s e m)] -> Tree (WidgetNode s e m)
parentWidget widget = fromList (defaultWidgetNode widget)

emptyState :: Maybe ()
emptyState = Nothing

widgetMatches :: (MonadState s m) => WidgetNode s e m -> WidgetNode s e m -> Bool
widgetMatches wn1 wn2 = _widgetType (_widget wn1) == _widgetType (_widget wn2) && _widgetKey wn1 == _widgetKey wn2

mergeTrees :: (MonadState s m) => Tree (WidgetNode s e m) -> Tree (WidgetNode s e m) -> Tree (WidgetNode s e m)
mergeTrees node1@(Node widget1 seq1) (Node widget2 seq2) = newNode where
  matches = widgetMatches widget1 widget2
  newNode = if | matches -> Node widget2 newChildren
               | otherwise -> node1
  newChildren = mergedChildren SQ.>< addedChildren
  mergedChildren = fmap mergeChild (SQ.zip seq1 seq2)
  addedChildren = SQ.drop (SQ.length seq2) seq1
  mergeChild = \(c1, c2) -> mergeTrees c1 c2

handleWidgetEvents :: (MonadState s m, Traversable t) => Widget s e m -> Rect -> Focused -> t SystemEvent -> EventResult s e m
handleWidgetEvents (Widget {..}) viewport focused systemEvents =
  foldl (\eventResult event -> eventResult <> _handleEvent viewport focused event) NoEvents systemEvents

handleEvents :: (MonadState s m, Traversable t) => Path -> Tree (WidgetNode s e m) -> Path -> t SystemEvent -> (Tree (WidgetNode s e m), SQ.Seq e)
handleEvents focusedPath (Node (wn@WidgetNode { .. }) children) currentPath systemEvents = (newNode, childEvents) where
  (newWidget, events) = case handleWidgetEvents _widget _viewport (focusedPath == currentPath) systemEvents of
                          NoEvents -> (_widget, [])
                          Events evts -> (_widget, evts)
                          EventsState evts wdt -> (wdt, evts)
  (newChildren, childEvents, _) = foldl (\(ws, evs, idx) widgetNode -> case handleEvents focusedPath widgetNode (idx : currentPath) systemEvents of
                                        (ws2, evs2) -> (ws SQ.|> ws2, evs SQ.>< evs2, idx + 1)) (SQ.empty, SQ.fromList events, 0) children
  newNode = Node (wn { _widget = newWidget }) newChildren

handleRender :: (MonadState s m) => Renderer m -> WidgetNode s e m -> Focused -> Timestamp -> m ()
handleRender renderer (WidgetNode { _widget = Widget{..}, .. }) focused ts = _render renderer _viewport _calculatedStyle _widgetEnabled focused ts

resizeUI :: (MonadState s m) => Renderer m -> Rect -> Tree (WidgetNode s e m) -> m (Tree (WidgetNode s e m))
resizeUI renderer assignedRect widgetNode = do
  preferredSizes <- buildPreferredSizes renderer widgetNode
  resizeNode renderer assignedRect preferredSizes widgetNode

buildPreferredSizes :: (MonadState s m) => Renderer m -> Tree (WidgetNode s e m) -> m (Tree Size)
buildPreferredSizes renderer (Node (WidgetNode {..}) children) = do
  childrenSizes <- mapM (buildPreferredSizes renderer) children
  size <- _preferredSize _widget renderer _style (seqToList childrenSizes)

  return $ Node size childrenSizes

resizeNode :: (MonadState s m) => Renderer m -> Rect -> Tree Size -> Tree (WidgetNode s e m) -> m (Tree (WidgetNode s e m))
resizeNode renderer assignedRect (Node _ {--widgetSize--} childrenSizes) (Node widgetNode childrenWns) = do
  let widget = _widget widgetNode
  let style = _style widgetNode
  let updatedNode = widgetNode { _viewport = assignedRect }
  let assignedRects = (_resizeChildren widget) assignedRect style (seqToList childrenSizes)
  let childrenPair = SQ.zip3 childrenSizes childrenWns (SQ.fromList assignedRects)
  let childResize = \(size, node, rect) -> resizeNode renderer rect size node

  newChildren <- mapM childResize childrenPair

  return (Node updatedNode newChildren)
