{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
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

data Direction = Horizontal | Vertical deriving (Show, Eq)

data Button = LeftBtn | RightBtn deriving (Show, Eq)
data ButtonState = PressedBtn | ReleasedBtn deriving (Show, Eq)

type KeyCode = Int
data KeyMotion = KeyPressed | KeyReleased deriving (Show, Eq)

data SystemEvent = Update Timestamp |
                   Click Point Button ButtonState |
                   KeyAction KeyCode KeyMotion deriving (Show, Eq)

data FontInstance = FontInstance

data Theme = Theme {
  _backgroundColor :: Color,
  _primaryColor :: Color,
  _secondaryColor :: Color,
  _palette :: [Color],
  _titleFont :: FontInstance,
  _subtitleFont :: FontInstance,
  _labelFont :: FontInstance,
  _messageFont :: FontInstance
}

data EventResult e m = NoEvents | Events [e] | EventsState [e] (Widget e m)
newtype WidgetType = WidgetType String deriving Eq
newtype WidgetKey = WidgetKey String deriving Eq

instance IsString WidgetType where
  fromString string = WidgetType string

instance IsString WidgetKey where
  fromString string = WidgetKey string

newtype NodePath = NodePath [Int]
data NodeInfo = NodeInfo WidgetType (Maybe WidgetKey)

instance Semigroup (EventResult e m) where
  (<>) NoEvents er2 = er2
  (<>) er1 NoEvents = er1
  (<>) (Events e1) (Events e2) = Events (e1 ++ e2)
  (<>) (EventsState e1 s1) (Events e2) = EventsState (e1 ++ e2) s1
  (<>) (Events e1) (EventsState e2 s2) = EventsState (e1 ++ e2) s2
  (<>) (EventsState e1 s1) (EventsState e2 s2) = EventsState (e1 ++ e2) s2

data Widget e m =
  (Monad m) => Widget {
    -- | Type of the widget
    _widgetType :: WidgetType,
    -- | Handles an event
    --
    -- Region assigned to the widget
    -- Event to handle
    --
    -- Returns: the list of generated events and, maybe, a new version of the widget if internal state changed
    _handleEvent :: Rect -> SystemEvent -> EventResult e m,
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
    -- Region assigned to the widget
    -- Style options
    -- Renderer
    --
    -- Returns: unit
    _render :: Renderer m -> Timestamp -> Rect -> Style -> m ()
  }

-- | Complementary information to a Widget, forming a node in the view tree
--
-- Type variables:
-- * n: Identifier for a node
data WidgetNode e m =
  (Monad m) => WidgetNode {
    -- | Key/Identifier of the widget. If provided, it needs to be unique in the same hierarchy level (not globally)
    _widgetKey :: Maybe WidgetKey,
    _widget :: Widget e m,
    _viewport :: Rect,
    _style :: Style,
    _calculatedStyle :: Style,
    _calculatedSize :: Size
  }

key :: (Monad m) => WidgetKey -> WidgetNode e m -> WidgetNode e m
key key wn = wn { _widgetKey = Just key }

style :: (Monad m) => Tree (WidgetNode e m) -> Style -> Tree (WidgetNode e m)
style (Node value children) newStyle = Node (value { _style = newStyle }) children

children :: (Monad m) => Tree (WidgetNode e m) -> [Tree (WidgetNode e m)] -> Tree (WidgetNode e m)
children (Node value _) newChildren = fromList value newChildren

cascadeStyle :: (Monad m) => Style -> Tree (WidgetNode e m) -> Tree (WidgetNode e m)
cascadeStyle parentStyle (Node (wn@WidgetNode{..}) children) = newNode where
  newNode = Node (wn { _calculatedStyle = newStyle }) newChildren
  newStyle = _style <> parentStyle
  newChildren = fmap (cascadeStyle newStyle) children

defaultWidgetNode :: (Monad m) => Widget e m -> WidgetNode e m
defaultWidgetNode widget = WidgetNode {
  _widgetKey = Nothing,
  _widget = widget,
  _viewport = def,
  _style = mempty,
  _calculatedStyle = mempty,
  _calculatedSize = def
}

singleWidget :: (Monad m) => Widget e m -> Tree (WidgetNode e m)
singleWidget widget = singleton (defaultWidgetNode widget)

parentWidget :: (Monad m) => Widget e m -> [Tree (WidgetNode e m)] -> Tree (WidgetNode e m)
parentWidget widget = fromList (defaultWidgetNode widget)

emptyState :: Maybe ()
emptyState = Nothing

widgetMatches :: (Monad m) => WidgetNode e m -> WidgetNode e m -> Bool
widgetMatches wn1 wn2 = _widgetType (_widget wn1) == _widgetType (_widget wn2) && _widgetKey wn1 == _widgetKey wn2

mergeTrees :: (Monad m) => Tree (WidgetNode e m) -> Tree (WidgetNode e m) -> Tree (WidgetNode e m)
mergeTrees node1@(Node widget1 seq1) (Node widget2 seq2) = newNode where
  matches = widgetMatches widget1 widget2
  newNode = if | matches -> Node widget2 newChildren
               | otherwise -> node1
  newChildren = mergedChildren SQ.>< addedChildren
  mergedChildren = fmap mergeChild (SQ.zip seq1 seq2)
  addedChildren = SQ.drop (SQ.length seq2) seq1
  mergeChild = \(c1, c2) -> mergeTrees c1 c2

handleWidgetEvents :: (Monad m, Traversable t) => Widget e m -> Rect -> t SystemEvent -> EventResult e m
handleWidgetEvents (Widget {..}) viewport systemEvents =
  foldl (\eventResult event -> eventResult <> _handleEvent viewport event) NoEvents systemEvents

handleEvents :: (Monad m, Traversable t) => Tree (WidgetNode e m) -> t SystemEvent -> (Tree (WidgetNode e m), SQ.Seq e)
handleEvents (Node (wn@WidgetNode { .. }) children) systemEvents = (newNode, childEvents) where
  (newWidget, events) = case handleWidgetEvents _widget _viewport systemEvents of
                          NoEvents -> (_widget, [])
                          Events evts -> (_widget, evts)
                          EventsState evts wdt -> (wdt, evts)
  (newChildren, childEvents) = foldl (\(ws, evs) widgetNode -> case handleEvents widgetNode systemEvents of
                                        (ws2, evs2) -> (ws SQ.|> ws2, evs SQ.>< evs2)) (SQ.empty, SQ.fromList events) children
  newNode = Node (wn { _widget = newWidget }) newChildren

handleRender :: (Monad m) => Renderer m -> Timestamp -> WidgetNode e m -> m ()
handleRender renderer ts (WidgetNode _ Widget{..} viewport _ calculatedStyle _) = _render renderer ts viewport calculatedStyle

resizeUI :: (Monad m) => Renderer m -> Rect -> Tree (WidgetNode e m) -> m (Tree (WidgetNode e m))
resizeUI renderer assignedRect widgetNode = do
  preferredSizes <- buildPreferredSizes renderer widgetNode
  resizeNode renderer assignedRect preferredSizes widgetNode

buildPreferredSizes :: (Monad m) => Renderer m -> Tree (WidgetNode e m) -> m (Tree Size)
buildPreferredSizes renderer (Node (WidgetNode {..}) children) = do
  childrenSizes <- mapM (buildPreferredSizes renderer) children
  size <- _preferredSize _widget renderer _style (seqToList childrenSizes)

  return $ Node size childrenSizes

resizeNode :: (Monad m) => Renderer m -> Rect -> Tree Size -> Tree (WidgetNode e m) -> m (Tree (WidgetNode e m))
resizeNode renderer assignedRect (Node _ {--widgetSize--} childrenSizes) (Node widgetNode childrenWns) = do
  let widget = _widget widgetNode
  let style = _style widgetNode
  let updatedNode = widgetNode { _viewport = assignedRect }
  let assignedRects = (_resizeChildren widget) assignedRect style (seqToList childrenSizes)
  let childrenPair = SQ.zip3 childrenSizes childrenWns (SQ.fromList assignedRects)
  let childResize = \(size, node, rect) -> resizeNode renderer rect size node

  newChildren <- mapM childResize childrenPair

  return (Node updatedNode newChildren)

--    _preferredSize :: Renderer m -> Style -> [Size] -> m Size,
--    _resizeChildren :: Rect -> Style -> [Size] -> [Rect],

{--
resizeNodeOld :: (Monad m) => Rect -> Tree (WidgetNode e m) -> Tree (WidgetNode e m)
resizeNodeOld !rt@(Rect x y w h) (Node (WidgetNode widgetKey widget _ style calculatedStyle calculatedSize) children) = newNode where
  newNode = Node (WidgetNode widgetKey widget rt style calculatedStyle calculatedSize) newChildren
  rows = floor $ sqrt $ fromIntegral (length children)
  cols = (length children) `div` rows
  iw = w / fromIntegral cols
  ih = h / fromIntegral rows
  newChildren = fmap (\(w, i) -> resizeNodeOld (newRt i) w) widgetIdxPairs
  widgetIdxPairs = SQ.zip children (SQ.fromList [0..(length children)])
  newRt i = Rect (x + fromIntegral (i `mod` cols) * iw) (y + fromIntegral (i `div` cols) * ih) iw ih
--}
