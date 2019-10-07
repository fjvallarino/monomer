{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Core where

import Control.Monad
import Control.Monad.State

import Data.Default
import Data.Maybe
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

isKeyPressed :: SystemEvent -> KeyCode -> Bool
isKeyPressed (KeyAction keyCode KeyPressed) keyCodeChecked = keyCode == keyCodeChecked
isKeyPressed _ _ = False

data WidgetEventResult s e m = WidgetEventResult {
  _eventResultStop :: Bool,
  _eventResultUserEvents :: [e],
  _eventResultNewWidget :: Maybe (Widget s e m)
}

instance Semigroup (WidgetEventResult s e m) where
  wer1 <> wer2 = WidgetEventResult {
    _eventResultStop = _eventResultStop wer1 || _eventResultStop wer2,
    _eventResultUserEvents = _eventResultUserEvents wer1 ++ _eventResultUserEvents wer2,
    _eventResultNewWidget = _eventResultNewWidget wer2
  }

instance Monoid (WidgetEventResult s e m) where
  mempty = WidgetEventResult False [] Nothing

mkWidgetEventResult :: Bool -> [e] -> (Widget s e m) -> Maybe (WidgetEventResult s e m)
mkWidgetEventResult stop userEvents newWidget = Just $ WidgetEventResult stop userEvents (Just newWidget)
{--
data EventResult s e m = NoEvents | Events [e] | EventsState [e] (Widget s e m)

instance Semigroup (EventResult s e m) where
  (<>) NoEvents er2 = er2
  (<>) er1 NoEvents = er1
  (<>) (Events e1) (Events e2) = Events (e1 ++ e2)
  (<>) (EventsState e1 s1) (Events e2) = EventsState (e1 ++ e2) s1
  (<>) (Events e1) (EventsState e2 s2) = EventsState (e1 ++ e2) s2
  (<>) (EventsState e1 s1) (EventsState e2 s2) = EventsState (e1 ++ e2) s2
--}

newtype WidgetType = WidgetType String deriving Eq
newtype WidgetKey = WidgetKey String deriving Eq

instance IsString WidgetType where
  fromString string = WidgetType string

instance IsString WidgetKey where
  fromString string = WidgetKey string

newtype NodePath = NodePath [Int]
data NodeInfo = NodeInfo WidgetType (Maybe WidgetKey)

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

isFocusable :: (MonadState s m) => WidgetNode s e m -> Bool
isFocusable (WidgetNode { _widgetNodeWidget = Widget{..}, ..}) = _widgetNodeEnabled && _widgetFocusable

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
    _widgetHandleEvent :: Rect -> Bool -> SystemEvent -> Maybe (WidgetEventResult s e m),
    -- | Minimum size desired by the widget
    --
    -- Style options
    -- Preferred size for each of the children widgets
    -- Renderer (mainly for text sizing functions)
    --
    -- Returns: the minimum size desired by the widget
    _widgetPreferredSize :: Renderer m -> Style -> [Size] -> m Size,
    -- | Resizes the children of this widget
    --
    -- Region assigned to the widget
    -- Style options
    -- Preferred size for each of the children widgets
    --
    -- Returns: the size assigned to each of the children
    _widgetResizeChildren :: Rect -> Style -> [Size] -> [Rect],
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
    _widgetRender :: Renderer m -> Rect -> Style -> Enabled -> Focused -> Timestamp -> m ()
  }

-- | Complementary information to a Widget, forming a node in the view tree
--
-- Type variables:
-- * n: Identifier for a node
data WidgetNode s e m =
  (MonadState s m) => WidgetNode {
    -- | Key/Identifier of the widget. If provided, it needs to be unique in the same hierarchy level (not globally)
    _widgetNodeKey :: Maybe WidgetKey,
    _widgetNodeWidget :: Widget s e m,
    _widgetNodeEnabled :: Enabled,
    _widgetNodeViewport :: Rect,
    _widgetNodeStyle :: Style,
    _widgetNodeComputedStyle :: Style,
    _widgetNodeComputedSize :: Size
  }

key :: (MonadState s m) => WidgetKey -> WidgetNode s e m -> WidgetNode s e m
key key wn = wn { _widgetNodeKey = Just key }

style :: (MonadState s m) => Tree (WidgetNode s e m) -> Style -> Tree (WidgetNode s e m)
style (Node value children) newStyle = Node (value { _widgetNodeStyle = newStyle }) children

children :: (MonadState s m) => Tree (WidgetNode s e m) -> [Tree (WidgetNode s e m)] -> Tree (WidgetNode s e m)
children (Node value _) newChildren = fromList value newChildren

cascadeStyle :: (MonadState s m) => Style -> Tree (WidgetNode s e m) -> Tree (WidgetNode s e m)
cascadeStyle parentStyle (Node (wn@WidgetNode{..}) children) = newNode where
  newNode = Node (wn { _widgetNodeComputedStyle = newStyle }) newChildren
  newStyle = _widgetNodeStyle <> parentStyle
  newChildren = fmap (cascadeStyle newStyle) children

defaultWidgetNode :: (MonadState s m) => Widget s e m -> WidgetNode s e m
defaultWidgetNode widget = WidgetNode {
  _widgetNodeKey = Nothing,
  _widgetNodeWidget = widget,
  _widgetNodeEnabled = True,
  _widgetNodeViewport = def,
  _widgetNodeStyle = mempty,
  _widgetNodeComputedStyle = mempty,
  _widgetNodeComputedSize = def
}

singleWidget :: (MonadState s m) => Widget s e m -> Tree (WidgetNode s e m)
singleWidget widget = singleton (defaultWidgetNode widget)

parentWidget :: (MonadState s m) => Widget s e m -> [Tree (WidgetNode s e m)] -> Tree (WidgetNode s e m)
parentWidget widget = fromList (defaultWidgetNode widget)

emptyState :: Maybe ()
emptyState = Nothing

widgetMatches :: (MonadState s m) => WidgetNode s e m -> WidgetNode s e m -> Bool
widgetMatches wn1 wn2 = _widgetType (_widgetNodeWidget wn1) == _widgetType (_widgetNodeWidget wn2) && _widgetNodeKey wn1 == _widgetNodeKey wn2

mergeTrees :: (MonadState s m) => Tree (WidgetNode s e m) -> Tree (WidgetNode s e m) -> Tree (WidgetNode s e m)
mergeTrees node1@(Node widget1 seq1) (Node widget2 seq2) = newNode where
  matches = widgetMatches widget1 widget2
  newNode = if | matches -> Node widget2 newChildren
               | otherwise -> node1
  newChildren = mergedChildren SQ.>< addedChildren
  mergedChildren = fmap mergeChild (SQ.zip seq1 seq2)
  addedChildren = SQ.drop (SQ.length seq2) seq1
  mergeChild = \(c1, c2) -> mergeTrees c1 c2

handleWidgetEvents :: (MonadState s m) => Widget s e m -> Rect -> Focused -> SystemEvent -> Maybe (WidgetEventResult s e m)
handleWidgetEvents (Widget {..}) viewport focused systemEvent = _widgetHandleEvent viewport focused systemEvent

handleEvent :: (MonadState s m) => Path -> Tree (WidgetNode s e m) -> Path -> SystemEvent -> (Bool, Tree (WidgetNode s e m), SQ.Seq e)
handleEvent focusedPath (Node (wn@WidgetNode {..}) children) currentPath systemEvent = (newStop, newNode, childEvents) where
  (stop, userEvents, newWidget) = case handleWidgetEvents _widgetNodeWidget _widgetNodeViewport (focusedPath == currentPath) systemEvent of
                          Nothing -> (False, [], _widgetNodeWidget)
                          Just (WidgetEventResult {..}) -> (_eventResultStop, _eventResultUserEvents, fromMaybe _widgetNodeWidget _eventResultNewWidget)
  (newStop, newChildren, childEvents, _) = foldl (\(st, ws, evs, idx) widgetNode -> case handleEvent focusedPath widgetNode (idx : currentPath) systemEvent of
                                        (st2, ws2, evs2) -> (st || st2, ws SQ.|> ws2, evs SQ.>< evs2, idx + 1)) (stop, SQ.empty, SQ.fromList userEvents, 0) children
  newNode = Node (wn { _widgetNodeWidget = newWidget }) newChildren

handleFocusedEvent :: (MonadState s m) => Path -> Tree (WidgetNode s e m) -> SystemEvent -> (Bool, SQ.Seq e, Maybe (Tree (WidgetNode s e m)))
handleFocusedEvent [] widgetNode _ = (False, SQ.empty, Nothing)
handleFocusedEvent (p:ps) treeNode@(Node wn@WidgetNode{..} children) systemEvent = (stopPropagation, userEvents, newTreeNode) where
  (stopPropagation, userEvents, newTreeNode) = case spChild of
    True -> (spChild, ueChild, newNode1)
    False -> (sp, ueChild SQ.>< ue, newNode2)
  (spChild, ueChild, tnChild) = case (SQ.lookup p children) of
    Nothing -> (False, SQ.empty, Nothing)
    Just childTreeNode -> handleFocusedEvent ps childTreeNode systemEvent
  (sp, ue, tn) = case handleWidgetEvents _widgetNodeWidget _widgetNodeViewport (null ps) systemEvent of
    Nothing -> (False, SQ.empty, Nothing)
    Just (WidgetEventResult sp2 ue2 widget) -> (sp2, SQ.fromList ue2, if isNothing widget then Nothing else Just (Node (wn { _widgetNodeWidget = fromJust widget }) children))
  newNode1 = case tnChild of
    Nothing -> Nothing
    Just wnChild -> Just $ Node wn (SQ.update p wnChild children)
  newNode2 = case (tn, tnChild) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just cn) -> newNode1
    (Just pn, Nothing) -> tn
    (Just (Node wn _), Just tnChild) -> Just $ Node wn (SQ.update p tnChild children)

--    (stopChild, widgetNodeChild, userEventsChild) = handleFocusedEvent ps widgetNode event
--    handleWidgetEvents _widgetNodeWidget _widgetNodeViewport (focusedPath == currentPath) systemEvent
--
--      (stopPropagation, Nothing, userEvents) ->
--      Just a -> (stopPropagation, newNode, userEvents) where

handleRender :: (MonadState s m) => Renderer m -> WidgetNode s e m -> Focused -> Timestamp -> m ()
handleRender renderer (WidgetNode { _widgetNodeWidget = Widget{..}, .. }) focused ts = _widgetRender renderer _widgetNodeViewport _widgetNodeComputedStyle _widgetNodeEnabled focused ts

resizeUI :: (MonadState s m) => Renderer m -> Rect -> Tree (WidgetNode s e m) -> m (Tree (WidgetNode s e m))
resizeUI renderer assignedRect widgetNode = do
  preferredSizes <- buildPreferredSizes renderer widgetNode
  resizeNode renderer assignedRect preferredSizes widgetNode

buildPreferredSizes :: (MonadState s m) => Renderer m -> Tree (WidgetNode s e m) -> m (Tree Size)
buildPreferredSizes renderer (Node (WidgetNode {..}) children) = do
  childrenSizes <- mapM (buildPreferredSizes renderer) children
  size <- _widgetPreferredSize _widgetNodeWidget renderer _widgetNodeStyle (seqToList childrenSizes)

  return $ Node size childrenSizes

resizeNode :: (MonadState s m) => Renderer m -> Rect -> Tree Size -> Tree (WidgetNode s e m) -> m (Tree (WidgetNode s e m))
resizeNode renderer assignedRect (Node _ {--widgetSize--} childrenSizes) (Node widgetNode childrenWns) = do
  let widget = _widgetNodeWidget widgetNode
  let style = _widgetNodeStyle widgetNode
  let updatedNode = widgetNode { _widgetNodeViewport = assignedRect }
  let assignedRects = (_widgetResizeChildren widget) assignedRect style (seqToList childrenSizes)
  let childrenPair = SQ.zip3 childrenSizes childrenWns (SQ.fromList assignedRects)
  let childResize = \(size, node, rect) -> resizeNode renderer rect size node

  newChildren <- mapM childResize childrenPair

  return (Node updatedNode newChildren)
