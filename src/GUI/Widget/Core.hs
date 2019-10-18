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

import GUI.Common.Core
import GUI.Common.Style
import GUI.Data.Tree

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

data SystemEvent = Click Point Button ButtonState |
                   KeyAction KeyCode KeyMotion deriving (Show, Eq)

isKeyboardEvent :: SystemEvent -> Bool
isKeyboardEvent (KeyAction _ _) = True
isKeyboardEvent _ = False

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

widgetEventResult :: Bool -> [e] -> (Widget s e m) -> Maybe (WidgetEventResult s e m)
widgetEventResult stop userEvents newWidget = Just $ WidgetEventResult stop userEvents (Just newWidget)

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
isFocusable (WidgetNode { _widgetNodeWidget = Widget{..}, ..}) = (_widgetStatusEnabled _widgetNodeStatus)  && _widgetFocusable

data Widget s e m =
  (MonadState s m) => Widget {
    -- | Type of the widget
    _widgetType :: WidgetType,
    -- | Indicates whether the widget makes changes to the render context that needs to be restored AFTER children render
    _widgetModifiesContext :: Bool,
    -- | Indicates whether the widget can receive focus
    _widgetFocusable :: Bool,
    -- | Handles an event
    --
    -- Region assigned to the widget
    -- Event to handle
    --
    -- Returns: the list of generated events and, maybe, a new version of the widget if internal state changed
    _widgetHandleEvent :: Rect -> SystemEvent -> Maybe (WidgetEventResult s e m),
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
    _widgetRender :: Renderer m -> Rect -> Style -> WidgetStatus -> Timestamp -> m ()
  }

data WidgetStatus = WidgetStatus {
  _widgetStatusEnabled :: Bool,
  _widgetStatusFocused :: Bool
} deriving (Show, Eq)

instance Default WidgetStatus where
  def = WidgetStatus {
    _widgetStatusEnabled = True,
    _widgetStatusFocused = False
  }

-- *********
-- *********
-- *********
-- *********
-- ********* Think about scrolling. Most likely solution:
-- *********
-- ********* - ScrollableContainer (may use viewport clipping/scissoring)
-- ********* - render receives two Rect:
-- *********   - viewport: the actual piece of the screen where the widget can draw
-- *********   - visibleRegion: the piece of the total preferred area that can be shown (viewport may remain fixed while this one varies)
-- *********
-- *********
-- *********
-- *********


-- | Complementary information to a Widget, forming a node in the view tree
--
-- Type variables:
-- * n: Identifier for a node
data WidgetNode s e m =
  (MonadState s m) => WidgetNode {
    -- | Key/Identifier of the widget. If provided, it needs to be unique in the same hierarchy level (not globally)
    _widgetNodeKey :: Maybe WidgetKey,
    _widgetNodeWidget :: Widget s e m,
    _widgetNodeStatus :: WidgetStatus,
    -- The area of the screen where the widget can draw.
    _widgetNodeViewport :: Rect,
    -- The are of the requested viewport that is actually visible. Used for optimization purposes.
    _widgetNodeVisibleRegion :: Rect,
    _widgetNodeStyle :: Style,
    _widgetNodeComputedStyle :: Style
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
  _widgetNodeStatus = def,
  _widgetNodeViewport = def,
  _widgetNodeVisibleRegion = def,
  _widgetNodeStyle = mempty,
  _widgetNodeComputedStyle = mempty
}

singleWidget :: (MonadState s m) => Widget s e m -> Tree (WidgetNode s e m)
singleWidget widget = singleton (defaultWidgetNode widget)

parentWidget :: (MonadState s m) => Widget s e m -> [Tree (WidgetNode s e m)] -> Tree (WidgetNode s e m)
parentWidget widget = fromList (defaultWidgetNode widget)

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

handleWidgetEvents :: (MonadState s m) => Widget s e m -> Rect -> SystemEvent -> Maybe (WidgetEventResult s e m)
handleWidgetEvents (Widget {..}) viewport systemEvent = _widgetHandleEvent viewport systemEvent

handleChildEvent :: (MonadState s m) => (a -> SQ.Seq (Tree (WidgetNode s e m)) -> (a, Maybe Int)) -> a -> Tree (WidgetNode s e m) -> SystemEvent -> (Bool, SQ.Seq e, Maybe (Tree (WidgetNode s e m)))
handleChildEvent selectorFn selector treeNode@(Node wn@WidgetNode{..} children) systemEvent = (stopPropagation, userEvents, newTreeNode) where
  (stopPropagation, userEvents, newTreeNode) = case spChild of
    True -> (spChild, ueChild, newNode1)
    False -> (sp, ueChild SQ.>< ue, newNode2)
  (spChild, ueChild, tnChild, tnChildIdx) = case selectorFn selector children of
    (_, Nothing) -> (False, SQ.empty, Nothing, 0)
    (newSelector, Just idx) -> (sp2, ue2, tn2, idx) where
      (sp2, ue2, tn2) = handleChildEvent selectorFn newSelector (SQ.index children idx) systemEvent
  (sp, ue, tn) = case handleWidgetEvents _widgetNodeWidget _widgetNodeViewport systemEvent of
    Nothing -> (False, SQ.empty, Nothing)
    Just (WidgetEventResult sp2 ue2 widget) -> (sp2, SQ.fromList ue2, if isNothing widget then Nothing else Just (Node (wn { _widgetNodeWidget = fromJust widget }) children))
  newNode1 = case tnChild of
    Nothing -> Nothing
    Just wnChild -> Just $ Node wn (SQ.update tnChildIdx wnChild children)
  newNode2 = case (tn, tnChild) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just cn) -> newNode1
    (Just pn, Nothing) -> tn
    (Just (Node wn _), Just tnChild) -> Just $ Node wn (SQ.update tnChildIdx tnChild children)

handleEventFromPath :: (MonadState s m) => Path -> Tree (WidgetNode s e m) -> SystemEvent -> (Bool, SQ.Seq e, Maybe (Tree (WidgetNode s e m)))
handleEventFromPath path widgetNode systemEvent = handleChildEvent pathSelector path widgetNode systemEvent where
  pathSelector [] _ = ([], Nothing)
  pathSelector (p:ps) children
    | length children > p = (ps, Just p)
    | otherwise = ([], Nothing)

handleEventFromPoint :: (MonadState s m) => Point -> Tree (WidgetNode s e m) -> SystemEvent -> (Bool, SQ.Seq e, Maybe (Tree (WidgetNode s e m)))
handleEventFromPoint cursorPos widgetNode systemEvent = handleChildEvent rectSelector cursorPos widgetNode systemEvent where
  rectSelector point children = (point, SQ.lookup 0 inRectList) where
    inRectList = fmap snd $ SQ.filter inNodeRect childrenPair
    inNodeRect = \(Node (WidgetNode {..}) _, _) -> inRect _widgetNodeViewport point
    childrenPair = SQ.zip children (SQ.fromList [0..(length children - 1)])

handleRender :: (MonadState s m) => Renderer m -> Tree (WidgetNode s e m) -> Timestamp -> m ()
handleRender renderer (Node (WidgetNode { _widgetNodeWidget = Widget{..}, .. }) children) ts = do
  when (_widgetModifiesContext) $ saveContext renderer

  _widgetRender renderer _widgetNodeViewport _widgetNodeComputedStyle _widgetNodeStatus ts
  mapM_ (\treeNode -> handleRender renderer treeNode ts) children

  when (_widgetModifiesContext) $ restoreContext renderer

updateWidgetNode :: Path -> Tree (WidgetNode s e m) -> (WidgetNode s e m -> WidgetNode s e m) -> Tree (WidgetNode s e m)
updateWidgetNode path root updateFn = updateNode path root (\(Node widgetNode children) -> Node (updateFn widgetNode) children)

setFocusedStatus :: Path -> Bool -> Tree (WidgetNode s e m) -> Tree (WidgetNode s e m)
setFocusedStatus path focused root = updateWidgetNode path root updateFn where
  updateFn wn@(WidgetNode {..}) = wn {
    _widgetNodeStatus = _widgetNodeStatus {
      _widgetStatusFocused = focused
    }
  }

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
resizeNode renderer assignedRect (Node _ childrenSizes) (Node widgetNode childrenWns) = do
  let widget = _widgetNodeWidget widgetNode
  let style = _widgetNodeStyle widgetNode
  let updatedNode = widgetNode { _widgetNodeViewport = assignedRect }
  let assignedRects = (_widgetResizeChildren widget) assignedRect style (seqToList childrenSizes)
  let childrenPair = SQ.zip3 childrenSizes childrenWns (SQ.fromList assignedRects)
  let childResize = \(size, node, rect) -> resizeNode renderer rect size node

  newChildren <- mapM childResize childrenPair

  return (Node updatedNode newChildren)
