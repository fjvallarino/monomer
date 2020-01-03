{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module GUI.Common.Core where

import Control.Concurrent.Async

import Control.Monad
import Control.Monad.State

import Data.Default
import Data.Dynamic
import Data.Maybe
import Data.String
import Data.Typeable (cast, Typeable)

import GUI.Common.Event
import GUI.Common.Style
import GUI.Common.Types
import GUI.Common.Util
import GUI.Data.Tree

import GHC.Generics

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Sequence as SQ

type Timestamp = Int

type WidgetNode s e m = Tree (WidgetInstance s e m)
type WidgetChildren s e m = SQ.Seq (WidgetNode s e m)

newtype WidgetType = WidgetType String deriving (Eq, Show)
newtype WidgetKey = WidgetKey String deriving Eq

instance IsString WidgetType where
  fromString string = WidgetType string

instance IsString WidgetKey where
  fromString string = WidgetKey string

data WidgetState = forall i . (Typeable i, Generic i) => WidgetState i

data SizeReq = SizeReq {
  _srSize :: Size,
  _srPolicyWidth :: SizePolicy,
  _srPolicyHeight :: SizePolicy,
  _srVisible :: Bool
} deriving (Show, Eq)

data WidgetEventResult s e m = WidgetEventResult {
  _eventResultRequest :: [EventRequest],
  _eventResultUserEvents :: [e],
  _eventResultNewWidget :: Maybe (Widget s e m)
}

data WidgetResizeResult s e m = WidgetResizeResult {
  _resizeResultRenderAreas :: [Rect],
  _resizeResultViewports :: [Rect],
  _resizeResultWidget :: Maybe (Widget s e m)
}

data WidgetTask = forall a . Typeable a => WidgetTask {
  widgetTaskPath :: Path,
  widgetTask :: Async a
}

data ChildEventResult s e m = ChildEventResult {
  cerIgnoreParentEvents :: Bool,
  cerEventRequests :: [(Path, EventRequest)],
  cerUserEvents :: SQ.Seq e,
  cerNewTreeNode :: Maybe (WidgetNode s e m)
}

data Widget s e m =
  (MonadState s m) => Widget {
    -- | Type of the widget
    _widgetType :: WidgetType,
    -- | Indicates whether the widget can receive focus
    _widgetFocusable :: Bool,
    -- | Provides the previous internal state to the new widget, which can choose to ignore it or update itself
    _widgetRestoreState :: WidgetState -> Maybe (Widget s e m),
    -- | Returns the current internal state, which can later be used to restore the widget
    _widgetSaveState :: Maybe WidgetState,
    -- | Handles an event
    --
    -- Region assigned to the widget
    -- Event to handle
    --
    -- Returns: the list of generated events and, maybe, a new version of the widget if internal state changed
    _widgetHandleEvent :: Rect -> SystemEvent -> Maybe (WidgetEventResult s e m),
    -- | Handles an custom asynchronous event
    --
    -- Result of asynchronous computation
    --
    -- Returns: the list of generated events and, maybe, a new version of the widget if internal state changed
    _widgetHandleCustom :: forall i . Typeable i => i -> Maybe (WidgetEventResult s e m),
    -- | Minimum size desired by the widget
    --
    -- Style options
    -- Preferred size for each of the children widgets
    -- Renderer (mainly for text sizing functions)
    --
    -- Returns: the minimum size desired by the widget
    _widgetPreferredSize :: Renderer m -> Style -> [SizeReq] -> m SizeReq,
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
    -- Style options
    -- renderArea: The area of the screen where the widget can draw
    -- viewport: The visible area of the screen assigned to the widget
    -- Indicates if the widget (and its children) are enabled
    -- Indicates if the widget has focus
    -- The current time in milliseconds
    --
    -- Returns: unit
    _widgetRender :: Renderer m -> WidgetInstance s e m -> WidgetChildren s e m -> Timestamp -> m ()
  }

-- | Complementary information to a Widget, forming a node in the view tree
--
-- Type variables:
-- * n: Identifier for a node
data WidgetInstance s e m =
  (MonadState s m) => WidgetInstance {
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
    --_widgetInstanceElementStyle :: Style
  }

data GUIContext app = GUIContext {
  _appContext :: app,
  _windowSize :: Rect,
  _useHiDPI :: Bool,
  _devicePixelRate :: Double,
  _focusRing :: [Path],
  _widgetTasks :: [WidgetTask]
}

initGUIContext :: app -> Rect -> Bool -> Double -> GUIContext app
initGUIContext app winSize useHiDPI devicePixelRate = GUIContext {
  _appContext = app,
  _windowSize = winSize,
  _useHiDPI = useHiDPI,
  _devicePixelRate = devicePixelRate,
  _focusRing = [],
  _widgetTasks = []
}

sizeReq :: Size -> SizePolicy -> SizePolicy -> SizeReq
sizeReq size policyWidth policyHeight = SizeReq size policyWidth policyHeight True

resultEvents :: [e] -> Maybe (WidgetEventResult s e m)
resultEvents userEvents = Just $ WidgetEventResult [] userEvents Nothing

resultEventsWidget :: [e] -> (Widget s e m) -> Maybe (WidgetEventResult s e m)
resultEventsWidget userEvents newWidget = Just $ WidgetEventResult [] userEvents (Just newWidget)

resultReqsEventsWidget :: [EventRequest] -> [e] -> (Widget s e m) -> Maybe (WidgetEventResult s e m)
resultReqsEventsWidget requests userEvents newWidget = Just $ WidgetEventResult requests userEvents (Just newWidget)

isFocusable :: (MonadState s m) => WidgetInstance s e m -> Bool
isFocusable (WidgetInstance { _widgetInstanceWidget = Widget{..}, ..}) = _widgetInstanceEnabled  && _widgetFocusable

defaultCustomHandler :: a -> Maybe (WidgetEventResult s e m)
defaultCustomHandler _ = Nothing

defaultRestoreState :: WidgetState -> Maybe (Widget s e m)
defaultRestoreState _ = Nothing

defaultSaveState :: Maybe WidgetState
defaultSaveState = Nothing

makeState :: (Typeable i, Generic i) => i -> Maybe WidgetState
makeState state = Just (WidgetState state)

useState ::  (Typeable i, Generic i) => WidgetState -> Maybe i
useState (WidgetState state) = cast state

key :: (MonadState s m) => WidgetKey -> WidgetInstance s e m -> WidgetInstance s e m
key key wn = wn { _widgetInstanceKey = Just key }

style :: (MonadState s m) => WidgetNode s e m -> Style -> WidgetNode s e m
style (Node value children) newStyle = Node (value { _widgetInstanceStyle = newStyle }) children

visible :: (MonadState s m) => WidgetNode s e m -> Bool -> WidgetNode s e m
visible (Node value children) visibility = Node (value { _widgetInstanceVisible = visibility }) children

children :: (MonadState s m) => WidgetNode s e m -> [WidgetNode s e m] -> WidgetNode s e m
children (Node value _) newChildren = fromList value newChildren

cascadeStyle :: (MonadState s m) => Style -> WidgetNode s e m -> WidgetNode s e m
cascadeStyle parentStyle (Node (wn@WidgetInstance{..}) children) = newNode where
  newNode = Node (wn { _widgetInstanceStyle = newStyle }) newChildren
  newStyle = _widgetInstanceStyle <> parentStyle
  newChildren = fmap (cascadeStyle newStyle) children

defaultWidgetInstance :: (MonadState s m) => Widget s e m -> WidgetInstance s e m
defaultWidgetInstance widget = WidgetInstance {
  _widgetInstanceKey = Nothing,
  _widgetInstanceWidget = widget,
  _widgetInstanceEnabled = True,
  _widgetInstanceVisible = True,
  _widgetInstanceFocused = False,
  _widgetInstanceViewport = def,
  _widgetInstanceRenderArea = def,
  _widgetInstanceStyle = mempty
}

singleWidget :: (MonadState s m) => Widget s e m -> WidgetNode s e m
singleWidget widget = singleton (defaultWidgetInstance widget)

parentWidget :: (MonadState s m) => Widget s e m -> [WidgetNode s e m] -> WidgetNode s e m
parentWidget widget = fromList (defaultWidgetInstance widget)

widgetMatches :: (MonadState s m) => WidgetInstance s e m -> WidgetInstance s e m -> Bool
widgetMatches wn1 wn2 = _widgetType (_widgetInstanceWidget wn1) == _widgetType (_widgetInstanceWidget wn2) && _widgetInstanceKey wn1 == _widgetInstanceKey wn2

mergeTrees :: (MonadState s m) => WidgetNode s e m -> WidgetNode s e m -> WidgetNode s e m
mergeTrees node1@(Node candidateInstance candidateChildren) (Node oldInstance oldChildren) = newNode where
  matches = widgetMatches candidateInstance oldInstance
  newNode = if | matches -> Node newInstance newChildren
               | otherwise -> node1
  oldWidget = _widgetInstanceWidget oldInstance
  candidateWidget = _widgetInstanceWidget candidateInstance
  newWidget = case _widgetSaveState oldWidget of
    Just st -> fromMaybe candidateWidget (_widgetRestoreState candidateWidget st)
    Nothing -> candidateWidget
  newInstance = candidateInstance { _widgetInstanceWidget = newWidget }
  newChildren = mergedChildren SQ.>< addedChildren
  mergedChildren = fmap mergeChild (SQ.zip candidateChildren oldChildren)
  addedChildren = SQ.drop (SQ.length oldChildren) candidateChildren
  mergeChild = \(c1, c2) -> mergeTrees c1 c2

handleWidgetEvents :: (MonadState s m) => Widget s e m -> Rect -> SystemEvent -> Maybe (WidgetEventResult s e m)
handleWidgetEvents (Widget {..}) viewport systemEvent = _widgetHandleEvent viewport systemEvent

handleChildEvent :: (MonadState s m) => (a -> SQ.Seq (WidgetNode s e m) -> (a, Maybe Int)) -> a -> Path -> WidgetNode s e m -> SystemEvent -> ChildEventResult s e m
handleChildEvent selectorFn selector path treeNode@(Node wn@WidgetInstance{..} children) systemEvent = ChildEventResult ignoreParentEvents eventRequests userEvents newTreeNode where
  (ignoreParentEvents, eventRequests, userEvents, newTreeNode) = case (ice, ipeChild) of
    (True, _) -> (ipe, er, ue, newNode1)
    (_, True) -> (ipeChild, erChild, ueChild, newNode1)
    (_, False) -> (ipe, erChild ++ er, ueChild SQ.>< ue, newNode2)
  -- Children widgets
  (ipeChild, erChild, ueChild, tnChild, tnChildIdx) = case selectorFn selector children of
    (_, Nothing) -> (False, [], SQ.empty, Nothing, 0)
    (newSelector, Just idx) -> (ipe2, er2, ue2, tn2, idx) where
      (ChildEventResult ipe2 er2 ue2 tn2) = handleChildEvent selectorFn newSelector widgetPath (SQ.index children idx) systemEvent
      widgetPath = reverse (idx:path)
  -- Current widget
  (ice, ipe, er, ue, tn) = case handleWidgetEvents _widgetInstanceWidget _widgetInstanceRenderArea systemEvent of
    Nothing -> (False, False, [], SQ.empty, Nothing)
    Just (WidgetEventResult er2 ue2 widget) -> (ice, ipe, pathEvents, SQ.fromList ue2, updatedNode) where
      ice = isJust $ L.find isIgnoreChildrenEvents er2
      ipe = isJust $ L.find isIgnoreParentEvents er2
      pathEvents = fmap (path,) er2
      updatedNode = if isNothing widget
                      then Nothing
                      else Just $ Node (wn { _widgetInstanceWidget = fromJust widget }) children
  newNode1 = case tnChild of
    Nothing -> Nothing
    Just wnChild -> Just $ Node wn (SQ.update tnChildIdx wnChild children)
  newNode2 = case (tn, tnChild) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just cn) -> newNode1
    (Just pn, Nothing) -> tn
    (Just (Node wn _), Just tnChild) -> Just $ Node wn (SQ.update tnChildIdx tnChild children)

handleEventFromPath :: (MonadState s m) => Path -> WidgetNode s e m -> SystemEvent -> ChildEventResult s e m
handleEventFromPath path widgetInstance systemEvent = handleChildEvent pathSelector path [] widgetInstance systemEvent where
  pathSelector [] _ = ([], Nothing)
  pathSelector (p:ps) children
    | length children > p = (ps, Just p)
    | otherwise = ([], Nothing)

handleEventFromPoint :: (MonadState s m) => Point -> WidgetNode s e m -> SystemEvent -> ChildEventResult s e m
handleEventFromPoint cursorPos widgetInstance systemEvent = handleChildEvent rectSelector cursorPos [] widgetInstance systemEvent where
  rectSelector point children = (point, SQ.lookup 0 inRectList) where
    inRectList = fmap snd $ SQ.filter inNodeRect childrenPair
    inNodeRect = \(Node (WidgetInstance {..}) _, _) -> inRect _widgetInstanceViewport point
    childrenPair = SQ.zip children (SQ.fromList [0..(length children - 1)])

handleCustomCommand :: (MonadState s m, Typeable i) => Path -> WidgetNode s e m -> i -> ChildEventResult s e m
handleCustomCommand path treeNode customData = case GUI.Data.Tree.lookup path treeNode of
  Just (WidgetInstance{ _widgetInstanceWidget = Widget{..}, ..}) ->
    case _widgetHandleCustom customData of
      Just (WidgetEventResult er ue tn) -> ChildEventResult False (fmap (path,) er) (SQ.fromList ue) Nothing
      Nothing -> ChildEventResult False [] SQ.Empty Nothing
  Nothing -> ChildEventResult False [] SQ.Empty Nothing

handleRender :: (MonadState s m) => Renderer m -> WidgetNode s e m -> Timestamp -> m ()
handleRender renderer (Node (widgetInstance@WidgetInstance { _widgetInstanceWidget = Widget{..}, .. }) children) ts = do
  when _widgetInstanceVisible $
    _widgetRender renderer widgetInstance children ts

handleRenderChildren :: (MonadState s m) => Renderer m -> WidgetChildren s e m -> Timestamp -> m ()
handleRenderChildren renderer children ts = do
  mapM_ (\treeNode -> handleRender renderer treeNode ts) children

updateWidgetInstance :: Path -> WidgetNode s e m -> (WidgetInstance s e m -> WidgetInstance s e m) -> Maybe (WidgetNode s e m)
updateWidgetInstance path root updateFn = updateNode path root (\(Node widgetInstance children) -> Node (updateFn widgetInstance) children)

setFocusedStatus :: Path -> Bool -> WidgetNode s e m -> WidgetNode s e m
setFocusedStatus path focused root = case updateWidgetInstance path root updateFn of
    Just newRoot -> newRoot
    Nothing -> root
  where
    updateFn wn@(WidgetInstance {..}) = wn {
      _widgetInstanceFocused = focused
    }

resizeUI :: (MonadState s m) => Renderer m -> Rect -> WidgetNode s e m -> m (WidgetNode s e m)
resizeUI renderer assignedRect widgetInstance = do
  preferredSizes <- buildPreferredSizes renderer True widgetInstance
  resizeNode renderer assignedRect assignedRect preferredSizes widgetInstance

buildPreferredSizes :: (MonadState s m) => Renderer m -> Bool -> WidgetNode s e m -> m (Tree SizeReq)
buildPreferredSizes renderer parentVisible (Node (WidgetInstance {..}) children) = do
  let isVisible = parentVisible && _widgetInstanceVisible

  childrenSizes <- mapM (buildPreferredSizes renderer isVisible) children
  size <- _widgetPreferredSize _widgetInstanceWidget renderer _widgetInstanceStyle (seqToList childrenSizes)

  return $ Node (size { _srVisible = isVisible}) childrenSizes

resizeNode :: (MonadState s m) => Renderer m -> Rect -> Rect -> Tree SizeReq -> WidgetNode s e m -> m (WidgetNode s e m)
resizeNode renderer viewport renderArea (Node _ childrenSizes) (Node widgetInstance childrenWns) = do
    newChildren <- mapM childResize childrenPair

    return (Node updatedNode newChildren)
  where
    widget = _widgetInstanceWidget widgetInstance
    style = _widgetInstanceStyle widgetInstance
    (WidgetResizeResult viewports renderAreas newWidget) = case (_widgetResizeChildren widget) viewport renderArea style (seqToList childrenSizes) of
      Nothing -> WidgetResizeResult [] [] Nothing
      Just wrr -> wrr
    updatedNode = widgetInstance {
      _widgetInstanceViewport = viewport,
      _widgetInstanceRenderArea = renderArea,
      _widgetInstanceWidget = fromMaybe widget newWidget
    }
    childrenPair = SQ.zip4 childrenSizes childrenWns (SQ.fromList viewports) (SQ.fromList renderAreas)
    childResize = \(size, node, viewport, renderArea) -> resizeNode renderer viewport renderArea size node
