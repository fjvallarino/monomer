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
    -- | Updates user state used by widget with internal state
    _widgetUpdateUserState :: m (),
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
  _inputStatus :: InputStatus,
  _focusRing :: [Path],
  _latestHover :: Maybe Path,
  _widgetTasks :: [WidgetTask]
}

initGUIContext :: app -> Rect -> Bool -> Double -> GUIContext app
initGUIContext app winSize useHiDPI devicePixelRate = GUIContext {
  _appContext = app,
  _windowSize = winSize,
  _useHiDPI = useHiDPI,
  _devicePixelRate = devicePixelRate,
  _inputStatus = defInputStatus,
  _focusRing = [],
  _latestHover = Nothing,
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
isFocusable (WidgetInstance { _widgetInstanceWidget = Widget{..}, ..}) = _widgetInstanceVisible && _widgetInstanceEnabled && _widgetFocusable

defaultCustomHandler :: a -> Maybe (WidgetEventResult s e m)
defaultCustomHandler _ = Nothing

defaultRestoreState :: WidgetState -> Maybe (Widget s e m)
defaultRestoreState _ = Nothing

defaultSaveState :: Maybe WidgetState
defaultSaveState = Nothing

defaultUpdateUserState :: (MonadState s m) => m ()
defaultUpdateUserState = return ()

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
  newStyle = Style {
    _fixedWidth = _fixedWidth _widgetInstanceStyle,
    _fixedHeight = _fixedHeight _widgetInstanceStyle,
    _padding = _padding parentStyle <> _padding _widgetInstanceStyle,
    _bgRadius = _bgRadius parentStyle <> _bgRadius _widgetInstanceStyle,
    _bgColor = firstJust (_bgColor parentStyle) (_bgColor _widgetInstanceStyle),
    _border = _border parentStyle <> _border _widgetInstanceStyle,
    _textStyle = _textStyle parentStyle <> _textStyle _widgetInstanceStyle
  }
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

type ChildrenSelector s e m a = a -> SQ.Seq (WidgetNode s e m) -> (a, Maybe Int)

data EventsParent s e m = EventsParent {
  epIgnoreChildrenEvents :: Bool,
  epIgnoreParentEvents :: Bool,
  epEventRequests :: [(Path, EventRequest)],
  epUserEvents :: SQ.Seq e,
  epUpdatedNode :: Maybe (Tree (WidgetInstance s e m))
}

data EventsChildren s e m = EventsChildren {
  ecIgnoreParentEvents :: Bool,
  ecEventRequests :: [(Path, EventRequest)],
  ecUserEvents :: SQ.Seq e,
  ecUpdatedNode :: Maybe (Tree (WidgetInstance s e m)),
  ecNodePosition :: Int
}

handleWidgetEvents :: (MonadState s m) => Widget s e m -> Rect -> SystemEvent -> Maybe (WidgetEventResult s e m)
handleWidgetEvents (Widget {..}) viewport systemEvent = _widgetHandleEvent viewport systemEvent

handleChildEvent :: (MonadState s m) => ChildrenSelector s e m a -> a -> Path -> WidgetNode s e m -> SystemEvent -> ChildEventResult s e m
handleChildEvent selectorFn selector path (Node widgetInstance children) systemEvent =
  createUpdatedNode widgetInstance children eventsParent eventsChildren
    where
      eventsParent = handleEventsParent path widgetInstance children systemEvent
      eventsChildren = handleEventsChildren selectorFn selector path children systemEvent

createUpdatedNode :: (MonadState s m) => WidgetInstance s e m -> WidgetChildren s e m -> EventsParent s e m -> EventsChildren s e m -> ChildEventResult s e m
createUpdatedNode widgetInstance children (EventsParent{..}) (EventsChildren{..}) = updatedNode where
  updatedNode = case (epIgnoreChildrenEvents, ecIgnoreParentEvents) of
    (True, _) -> ChildEventResult epIgnoreParentEvents epEventRequests epUserEvents epUpdatedNode
    (_, True) -> ChildEventResult ecIgnoreParentEvents ecEventRequests ecUserEvents newNodeOldParent
    (_, False) -> ChildEventResult epIgnoreParentEvents (ecEventRequests ++ epEventRequests) (ecUserEvents SQ.>< epUserEvents) newNodeMixed
  newNodeOldParent = createNewNodeOldParent widgetInstance children ecNodePosition ecUpdatedNode
  newNodeMixed = createNewNodeMixed widgetInstance children ecNodePosition epUpdatedNode ecUpdatedNode

handleEventsParent :: (MonadState s m) => Path -> WidgetInstance s e m -> WidgetChildren s e m -> SystemEvent -> EventsParent s e m
handleEventsParent path (wi@WidgetInstance{..}) children systemEvent = case handleWidgetEvents _widgetInstanceWidget _widgetInstanceRenderArea systemEvent of
  Nothing -> EventsParent False False [] SQ.empty Nothing
  Just (WidgetEventResult er ue widget) -> EventsParent {
      epIgnoreChildrenEvents = isJust $ L.find isIgnoreChildrenEvents er,
      epIgnoreParentEvents = isJust $ L.find isIgnoreParentEvents er,
      epEventRequests = fmap (path,) er,
      epUserEvents = SQ.fromList ue,
      epUpdatedNode = if isNothing widget
                        then Nothing
                        else Just $ Node (wi { _widgetInstanceWidget = fromJust widget }) children
    }

handleEventsChildren :: (MonadState s m) => ChildrenSelector s e m a -> a -> Path -> WidgetChildren s e m -> SystemEvent -> EventsChildren s e m
handleEventsChildren selectorFn selector path children systemEvent = case selectorFn selector children of
  (_, Nothing) -> EventsChildren False [] SQ.empty Nothing 0
  (newSelector, Just idx) -> EventsChildren {
      ecIgnoreParentEvents = ipe,
      ecEventRequests = er,
      ecUserEvents = ue,
      ecUpdatedNode = tn,
      ecNodePosition = idx
    }
    where
      (ChildEventResult ipe er ue tn) = handleChildEvent selectorFn newSelector widgetPath (SQ.index children idx) systemEvent
      widgetPath = (idx:path)

replaceChild :: (MonadState s m) => WidgetInstance s e m -> WidgetChildren s e m -> Int -> WidgetNode s e m -> Maybe (WidgetNode s e m)
replaceChild widgetInstance children childIdx newChild = Just $ Node widgetInstance (SQ.update childIdx newChild children)

createNewNodeOldParent :: (MonadState s m) => WidgetInstance s e m -> WidgetChildren s e m -> Int -> Maybe (WidgetNode s e m) -> Maybe (WidgetNode s e m)
createNewNodeOldParent widgetInstance children childIdx newChild = case newChild of
  Nothing -> Nothing
  Just tnChild -> replaceChild widgetInstance children childIdx tnChild

createNewNodeMixed :: (MonadState s m) => WidgetInstance s e m -> WidgetChildren s e m -> Int -> Maybe (WidgetNode s e m) -> Maybe (WidgetNode s e m) -> Maybe (WidgetNode s e m)
createNewNodeMixed oldParentWidgetInstance children childIdx newParent newChild = case (newParent, newChild) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just cn) -> replaceChild oldParentWidgetInstance children childIdx cn
  (Just _, Nothing) -> newParent
  (Just (Node newParentWidgetInstance _), Just newChildTreeNode) -> replaceChild newParentWidgetInstance children childIdx newChildTreeNode

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

handleUserUpdateState :: (MonadState s m) => Path -> WidgetNode s e m -> m ()
handleUserUpdateState path treeNode = case GUI.Data.Tree.lookup path treeNode of
  Just (WidgetInstance{ _widgetInstanceWidget = Widget{..}, ..}) -> _widgetUpdateUserState
  Nothing -> return ()

findPathFromPoint :: (MonadState s m) => Point -> WidgetNode s e m -> Path
findPathFromPoint p widgetInstance = path where
  path = case SQ.lookup 0 inRectList of
    Just (child, idx) -> idx : findPathFromPoint p child
    Nothing -> []
  children = nodeChildren widgetInstance
  inRectList = SQ.filter inNodeRect childrenPair
  inNodeRect = \(Node (WidgetInstance {..}) _, _) -> inRect _widgetInstanceViewport p
  childrenPair = SQ.zip children (SQ.fromList [0..(length children - 1)])

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
  sr <- _widgetPreferredSize _widgetInstanceWidget renderer _widgetInstanceStyle (seqToList childrenSizes)

  let updatedSr = updateSizeReq sr (_fixedWidth _widgetInstanceStyle) (_fixedHeight _widgetInstanceStyle)

  return $ Node (updatedSr { _srVisible = isVisible}) childrenSizes

updateSizeReq :: SizeReq -> Maybe Double -> Maybe Double -> SizeReq
updateSizeReq sr Nothing Nothing = sr
updateSizeReq sr (Just width) Nothing = sr {
  _srSize = Size width (_h . _srSize $ sr),
  _srPolicyWidth = StrictSize
}
updateSizeReq sr Nothing (Just height) = sr {
  _srSize = Size (_w . _srSize $ sr) height,
  _srPolicyHeight = StrictSize
}
updateSizeReq sr (Just width) (Just height) = sr {
  _srSize = Size width height,
  _srPolicyWidth = StrictSize,
  _srPolicyHeight = StrictSize
}

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
