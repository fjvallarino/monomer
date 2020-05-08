{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Monomer.Common.Core where

import Control.Concurrent.Async

import Control.Monad
import Control.Monad.State

import Data.Default
import Data.Maybe
import Data.String
import Data.Typeable (cast, Typeable)

import Monomer.Common.Event
import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Data.Tree

import GHC.Generics

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Sequence as SQ

data UserTask e = UserTask {
  userTask :: Async e
}

type Timestamp = Int

type MonomerM s e m = (MonadState (MonomerContext s e) m, MonadIO m, Eq s)
data EventResponse s e = State s | StateEvent s e | Task s (IO (Maybe e))

type WidgetNode s e m = Tree (WidgetInstance s e m)
type WidgetChildren s e m = SQ.Seq (WidgetNode s e m)

type UIBuilder s e m = s -> WidgetNode s e m
type AppEventHandler s e = s -> e -> EventResponse s e

data MonomerApp s e m = MonomerApp {
  _uiBuilder :: UIBuilder s e m,
  _appEventHandler :: AppEventHandler s e
}

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
  _eventResultNewWidget :: Maybe (Widget s e m),
  _eventResultNewState :: s -> s
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
    --_widgetInstanceElementStyle :: Style
  }

data MonomerContext s e = MonomerContext {
  _appContext :: s,
  _windowSize :: Rect,
  _useHiDPI :: Bool,
  _devicePixelRate :: Double,
  _inputStatus :: InputStatus,
  _focusRing :: [Path],
  _latestHover :: Maybe Path,
  _userTasks :: [UserTask (Maybe e)],
  _widgetTasks :: [WidgetTask]
}

initMonomerContext :: s -> Rect -> Bool -> Double -> MonomerContext s e
initMonomerContext app winSize useHiDPI devicePixelRate = MonomerContext {
  _appContext = app,
  _windowSize = winSize,
  _useHiDPI = useHiDPI,
  _devicePixelRate = devicePixelRate,
  _inputStatus = defInputStatus,
  _focusRing = [],
  _latestHover = Nothing,
  _userTasks = [],
  _widgetTasks = []
}

sizeReq :: Size -> SizePolicy -> SizePolicy -> SizeReq
sizeReq size policyWidth policyHeight = SizeReq size policyWidth policyHeight True

resultEvents :: [e] -> Maybe (WidgetEventResult s e m)
resultEvents userEvents = Just $ WidgetEventResult [] userEvents Nothing id

resultEventsWidget :: [e] -> (Widget s e m) -> Maybe (WidgetEventResult s e m)
resultEventsWidget userEvents newWidget = Just $ WidgetEventResult [] userEvents (Just newWidget) id

isFocusable :: (Monad m) => WidgetInstance s e m -> Bool
isFocusable (WidgetInstance { _widgetInstanceWidget = Widget{..}, ..}) = _widgetInstanceVisible && _widgetInstanceEnabled && _widgetFocusable

defaultCustomHandler :: Typeable i => s -> i -> Maybe (WidgetEventResult s e m)
defaultCustomHandler _ _ = Nothing

ignoreRestoreState :: s -> Maybe WidgetState -> Maybe (Widget s e m)
ignoreRestoreState _ _ = Nothing

ignoreSaveState :: s -> Maybe WidgetState
ignoreSaveState _ = Nothing

ignoreHandleEvent :: (Monad m) => s -> Rect -> SystemEvent -> Maybe (WidgetEventResult s e m)
ignoreHandleEvent _ _ _ = Nothing

ignorePreferredSize :: (Monad m) => Renderer m -> s -> Style -> [SizeReq] -> m SizeReq
ignorePreferredSize _ _ _ _ = return $ SizeReq (Size 0 0) FlexibleSize FlexibleSize False

ignoreResizeChildren :: (Monad m) => Rect -> Rect -> Style -> [SizeReq] -> Maybe (WidgetResizeResult s e m)
ignoreResizeChildren _ _ _ _ = Nothing

ignoreRender :: (Monad m) => Renderer m -> s -> WidgetInstance s e m -> Timestamp -> m ()
ignoreRender _ _ _ _ = return ()

ignoreRenderPost :: (Monad m) => Renderer m -> s -> WidgetInstance s e m -> Timestamp -> m ()
ignoreRenderPost _ _ _ _ = return ()

baseWidget :: (Monad m) => Widget s e m
baseWidget = Widget {
  _widgetType = "base",
  _widgetFocusable = False,
  _widgetRestoreState = ignoreRestoreState,
  _widgetSaveState = ignoreSaveState,
  _widgetHandleEvent = ignoreHandleEvent,
  _widgetHandleCustom = defaultCustomHandler,
  _widgetPreferredSize = ignorePreferredSize,
  _widgetResizeChildren = ignoreResizeChildren,
  _widgetRender = ignoreRender,
  _widgetRenderPost = ignoreRenderPost
}

makeState :: (Typeable i, Generic i) => i -> s -> Maybe WidgetState
makeState state app = Just (WidgetState state)

useState ::  (Typeable i, Generic i) => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

defaultRestoreState :: (Monad m, Typeable i, Generic i) => (i -> Widget s e m) -> s -> Maybe WidgetState -> Maybe (Widget s e m)
defaultRestoreState makeState _ oldState = fmap makeState $ useState oldState

key :: (Monad m) => WidgetKey -> WidgetInstance s e m -> WidgetInstance s e m
key key wn = wn { _widgetInstanceKey = Just key }

style :: (Monad m) => WidgetNode s e m -> Style -> WidgetNode s e m
style (Node value children) newStyle = Node (value { _widgetInstanceStyle = newStyle }) children

visible :: (Monad m) => WidgetNode s e m -> Bool -> WidgetNode s e m
visible (Node value children) visibility = Node (value { _widgetInstanceVisible = visibility }) children

children :: (Monad m) => WidgetNode s e m -> [WidgetNode s e m] -> WidgetNode s e m
children (Node value _) newChildren = fromList value newChildren

cascadeStyle :: (Monad m) => Style -> WidgetNode s e m -> WidgetNode s e m
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

defaultWidgetInstance :: (Monad m) => Widget s e m -> WidgetInstance s e m
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

singleWidget :: (Monad m) => Widget s e m -> WidgetNode s e m
singleWidget widget = singleton (defaultWidgetInstance widget)

parentWidget :: (Monad m) => Widget s e m -> [WidgetNode s e m] -> WidgetNode s e m
parentWidget widget = fromList (defaultWidgetInstance widget)

widgetMatches :: (Monad m) => WidgetInstance s e m -> WidgetInstance s e m -> Bool
widgetMatches wn1 wn2 = _widgetType (_widgetInstanceWidget wn1) == _widgetType (_widgetInstanceWidget wn2) && _widgetInstanceKey wn1 == _widgetInstanceKey wn2

mergeTrees :: (Monad m) => s -> WidgetNode s e m -> WidgetNode s e m -> WidgetNode s e m
mergeTrees app node1@(Node candidateInstance candidateChildren) (Node oldInstance oldChildren) = newNode where
  matches = widgetMatches candidateInstance oldInstance
  newNode = if | matches -> Node newInstance newChildren
               | otherwise -> node1
  oldWidget = _widgetInstanceWidget oldInstance
  oldState = _widgetSaveState oldWidget app
  candidateWidget = _widgetInstanceWidget candidateInstance
  newWidget = fromMaybe candidateWidget (_widgetRestoreState candidateWidget app oldState)
  newInstance = candidateInstance { _widgetInstanceWidget = newWidget }
  newChildren = mergedChildren SQ.>< addedChildren
  mergedChildren = fmap mergeChild (SQ.zip candidateChildren oldChildren)
  addedChildren = SQ.drop (SQ.length oldChildren) candidateChildren
  mergeChild = \(c1, c2) -> mergeTrees app c1 c2

type ChildrenSelector s e m a = a -> SQ.Seq (WidgetNode s e m) -> (a, Maybe Int)

data EventsParent s e m = EventsParent {
  epIgnoreChildrenEvents :: Bool,
  epIgnoreParentEvents :: Bool,
  epEventRequests :: [(Path, EventRequest)],
  epUserEvents :: [e],
  epUpdatedNode :: Maybe (Tree (WidgetInstance s e m)),
  epNewStates :: [s -> s]
}

data EventsChildren s e m = EventsChildren {
  ecIgnoreParentEvents :: Bool,
  ecEventRequests :: [(Path, EventRequest)],
  ecUserEvents :: [e],
  ecUpdatedNode :: Maybe (Tree (WidgetInstance s e m)),
  ecNewStates :: [s -> s],
  ecNodePosition :: Int
}

handleWidgetEvents :: (Monad m) => s -> Widget s e m -> Rect -> SystemEvent -> Maybe (WidgetEventResult s e m)
handleWidgetEvents app widget viewport systemEvent = _widgetHandleEvent widget app viewport systemEvent

handleChildEvent :: (Monad m) => s -> ChildrenSelector s e m a -> a -> Path -> WidgetNode s e m -> SystemEvent -> ChildEventResult s e m
handleChildEvent app selectorFn selector path (Node widgetInstance children) systemEvent =
  createUpdatedNode widgetInstance children eventsParent eventsChildren
    where
      eventsParent = handleEventsParent app path widgetInstance children systemEvent
      eventsChildren = handleEventsChildren app selectorFn selector path children systemEvent

createUpdatedNode :: (Monad m) => WidgetInstance s e m -> WidgetChildren s e m -> EventsParent s e m -> EventsChildren s e m -> ChildEventResult s e m
createUpdatedNode widgetInstance children (EventsParent{..}) (EventsChildren{..}) = updatedNode where
  updatedNode = case (epIgnoreChildrenEvents, ecIgnoreParentEvents) of
    (True, _) -> ChildEventResult epIgnoreParentEvents epEventRequests epUserEvents epUpdatedNode epNewStates
    (_, True) -> ChildEventResult ecIgnoreParentEvents ecEventRequests ecUserEvents newNodeOldParent ecNewStates
    (_, False) -> ChildEventResult epIgnoreParentEvents (epEventRequests ++ ecEventRequests) (epUserEvents ++ ecUserEvents) newNodeMixed (epNewStates ++ ecNewStates)
  newNodeOldParent = createNewNodeOldParent widgetInstance children ecNodePosition ecUpdatedNode
  newNodeMixed = createNewNodeMixed widgetInstance children ecNodePosition epUpdatedNode ecUpdatedNode

handleEventsParent :: (Monad m) => s -> Path -> WidgetInstance s e m -> WidgetChildren s e m -> SystemEvent -> EventsParent s e m
handleEventsParent app path (wi@WidgetInstance{..}) children systemEvent = case handleWidgetEvents app _widgetInstanceWidget _widgetInstanceRenderArea systemEvent of
  Nothing -> EventsParent False False [] [] Nothing []
  Just (WidgetEventResult er ue nw ns) -> EventsParent {
      epIgnoreChildrenEvents = isJust $ L.find isIgnoreChildrenEvents er,
      epIgnoreParentEvents = isJust $ L.find isIgnoreParentEvents er,
      epEventRequests = fmap (path,) er,
      epUserEvents = ue,
      epUpdatedNode = if isNothing nw
                        then Nothing
                        else Just $ Node (wi { _widgetInstanceWidget = fromJust nw }) children,
      epNewStates = [ns]
    }

handleEventsChildren :: (Monad m) => s -> ChildrenSelector s e m a -> a -> Path -> WidgetChildren s e m -> SystemEvent -> EventsChildren s e m
handleEventsChildren app selectorFn selector path children systemEvent = case selectorFn selector children of
  (_, Nothing) -> EventsChildren False [] [] Nothing [] 0
  (newSelector, Just idx) -> EventsChildren {
      ecIgnoreParentEvents = ipe,
      ecEventRequests = er,
      ecUserEvents = ue,
      ecUpdatedNode = nw,
      ecNewStates = ns,
      ecNodePosition = idx
    }
    where
      (ChildEventResult ipe er ue nw ns) = handleChildEvent app selectorFn newSelector widgetPath (SQ.index children idx) systemEvent
      widgetPath = (idx:path)

replaceChild :: (Monad m) => WidgetInstance s e m -> WidgetChildren s e m -> Int -> WidgetNode s e m -> Maybe (WidgetNode s e m)
replaceChild widgetInstance children childIdx newChild = Just $ Node widgetInstance (SQ.update childIdx newChild children)

createNewNodeOldParent :: (Monad m) => WidgetInstance s e m -> WidgetChildren s e m -> Int -> Maybe (WidgetNode s e m) -> Maybe (WidgetNode s e m)
createNewNodeOldParent widgetInstance children childIdx newChild = case newChild of
  Nothing -> Nothing
  Just tnChild -> replaceChild widgetInstance children childIdx tnChild

createNewNodeMixed :: (Monad m) => WidgetInstance s e m -> WidgetChildren s e m -> Int -> Maybe (WidgetNode s e m) -> Maybe (WidgetNode s e m) -> Maybe (WidgetNode s e m)
createNewNodeMixed oldParentWidgetInstance children childIdx newParent newChild = case (newParent, newChild) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just cn) -> replaceChild oldParentWidgetInstance children childIdx cn
  (Just _, Nothing) -> newParent
  (Just (Node newParentWidgetInstance _), Just newChildTreeNode) -> replaceChild newParentWidgetInstance children childIdx newChildTreeNode

handleEventFromPath :: (Monad m) => s -> Path -> WidgetNode s e m -> SystemEvent -> ChildEventResult s e m
handleEventFromPath app path widgetInstance systemEvent = handleChildEvent app pathSelector path [] widgetInstance systemEvent where
  pathSelector [] _ = ([], Nothing)
  pathSelector (p:ps) children
    | length children > p = (ps, Just p)
    | otherwise = ([], Nothing)

handleEventFromPoint :: (Monad m) => s -> Point -> WidgetNode s e m -> SystemEvent -> ChildEventResult s e m
handleEventFromPoint app cursorPos widgetInstance systemEvent = handleChildEvent app rectSelector cursorPos [] widgetInstance systemEvent where
  rectSelector point children = (point, SQ.lookup 0 inRectList) where
    inRectList = fmap snd $ SQ.filter inNodeRect childrenPair
    inNodeRect = \(Node (WidgetInstance {..}) _, _) -> inRect _widgetInstanceViewport point
    childrenPair = SQ.zip children (SQ.fromList [0..(length children - 1)])

handleCustomCommand :: (Monad m, Typeable i) => s -> Path -> WidgetNode s e m -> i -> ChildEventResult s e m
handleCustomCommand app path treeNode customData = case Monomer.Data.Tree.lookup path treeNode of
  Just (WidgetInstance{ _widgetInstanceWidget = Widget{..}, ..}) ->
    case _widgetHandleCustom app customData of
      Just (WidgetEventResult er ue nw ns) -> ChildEventResult False (fmap (path,) er) ue Nothing [ns]
      Nothing -> ChildEventResult False [] [] Nothing []
  Nothing -> ChildEventResult False [] [] Nothing []

findPathFromPoint :: (Monad m) => Point -> WidgetNode s e m -> Path
findPathFromPoint p widgetInstance = path where
  path = case SQ.lookup 0 inRectList of
    Just (child, idx) -> idx : findPathFromPoint p child
    Nothing -> []
  children = nodeChildren widgetInstance
  inRectList = SQ.filter inNodeRect childrenPair
  inNodeRect = \(Node (WidgetInstance {..}) _, _) -> inRect _widgetInstanceViewport p
  childrenPair = SQ.zip children (SQ.fromList [0..(length children - 1)])

handleRender :: (Monad m) => Renderer m -> s -> Path -> WidgetNode s e m -> Timestamp -> m ()
handleRender renderer app path (Node (widgetInstance@WidgetInstance { _widgetInstanceWidget = Widget{..}, .. }) children) ts = do
  when _widgetInstanceVisible $ do
    renderWidget renderer path RenderNormal $
      _widgetRender renderer app widgetInstance ts

    handleRenderChildren renderer app path children ts

    renderWidget renderer path RenderPost $
      _widgetRenderPost renderer app widgetInstance ts

handleRenderChildren :: (Monad m) => Renderer m -> s -> Path -> WidgetChildren s e m -> Timestamp -> m ()
handleRenderChildren renderer app parentPath children ts = do
  let childrenPair = SQ.zip children (SQ.fromList [0..(length children - 1)])

  mapM_ (\(treeNode, idx) -> handleRender renderer app (idx:parentPath) treeNode ts) childrenPair

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

resizeUI :: (Monad m) => Renderer m -> s -> Rect -> WidgetNode s e m -> m (WidgetNode s e m)
resizeUI renderer app assignedRect widgetInstance = do
  preferredSizes <- buildPreferredSizes renderer app True widgetInstance
  resizeNode renderer assignedRect assignedRect preferredSizes widgetInstance

buildPreferredSizes :: (Monad m) => Renderer m -> s -> Bool -> WidgetNode s e m -> m (Tree SizeReq)
buildPreferredSizes renderer app parentVisible (Node (WidgetInstance {..}) children) = do
  let isVisible = parentVisible && _widgetInstanceVisible

  childrenSizes <- mapM (buildPreferredSizes renderer app isVisible) children
  sr <- _widgetPreferredSize _widgetInstanceWidget renderer app _widgetInstanceStyle (seqToList childrenSizes)

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

resizeNode :: (Monad m) => Renderer m -> Rect -> Rect -> Tree SizeReq -> WidgetNode s e m -> m (WidgetNode s e m)
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
