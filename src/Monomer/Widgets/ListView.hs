{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.ListView (
  ListViewCfg,
  ListItem(..),
  listView,
  listView_,
  listViewV,
  listViewV_,
  listViewD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (^?), (^?!), (.~), (%~), (?~), (<>~), ix, non)
import Control.Monad (when)
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Monomer.Graphics.Lens
import Monomer.Widgets.Box
import Monomer.Widgets.Container
import Monomer.Widgets.Label
import Monomer.Widgets.Scroll
import Monomer.Widgets.Spacer
import Monomer.Widgets.Stack

import qualified Monomer.Core.Lens as L

type ListItem a = (Eq a, Show a, Typeable a)
type MakeRow s e a = a -> WidgetNode s e

data ListViewCfg s e a = ListViewCfg {
  _lvcSelectOnBlur :: Maybe Bool,
  _lvcItemStyle :: Maybe Style,
  _lvcItemSelectedStyle :: Maybe Style,
  _lvcMergeRequired :: Maybe (Seq a -> Seq a -> Bool),
  _lvcOnFocus :: [e],
  _lvcOnFocusReq :: [WidgetRequest s],
  _lvcOnBlur :: [e],
  _lvcOnBlurReq :: [WidgetRequest s],
  _lvcOnChange :: [a -> e],
  _lvcOnChangeReq :: [WidgetRequest s],
  _lvcOnChangeIdx :: [Int -> a -> e],
  _lvcOnChangeIdxReq :: [Int -> WidgetRequest s]
}

instance Default (ListViewCfg s e a) where
  def = ListViewCfg {
    _lvcSelectOnBlur = Nothing,
    _lvcItemStyle = Nothing,
    _lvcItemSelectedStyle = Nothing,
    _lvcMergeRequired = Nothing,
    _lvcOnFocus = [],
    _lvcOnFocusReq = [],
    _lvcOnBlur = [],
    _lvcOnBlurReq = [],
    _lvcOnChange = [],
    _lvcOnChangeReq = [],
    _lvcOnChangeIdx = [],
    _lvcOnChangeIdxReq = []
  }

instance Semigroup (ListViewCfg s e a) where
  (<>) t1 t2 = ListViewCfg {
    _lvcSelectOnBlur = _lvcSelectOnBlur t2 <|> _lvcSelectOnBlur t1,
    _lvcItemStyle = _lvcItemStyle t2 <|> _lvcItemStyle t1,
    _lvcItemSelectedStyle = _lvcItemSelectedStyle t2 <|> _lvcItemSelectedStyle t1,
    _lvcMergeRequired = _lvcMergeRequired t2 <|> _lvcMergeRequired t1,
    _lvcOnFocus = _lvcOnFocus t1 <> _lvcOnFocus t2,
    _lvcOnFocusReq = _lvcOnFocusReq t1 <> _lvcOnFocusReq t2,
    _lvcOnBlur = _lvcOnBlur t1 <> _lvcOnBlur t2,
    _lvcOnBlurReq = _lvcOnBlurReq t1 <> _lvcOnBlurReq t2,
    _lvcOnChange = _lvcOnChange t1 <> _lvcOnChange t2,
    _lvcOnChangeReq = _lvcOnChangeReq t1 <> _lvcOnChangeReq t2,
    _lvcOnChangeIdx = _lvcOnChangeIdx t1 <> _lvcOnChangeIdx t2,
    _lvcOnChangeIdxReq = _lvcOnChangeIdxReq t1 <> _lvcOnChangeIdxReq t2
  }

instance Monoid (ListViewCfg s e a) where
  mempty = def

instance CmbOnFocus (ListViewCfg s e a) e where
  onFocus fn = def {
    _lvcOnFocus = [fn]
  }

instance CmbOnFocusReq (ListViewCfg s e a) s where
  onFocusReq req = def {
    _lvcOnFocusReq = [req]
  }

instance CmbOnBlur (ListViewCfg s e a) e where
  onBlur fn = def {
    _lvcOnBlur = [fn]
  }

instance CmbOnBlurReq (ListViewCfg s e a) s where
  onBlurReq req = def {
    _lvcOnBlurReq = [req]
  }

instance CmbOnChange (ListViewCfg s e a) a e where
  onChange fn = def {
    _lvcOnChange = [fn]
  }

instance CmbOnChangeReq (ListViewCfg s e a) s where
  onChangeReq req = def {
    _lvcOnChangeReq = [req]
  }

instance CmbOnChangeIdx (ListViewCfg s e a) a e where
  onChangeIdx fn = def {
    _lvcOnChangeIdx = [fn]
  }

instance CmbOnChangeIdxReq (ListViewCfg s e a) s where
  onChangeIdxReq req = def {
    _lvcOnChangeIdxReq = [req]
  }

instance CmbSelectOnBlur (ListViewCfg s e a) where
  selectOnBlur select = def {
    _lvcSelectOnBlur = Just select
  }

instance CmbItemNormalStyle (ListViewCfg s e a) Style where
  itemNormalStyle style = def {
    _lvcItemStyle = Just style
  }

instance CmbItemSelectedStyle (ListViewCfg s e a) Style where
  itemSelectedStyle style = def {
    _lvcItemSelectedStyle = Just style
  }

instance CmbMergeRequired (ListViewCfg s e a) (Seq a) where
  mergeRequired fn = def {
    _lvcMergeRequired = Just fn
  }

data ListViewState a = ListViewState {
  _prevItems :: Seq a,
  _prevSel :: Maybe a,
  _highlighted :: Int,
  _resizeReq :: Bool
}

newtype ListViewMessage
  = OnClickMessage Int
  deriving Typeable

listView
  :: (WidgetModel s, WidgetEvent e, Traversable t, ListItem a)
  => ALens' s a
  -> t a
  -> MakeRow s e a
  -> WidgetNode s e
listView field items makeRow = listView_ field items makeRow def

listView_
  :: (WidgetModel s, WidgetEvent e, Traversable t, ListItem a)
  => ALens' s a
  -> t a
  -> MakeRow s e a
  -> [ListViewCfg s e a]
  -> WidgetNode s e
listView_ field items makeRow configs = newNode where
  newNode = listViewD_ (WidgetLens field) items makeRow configs

listViewV
  :: (WidgetModel s, WidgetEvent e, Traversable t, ListItem a)
  => a
  -> (Int -> a -> e)
  -> t a
  -> MakeRow s e a
  -> WidgetNode s e
listViewV value handler items makeRow = newNode where
  newNode = listViewV_ value handler items makeRow def

listViewV_
  :: (WidgetModel s, WidgetEvent e, Traversable t, ListItem a)
  => a
  -> (Int -> a -> e)
  -> t a
  -> MakeRow s e a
  -> [ListViewCfg s e a]
  -> WidgetNode s e
listViewV_ value handler items makeRow configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChangeIdx handler : configs
  newNode = listViewD_ widgetData items makeRow newConfigs

listViewD_
  :: (WidgetModel s, WidgetEvent e, Traversable t, ListItem a)
  => WidgetData s a
  -> t a
  -> MakeRow s e a
  -> [ListViewCfg s e a]
  -> WidgetNode s e
listViewD_ widgetData items makeRow configs = makeInstance widget where
  config = mconcat configs
  newItems = foldl' (|>) Empty items
  newState = ListViewState newItems Nothing 0 False
  widget = makeListView widgetData newItems makeRow config newState

makeInstance
  :: (WidgetModel s, WidgetEvent e)
  => Widget s e
  -> WidgetNode s e
makeInstance widget = scroll_ childNode [scrollStyle L.listViewStyle] where
  childNode = defaultWidgetNode "listView" widget
    & L.widgetInstance . L.focusable .~ True

makeListView
  :: (WidgetModel s, WidgetEvent e, ListItem a)
  => WidgetData s a
  -> Seq a
  -> MakeRow s e a
  -> ListViewCfg s e a
  -> ListViewState a
  -> Widget s e
makeListView widgetData items makeRow config state = widget where
  baseWidget = createContainer def {
    containerResizeRequired = _resizeReq state,
    containerInit = init,
    containerGetState = makeState state,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetMerge = mergeWrapper,
    widgetRender = render
  }

  currentValue wenv = widgetDataGet (_weModel wenv) widgetData

  createListViewChildren wenv node = children where
    path = node ^. L.widgetInstance . L.path
    selected = currentValue wenv
    itemsList = makeItemsList wenv items makeRow config path selected
    children = Seq.singleton itemsList

  init wenv node = resultWidget newNode where
    children = createListViewChildren wenv node
    sel = Just $ currentValue wenv
    newState = state {
      _prevSel = sel,
      _resizeReq = True
    }
    tmpNode = node
      & L.widget .~ makeListView widgetData items makeRow config newState
      & L.children .~ children
    newNode = updateSelStyle makeRow config Empty items Nothing sel wenv tmpNode

  mergeWrapper wenv oldNode newNode = newResult where
    sel = Just $ currentValue wenv
    oldNodeState = widgetGetState (oldNode ^. L.widget) wenv
    oldState = fromMaybe state (useState oldNodeState)
    oldSel = _prevSel oldState
    oldItems = _prevItems oldState
    mergeRequiredFn = fromMaybe (/=) (_lvcMergeRequired config)
    mergeRequired = mergeRequiredFn oldItems items
    getBaseStyle _ _ = Nothing
    styledNode = initInstanceStyle getBaseStyle wenv newNode
    newState = oldState {
      _prevSel = sel,
      _resizeReq = mergeRequired
    }
    tempNode = styledNode
      & L.widget .~ makeListView widgetData items makeRow config newState
      & L.widgetInstance . L.viewport .~ oldNode ^. L.widgetInstance . L.viewport
      & L.widgetInstance . L.renderArea .~ oldNode ^. L.widgetInstance . L.renderArea
      & L.widgetInstance . L.sizeReqW .~ oldNode ^. L.widgetInstance . L.sizeReqW
      & L.widgetInstance . L.sizeReqH .~ oldNode ^. L.widgetInstance . L.sizeReqH
    children
      | mergeRequired = createListViewChildren wenv tempNode
      | otherwise = oldNode ^. L.children
    node2 = tempNode & L.children .~ children
    node3 = updateSelStyle makeRow config oldItems items oldSel sel wenv node2
    pResult = resultWidget node3
    newResult
      | mergeRequired = mergeChildren wenv oldNode pResult
      | otherwise = pResult

  handleEvent wenv target evt node = case evt of
    Focus -> handleFocusChange _lvcOnFocus _lvcOnFocusReq config node
    Blur -> result where
      isTabPressed = getKeyStatus (_weInputStatus wenv) keyTab == KeyPressed
      changeReq = isTabPressed && _lvcSelectOnBlur config == Just True
      WidgetResult tempNode tempReqs tempEvts
        | changeReq = selectItem wenv node (_highlighted state)
        | otherwise = resultWidget node
      evts = tempEvts <> Seq.fromList (_lvcOnBlur config)
      reqs = tempReqs <> Seq.fromList (_lvcOnBlurReq config)
      mergedResult = Just $ WidgetResult tempNode reqs evts
      result
        | changeReq || not (null evts && null reqs) = mergedResult
        | otherwise = Nothing
    KeyAction mode code status
      | isKeyDown code && status == KeyPressed -> highlightNext wenv node
      | isKeyUp code && status == KeyPressed -> highlightPrev wenv node
      | isSelectKey code && status == KeyPressed -> resultSelected
      where
        resultSelected = Just $ selectItem wenv node (_highlighted state)
        isSelectKey code = isKeyReturn code || isKeySpace code
    _ -> Nothing

  highlightNext wenv node = highlightItem wenv node nextIdx where
    tempIdx = _highlighted state
    nextIdx
      | tempIdx < length items - 1 = tempIdx + 1
      | otherwise = tempIdx

  highlightPrev wenv node = highlightItem wenv node nextIdx where
    tempIdx = _highlighted state
    nextIdx
      | tempIdx > 0 = tempIdx - 1
      | otherwise = tempIdx

  handleMessage wenv target message node = result where
    handleSelect (OnClickMessage idx) = handleItemClick wenv node idx
    result = fmap handleSelect (cast message)

  handleItemClick wenv node idx = result where
    focusReq = Seq.singleton (SetFocus $ node ^. L.widgetInstance . L.path)
    tempResult = selectItem wenv node idx
    result
      | isFocused wenv node = tempResult
      | otherwise = tempResult & L.requests <>~ focusReq

  highlightItem wenv node nextIdx = Just result where
    newState = state {
      _highlighted = nextIdx
    }
    newNode = node
      & L.widget .~ makeListView widgetData items makeRow config newState
    reqs = itemScrollTo node nextIdx
    result = resultReqs newNode reqs

  selectItem wenv node idx = result where
    selected = currentValue wenv
    value = fromMaybe selected (Seq.lookup idx items)
    valueSetReq = widgetDataSet widgetData value
    scrollToReq = itemScrollTo node idx
    events = fmap ($ value) (_lvcOnChange config)
      ++ fmap (\fn -> fn idx value) (_lvcOnChangeIdx config)
    changeReqs = _lvcOnChangeReq config
      ++ fmap ($ idx) (_lvcOnChangeIdxReq config)
    requests = valueSetReq ++ scrollToReq ++ changeReqs
    newState = state {
      _highlighted = idx,
      _prevSel = Just selected
    }
    newNode = node
      & L.widget .~ makeListView widgetData items makeRow config newState
    result = resultReqsEvts newNode requests events

  itemScrollTo node idx = maybeToList (fmap scrollReq renderArea) where
    renderArea = itemRenderArea node idx
    scrollPath =  parentPath node
    scrollReq rect = SendMessage scrollPath (ScrollTo rect)

  itemRenderArea node idx = renderArea where
    lookup idx node = Seq.lookup idx (node ^. L.children)
    renderArea = fmap (_wiRenderArea . _wnWidgetInstance) $ pure node
      >>= lookup 0 -- vstack
      >>= lookup idx -- item

  getSizeReq wenv node children = (newSizeReqW, newSizeReqH) where
    child = Seq.index children 0
    newSizeReqW = _wiSizeReqW . _wnWidgetInstance $ child
    newSizeReqH = _wiSizeReqH . _wnWidgetInstance $ child

  resize wenv viewport renderArea children node = resized where
    assignedArea = Seq.singleton (viewport, renderArea)
    resized = (node, assignedArea)

  render renderer wenv node =
    renderContainer defaultRender renderer wenv (buildRenderNode wenv node)

  buildRenderNode wenv node = newNode where
    viewport = node ^. L.widgetInstance . L.viewport
    hlIdx = _highlighted state
    foldItem items idx item
      | isWidgetVisible item viewport = items |> updateStyle idx item
      | otherwise = items
    updateStyle idx item
      | idx == hlIdx = setFocusedItemStyle wenv item
      | otherwise = item
    children = node ^. L.children . ix 0 . L.children
    newChildren = Seq.foldlWithIndex foldItem Empty children
    newNode = node & L.children . ix 0 . L.children .~ newChildren

setChildStyle
  :: Seq a
  -> MakeRow s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Int
  -> Style
  -> WidgetNode s e
setChildStyle items makeRow wenv parent idx style = newParent where
  makeItem v = makeRow v & L.widgetInstance . L.style .~ style
  newChild = fmap makeItem (Seq.lookup idx items)
  merge newItem oldItem = newWidget where
    res = widgetMerge (newItem ^. L.widget) wenv oldItem newItem
    widget = res ^. L.widget
    newWidget = widget
      & L.widgetInstance . L.path .~ oldItem ^. L.widgetInstance . L.path
      & L.widgetInstance . L.viewport .~ oldItem ^. L.widgetInstance . L.viewport
      & L.widgetInstance . L.renderArea .~ oldItem ^. L.widgetInstance . L.renderArea
  listLens = L.children . ix 0
  boxLens = L.children . ix idx
  itemLens = L.children . ix 0
  newParent = case newChild of
    Just newNode -> parent & listLens . boxLens . itemLens %~ merge newNode
    _ -> parent

updateSelStyle
  :: ListItem a
  => MakeRow s e a
  -> ListViewCfg s e a
  -> Seq a
  -> Seq a
  -> Maybe a
  -> Maybe a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
updateSelStyle makeRow cfg oldIs newIs oldSel newSel wenv node1 = node3 where
  oldIdx = oldSel >>= flip Seq.elemIndexL oldIs
  newIdx = newSel >>= flip Seq.elemIndexL newIs
  node2 = maybe node1 (setDefChildStyle oldIs makeRow cfg wenv node1) oldIdx
  node3 = maybe node2 (setSelChildStyle newIs makeRow cfg wenv node2) newIdx

setDefChildStyle
  :: Seq a
  -> MakeRow s e a
  -> ListViewCfg s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Int
  -> WidgetNode s e
setDefChildStyle items makeRow config wenv parent idx = newParent where
  normalTheme = collectTheme wenv L.listViewItemStyle
  normalStyle = fromJust (Just normalTheme <> _lvcItemStyle config)
  newParent = setChildStyle items makeRow wenv parent idx normalStyle

setSelChildStyle
  :: Seq a
  -> MakeRow s e a
  -> ListViewCfg s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Int
  -> WidgetNode s e
setSelChildStyle items makeRow config wenv parent idx = newParent where
  selectedTheme = collectTheme wenv L.listViewItemSelectedStyle
  selectedStyleCfg = _lvcItemSelectedStyle config
  selectedStyle = fromJust (Just selectedTheme <> selectedStyleCfg)
  newParent = setChildStyle items makeRow wenv parent idx selectedStyle

setFocusedItemStyle :: WidgetEnv s e -> WidgetNode s e -> WidgetNode s e
setFocusedItemStyle wenv item
  | isHovered wenv item = item & hoverLens .~ (hoverStyle <> focusStyle)
  | otherwise = item & basicLens .~ focusStyle
  where
    basicLens = L.children . ix 0 . L.widgetInstance . L.style . L.basic
    hoverLens = L.children . ix 0 . L.widgetInstance . L.style . L.hover
    hoverStyle = item ^. L.children . ix 0 . L.widgetInstance . L.style . L.hover
    focusStyle = item ^. L.children . ix 0 . L.widgetInstance . L.style . L.focus

makeItemsList
  :: (WidgetModel s, WidgetEvent e, Eq a)
  => WidgetEnv s e
  -> Seq a
  -> MakeRow s e a
  -> ListViewCfg s e a
  -> Path
  -> a
  -> WidgetNode s e
makeItemsList wenv items makeRow config path selected = itemsList where
  normalTheme = collectTheme wenv L.listViewItemStyle
  normalStyle = fromJust (Just normalTheme <> _lvcItemStyle config)
  makeItem idx item = newItem where
    clickCfg = onClickReq $ SendMessage path (OnClickMessage idx)
    itemCfg = [expandContent, clickCfg]
    content = makeRow item
    newItem = box_ (content & L.widgetInstance . L.style .~ normalStyle) itemCfg
  itemsList = vstack $ Seq.mapWithIndex makeItem items
