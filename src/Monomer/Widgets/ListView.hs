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
import Control.Lens (ALens', (&), (^.), (^?), (^?!), (.~), (%~), (?~), (<>~), at, ix, non, _Just)
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

import qualified Monomer.Lens as L

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
  _slIdx :: Int,
  _hlIdx :: Int,
  _slStyle :: Maybe Style,
  _hlStyle :: Maybe Style,
  _resizeReq :: Bool
}

newtype ListViewMessage
  = OnClickMessage Int

listView
  :: (Traversable t, ListItem a)
  => ALens' s a
  -> t a
  -> MakeRow s e a
  -> WidgetNode s e
listView field items makeRow = listView_ field items makeRow def

listView_
  :: (Traversable t, ListItem a)
  => ALens' s a
  -> t a
  -> MakeRow s e a
  -> [ListViewCfg s e a]
  -> WidgetNode s e
listView_ field items makeRow configs = newNode where
  newNode = listViewD_ (WidgetLens field) items makeRow configs

listViewV
  :: (Traversable t, ListItem a)
  => a
  -> (Int -> a -> e)
  -> t a
  -> MakeRow s e a
  -> WidgetNode s e
listViewV value handler items makeRow = newNode where
  newNode = listViewV_ value handler items makeRow def

listViewV_
  :: (Traversable t, ListItem a)
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
  :: (Traversable t, ListItem a)
  => WidgetData s a
  -> t a
  -> MakeRow s e a
  -> [ListViewCfg s e a]
  -> WidgetNode s e
listViewD_ widgetData items makeRow configs = makeNode widget where
  config = mconcat configs
  newItems = foldl' (|>) Empty items
  newState = ListViewState newItems (-1) 0 Nothing Nothing False
  widget = makeListView widgetData newItems makeRow config newState

makeNode :: Widget s e -> WidgetNode s e
makeNode widget = scroll_ childNode [scrollStyle L.listViewStyle] where
  childNode = defaultWidgetNode "listView" widget
    & L.info . L.focusable .~ True

makeListView
  :: (ListItem a)
  => WidgetData s a
  -> Seq a
  -> MakeRow s e a
  -> ListViewCfg s e a
  -> ListViewState a
  -> Widget s e
makeListView widgetData items makeRow config state = widget where
  widget = createContainer state def {
    containerResizeRequired = _resizeReq state,
    containerInit = init,
    containerMerge = merge,
    containerMergeChildrenReq = mergeChildrenReq,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  currentValue wenv = widgetDataGet (_weModel wenv) widgetData

  createListViewChildren wenv node = children where
    path = node ^. L.info . L.path
    selected = currentValue wenv
    itemsList = makeItemsList wenv items makeRow config path selected
    children = Seq.singleton itemsList

  init wenv node = resultWidget newNode where
    children = createListViewChildren wenv node
    newState = state {
      _resizeReq = True
    }
    newNode = node
      & L.widget .~ makeListView widgetData items makeRow config newState
      & L.children .~ children

  mergeChildrenReq wenv oldState oldNode node = result where
    oldItems = _prevItems oldState
    mergeRequiredFn = fromMaybe (/=) (_lvcMergeRequired config)
    result = mergeRequiredFn oldItems items

  merge wenv oldState oldNode node = resultReqs newNode reqs where
    oldItems = _prevItems oldState
    mergeRequiredFn = fromMaybe (/=) (_lvcMergeRequired config)
    mergeRequired = mergeRequiredFn oldItems items
    children
      | mergeRequired = createListViewChildren wenv node
      | otherwise = oldNode ^. L.children
    newState = oldState {
      _resizeReq = mergeRequired
    }
    tmpNode = node
      & L.widget .~ makeListView widgetData items makeRow config newState
      & L.children .~ children
    slIdx = _slIdx newState
    hlIdx = _hlIdx newState
    (newNode, reqs) = updateStyles wenv config newState tmpNode slIdx hlIdx

  handleEvent wenv target evt node = case evt of
    ButtonAction _ btn PressedBtn _
      | btn == wenv ^. L.mainButton -> result where
        result = Just $ resultReqs node [SetFocus (node ^. L.info . L.path)]
    Focus -> handleFocusChange _lvcOnFocus _lvcOnFocusReq config node
    Blur -> result where
      isTabPressed = getKeyStatus (_weInputStatus wenv) keyTab == KeyPressed
      changeReq = isTabPressed && _lvcSelectOnBlur config == Just True
      WidgetResult tempNode tempReqs tempEvts
        | changeReq = selectItem wenv node (_hlIdx state)
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
        resultSelected = Just $ selectItem wenv node (_hlIdx state)
        isSelectKey code = isKeyReturn code || isKeySpace code
    _ -> Nothing

  highlightNext wenv node = highlightItem wenv node nextIdx where
    tempIdx = _hlIdx state
    nextIdx
      | tempIdx < length items - 1 = tempIdx + 1
      | otherwise = tempIdx

  highlightPrev wenv node = highlightItem wenv node nextIdx where
    tempIdx = _hlIdx state
    nextIdx
      | tempIdx > 0 = tempIdx - 1
      | otherwise = tempIdx

  handleMessage wenv target message node = result where
    handleSelect (OnClickMessage idx) = handleItemClick wenv node idx
    result = fmap handleSelect (cast message)

  handleItemClick wenv node idx = result where
    focusReq = SetFocus $ node ^. L.info . L.path
    tempResult = selectItem wenv node idx
    result
      | isFocused wenv node = tempResult
      | otherwise = tempResult & L.requests %~ (|> focusReq)

  highlightItem wenv node nextIdx = Just result where
    newHlStyle
      | nextIdx /= _hlIdx state = Just (getItemStyle node nextIdx)
      | otherwise = _hlStyle state
    newState = state {
      _hlIdx = nextIdx,
      _hlStyle = newHlStyle
    }
    tmpNode = node
      & L.widget .~ makeListView widgetData items makeRow config newState
    slIdx = _slIdx state
    (newNode, resizeReq) = updateStyles wenv config state tmpNode slIdx nextIdx
    reqs = itemScrollTo newNode nextIdx ++ resizeReq
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
    (styledNode, resizeReq) = updateStyles wenv config state node idx (-1)
    newSlStyle
      | idx == _hlIdx state = _hlStyle state
      | idx /= _slIdx state = Just (getItemStyle node idx)
      | otherwise = _slStyle state
    newState = state {
      _slIdx = idx,
      _hlIdx = idx,
      _slStyle = newSlStyle,
      _resizeReq = not (null resizeReq)
    }
    newNode = styledNode
      & L.widget .~ makeListView widgetData items makeRow config newState
    requests = valueSetReq ++ scrollToReq ++ changeReqs ++ resizeReq
    result = resultReqsEvts newNode requests events

  itemScrollTo node idx = maybeToList (fmap scrollReq renderArea) where
    renderArea = itemRenderArea node idx
    scrollPath =  parentPath node
    scrollReq rect = SendMessage scrollPath (ScrollTo rect)

  itemRenderArea node idx = renderArea where
    lookup idx node = Seq.lookup idx (node ^. L.children)
    renderArea = fmap (_wniRenderArea . _wnInfo) $ pure node
      >>= lookup 0 -- vstack
      >>= lookup idx -- item

  getSizeReq wenv currState node children = (newSizeReqW, newSizeReqH) where
    child = Seq.index children 0
    newSizeReqW = _wniSizeReqW . _wnInfo $ child
    newSizeReqH = _wniSizeReqH . _wnInfo $ child

  resize wenv viewport renderArea children node = resized where
    newState = state { _resizeReq = False }
    newNode = node
      & L.widget .~ makeListView widgetData items makeRow config newState
    assignedArea = Seq.singleton (viewport, renderArea)
    resized = (newNode, assignedArea)

updateStyles
  :: WidgetEnv s e
  -> ListViewCfg s e a
  -> ListViewState a
  -> WidgetNode s e
  -> Int
  -> Int
  -> (WidgetNode s e, [WidgetRequest s])
updateStyles wenv config state node newSlIdx newHlIdx = (newNode, newReqs) where
  items = node ^. L.children . ix 0 . L.children
  slStyle = getSlStyle wenv config
  hlStyle = getHlStyle wenv config
  (newChildren, resizeReq) = (items, False)
    & updateItemStyle wenv False (_slIdx state) (_slStyle state)
    & updateItemStyle wenv False (_hlIdx state) (_hlStyle state)
    & updateItemStyle wenv True newHlIdx (Just hlStyle)
    & updateItemStyle wenv True newSlIdx (Just slStyle)
  newNode = node
    & L.children . ix 0 . L.children .~ newChildren
  newReqs = [ ResizeWidgets | resizeReq ]

updateItemStyle
  :: WidgetEnv s e
  -> Bool
  -> Int
  -> Maybe Style
  -> (Seq (WidgetNode s e), Bool)
  -> (Seq (WidgetNode s e), Bool)
updateItemStyle wenv merge idx mstyle (items, resizeReq) = result where
  result = case Seq.lookup idx items of
    Just item -> (newItems, resizeReq || newResizeReq) where
      tmpItem
        | merge = mergeItemStyle item mstyle
        | otherwise = resetItemStyle item mstyle
      (newItem, newResizeReq) = updateItemSizeReq wenv tmpItem
      newItems = Seq.update idx newItem items
    Nothing -> (items, resizeReq)

updateItemSizeReq :: WidgetEnv s e -> WidgetNode s e -> (WidgetNode s e, Bool)
updateItemSizeReq wenv item = (newItem, resizeReq) where
  result = widgetMerge (item ^. L.widget) wenv item item
  newItem = result ^. L.node
  resizeReq = isResizeResult (Just result)

mergeItemStyle :: WidgetNode s e -> Maybe Style -> WidgetNode s e
mergeItemStyle item Nothing = item
mergeItemStyle item (Just st) = item
  & L.children . ix 0 . L.info . L.style <>~ st

resetItemStyle :: WidgetNode s e -> Maybe Style -> WidgetNode s e
resetItemStyle item Nothing = item
resetItemStyle item (Just st) = item
  & L.children . ix 0 . L.info . L.style .~ st

getItemStyle :: WidgetNode s e -> Int -> Style
getItemStyle node idx = itStyle where
  -- ListView -> Stack -> Box -> Content
  itemLens = L.children . ix 0 . L.children . ix idx . L.children . ix 0
  itStyle = node ^. itemLens . L.info . L.style

getSlStyle :: WidgetEnv s e -> ListViewCfg s e a -> Style
getSlStyle wenv config = slStyle where
  theme = collectTheme wenv L.listViewItemSelectedStyle
  style = fromJust (Just theme <> _lvcItemSelectedStyle config)
  slStyle = style
    & L.hover .~ style ^. L.focusHover
    & L.basic .~ style ^. L.focus

getHlStyle :: WidgetEnv s e -> ListViewCfg s e a -> Style
getHlStyle wenv config = hlStyle where
  theme = collectTheme wenv L.listViewItemStyle
  style = fromJust (Just theme <> _lvcItemStyle config)
  hlStyle = style
    & L.hover .~ style ^. L.focusHover
    & L.basic .~ style ^. L.focus

makeItemsList
  :: (Eq a)
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
    newItem = box_ (content & L.info . L.style .~ normalStyle) itemCfg
  itemsList = vstack $ Seq.mapWithIndex makeItem items
