{-|
Module      : Monomer.Widgets.Containers.SelectList
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Select list widget, allowing selection of a single item. List content (rows) is
customizable, plus its styling.

Configs:

- onFocus: event to raise when focus is received.
- onFocusReq: WidgetReqest to generate when focus is received.
- onBlur: event to raise when focus is lost.
- onBlurReq: WidgetReqest to generate when focus is lost.
- onChange: event to raise when selected item changes.
- onChangeReq: WidgetRequest to generate when selected item changes.
- onChangeIdx: event to raise when selected item changes. Includes index,
- onChangeIdxReq: WidgetRequest to generate when selected item changes. Includes
index.
- selectOnBlur: whether to select the currently highlighted item when navigating
away from the widget with tab key.
- itemNormalStyle: style of an item in the list when not selected.
- itemSelectedStyle: style of the selected item in the list.
- mergeRequired: whether merging children is required. Useful when select list
is part of another widget such as dropdown.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Containers.SelectList (
  SelectListCfg,
  SelectListItem(..),
  selectList,
  selectList_,
  selectListV,
  selectListV_,
  selectListD_
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
import Monomer.Widgets.Container
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Scroll
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

type SelectListItem a = (Eq a, Show a, Typeable a)
type MakeRow s e a = a -> WidgetNode s e

data SelectListCfg s e a = SelectListCfg {
  _slcSelectOnBlur :: Maybe Bool,
  _slcItemStyle :: Maybe Style,
  _slcItemSelectedStyle :: Maybe Style,
  _slcMergeRequired :: Maybe (Seq a -> Seq a -> Bool),
  _slcOnFocus :: [Path -> e],
  _slcOnFocusReq :: [WidgetRequest s e],
  _slcOnBlur :: [Path -> e],
  _slcOnBlurReq :: [WidgetRequest s e],
  _slcOnChange :: [a -> e],
  _slcOnChangeReq :: [a -> WidgetRequest s e],
  _slcOnChangeIdx :: [Int -> a -> e],
  _slcOnChangeIdxReq :: [Int -> a -> WidgetRequest s e]
}

instance Default (SelectListCfg s e a) where
  def = SelectListCfg {
    _slcSelectOnBlur = Nothing,
    _slcItemStyle = Nothing,
    _slcItemSelectedStyle = Nothing,
    _slcMergeRequired = Nothing,
    _slcOnFocus = [],
    _slcOnFocusReq = [],
    _slcOnBlur = [],
    _slcOnBlurReq = [],
    _slcOnChange = [],
    _slcOnChangeReq = [],
    _slcOnChangeIdx = [],
    _slcOnChangeIdxReq = []
  }

instance Semigroup (SelectListCfg s e a) where
  (<>) t1 t2 = SelectListCfg {
    _slcSelectOnBlur = _slcSelectOnBlur t2 <|> _slcSelectOnBlur t1,
    _slcItemStyle = _slcItemStyle t2 <|> _slcItemStyle t1,
    _slcItemSelectedStyle = _slcItemSelectedStyle t2 <|> _slcItemSelectedStyle t1,
    _slcMergeRequired = _slcMergeRequired t2 <|> _slcMergeRequired t1,
    _slcOnFocus = _slcOnFocus t1 <> _slcOnFocus t2,
    _slcOnFocusReq = _slcOnFocusReq t1 <> _slcOnFocusReq t2,
    _slcOnBlur = _slcOnBlur t1 <> _slcOnBlur t2,
    _slcOnBlurReq = _slcOnBlurReq t1 <> _slcOnBlurReq t2,
    _slcOnChange = _slcOnChange t1 <> _slcOnChange t2,
    _slcOnChangeReq = _slcOnChangeReq t1 <> _slcOnChangeReq t2,
    _slcOnChangeIdx = _slcOnChangeIdx t1 <> _slcOnChangeIdx t2,
    _slcOnChangeIdxReq = _slcOnChangeIdxReq t1 <> _slcOnChangeIdxReq t2
  }

instance Monoid (SelectListCfg s e a) where
  mempty = def

instance CmbOnFocus (SelectListCfg s e a) e Path where
  onFocus fn = def {
    _slcOnFocus = [fn]
  }

instance CmbOnFocusReq (SelectListCfg s e a) s e where
  onFocusReq req = def {
    _slcOnFocusReq = [req]
  }

instance CmbOnBlur (SelectListCfg s e a) e Path where
  onBlur fn = def {
    _slcOnBlur = [fn]
  }

instance CmbOnBlurReq (SelectListCfg s e a) s e where
  onBlurReq req = def {
    _slcOnBlurReq = [req]
  }

instance CmbOnChange (SelectListCfg s e a) a e where
  onChange fn = def {
    _slcOnChange = [fn]
  }

instance CmbOnChangeReq (SelectListCfg s e a) s e a where
  onChangeReq req = def {
    _slcOnChangeReq = [req]
  }

instance CmbOnChangeIdx (SelectListCfg s e a) e a where
  onChangeIdx fn = def {
    _slcOnChangeIdx = [fn]
  }

instance CmbOnChangeIdxReq (SelectListCfg s e a) s e a where
  onChangeIdxReq req = def {
    _slcOnChangeIdxReq = [req]
  }

instance CmbSelectOnBlur (SelectListCfg s e a) where
  selectOnBlur_ select = def {
    _slcSelectOnBlur = Just select
  }

instance CmbItemNormalStyle (SelectListCfg s e a) Style where
  itemNormalStyle style = def {
    _slcItemStyle = Just style
  }

instance CmbItemSelectedStyle (SelectListCfg s e a) Style where
  itemSelectedStyle style = def {
    _slcItemSelectedStyle = Just style
  }

instance CmbMergeRequired (SelectListCfg s e a) (Seq a) where
  mergeRequired fn = def {
    _slcMergeRequired = Just fn
  }

data SelectListState a = SelectListState {
  _prevItems :: Seq a,
  _slIdx :: Int,
  _hlIdx :: Int,
  _slStyle :: Maybe Style,
  _hlStyle :: Maybe Style,
  _resizeReq :: Bool
} deriving (Eq, Show)

newtype SelectListMessage
  = OnClickMessage Int

-- | Creates a select list using the given lens.
selectList
  :: (Traversable t, SelectListItem a, WidgetEvent e)
  => ALens' s a      -- ^ The lens into the model.
  -> t a             -- ^ The list of selectable items.
  -> MakeRow s e a   -- ^ Function to create the list items.
  -> WidgetNode s e  -- ^ The created dropdown.
selectList field items makeRow = selectList_ field items makeRow def

-- | Creates a select list using the given lens. Accepts config.
selectList_
  :: (Traversable t, SelectListItem a, WidgetEvent e)
  => ALens' s a             -- ^ The lens into the model.
  -> t a                    -- ^ The list of selectable items.
  -> MakeRow s e a          -- ^ Function to create the list items.
  -> [SelectListCfg s e a]  -- ^ The config options.
  -> WidgetNode s e         -- ^ The created dropdown.
selectList_ field items makeRow configs = newNode where
  newNode = selectListD_ (WidgetLens field) items makeRow configs

-- | Creates a select list using the given value and onChange event handler.
selectListV
  :: (Traversable t, SelectListItem a, WidgetEvent e)
  => a                -- ^ The event to raise on change.
  -> (Int -> a -> e)  -- ^ The list of selectable items.
  -> t a              -- ^ The list of selectable items.
  -> MakeRow s e a    -- ^ Function to create the list items.
  -> WidgetNode s e   -- ^ The created dropdown.
selectListV value handler items makeRow = newNode where
  newNode = selectListV_ value handler items makeRow def

-- | Creates a select list using the given value and onChange event handler.
-- | Accepts config.
selectListV_
  :: (Traversable t, SelectListItem a, WidgetEvent e)
  => a                      -- ^ The event to raise on change.
  -> (Int -> a -> e)        -- ^ The list of selectable items.
  -> t a                    -- ^ The list of selectable items.
  -> MakeRow s e a          -- ^ Function to create the list items.
  -> [SelectListCfg s e a]  -- ^ The config options.
  -> WidgetNode s e         -- ^ The created dropdown.
selectListV_ value handler items makeRow configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChangeIdx handler : configs
  newNode = selectListD_ widgetData items makeRow newConfigs

-- | Creates a dropdown providing a WidgetData instance and config.
selectListD_
  :: (Traversable t, SelectListItem a, WidgetEvent e)
  => WidgetData s a         -- ^ The WidgetData to retrieve the value from.
  -> t a                    -- ^ The list of selectable items.
  -> MakeRow s e a          -- ^ Function to create the list items.
  -> [SelectListCfg s e a]  -- ^ The config options.
  -> WidgetNode s e         -- ^ The created dropdown.
selectListD_ widgetData items makeRow configs = makeNode widget where
  config = mconcat configs
  newItems = foldl' (|>) Empty items
  newState = SelectListState newItems (-1) 0 Nothing Nothing False
  widget = makeSelectList widgetData newItems makeRow config newState

makeNode :: Widget s e -> WidgetNode s e
makeNode widget = scroll_ [scrollStyle L.selectListStyle] childNode where
  childNode = defaultWidgetNode "selectList" widget
    & L.info . L.focusable .~ True

makeSelectList
  :: (SelectListItem a, WidgetEvent e)
  => WidgetData s a
  -> Seq a
  -> MakeRow s e a
  -> SelectListCfg s e a
  -> SelectListState a
  -> Widget s e
makeSelectList widgetData items makeRow config state = widget where
  widget = createContainer state def {
    containerResizeRequired = _resizeReq state,
    containerInit = init,
    containerMergeChildrenReq = mergeChildrenReq,
    containerMerge = merge,
    containerMergePost = mergePost,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  currentValue wenv = widgetDataGet (_weModel wenv) widgetData

  createSelectListChildren wenv node = children where
    widgetId = node ^. L.info . L.widgetId
    selected = currentValue wenv
    itemsList = makeItemsList wenv items makeRow config widgetId selected
    children = Seq.singleton itemsList

  init wenv node = resultNode newNode where
    children = createSelectListChildren wenv node
    newState = state {
      _resizeReq = True
    }
    newNode = node
      & L.widget .~ makeSelectList widgetData items makeRow config newState
      & L.children .~ children

  mergeChildrenReq wenv node oldNode oldState = result where
    oldItems = _prevItems oldState
    mergeRequiredFn = fromMaybe (/=) (_slcMergeRequired config)
    result = mergeRequiredFn oldItems items

  merge wenv node oldNode oldState = result where
    oldItems = _prevItems oldState
    mergeRequiredFn = fromMaybe (/=) (_slcMergeRequired config)
    flagsChanged = childrenFlagsChanged oldNode node
    mergeRequired = mergeRequiredFn oldItems items || flagsChanged
    children
      | mergeRequired = createSelectListChildren wenv node
      | otherwise = oldNode ^. L.children
    result = updateState wenv node oldState mergeRequired children

  mergePost wenv node oldNode oldState result = newResult where
    newResult = updateResultStyle wenv result oldState

  updateState wenv node oldState resizeReq children = resultNode newNode where
    newState = oldState {
      _prevItems = items,
      _resizeReq = resizeReq
    }
    newNode = node
      & L.widget .~ makeSelectList widgetData items makeRow config newState
      & L.children .~ children

  updateResultStyle wenv result state = newResult where
    slIdx = _slIdx state
    hlIdx = _hlIdx state
    tmpNode = result ^. L.node
    (newNode, reqs) = updateStyles wenv config state tmpNode slIdx hlIdx
    newResult = resultReqs newNode reqs

  handleEvent wenv node target evt = case evt of
    ButtonAction _ btn PressedBtn _
      | btn == wenv ^. L.mainButton -> result where
        result = Just $ resultReqs node [SetFocus (node ^. L.info . L.widgetId)]
    Focus prev -> handleFocusChange _slcOnFocus _slcOnFocusReq config prev node
    Blur next -> result where
      isTabPressed = getKeyStatus (_weInputStatus wenv) keyTab == KeyPressed
      changeReq = isTabPressed && _slcSelectOnBlur config == Just True
      WidgetResult tempNode tempReqs
        | changeReq = selectItem wenv node (_hlIdx state)
        | otherwise = resultNode node
      evts = RaiseEvent <$> Seq.fromList (($ next) <$> _slcOnBlur config)
      reqs = tempReqs <> Seq.fromList (_slcOnBlurReq config)
      mergedResult = Just $ WidgetResult tempNode (reqs <> evts)
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

  handleMessage wenv node target message = result where
    handleSelect (OnClickMessage idx) = handleItemClick wenv node idx
    result = fmap handleSelect (cast message)

  handleItemClick wenv node idx = result where
    focusReq = SetFocus $ node ^. L.info . L.widgetId
    tempResult = selectItem wenv node idx
    result
      | isNodeFocused wenv node = tempResult
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
      & L.widget .~ makeSelectList widgetData items makeRow config newState
    slIdx = _slIdx state
    (newNode, resizeReq) = updateStyles wenv config state tmpNode slIdx nextIdx
    reqs = itemScrollTo wenv newNode nextIdx ++ resizeReq
    result = resultReqs newNode reqs

  selectItem wenv node idx = result where
    selected = currentValue wenv
    value = fromMaybe selected (Seq.lookup idx items)
    valueSetReq = widgetDataSet widgetData value
    scrollToReq = itemScrollTo wenv node idx
    events = fmap ($ value) (_slcOnChange config)
      ++ fmap (\fn -> fn idx value) (_slcOnChangeIdx config)
    changeReqs = fmap ($ value) (_slcOnChangeReq config)
      ++ fmap (\fn -> fn idx value) (_slcOnChangeIdxReq config)
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
      & L.widget .~ makeSelectList widgetData items makeRow config newState
    requests = valueSetReq ++ scrollToReq ++ changeReqs ++ resizeReq
    result = resultReqsEvts newNode requests events

  itemScrollTo wenv node idx = maybeToList (scrollToReq <$> mwid <*> vp) where
    vp = itemViewport node idx
    mwid = findWidgetIdFromPath wenv (parentPath node)
    scrollToReq wid rect = SendMessage wid (ScrollTo rect)

  itemViewport node idx = viewport where
    lookup idx node = Seq.lookup idx (node ^. L.children)
    viewport = fmap (_wniViewport . _wnInfo) $ pure node
      >>= lookup 0 -- vstack
      >>= lookup idx -- item

  getSizeReq wenv node children = (newSizeReqW, newSizeReqH) where
    child = Seq.index children 0
    newSizeReqW = _wniSizeReqW . _wnInfo $ child
    newSizeReqH = _wniSizeReqH . _wnInfo $ child

  resize wenv node viewport children = resized where
    newState = state { _resizeReq = False }
    newNode = node
      & L.widget .~ makeSelectList widgetData items makeRow config newState
    assignedArea = Seq.singleton viewport
    resized = (resultNode newNode, assignedArea)

updateStyles
  :: WidgetEnv s e
  -> SelectListCfg s e a
  -> SelectListState a
  -> WidgetNode s e
  -> Int
  -> Int
  -> (WidgetNode s e, [WidgetRequest s e])
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
  (oldReqW, oldReqH) = (item^. L.info . L.sizeReqW, item^. L.info . L.sizeReqH)
  (newReqW, newReqH) = widgetGetSizeReq (item ^. L.widget) wenv item
  newItem = item
    & L.info . L.sizeReqW .~ newReqW
    & L.info . L.sizeReqH .~ newReqH
  resizeReq = (oldReqW, oldReqH) /= (newReqW, newReqH)

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
  -- SelectList -> Stack -> Box -> Content
  itemLens = L.children . ix 0 . L.children . ix idx . L.children . ix 0
  itStyle = node ^. itemLens . L.info . L.style

getSlStyle :: WidgetEnv s e -> SelectListCfg s e a -> Style
getSlStyle wenv config = slStyle where
  theme = collectTheme wenv L.selectListItemSelectedStyle
  style = fromJust (Just theme <> _slcItemSelectedStyle config)
  slStyle = style
    & L.basic .~ style ^. L.focus

getHlStyle :: WidgetEnv s e -> SelectListCfg s e a -> Style
getHlStyle wenv config = hlStyle where
  theme = collectTheme wenv L.selectListItemStyle
  style = fromJust (Just theme <> _slcItemStyle config)
  hlStyle = style
    & L.basic .~ style ^. L.focus

makeItemsList
  :: (Eq a, WidgetEvent e)
  => WidgetEnv s e
  -> Seq a
  -> MakeRow s e a
  -> SelectListCfg s e a
  -> WidgetId
  -> a
  -> WidgetNode s e
makeItemsList wenv items makeRow config widgetId selected = itemsList where
  normalTheme = collectTheme wenv L.selectListItemStyle
  normalStyle = fromJust (Just normalTheme <> _slcItemStyle config)
  makeItem idx item = newItem where
    clickCfg = onClickReq $ SendMessage widgetId (OnClickMessage idx)
    itemCfg = [expandContent, clickCfg]
    content = makeRow item
    newItem = box_ itemCfg (content & L.info . L.style .~ normalStyle)
  itemsList = vstack $ Seq.mapWithIndex makeItem items
