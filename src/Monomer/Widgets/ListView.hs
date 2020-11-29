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
type MakeRow s e a = a -> WidgetInstance s e

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
  :: (Traversable t, ListItem a)
  => ALens' s a
  -> t a
  -> MakeRow s e a
  -> WidgetInstance s e
listView field items makeRow = listView_ field items makeRow def

listView_
  :: (Traversable t, ListItem a)
  => ALens' s a
  -> t a
  -> MakeRow s e a
  -> [ListViewCfg s e a]
  -> WidgetInstance s e
listView_ field items makeRow configs = newInst where
  newInst = listViewD_ (WidgetLens field) items makeRow configs

listViewV
  :: (Traversable t, ListItem a)
  => a
  -> (Int -> a -> e)
  -> t a
  -> MakeRow s e a
  -> WidgetInstance s e
listViewV value handler items makeRow = newInst where
  newInst = listViewV_ value handler items makeRow def

listViewV_
  :: (Traversable t, ListItem a)
  => a
  -> (Int -> a -> e)
  -> t a
  -> MakeRow s e a
  -> [ListViewCfg s e a]
  -> WidgetInstance s e
listViewV_ value handler items makeRow configs = newInst where
  widgetData = WidgetValue value
  newConfigs = onChangeIdx handler : configs
  newInst = listViewD_ widgetData items makeRow newConfigs

listViewD_
  :: (Traversable t, ListItem a)
  => WidgetData s a
  -> t a
  -> MakeRow s e a
  -> [ListViewCfg s e a]
  -> WidgetInstance s e
listViewD_ widgetData items makeRow configs = makeInstance widget where
  config = mconcat configs
  newItems = foldl' (|>) Empty items
  newState = ListViewState newItems Nothing 0 False
  widget = makeListView widgetData newItems makeRow config newState

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = scroll_ childInst [scrollStyle L.listViewStyle] where
  childInst = (defaultWidgetInstance "listView" widget) {
    _wiFocusable = True
  }

makeListView
  :: (ListItem a)
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

  createListViewChildren wenv inst = children where
    path = _wiPath inst
    selected = currentValue wenv
    itemsList = makeItemsList wenv items makeRow config path selected
    children = Seq.singleton itemsList

  init wenv inst = resultWidget newInst where
    children = createListViewChildren wenv inst
    sel = Just $ currentValue wenv
    newState = state {
      _prevSel = sel,
      _resizeReq = True
    }
    tmpInst = inst {
      _wiWidget = makeListView widgetData items makeRow config newState,
      _wiChildren = children
    }
    newInst = updateSelStyle makeRow config Empty items Nothing sel wenv tmpInst

  mergeWrapper wenv oldModel oldInst newInst = newResult where
    sel = Just $ currentValue wenv
    oldInstState = widgetGetState (_wiWidget oldInst) wenv
    oldState = fromMaybe state (useState oldInstState)
    oldSel = _prevSel oldState
    oldItems = _prevItems oldState
    mergeRequiredFn = fromMaybe (/=) (_lvcMergeRequired config)
    mergeRequired = mergeRequiredFn oldItems items
    getBaseStyle _ _ = Nothing
    styledInst = initInstanceStyle getBaseStyle wenv newInst
    newState = oldState {
      _prevSel = sel,
      _resizeReq = mergeRequired
    }
    tempWidget = styledInst {
      _wiWidget = makeListView widgetData items makeRow config newState,
      _wiViewport = _wiViewport oldInst,
      _wiRenderArea = _wiRenderArea oldInst,
      _wiSizeReqW = _wiSizeReqW oldInst,
      _wiSizeReqH = _wiSizeReqH oldInst
    }
    children
      | mergeRequired = createListViewChildren wenv tempWidget
      | otherwise = oldInst ^. L.children
    inst2 = tempWidget & L.children .~ children
    inst3 = updateSelStyle makeRow config oldItems items oldSel sel wenv inst2
    pResult = resultWidget inst3
    newResult
      | mergeRequired = mergeChildren wenv oldModel oldInst pResult
      | otherwise = pResult

  handleEvent wenv target evt inst = case evt of
    Focus -> handleFocusChange _lvcOnFocus _lvcOnFocusReq config inst
    Blur -> result where
      isTabPressed = getKeyStatus (_weInputStatus wenv) keyTab == KeyPressed
      changeReq = isTabPressed && _lvcSelectOnBlur config == Just True
      WidgetResult tempReqs tempEvts tempInst
        | changeReq = selectItem wenv inst (_highlighted state)
        | otherwise = resultWidget inst
      evts = tempEvts <> Seq.fromList (_lvcOnBlur config)
      reqs = tempReqs <> Seq.fromList (_lvcOnBlurReq config)
      mergedResult = Just $ WidgetResult reqs evts tempInst
      result
        | changeReq || not (null evts && null reqs) = mergedResult
        | otherwise = Nothing
    KeyAction mode code status
      | isKeyDown code && status == KeyPressed -> highlightNext wenv inst
      | isKeyUp code && status == KeyPressed -> highlightPrev wenv inst
      | isSelectKey code && status == KeyPressed -> resultSelected
      where
        resultSelected = Just $ selectItem wenv inst (_highlighted state)
        isSelectKey code = isKeyReturn code || isKeySpace code
    _ -> Nothing

  highlightNext wenv inst = highlightItem wenv inst nextIdx where
    tempIdx = _highlighted state
    nextIdx
      | tempIdx < length items - 1 = tempIdx + 1
      | otherwise = tempIdx

  highlightPrev wenv inst = highlightItem wenv inst nextIdx where
    tempIdx = _highlighted state
    nextIdx
      | tempIdx > 0 = tempIdx - 1
      | otherwise = tempIdx

  handleMessage wenv target message inst = result where
    handleSelect (OnClickMessage idx) = handleItemClick wenv inst idx
    result = fmap handleSelect (cast message)

  handleItemClick wenv inst idx = result where
    focusReq = Seq.singleton (SetFocus $ inst ^. L.path)
    tempResult = selectItem wenv inst idx
    result
      | isFocused wenv inst = tempResult
      | otherwise = tempResult & L.requests <>~ focusReq

  highlightItem wenv inst nextIdx = Just result where
    newState = state {
      _highlighted = nextIdx
    }
    newInst = inst {
      _wiWidget = makeListView widgetData items makeRow config newState
    }
    reqs = itemScrollTo inst nextIdx
    result = resultReqs reqs newInst

  selectItem wenv inst idx = result where
    selected = currentValue wenv
    value = fromMaybe selected (Seq.lookup idx items)
    valueSetReq = widgetDataSet widgetData value
    scrollToReq = itemScrollTo inst idx
    events = fmap ($ value) (_lvcOnChange config)
      ++ fmap (\fn -> fn idx value) (_lvcOnChangeIdx config)
    changeReqs = _lvcOnChangeReq config
      ++ fmap ($ idx) (_lvcOnChangeIdxReq config)
    requests = valueSetReq ++ scrollToReq ++ changeReqs
    newState = state {
      _highlighted = idx,
      _prevSel = Just selected
    }
    newInst = inst {
      _wiWidget = makeListView widgetData items makeRow config newState
    }
    result = resultReqsEvents requests events newInst

  itemScrollTo inst idx = maybeToList (fmap scrollReq renderArea) where
    renderArea = itemRenderArea inst idx
    scrollPath =  parentPath inst
    scrollReq rect = SendMessage scrollPath (ScrollTo rect)

  itemRenderArea inst idx = renderArea where
    lookup idx inst = Seq.lookup idx (_wiChildren inst)
    renderArea = fmap _wiRenderArea $ pure inst
      >>= lookup 0 -- vstack
      >>= lookup idx -- item

  getSizeReq wenv inst children = (newSizeReqW, newSizeReqH) where
    child = Seq.index children 0
    newSizeReqW = _wiSizeReqW child
    newSizeReqH = _wiSizeReqH child

  resize wenv viewport renderArea children inst = resized where
    assignedArea = Seq.singleton (viewport, renderArea)
    resized = (inst, assignedArea)

  render renderer wenv inst =
    renderContainer defaultRender renderer wenv (buildRenderInst wenv inst)

  buildRenderInst wenv inst = newInst where
    viewport = _wiViewport inst
    hlIdx = _highlighted state
    foldItem items idx item
      | isWidgetVisible item viewport = items |> updateStyle idx item
      | otherwise = items
    updateStyle idx item
      | idx == hlIdx = setFocusedItemStyle wenv item
      | otherwise = item
    children = inst ^. L.children . ix 0 . L.children
    newChildren = Seq.foldlWithIndex foldItem Empty children
    newInst = inst & L.children . ix 0 . L.children .~ newChildren

setChildStyle
  :: Seq a
  -> MakeRow s e a
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> Int
  -> Style
  -> WidgetInstance s e
setChildStyle items makeRow wenv parent idx style = newParent where
  makeItem v = makeRow v & L.style .~ style
  newChild = fmap makeItem (Seq.lookup idx items)
  merge newItem oldItem = newWidget where
    res = widgetMerge (_wiWidget newItem) wenv (_weModel wenv) oldItem newItem
    widget = _wrWidget res
    newWidget = widget
      & L.path .~ oldItem ^. L.path
      & L.viewport .~ oldItem ^. L.viewport
      & L.renderArea .~ oldItem ^. L.renderArea
  listLens = L.children . ix 0
  boxLens = L.children . ix idx
  itemLens = L.children . ix 0
  newParent = case newChild of
    Just newInst -> parent & listLens . boxLens . itemLens %~ merge newInst
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
  -> WidgetInstance s e
  -> WidgetInstance s e
updateSelStyle makeRow cfg oldIs newIs oldSel newSel wenv inst = ins3 where
  oldIdx = oldSel >>= flip Seq.elemIndexL oldIs
  newIdx = newSel >>= flip Seq.elemIndexL newIs
  ins2 = maybe inst (setDefChildStyle oldIs makeRow cfg wenv inst) oldIdx
  ins3 = maybe ins2 (setSelChildStyle newIs makeRow cfg wenv ins2) newIdx

setDefChildStyle
  :: Seq a
  -> MakeRow s e a
  -> ListViewCfg s e a
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> Int
  -> WidgetInstance s e
setDefChildStyle items makeRow config wenv parent idx = newParent where
  normalTheme = collectTheme wenv L.listViewItemStyle
  normalStyle = fromJust (Just normalTheme <> _lvcItemStyle config)
  newParent = setChildStyle items makeRow wenv parent idx normalStyle

setSelChildStyle
  :: Seq a
  -> MakeRow s e a
  -> ListViewCfg s e a
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> Int
  -> WidgetInstance s e
setSelChildStyle items makeRow config wenv parent idx = newParent where
  selectedTheme = collectTheme wenv L.listViewItemSelectedStyle
  selectedStyleCfg = _lvcItemSelectedStyle config
  selectedStyle = fromJust (Just selectedTheme <> selectedStyleCfg)
  newParent = setChildStyle items makeRow wenv parent idx selectedStyle

setFocusedItemStyle :: WidgetEnv s e -> WidgetInstance s e -> WidgetInstance s e
setFocusedItemStyle wenv item
  | isHovered wenv item = item & hoverLens .~ (hoverStyle <> focusStyle)
  | otherwise = item & basicLens .~ focusStyle
  where
    basicLens = L.children . ix 0 . L.style . L.basic
    hoverLens = L.children . ix 0 . L.style . L.hover
    hoverStyle = item ^. L.children . ix 0 . L.style . L.hover
    focusStyle = item ^. L.children . ix 0 . L.style . L.focus

makeItemsList
  :: (Eq a)
  => WidgetEnv s e
  -> Seq a
  -> MakeRow s e a
  -> ListViewCfg s e a
  -> Path
  -> a
  -> WidgetInstance s e
makeItemsList wenv items makeRow config path selected = itemsList where
  normalTheme = collectTheme wenv L.listViewItemStyle
  normalStyle = fromJust (Just normalTheme <> _lvcItemStyle config)
  makeItem idx item = newItem where
    clickCfg = onClickReq $ SendMessage path (OnClickMessage idx)
    itemCfg = [expandContent, clickCfg]
    content = makeRow item
    newItem = box_ (content & L.style .~ normalStyle) itemCfg
  itemsList = vstack $ Seq.mapWithIndex makeItem items
