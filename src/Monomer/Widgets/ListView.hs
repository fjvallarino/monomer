{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.ListView (
  ListViewCfg,
  listView,
  listView_,
  listViewV,
  listViewV_,
  listViewD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (.~))
import Control.Monad (when)
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import qualified Data.Sequence as Seq

import Monomer.Core.BaseContainer
import Monomer.Core.BasicTypes
import Monomer.Core.Style
import Monomer.Core.StyleCombinators
import Monomer.Core.Types
import Monomer.Core.Util
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Graphics.Color
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widgets.Box
import Monomer.Widgets.Label
import Monomer.Widgets.Scroll
import Monomer.Widgets.Spacer
import Monomer.Widgets.Stack
import Monomer.Widgets.WidgetCombinators

import qualified Monomer.Core.LensStyle as S
import qualified Monomer.Core.LensCore as C

data ListViewCfg s e a = ListViewCfg {
  _lvcOnChangeIdx :: [Int -> a -> e],
  _lvcOnChangeReqIdx :: [Int -> WidgetRequest s],
  _lvcSelectedStyle :: Maybe StyleState,
  _lvcHoverStyle :: Maybe StyleState,
  _lvcHighlightedColor :: Maybe Color
}

instance Default (ListViewCfg s e a) where
  def = ListViewCfg {
    _lvcOnChangeIdx = [],
    _lvcOnChangeReqIdx = [],
    _lvcSelectedStyle = Just $ bgColor gray,
    _lvcHoverStyle = Just $ bgColor darkGray,
    _lvcHighlightedColor = Just lightGray
  }

instance Semigroup (ListViewCfg s e a) where
  (<>) t1 t2 = ListViewCfg {
    _lvcOnChangeIdx = _lvcOnChangeIdx t1 <> _lvcOnChangeIdx t2,
    _lvcOnChangeReqIdx = _lvcOnChangeReqIdx t1 <> _lvcOnChangeReqIdx t2,
    _lvcSelectedStyle = _lvcSelectedStyle t2 <|> _lvcSelectedStyle t1,
    _lvcHoverStyle = _lvcHoverStyle t2 <|> _lvcHoverStyle t1,
    _lvcHighlightedColor = _lvcHighlightedColor t2 <|> _lvcHighlightedColor t1
  }

instance Monoid (ListViewCfg s e a) where
  mempty = def

instance OnChangeIdx (ListViewCfg s e a) a e where
  onChangeIdx fn = def {
    _lvcOnChangeIdx = [fn]
  }

instance OnChangeReqIdx (ListViewCfg s e a) s where
  onChangeReqIdx req = def {
    _lvcOnChangeReqIdx = [req]
  }

instance SelectedStyle (ListViewCfg s e a) where
  selectedStyle style = def {
    _lvcSelectedStyle = Just style
  }

instance HoverStyle (ListViewCfg s e a) where
  hoverStyle style = def {
    _lvcHoverStyle = Just style
  }

instance HighlightedColor (ListViewCfg s e a) where
  highlightedColor color = def {
    _lvcHighlightedColor = Just color
  }

newtype ListViewState = ListViewState {
  _highlighted :: Int
}

newtype ListViewMessage
  = OnClickMessage Int
  deriving Typeable

listView
  :: (Traversable t, Eq a)
  => ALens' s a
  -> t a
  -> (a -> WidgetInstance s e)
  -> WidgetInstance s e
listView field items makeRow = listView_ field items makeRow def

listView_
  :: (Traversable t, Eq a)
  => ALens' s a
  -> t a
  -> (a -> WidgetInstance s e)
  -> [ListViewCfg s e a]
  -> WidgetInstance s e
listView_ field items makeRow configs = newInst where
  newInst = listViewD_ (WidgetLens field) items makeRow configs

listViewV
  :: (Traversable t, Eq a)
  => a
  -> (Int -> a -> e)
  -> t a
  -> (a -> WidgetInstance s e)
  -> WidgetInstance s e
listViewV value handler items makeRow = newInst where
  newInst = listViewV_ value handler items makeRow def

listViewV_
  :: (Traversable t, Eq a)
  => a
  -> (Int -> a -> e)
  -> t a
  -> (a -> WidgetInstance s e)
  -> [ListViewCfg s e a]
  -> WidgetInstance s e
listViewV_ value handler items makeRow configs = newInst where
  widgetData = WidgetValue value
  newConfigs = onChangeIdx handler : configs
  newInst = listViewD_ widgetData items makeRow newConfigs

listViewD_
  :: (Traversable t, Eq a)
  => WidgetData s a
  -> t a
  -> (a -> WidgetInstance s e)
  -> [ListViewCfg s e a]
  -> WidgetInstance s e
listViewD_ widgetData items makeRow configs = makeInstance widget where
  config = mconcat configs
  newItems = foldl' (|>) Empty items
  newState = ListViewState 0
  widget = makeListView widgetData newItems makeRow config newState

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = scroll $ (defaultWidgetInstance "listView" widget) {
  _wiFocusable = True
}

makeListView
  :: (Eq a)
  => WidgetData s a
  -> Seq a
  -> (a -> WidgetInstance s e)
  -> ListViewCfg s e a
  -> ListViewState
  -> Widget s e
makeListView widgetData items makeRow config state = widget where
  baseWidget = createContainer def {
    containerInit = init,
    containerGetState = makeState state,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetRender = render
  }

  currentValue wenv = widgetDataGet (_weModel wenv) widgetData

  createListView wenv newState widgetInst = newInstance where
    selected = currentValue wenv
    highlighted = _highlighted newState
    path = _wiPath widgetInst
    itemsList = makeItemsList items makeRow config path selected highlighted
    newInstance = widgetInst {
      _wiWidget = makeListView widgetData items makeRow config newState,
      _wiChildren = Seq.singleton itemsList
    }

  init wenv widgetInst = resultWidget $ createListView wenv state widgetInst

  merge wenv oldState newInstance = result where
    newState = fromMaybe state (useState oldState)
    result = resultWidget $ createListView wenv newState newInstance

  handleEvent wenv target evt widgetInst = case evt of
    KeyAction mode code status
      | isKeyDown code && status == KeyPressed -> highlightNext wenv widgetInst
      | isKeyUp code && status == KeyPressed -> highlightPrev wenv widgetInst
      | isSelectKey code && status == KeyPressed -> resultSelected
      where
        resultSelected = Just $ selectItem wenv widgetInst (_highlighted state)
        isSelectKey code = isKeyReturn code || isKeySpace code
    _ -> Nothing

  highlightNext wenv widgetInst = highlightItem wenv widgetInst nextIdx where
    tempIdx = _highlighted state
    nextIdx
      | tempIdx < length items - 1 = tempIdx + 1
      | otherwise = tempIdx

  highlightPrev wenv widgetInst = highlightItem wenv widgetInst nextIdx where
    tempIdx = _highlighted state
    nextIdx
      | tempIdx > 0 = tempIdx - 1
      | otherwise = tempIdx

  handleMessage wenv target message widgetInst = result where
    handleSelect (OnClickMessage idx) = selectItem wenv widgetInst idx
    result = fmap handleSelect (cast message)

  highlightItem wenv widgetInst nextIdx = result where
    newState = ListViewState nextIdx
    newWidget = makeListView widgetData items makeRow config newState
    -- ListView's merge uses the old widget's state. Since we want the newly
    -- created state, the old widget is replaced here
    oldInstance = widgetInst {
      _wiWidget = newWidget
    }
    -- ListView's tree will be rebuilt in merge, before merging its children,
    -- so it does not matter what we currently have
    newInstance = oldInstance
    widgetResult = widgetMerge newWidget wenv oldInstance newInstance
    scrollToReq = itemScrollTo widgetInst nextIdx
    requests = Seq.fromList scrollToReq
    result = Just $ widgetResult {
      _wrRequests = requests,
      _wrWidget = resizeInstance wenv (_wrWidget widgetResult)
    }

  selectItem wenv widgetInst idx = resultReqs requests newInstance where
    selected = currentValue wenv
    value = fromMaybe selected (Seq.lookup idx items)
    valueSetReq = widgetDataSet widgetData value
    scrollToReq = itemScrollTo widgetInst idx
    changeReqs = fmap ($ idx) (_lvcOnChangeReqIdx config)
    focusReq = [SetFocus $ _wiPath widgetInst]
    requests = valueSetReq ++ scrollToReq ++ changeReqs ++ focusReq
    newState = ListViewState idx
    newInstance = widgetInst {
      _wiWidget = makeListView widgetData items makeRow config newState
    }

  itemScrollTo widgetInst idx = maybeToList (fmap scrollReq renderArea) where
    renderArea = itemRenderArea widgetInst idx
    scrollPath =  parentPath widgetInst
    scrollReq rect = SendMessage scrollPath (ScrollTo rect)

  itemRenderArea widgetInst idx = renderArea where
    lookup idx inst = Seq.lookup idx (_wiChildren inst)
    renderArea = fmap _wiRenderArea $ pure widgetInst
      >>= lookup 0 -- vstack
      >>= lookup idx -- item

  getSizeReq wenv widgetInst children = (newSizeReqW, newSizeReqH) where
    child = Seq.index children 0
    newSizeReqW = _wiSizeReqW child
    newSizeReqH = _wiSizeReqW child

  resize wenv viewport renderArea children widgetInst = resized where
    assignedArea = Seq.singleton (viewport, renderArea)
    resized = (widgetInst, assignedArea)

  render renderer wenv inst = do
    renderWrapper defaultRender renderer wenv inst

    when (isJust itemRect && isJust baseColor) $
      drawRectBorder renderer (fromJust itemRect) itemBorder Nothing
    where
      children = _wiChildren inst
      itemIdx = _highlighted state
      itemRect = itemRenderArea inst itemIdx
      baseColor = _lvcHighlightedColor config
      highlightColor
        | isFocused wenv inst = fromJust baseColor
        | otherwise = fromJust baseColor & alpha .~ 0.4
      bs = Just $ BorderSide 1 highlightColor
      itemBorder = Border bs bs bs bs

makeItemsList
  :: (Eq a)
  => Seq a
  -> (a -> WidgetInstance s e)
  -> ListViewCfg s e a
  -> Path
  -> a
  -> Int
  -> WidgetInstance s e
makeItemsList items makeRow config path selected hlIdx = itemsList where
  ListViewCfg{..} = config
  isSelected item = item == selected
  selectedStyle item
    | isSelected item = _lvcSelectedStyle
    | otherwise = Nothing
  itemStyle idx item = def
    & S.basic .~ selectedStyle item
    & S.hover .~ _lvcHoverStyle
  makeItem idx item = newItem where
    clickCfg = onClickReq $ SendMessage path (OnClickMessage idx)
    itemCfg = [expandContent, clickCfg]
    content = makeRow item
    newItem = box_ (content & C.style .~ itemStyle idx item) itemCfg
  pairs = Seq.zip (Seq.fromList [0..length items]) items
  itemsList = vstack $ fmap (uncurry makeItem) pairs
