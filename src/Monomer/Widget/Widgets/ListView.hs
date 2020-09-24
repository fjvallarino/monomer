{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.ListView (
  ListViewCfg(..),
  textListView,
  textListView_,
  listView,
  listView_,
  listViewF_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (.~))
import Data.Default
import Data.List (foldl')
import Data.Maybe (fromMaybe, maybeToList)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import qualified Data.Sequence as Seq

import Monomer.Common.Style
import Monomer.Common.StyleCombinators
import Monomer.Common.Tree
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Graphics.Color
import Monomer.Widget.BaseContainer
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.Box
import Monomer.Widget.Widgets.Label
import Monomer.Widget.Widgets.Scroll
import Monomer.Widget.Widgets.Spacer
import Monomer.Widget.Widgets.Stack
import Monomer.Widget.Widgets.WidgetCombinators

import qualified Monomer.Common.LensStyle as S
import qualified Monomer.Widget.LensCore as C

data ListViewCfg s e a = ListViewCfg {
  _lvcOnChangeIdx :: [Int -> a -> e],
  _lvcOnChangeReqIdx :: [Int -> WidgetRequest s],
  _lvcSelectedStyle :: Maybe StyleState,
  _lvcHighlightedStyle :: Maybe StyleState,
  _lvcHoverStyle :: Maybe StyleState
}

instance Default (ListViewCfg s e a) where
  def = ListViewCfg {
    _lvcOnChangeIdx = [],
    _lvcOnChangeReqIdx = [],
    _lvcSelectedStyle = Just $ bgColor gray,
    _lvcHighlightedStyle = Just $ border 1 darkGray,
    _lvcHoverStyle = Just $ bgColor lightGray
  }

instance Semigroup (ListViewCfg s e a) where
  (<>) t1 t2 = ListViewCfg {
    _lvcOnChangeIdx = _lvcOnChangeIdx t1 <> _lvcOnChangeIdx t2,
    _lvcOnChangeReqIdx = _lvcOnChangeReqIdx t1 <> _lvcOnChangeReqIdx t2,
    _lvcSelectedStyle = _lvcSelectedStyle t2 <|> _lvcSelectedStyle t1,
    _lvcHighlightedStyle = _lvcHighlightedStyle t2 <|> _lvcHighlightedStyle t1,
    _lvcHoverStyle = _lvcHoverStyle t2 <|> _lvcHoverStyle t1
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

instance HighlightedStyle (ListViewCfg s e a) where
  highlightedStyle style = def {
    _lvcHighlightedStyle = Just style
  }

instance HoverStyle (ListViewCfg s e a) where
  hoverStyle style = def {
    _lvcHoverStyle = Just style
  }

newtype ListViewState = ListViewState {
  _highlighted :: Int
}

newtype ListViewMessage
  = OnClickMessage Int
  deriving Typeable

textListView
  :: (Traversable t, Eq a)
  => ALens' s a
  -> t a
  -> (a -> Text)
  -> WidgetInstance s e
textListView field items itemDesc = textListView_ field items itemDesc def

textListView_
  :: (Traversable t, Eq a)
  => ALens' s a
  -> t a
  -> (a -> Text)
  -> ListViewCfg s e a
  -> WidgetInstance s e
textListView_ field items itemDesc config = inst where
  makeRow item = label (itemDesc item)
  inst = listView_ field items makeRow config

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
  -> ListViewCfg s e a
  -> WidgetInstance s e
listView_ field items makeRow config = newInst where
  value = WidgetLens field
  newInst = listViewF_ value items makeRow config

listViewF_
  :: (Traversable t, Eq a)
  => WidgetValue s a
  -> t a
  -> (a -> WidgetInstance s e)
  -> ListViewCfg s e a
  -> WidgetInstance s e
listViewF_ value items makeRow config = makeInstance widget where
  newItems = foldl' (|>) Empty items
  newState = ListViewState 0
  widget = makeListView value newItems makeRow config newState

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "listView" widget) {
  _wiFocusable = True
}

makeListView
  :: (Eq a)
  => WidgetValue s a
  -> Seq a
  -> (a -> WidgetInstance s e)
  -> ListViewCfg s e a
  -> ListViewState
  -> Widget s e
makeListView field items makeRow config state = widget where
  widget = createContainer def {
    containerInit = init,
    containerGetState = makeState state,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  currentValue wenv = widgetValueGet (_weModel wenv) field

  createListView wenv newState widgetInst = newInstance where
    selected = currentValue wenv
    highlighted = _highlighted newState
    path = _wiPath widgetInst
    itemsList = makeItemsList items makeRow config path selected highlighted
    newInstance = widgetInst {
      _wiWidget = makeListView field items makeRow config newState,
      _wiChildren = Seq.singleton (scroll itemsList)
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
    newWidget = makeListView field items makeRow config newState
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
    valueSetReq = widgetValueSet field value
    scrollToReq = itemScrollTo widgetInst idx
    changeReqs = fmap ($ idx) (_lvcOnChangeReqIdx config)
    focusReq = [SetFocus $ _wiPath widgetInst]
    requests = valueSetReq ++ scrollToReq ++ changeReqs ++ focusReq
    newState = ListViewState idx
    newInstance = widgetInst {
      _wiWidget = makeListView field items makeRow config newState
    }

  itemScrollTo widgetInst idx = maybeToList (fmap scrollReq renderArea) where
    lookup idx inst = Seq.lookup idx (_wiChildren inst)
    renderArea = fmap _wiRenderArea $ pure widgetInst
      >>= lookup 0 -- scroll
      >>= lookup 0 -- vstack
      >>= lookup idx -- item
    scrollPath = firstChildPath widgetInst
    scrollReq rect = SendMessage scrollPath (ScrollTo rect)

  getSizeReq wenv widgetInst children = sizeReq where
    sizeReq = _wiSizeReq $ Seq.index children 0

  resize wenv viewport renderArea children widgetInst = resized where
    assignedArea = Seq.singleton (viewport, renderArea)
    resized = (widgetInst, assignedArea)

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
  highlightedStyle idx
    | idx == hlIdx = _lvcHighlightedStyle
    | otherwise = Nothing
  itemStyle idx item = def
    & S.basic .~ (selectedStyle item <|> highlightedStyle idx)
    & S.hover .~ _lvcHoverStyle
  makeItem idx item = newItem where
    itemCfg = onClickReq $ SendMessage path (OnClickMessage idx)
    content = makeRow item
    newItem = box itemCfg content & C.style .~ itemStyle idx item
  pairs = Seq.zip (Seq.fromList [0..length items]) items
  itemsList = vstack $ fmap (uncurry makeItem) pairs
