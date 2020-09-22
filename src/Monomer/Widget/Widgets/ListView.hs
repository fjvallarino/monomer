{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.ListView (
  ListViewCfg(..),
  listView,
  listView_,
  listViewCfg
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^#), (#~), (.~), (?~), non)
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
  _lvcValue :: WidgetValue s a,
  _lvcItems :: Seq a,
  _lvcItemToText :: a -> Text,
  _lvcOnChange :: [Int -> a -> e],
  _lvcOnChangeReq :: [Int -> WidgetRequest s],
  _lvcSelectedStyle :: StyleState,
  _lvcHighlightedStyle :: StyleState,
  _lvcHoverStyle :: StyleState
}

newtype ListViewState = ListViewState {
  _highlighted :: Int
}

newtype ListViewMessage
  = OnClickMessage Int
  deriving Typeable

listViewCfg
  :: WidgetValue s a -> Seq a -> (a -> Text) -> ListViewCfg s e a
listViewCfg value items itemToText = ListViewCfg {
  _lvcValue = value,
  _lvcItems = items,
  _lvcItemToText = itemToText,
  _lvcOnChange = [],
  _lvcOnChangeReq = [],
  _lvcSelectedStyle = bgColor gray,
  _lvcHighlightedStyle = border 1 darkGray,
  _lvcHoverStyle = bgColor lightGray
}

listView
  :: (Traversable t, Eq a)
  => ALens' s a -> t a -> (a -> Text) -> WidgetInstance s e
listView field items itemToText = listView_ config where
  config = listViewCfg (WidgetLens field) newItems itemToText
  newItems = foldl' (|>) Empty items

listView_ :: (Eq a) => ListViewCfg s e a -> WidgetInstance s e
listView_ config = makeInstance (makeListView config newState) where
  newState = ListViewState 0

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "listView" widget) {
  _wiFocusable = True
}

makeListView :: (Eq a) => ListViewCfg s e a -> ListViewState -> Widget s e
makeListView config state = widget where
  widget = createContainer def {
    containerInit = init,
    containerGetState = makeState state,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  currentValue wenv = widgetValueGet (_weModel wenv) (_lvcValue config)

  createListView wenv newState widgetInst = newInstance where
    selected = currentValue wenv
    path = _wiPath widgetInst
    itemsList = makeItemsList config path selected (_highlighted newState)
    newInstance = widgetInst {
      _wiWidget = makeListView config newState,
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
      | tempIdx < length (_lvcItems config) - 1 = tempIdx + 1
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
    newWidget = makeListView config newState
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
    value = fromMaybe selected (Seq.lookup idx (_lvcItems config))
    valueSetReq = widgetValueSet (_lvcValue config) value
    scrollToReq = itemScrollTo widgetInst idx
    changeReqs = fmap ($ idx) (_lvcOnChangeReq config)
    focusReq = [SetFocus $ _wiPath widgetInst]
    requests = valueSetReq ++ scrollToReq ++ changeReqs ++ focusReq
    newState = ListViewState idx
    newInstance = widgetInst {
      _wiWidget = makeListView config newState
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
  :: (Eq a) => ListViewCfg s e a -> Path -> a -> Int -> WidgetInstance s e
makeItemsList lvConfig lvPath selected highlightedIdx = itemsList where
  ListViewCfg{..} = lvConfig
  isSelected item = item == selected
  selectedStyle item
    | isSelected item = Just _lvcSelectedStyle
    | otherwise = Nothing
  highlightedStyle idx
    | idx == highlightedIdx = Just _lvcHighlightedStyle
    | otherwise = Nothing
  itemStyle idx item = def
    & S.basic .~ (selectedStyle item <|> highlightedStyle idx)
    & S.hover ?~ _lvcHoverStyle
  makeItem idx item = newItem where
    config = onClickReq $ SendMessage lvPath (OnClickMessage idx)
    content = label (_lvcItemToText item)
    newItem = box config content & C.style .~ itemStyle idx item
  pairs = Seq.zip (Seq.fromList [0..length _lvcItems]) _lvcItems
  itemsList = vstack $ fmap (uncurry makeItem) pairs
