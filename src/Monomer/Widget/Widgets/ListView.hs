{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.ListView (
  ListViewConfig(..),
  listView,
  listView_,
  listViewConfig
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^#), (#~))
import Control.Monad
import Data.Default
import Data.Foldable (find)
import Data.List (foldl')
import Data.Maybe (fromMaybe, maybeToList)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Traversable
import Data.Typeable (Typeable, cast)

import qualified Data.Map as M
import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Graphics.Color
import Monomer.Graphics.Drawing
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types
import Monomer.Widget.BaseContainer
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.Container
import Monomer.Widget.Widgets.Label
import Monomer.Widget.Widgets.Scroll
import Monomer.Widget.Widgets.Spacer
import Monomer.Widget.Widgets.Stack

data ListViewConfig s e a = ListViewConfig {
  _lvValue :: WidgetValue s a,
  _lvItems :: Seq a,
  _lvItemToText :: a -> Text,
  _lvOnChange :: [Int -> a -> e],
  _lvOnChangeReq :: [Int -> WidgetRequest s],
  _lvSelectedColor :: Color,
  _lvHighlightedColor :: Color,
  _lvHoverColor :: Color
}

newtype ListViewState = ListViewState {
  _highlighted :: Int
}

newtype ListViewMessage = OnClickMessage Int deriving Typeable

listViewConfig
  :: WidgetValue s a -> Seq a -> (a -> Text) -> ListViewConfig s e a
listViewConfig value items itemToText = ListViewConfig {
  _lvValue = value,
  _lvItems = items,
  _lvItemToText = itemToText,
  _lvOnChange = [],
  _lvOnChangeReq = [],
  _lvSelectedColor = gray,
  _lvHighlightedColor = darkGray,
  _lvHoverColor = lightGray
}

listView
  :: (Traversable t, Eq a)
  => ALens' s a -> t a -> (a -> Text) -> WidgetInstance s e
listView field items itemToText = listView_ config where
  config = listViewConfig (WidgetLens field) newItems itemToText
  newItems = foldl' (|>) Empty items

listView_ :: (Eq a) => ListViewConfig s e a -> WidgetInstance s e
listView_ config = makeInstance (makeListView config newState) where
  newState = ListViewState 0

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "listView" widget) {
  _instanceFocusable = True
}

makeListView :: (Eq a) => ListViewConfig s e a -> ListViewState -> Widget s e
makeListView config state = widget where
  widget = createContainer {
    _widgetInit = containerInit init,
    _widgetGetState = makeState state,
    _widgetMerge = containerMergeTrees merge,
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetHandleMessage = containerHandleMessage handleMessage,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize
  }

  currentValue wenv = widgetValueGet (_weModel wenv) (_lvValue config)

  createListView wenv newState widgetInst = newInstance where
    selected = currentValue wenv
    path = _instancePath widgetInst
    itemsList = makeItemsList config path selected (_highlighted newState)
    newInstance = widgetInst {
      _instanceWidget = makeListView config newState,
      _instanceChildren = Seq.singleton (scroll itemsList)
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
      | tempIdx < length (_lvItems config) - 1 = tempIdx + 1
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
      _instanceWidget = newWidget
    }
    -- ListView's tree will be rebuilt in merge, before merging its children,
    -- so it does not matter what we currently have
    newInstance = oldInstance
    widgetResult = _widgetMerge newWidget wenv oldInstance newInstance
    scrollToReq = itemScrollTo widgetInst nextIdx
    requests = Seq.fromList scrollToReq
    result = Just $ widgetResult { _wrRequests = requests }

  selectItem wenv widgetInst idx = resultReqs requests newInstance where
    selected = currentValue wenv
    value = fromMaybe selected (Seq.lookup idx (_lvItems config))
    valueSetReq = widgetValueSet (_lvValue config) value
    scrollToReq = itemScrollTo widgetInst idx
    changeReqs = fmap ($ idx) (_lvOnChangeReq config)
    focusReq = [SetFocus $ _instancePath widgetInst]
    requests = valueSetReq ++ scrollToReq ++ changeReqs ++ focusReq
    newState = ListViewState idx
    newInstance = widgetInst {
      _instanceWidget = makeListView config newState
    }

  itemScrollTo widgetInst idx = maybeToList (fmap scrollReq renderArea) where
    lookup idx inst = Seq.lookup idx (_instanceChildren inst)
    renderArea = fmap _instanceRenderArea $ pure widgetInst
      >>= lookup 0 -- scroll
      >>= lookup 0 -- vstack
      >>= lookup idx -- item
    scrollPath = firstChildPath widgetInst
    scrollReq rect = SendMessage scrollPath (ScrollTo rect)

  preferredSize wenv widgetInst children reqs = Node sizeReq reqs where
    sizeReq = nodeValue $ Seq.index reqs 0

  resize wenv viewport renderArea children reqs widgetInst = resized where
    assignedArea = Seq.singleton (viewport, renderArea)
    resized = (widgetInst, assignedArea)

makeItemsList
  :: (Eq a) => ListViewConfig s e a -> Path -> a -> Int -> WidgetInstance s e
makeItemsList lvConfig lvPath selected highlightedIdx = itemsList where
  ListViewConfig{..} = lvConfig
  isSelected item = item == selected
  selectedColor item
    | isSelected item = Just _lvSelectedColor
    | otherwise = Nothing
  highlightedColor idx
    | idx == highlightedIdx = Just _lvHighlightedColor
    | otherwise = Nothing
  itemStyle idx item = def {
    _styleColor = selectedColor item <|> highlightedColor idx,
    _styleHover = Just _lvHoverColor
  }
  itemConfig idx = containerConfig {
    _ctOnClickReq = [SendMessage lvPath (OnClickMessage idx)]
  }
  makeItem idx item = container config content `style` itemStyle idx item where
    config = itemConfig idx
    content = label (_lvItemToText item)
  pairs = Seq.zip (Seq.fromList [0..length _lvItems]) _lvItems
  itemsList = vstack $ fmap (uncurry makeItem) pairs
