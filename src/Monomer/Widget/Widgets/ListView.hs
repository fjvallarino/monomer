{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widget.Widgets.ListView (ListViewConfig(..), listView, listView_) where

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
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.Container
import Monomer.Widget.Widgets.Label
import Monomer.Widget.Widgets.Scroll
import Monomer.Widget.Widgets.Spacer
import Monomer.Widget.Widgets.Stack

data ListViewConfig s e a = ListViewConfig {
  _lvValue :: WidgetValue s a,
  _lvOnChange :: [Int -> a -> e],
  _lvOnChangeReq :: [WidgetRequest s]
}

newtype ListViewState = ListViewState {
  _highlighted :: Int
}

newtype ListViewMessage = ClickMessage Int deriving Typeable

listView :: (Traversable t, Eq a) => ALens' s a -> t a -> (a -> Text) -> WidgetInstance s e
listView field items itemToText = listView_ config items itemToText where
  config = ListViewConfig (WidgetLens field) [] []

listView_ :: (Traversable t, Eq a) => ListViewConfig s e a -> t a -> (a -> Text) -> WidgetInstance s e
listView_ config items itemToText = makeInstance (makeListView config newState newItems itemToText) where
  newItems = foldl' (|>) Empty items
  newState = ListViewState 0

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "listView" widget) {
  _instanceFocusable = True
}

makeListView :: (Eq a) => ListViewConfig s e a -> ListViewState -> Seq a -> (a -> Text) -> Widget s e
makeListView config state items itemToText = createContainer {
    _widgetInit = init,
    _widgetGetState = getState,
    _widgetMerge = containerMergeTrees merge,
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetHandleMessage = containerHandleMessage handleMessage,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize
  }
  where
    createListView wctx ctx newState = newInstance where
      selected = widgetValueGet (_wcApp wctx) (_lvValue config)
      itemsList = makeItemsList ctx items selected (_highlighted newState) itemToText
      newInstance = (makeInstance $ makeListView config newState items itemToText) {
        _instanceChildren = Seq.singleton (scroll itemsList)
      }

    init wctx ctx widgetInstance = resultWidget $ createListView wctx ctx state

    getState = makeState state

    merge wctx ctx oldState newInstance = createListView wctx ctx newState where
      newState = fromMaybe state (useState oldState)

    handleEvent wctx ctx evt widgetInstance = case evt of
      KeyAction mode code status
        | isKeyDown code && status == KeyPressed -> handleHighlightNext wctx ctx widgetInstance
        | isKeyUp code && status == KeyPressed -> handleHighlightPrev wctx ctx widgetInstance
        | isKeyReturn code && status == KeyPressed -> Just $ selectItem wctx ctx widgetInstance (_highlighted state)
      _ -> Nothing

    handleHighlightNext wctx ctx widgetInstance = highlightItem wctx ctx widgetInstance nextIdx where
      tempIdx = _highlighted state
      nextIdx = if tempIdx < length items - 1 then tempIdx + 1 else tempIdx

    handleHighlightPrev wctx ctx widgetInstance = highlightItem wctx ctx widgetInstance nextIdx where
      tempIdx = _highlighted state
      nextIdx = if tempIdx > 0 then tempIdx - 1 else tempIdx

    handleMessage wctx ctx message widgetInstance = fmap handleSelect (cast message) where
      handleSelect (ClickMessage idx) = selectItem wctx ctx widgetInstance idx

    highlightItem wctx ctx widgetInstance nextIdx = Just $ widgetResult { _resultRequests = requests } where
      newState = ListViewState nextIdx
      newWidget = makeListView config newState items itemToText
      -- ListView's merge uses the old widget's state. Since we want the newly created state, the old widget is replaced here
      oldInstance = widgetInstance {
        _instanceWidget = newWidget
      }
      -- ListView's tree will be rebuilt in merge, before merging its children, so it does not matter what we currently have
      newInstance = oldInstance
      widgetResult = _widgetMerge newWidget wctx ctx newInstance oldInstance
      scrollToReq = itemScrollTo ctx widgetInstance nextIdx
      requests = Seq.fromList scrollToReq

    selectItem wctx ctx widgetInstance idx = resultReqs requests newInstance where
      selected = widgetValueGet (_wcApp wctx) (_lvValue config)
      value = fromMaybe selected (Seq.lookup idx items)
      valueSetReq = widgetValueSet (_lvValue config) value
      scrollToReq = itemScrollTo ctx widgetInstance idx
      requests = valueSetReq ++ scrollToReq
      newState = ListViewState idx
      newInstance = widgetInstance {
        _instanceWidget = makeListView config newState items itemToText
      }

    itemScrollTo ctx widgetInstance idx = maybeToList (fmap makeScrollReq renderArea) where
      lookup idx inst = Seq.lookup idx (_instanceChildren inst)
      renderArea = fmap _instanceRenderArea $ pure widgetInstance
        >>= lookup 0 -- scroll
        >>= lookup 0 -- vstack
        >>= lookup idx -- item
      scrollPath = currentPath $ childContext ctx
      makeScrollReq rect = SendMessage scrollPath (ScrollTo rect)

    preferredSize renderer wctx widgetInstance childrenPairs = Node sizeReq childrenReqs where
      childrenReqs = fmap snd childrenPairs
      sizeReq = nodeValue $ Seq.index childrenReqs 0

    resize wctx viewport renderArea widgetInstance childrenPairs = (widgetInstance, assignedArea) where
      assignedArea = Seq.singleton (viewport, renderArea)

makeItemsList :: (Eq a) => PathContext -> Seq a -> a -> Int -> (a -> Text) -> WidgetInstance s e
makeItemsList ctx items selected highlightedIdx itemToText = makeItemsList where
  path = _pathCurrent ctx
  isSelected item = item == selected
  selectedColor item = if isSelected item then Just gray else Nothing
  highlightedColor idx = if idx == highlightedIdx then Just darkGray else Nothing
  pairs = Seq.zip (Seq.fromList [0..length items]) items
  makeItemsList = vstack $ fmap (uncurry makeItem) pairs
  makeItem idx item = container (config idx item) $ label (itemToText item)
  config idx item = def {
    _ctOnClickReq = [SendMessage path (ClickMessage idx)],
    _ctBgColor = highlightedColor idx <|> selectedColor item,
    _ctHoverColor = Just lightGray
  }
