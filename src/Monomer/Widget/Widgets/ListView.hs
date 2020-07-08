{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widget.Widgets.ListView (listView) where

import Debug.Trace

import Control.Applicative ((<|>))
import Control.Lens (Lens', (&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Foldable (find)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
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

newtype ListView = ListView {
  _highlighted :: Int
}

newtype ClickMessage = ClickMessage Int deriving Typeable

listView :: (Traversable t, Eq a) => Lens' s a -> t a -> (a -> Text) -> WidgetInstance s e
listView field items itemToText = makeInstance (makeListView newStatus field newItems itemToText) where
  newItems = foldl' (|>) Empty items
  newStatus = ListView 0

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "listView" widget) {
  _instanceFocusable = True
}

makeListView :: (Eq a) => ListView -> Lens' s a -> Seq a -> (a -> Text) -> Widget s e
makeListView state field items itemToText = createContainer {
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
      selected = _wcApp wctx ^. field
      newInstance = (makeInstance $ makeListView newState field items itemToText) {
        _instanceChildren = Seq.singleton $ makeScrollableList ctx field items selected (_highlighted newState) itemToText
      }

    init wctx ctx widgetInstance = resultWidget $ createListView wctx ctx state

    getState = makeState state

    merge wctx ctx oldState newInstance = createListView wctx ctx newState where
      newState = fromMaybe state (useState oldState)

    handleEvent wctx ctx evt widgetInstance = case evt of
      KeyAction mode code status
        | isKeyDown code && status == KeyPressed -> handleSelectNext wctx ctx
        | isKeyUp code && status == KeyPressed -> handleSelectPrev wctx ctx
        | isKeyReturn code && status == KeyPressed -> Just $ selectItem wctx ctx (_highlighted state)
      _ -> Nothing

    handleSelectNext wctx ctx = Just $ resultWidget newInstance where
      tempIdx = _highlighted state
      nextIdx = if tempIdx < length items - 1 then tempIdx + 1 else tempIdx
      newInstance = createListView wctx ctx $ ListView nextIdx

    handleSelectPrev wctx ctx = Just $ resultWidget newInstance where
      tempIdx = _highlighted state
      nextIdx = if tempIdx > 0 then tempIdx - 1 else tempIdx
      newInstance = createListView wctx ctx $ ListView nextIdx

--    handleMessage wctx ctx message widgetInstance = case cast message of
--      Just (ClickMessage idx) -> selectItem wctx ctx idx
--      Nothing -> Nothing

    handleMessage wctx ctx message widgetInstance = fmap handleSelect (cast message) where
      handleSelect (ClickMessage idx) = selectItem wctx ctx idx

    selectItem wctx ctx idx = resultReqs requests newInstance where
      selected = _wcApp wctx ^. field
      value = fromMaybe selected (Seq.lookup idx items)
      requests = [UpdateUserState $ \model -> model & field .~ value]
      newInstance = createListView wctx ctx $ ListView idx

    preferredSize renderer wctx childrenPairs = Node sizeReq childrenReqs where
      childrenReqs = fmap snd childrenPairs
      sizeReq = nodeValue $ Seq.index childrenReqs 0

    resize wctx viewport renderArea widgetInstance childrenPairs = (widgetInstance, assignedArea) where
      assignedArea = Seq.singleton (viewport, renderArea)

makeScrollableList :: (Eq a) => PathContext -> Lens' s a -> Seq a -> a -> Int -> (a -> Text) -> WidgetInstance s e
makeScrollableList ctx field items selected highlightedIdx itemToText = scroll makeGrid where
  path = _pathCurrent ctx
  isSelected item = item == selected
  selectedColor item = if isSelected item then Just gray else Nothing
  highlightedColor idx = if idx == highlightedIdx then Just darkGray else Nothing
  pairs = Seq.zip (Seq.fromList [0..length items]) items
  makeGrid = vstack $ fmap (uncurry makeItem) pairs
  makeItem idx item = container (config idx item) $ label (itemToText item)
  config idx item = def {
    _ctOnClickReq = [SendMessage path (ClickMessage idx)],
    _ctBgColor = highlightedColor idx <|> selectedColor item,
    _ctHoverColor = Just lightGray
  }
