{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Dropdown (dropdown) where

import Debug.Trace

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^#), (#~))
import Control.Monad
import Data.Default
import Data.Foldable (find)
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Traversable

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
import Monomer.Widget.Widgets.ListView

data DropdownConfig s e a = DropdownConfig {
  _ddValue :: WidgetValue s a,
  _ddOnChange :: [e],
  _ddOnChangeReq :: [WidgetRequest s]
}

newtype DropdownState = DropdownState {
  _isOpen :: Bool
}

dropdown :: (Traversable t, Eq a) => ALens' s a -> t a -> (a -> Text) -> WidgetInstance s e
dropdown field items itemToText = dropdown_ config items itemToText where
  config = DropdownConfig (WidgetLens field) [] []

dropdown_ :: (Traversable t, Eq a) => DropdownConfig s e a -> t a -> (a -> Text) -> WidgetInstance s e
dropdown_ config items itemToText = makeInstance (makeDropdown config newState newItems itemToText) where
  newItems = foldl' (|>) Empty items
  newState = DropdownState False

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "dropdown" widget) {
  _instanceFocusable = True
}

makeDropdown :: (Eq a) => DropdownConfig s e a -> DropdownState -> Seq a -> (a -> Text) -> Widget s e
makeDropdown config state items itemToText = createContainer {
    _widgetInit = init,
    _widgetGetState = getState,
    _widgetMerge = containerMergeTrees merge,
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize,
    _widgetRender = containerRender render
  }
  where
    isOpen = _isOpen state
    currentValue wctx = widgetValueGet (_wcApp wctx) (_ddValue config)

    createDropdown wctx ctx newState = newInstance where
      selected = currentValue wctx
      newInstance = (makeInstance $ makeDropdown config newState items itemToText) {
        _instanceChildren = Seq.singleton $ makeListView items selected itemToText
      }

    init wctx ctx widgetInstance = resultWidget $ createDropdown wctx ctx state

    getState = makeState state

    merge wctx ctx oldState newInstance = createDropdown wctx ctx newState where
      newState = fromMaybe state (useState oldState)

    handleEvent wctx ctx evt widgetInstance = case evt of
      Click p@(Point x y) _ status
        | clicked && openRequired p widgetInstance -> handleOpenDropdown wctx ctx
        | clicked && closeRequired p widgetInstance -> handleCloseDropdown wctx ctx
        where
          clicked = status == PressedBtn
      KeyAction mode code status
        | isKeyDown code && not isOpen -> handleOpenDropdown wctx ctx
        | isKeyEsc code && isOpen -> handleCloseDropdown wctx ctx
      _
        | not isOpen -> Just $ resultReqs [IgnoreChildrenEvents] widgetInstance
        | otherwise -> Nothing

    openRequired point widgetInstance = not isOpen && inViewport where
      inViewport = pointInRect point (_instanceViewport widgetInstance)

    closeRequired point widgetInstance = isOpen && not inOverlay where
      inOverlay = case Seq.lookup 0 (_instanceChildren widgetInstance) of
        Just inst -> pointInRect point (_instanceViewport inst)
        Nothing -> False

    handleOpenDropdown wctx ctx = Just $ resultReqs requests newInstance where
      selected = currentValue wctx
      selectedIdx = fromMaybe 0 (Seq.elemIndexL selected items)
      newInstance = createDropdown wctx ctx $ DropdownState True
      requests = [SetOverlay $ _pathCurrent ctx]

    handleCloseDropdown wctx ctx = Just $ resultReqs requests newInstance where
      newInstance = createDropdown wctx ctx $ DropdownState False
      requests = [ResetOverlay]

    dropdownLabel wctx = if value == "" then " " else value where
      value = itemToText $ currentValue wctx

    preferredSize renderer wctx widgetInstance childrenPairs = Node sizeReq childrenReqs where
      Style{..} = def _instanceStyle widgetInstance
      size = calcTextBounds renderer _styleText (dropdownLabel wctx)
      sizeReq = SizeReq size FlexibleSize StrictSize
      childrenReqs = fmap snd childrenPairs

    resize wctx viewport renderArea widgetInstance reqs = (widgetInstance, Seq.singleton assignedArea) where
      assignedArea = case Seq.lookup 0 reqs of
        Just (child, reqChild) -> (oViewport, oRenderArea) where
          reqHeight = _h . _sizeRequested . nodeValue $ reqChild
          maxHeight = min reqHeight 150
          oViewport = viewport { _ry = _ry viewport + _rh viewport, _rh = maxHeight }
          oRenderArea = renderArea { _ry = _ry renderArea + _rh viewport }
        Nothing -> (viewport, renderArea)

    render renderer wctx ctx WidgetInstance{..} =
      do
        drawStyledBackground renderer _instanceRenderArea _instanceStyle
        drawStyledText_ renderer _instanceRenderArea _instanceStyle (dropdownLabel wctx)

        when (isOpen && isJust listViewOverlay) $
          createOverlay renderer $ renderOverlay renderer wctx ctx (fromJust listViewOverlay)
      where
        listViewOverlay = Seq.lookup 0 _instanceChildren

    renderOverlay renderer wctx ctx overlayInstance = renderAction where
      renderAction = _widgetRender (_instanceWidget overlayInstance) renderer wctx ctx overlayInstance

makeListView :: (Eq a) => Seq a -> a -> (a -> Text) -> WidgetInstance s e
makeListView items selected itemToText = listView_ lvConfig items itemToText where
  lvConfig = ListViewConfig {
    _lvValue = WidgetValue selected,
    _lvOnChange = [],
    _lvOnChangeReq = []
  }
