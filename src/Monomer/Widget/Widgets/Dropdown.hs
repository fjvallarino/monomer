{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Dropdown (DropdownConfig(..), dropdown) where

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
import Monomer.Widget.Widgets.ListView

data DropdownConfig s e a = DropdownConfig {
  _ddValue :: WidgetValue s a,
  _ddItems :: Seq a,
  _ddItemToText :: a -> Text,
  _ddOnChange :: [a -> e],
  _ddOnChangeReq :: [WidgetRequest s]
}

newtype DropdownState = DropdownState {
  _isOpen :: Bool
}

newtype DropdownMessage = OnChangeMessage Int deriving Typeable

dropdown :: (Traversable t, Eq a) => ALens' s a -> t a -> (a -> Text) -> WidgetInstance s e
dropdown field items itemToText = dropdown_ config where
  config = DropdownConfig (WidgetLens field) newItems itemToText [] []
  newItems = foldl' (|>) Empty items

dropdown_ :: (Eq a) => DropdownConfig s e a -> WidgetInstance s e
dropdown_ config = makeInstance (makeDropdown config newState) where
  newState = DropdownState False

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "dropdown" widget) {
  _instanceFocusable = True
}

makeDropdown :: (Eq a) => DropdownConfig s e a -> DropdownState -> Widget s e
makeDropdown config state = createContainer {
    _widgetInit = containerInit init,
    _widgetGetState = makeState state,
    _widgetMerge = containerMergeTrees merge,
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetHandleMessage = containerHandleMessage handleMessage,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize,
    _widgetRender = render
  }
  where
    isOpen = _isOpen state
    currentValue wctx = widgetValueGet (_wcApp wctx) (_ddValue config)

    createDropdown wctx ctx newState widgetInstance = newInstance where
      selected = currentValue wctx
      newInstance = widgetInstance {
        _instanceWidget = makeDropdown config newState,
        _instanceChildren = Seq.singleton $ makeListView config ctx selected
      }

    init wctx ctx widgetInstance = resultWidget $ createDropdown wctx ctx state widgetInstance

    merge wctx ctx oldState newInstance = resultWidget $ createDropdown wctx ctx newState newInstance where
      newState = fromMaybe state (useState oldState)

    handleEvent wctx ctx evt widgetInstance = case evt of
      Click point _
        | openRequired point widgetInstance -> Just $ handleOpenDropdown wctx ctx widgetInstance
        | closeRequired point widgetInstance -> Just $ handleCloseDropdown wctx ctx widgetInstance
      KeyAction mode code status
        | isKeyDown code && not isOpen -> Just $ handleOpenDropdown wctx ctx widgetInstance
        | isKeyEsc code && isOpen -> Just $ handleCloseDropdown wctx ctx widgetInstance
      _
        | not isOpen -> Just $ resultReqs [IgnoreChildrenEvents] widgetInstance
        | otherwise -> Nothing

    openRequired point widgetInstance = not isOpen && inViewport where
      inViewport = pointInRect point (_instanceViewport widgetInstance)

    closeRequired point widgetInstance = isOpen && not inOverlay where
      inOverlay = case Seq.lookup 0 (_instanceChildren widgetInstance) of
        Just inst -> pointInRect point (_instanceViewport inst)
        Nothing -> False

    handleOpenDropdown wctx ctx widgetInstance = resultReqs requests newInstance where
      selected = currentValue wctx
      selectedIdx = fromMaybe 0 (Seq.elemIndexL selected (_ddItems config))
      newState = DropdownState True
      newInstance = widgetInstance {
        _instanceWidget = makeDropdown config newState
      }
      lvPath = currentPath (childContext ctx)
      requests = [SetOverlay (currentPath ctx), SetFocus lvPath]

    handleCloseDropdown wctx ctx widgetInstance = resultReqs requests newInstance where
      newState = DropdownState False
      newInstance = widgetInstance {
        _instanceWidget = makeDropdown config newState
      }
      requests = [ResetOverlay, SetFocus (currentPath ctx)]

    handleMessage wctx ctx message widgetInstance = cast message
      >>= \(OnChangeMessage idx) -> Seq.lookup idx (_ddItems config)
      >>= \value -> Just $ handleOnChange wctx ctx idx value widgetInstance

    handleOnChange wctx ctx idx item widgetInstance = WidgetResult (reqs <> newReqs) (events <> newEvents) newInstance where
      WidgetResult reqs events newInstance = handleCloseDropdown wctx ctx widgetInstance
      newReqs = Seq.fromList $ widgetValueSet (_ddValue config) item
      newEvents = Seq.fromList $ fmap ($ item) (_ddOnChange config)

    preferredSize renderer wctx widgetInstance childrenPairs = Node sizeReq childrenReqs where
      Style{..} = _instanceStyle widgetInstance
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
      renderAction = _widgetRender (_instanceWidget overlayInstance) renderer wctx (childContext ctx) overlayInstance

    dropdownLabel wctx = _ddItemToText config $ currentValue wctx

makeListView :: (Eq a) => DropdownConfig s e a -> PathContext -> a -> WidgetInstance s e
makeListView DropdownConfig{..} ctx selected = listView_ lvConfig where
  path = _pathCurrent ctx
  lvConfig = ListViewConfig {
    _lvValue = WidgetValue selected,
    _lvItems = _ddItems,
    _lvItemToText = _ddItemToText,
    _lvOnChange = [],
    _lvOnChangeReq = [SendMessage path . OnChangeMessage]
  }
