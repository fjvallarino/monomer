{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Dropdown (
  DropdownCfg(..),
  dropdown,
  dropdownCfg
) where

import Control.Lens (ALens', (&), (^#), (#~))
import Control.Monad
import Data.Default
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import qualified Data.Map as M
import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.StyleCombinators
import Monomer.Common.Tree
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Graphics.Color
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseContainer
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.Label
import Monomer.Widget.Widgets.ListView
import Monomer.Widget.Widgets.WidgetCombinators

data DropdownCfg s e a = DropdownCfg {
  _ddcValue :: WidgetValue s a,
  _ddcItems :: Seq a,
  _ddcItemToText :: a -> Text,
  _ddcOnChange :: [a -> e],
  _ddcOnChangeReq :: [WidgetRequest s],
  _ddcSelectedStyle :: StyleState,
  _ddcHighlightedStyle :: StyleState,
  _ddcHoverStyle :: StyleState
}

newtype DropdownState = DropdownState {
  _isOpen :: Bool
}

newtype DropdownMessage
  = OnChangeMessage Int
  deriving Typeable

dropdownCfg
  :: WidgetValue s a -> Seq a -> (a -> Text) -> DropdownCfg s e a
dropdownCfg value items itemToText = DropdownCfg {
  _ddcValue = value,
  _ddcItems = items,
  _ddcItemToText = itemToText,
  _ddcOnChange = [],
  _ddcOnChangeReq = [],
  _ddcSelectedStyle = bgColor gray,
  _ddcHighlightedStyle = border 1 darkGray,
  _ddcHoverStyle = bgColor lightGray
}

dropdown
  :: (Traversable t, Eq a)
  => ALens' s a -> t a -> (a -> Text) -> WidgetInstance s e
dropdown field items itemToText = dropdown_ config where
  config = dropdownCfg (WidgetLens field) newItems itemToText
  newItems = foldl' (|>) Empty items

dropdown_ :: (Eq a) => DropdownCfg s e a -> WidgetInstance s e
dropdown_ config = makeInstance (makeDropdown config newState) where
  newState = DropdownState False

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "dropdown" widget) {
  _wiFocusable = True
}

makeDropdown :: (Eq a) => DropdownCfg s e a -> DropdownState -> Widget s e
makeDropdown config state = widget where
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

  isOpen = _isOpen state
  currentValue wenv = widgetValueGet (_weModel wenv) (_ddcValue config)

  createDropdown wenv newState widgetInst = newInstance where
    selected = currentValue wenv
    path = _wiPath widgetInst
    newInstance = widgetInst {
      _wiWidget = makeDropdown config newState,
      _wiChildren = Seq.singleton $ makeListView config path selected
    }

  init wenv widgetInst = resultWidget $ createDropdown wenv state widgetInst

  merge wenv oldState newInst = result where
    newState = fromMaybe state (useState oldState)
    result = resultWidget $ createDropdown wenv newState newInst

  handleEvent wenv target evt widgetInst = case evt of
    Click point _
      | openRequired point widgetInst -> Just $ openDropdown wenv widgetInst
      | closeRequired point widgetInst -> Just $ closeDropdown wenv widgetInst
    KeyAction mode code status
      | isKeyDown code && not isOpen -> Just $ openDropdown wenv widgetInst
      | isKeyEsc code && isOpen -> Just $ closeDropdown wenv widgetInst
    _
      | not isOpen -> Just $ resultReqs [IgnoreChildrenEvents] widgetInst
      | otherwise -> Nothing

  openRequired point widgetInst = not isOpen && inViewport where
    inViewport = pointInRect point (_wiViewport widgetInst)

  closeRequired point widgetInst = isOpen && not inOverlay where
    inOverlay = case Seq.lookup 0 (_wiChildren widgetInst) of
      Just inst -> pointInRect point (_wiViewport inst)
      Nothing -> False

  openDropdown wenv widgetInst = resultReqs requests newInstance where
    selected = currentValue wenv
    selectedIdx = fromMaybe 0 (Seq.elemIndexL selected (_ddcItems config))
    newState = DropdownState True
    newInstance = widgetInst {
      _wiWidget = makeDropdown config newState
    }
    path = _wiPath widgetInst
    lvPath = firstChildPath widgetInst
    requests = [SetOverlay path, SetFocus lvPath]

  closeDropdown wenv widgetInst = resultReqs requests newInstance where
    path = _wiPath widgetInst
    newState = DropdownState False
    newInstance = widgetInst {
      _wiWidget = makeDropdown config newState
    }
    requests = [ResetOverlay, SetFocus path]

  handleMessage wenv target message widgetInst = cast message
    >>= \(OnChangeMessage idx) -> Seq.lookup idx (_ddcItems config)
    >>= \value -> Just $ onChange wenv idx value widgetInst

  onChange wenv idx item widgetInst = result where
    WidgetResult reqs events newInstance = closeDropdown wenv widgetInst
    newReqs = Seq.fromList $ widgetValueSet (_ddcValue config) item
    newEvents = Seq.fromList $ fmap ($ item) (_ddcOnChange config)
    result = WidgetResult (reqs <> newReqs) (events <> newEvents) newInstance

  getSizeReq wenv widgetInst children = sizeReq where
    theme = activeTheme wenv widgetInst
    style = activeStyle wenv widgetInst
    size = getTextSize wenv theme style (dropdownLabel wenv)
    sizeReq = SizeReq size FlexibleSize StrictSize

  resize wenv viewport renderArea children widgetInst = resized where
    area = case Seq.lookup 0 children of
      Just child -> (oViewport, oRenderArea) where
        reqHeight = _sH . _srSize . _wiSizeReq $ child
        maxHeight = min reqHeight 150
        oViewport = viewport {
          _rY = _rY viewport + _rH viewport,
          _rH = maxHeight
        }
        oRenderArea = renderArea {
          _rY = _rY renderArea + _rH viewport
        }
      Nothing -> (viewport, renderArea)
    assignedArea = Seq.singleton area
    resized = (widgetInst, assignedArea)

  render renderer wenv widgetInst@WidgetInstance{..} = do
    drawStyledBackground renderer renderArea style
    drawStyledText_ renderer renderArea style (dropdownLabel wenv)

    when (isOpen && isJust listViewOverlay) $
      createOverlay renderer $
        renderOverlay renderer wenv (fromJust listViewOverlay)
    where
      listViewOverlay = Seq.lookup 0 _wiChildren
      renderArea = _wiRenderArea
      style = activeStyle wenv widgetInst

  renderOverlay renderer wenv overlayInstance = renderAction where
    widget = _wiWidget overlayInstance
    renderAction = widgetRender widget renderer wenv overlayInstance

  dropdownLabel wenv = _ddcItemToText config $ currentValue wenv

makeListView
  :: (Eq a) => DropdownCfg s e a -> Path -> a -> WidgetInstance s e
makeListView DropdownCfg{..} dropdownPath selected = listViewInst where
  value = WidgetValue selected
  items = _ddcItems
  makeRow item = label (_ddcItemToText item)
  config = onChangeReqIdx (SendMessage dropdownPath . OnChangeMessage)
  listViewInst = listViewF_ value items makeRow config
