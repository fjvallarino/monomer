{-|
Module      : Monomer.Widgets.Containers.Popup
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Popup widget, used to display content overlaid on top of the active widget tree.
When the popup is open events will not reach the widgets below it.

Besides the content to display when open, a popup requires a lens or value to
indicate if the content should be visible or not. This flag can be used to
programatically open/close the popup. The popup can also be closed by clicking
outside its content.

In general it is a good idea to set a background color to the top level content
widget, since by default most widgets have a transparent background; this is
true in particular for containers.

@
popup visiblePopup $  -- visiblePopup is a lens to a field in the model
  label "This will appear on top of the widget tree"
    `styleBasic` [bgColor gray, padding 10]
@

By default the popup will be open at the top-left location the widget would be,
if it was directly embedded in the widget tree. One common pattern is having a
popup open when clicking a button, and the expectation is it will open below the
button. This can be achieved with:

@
vstack [
  button "Open" OpenPopup,
  popup visiblePopup (label "Content")
]
@

For an example of popup's use, check 'Monomer.Widgets.Singles.ColorPopup'.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Containers.Popup (
  -- * Configuration
  PopupCfg,
  popupOffset,
  popupOpenAtClick,
  popupOpenAtClick_,

  -- * Constructors
  popup,
  popup_,
  popupV,
  popupV_,
  popupD_
) where

import Control.Applicative ((<|>))
import Control.Lens -- ((&), (^.), (^?!), (.~), ALens', ix)
import Control.Monad (when)
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

{-|
Configuration options for popup:

- 'popupOpenAtClick': whether to open the content at the location of the mouse
  pointer.
- 'popupOffset': offset to add to the default location of the popup.
- 'onChange': event to raise when the popup is opened/closed.
- 'onChangeReq': 'WidgetRequest' to generate when the popup is opened/closed.
-}
data PopupCfg s e = PopupCfg {
  _ppcOpenAtClickPos :: Maybe Bool,
  _ppcOffset :: Maybe Point,
  _ppcOnChangeReq :: [Bool -> WidgetRequest s e]
}

instance Default (PopupCfg s e) where
  def = PopupCfg {
    _ppcOpenAtClickPos = Nothing,
    _ppcOffset = Nothing,
    _ppcOnChangeReq = []
  }

instance Semigroup (PopupCfg s e) where
  (<>) t1 t2 = PopupCfg {
    _ppcOpenAtClickPos = _ppcOpenAtClickPos t2 <|> _ppcOpenAtClickPos t1,
    _ppcOffset = _ppcOffset t2 <|> _ppcOffset t1,
    _ppcOnChangeReq = _ppcOnChangeReq t1 <> _ppcOnChangeReq t2
  }

instance Monoid (PopupCfg s e) where
  mempty = def

instance WidgetEvent e => CmbOnChange (PopupCfg s e) Bool e where
  onChange fn = def {
    _ppcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (PopupCfg s e) s e Bool where
  onChangeReq req = def {
    _ppcOnChangeReq = [req]
  }

data PopupState = PopupState {
  _ppsClickPos :: Point,
  _ppsOffset :: Point
} deriving (Eq, Show)

popupOffset :: Point -> PopupCfg s e
popupOffset point = def {
  _ppcOffset = Just point
}

popupOpenAtClick :: PopupCfg s e
popupOpenAtClick = popupOpenAtClick_ True

popupOpenAtClick_ :: Bool -> PopupCfg s e
popupOpenAtClick_ open = def {
  _ppcOpenAtClickPos = Just open
}

-- | Creates a popup with the given lens to determine its visibility.
popup
  :: WidgetModel s
  => ALens' s Bool
  -> WidgetNode s e
  -> WidgetNode s e
popup field managed = popup_ field def managed

{-|
Creates a popup with the given lens to determine its visibility. Accepts config.
-}
popup_
  :: WidgetModel s
  => ALens' s Bool
  -> [PopupCfg s e]
  -> WidgetNode s e
  -> WidgetNode s e
popup_ field configs managed = newNode where
  newNode = popupD_ (WidgetLens field) configs managed

{-|
Creates a popup using the given value to determine its visibility and 'onChange'
event handler.
-}
popupV
  :: (WidgetModel s, WidgetEvent e)
  => Bool
  -> (Bool -> e)
  -> WidgetNode s e
  -> WidgetNode s e
popupV value handler managed = popupV_ value handler def managed

{-|
Creates a popup using the given value to determine its visibility and 'onChange'
event handler. Accepts config.
-}
popupV_
  :: (WidgetModel s, WidgetEvent e)
  => Bool
  -> (Bool -> e)
  -> [PopupCfg s e]
  -> WidgetNode s e
  -> WidgetNode s e
popupV_ value handler configs managed = newNode where
  newConfigs = onChange handler : configs
  newNode = popupD_ (WidgetValue value) newConfigs managed

{-|
Creates a popup providing a 'WidgetData' instance to determine its visibility
and config.
-}
popupD_
  :: WidgetModel s
  => WidgetData s Bool
  -> [PopupCfg s e]
  -> WidgetNode s e
  -> WidgetNode s e
popupD_ wdata configs managed = makeNode widget managed where
  config = mconcat configs
  state = PopupState def def
  widget = makePopup wdata config state

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "popup" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makePopup
  :: forall s e . WidgetModel s
  => WidgetData s Bool
  -> PopupCfg s e
  -> PopupState
  -> Widget s e
makePopup field config state = widget where
  container = def {
    containerAddStyleReq = False,
    containerChildrenOffset = Just (_ppsOffset state),
    containerCreateContainerFromModel = createContainerFromModel,
    containerInit = init,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  baseWidget = createContainer state container
  widget = baseWidget {
    widgetRender = render
  }

  createContainerFromModel wenv node state = Just newContainer where
    newContainer = container {
      containerChildrenOffset = Just (_ppsOffset state)
    }

  init wenv node = result where
    result = checkPopup field config state wenv node

  merge wenv node oldNode oldState = result where
    result = checkPopup field config oldState wenv node

  handleEvent wenv node target evt = case evt of
    KeyAction mod code KeyPressed
      | isVisible && isKeyEscape code -> Just (closePopup field node)
    ButtonAction point button BtnPressed clicks
      | isVisible && not (insidePopup point) -> Just (closePopup field node)
    _ -> Nothing
    where
      isVisible = widgetDataGet (wenv ^. L.model) field
      offset = _ppsOffset state

      child = Seq.index (node ^. L.children) 0
      cviewport = moveRect offset (child ^. L.info . L.viewport)
      insidePopup point = pointInRect point cviewport

  getSizeReq wenv node children = (newReqW, newReqH) where
    -- Width and height do not matter, only location is important.
    newReqW = flexWidth 0
    newReqH = flexHeight 0

  resize :: ContainerResizeHandler s e
  resize wenv node viewport children = resized where
    Rect px py pw ph = viewport
    Point sx sy = _ppsClickPos state
    Point ox oy = fromMaybe def (_ppcOffset config)

    openAtMouse = fromMaybe False (_ppcOpenAtClickPos config)
    child = Seq.index children 0

    (cx, cy)
      | openAtMouse = (sx, sy)
      | otherwise = (px, py)
    cw = sizeReqMaxBounded (child ^. L.info . L.sizeReqW)
    ch = sizeReqMaxBounded (child ^. L.info . L.sizeReqH)
    carea = Rect (cx + ox) (cy + oy) cw ch

    newState = state {
      _ppsOffset = calcPopupOffset wenv carea
    }
    newNode = node
      & L.widget .~ makePopup field config newState

    assignedAreas = Seq.fromList [carea]
    resized = (resultNode newNode, assignedAreas)

  calcPopupOffset wenv viewport = Point offsetX offsetY where
    Size winW winH = _weWindowSize wenv
    Rect cx cy cw ch = moveRect (wenv ^. L.offset) viewport

    offsetX
      | cx + cw > winW = winW - cx - cw
      | otherwise = 0
    offsetY
      | cy + ch > winH = winH - cy - ch
      | otherwise = 0

  render wenv node renderer =
    when isVisible $
      createOverlay renderer $
        drawInTranslation renderer totalOffset $
          widgetRender (child ^. L.widget) cwenv child renderer
    where
      child = Seq.index (node ^. L.children) 0
      cviewport = child ^. L.info . L.viewport
      isVisible = widgetDataGet (wenv ^. L.model) field
      scOffset = wenv ^. L.offset
      offset = _ppsOffset state
      totalOffset = addPoint scOffset offset
      cwenv = updateWenvOffset container wenv node cviewport
        & L.viewport .~ cviewport

checkPopup
  :: WidgetModel s
  => WidgetData s Bool
  -> PopupCfg s e
  -> PopupState
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
checkPopup field config state wenv node = result where
  shouldDisplay = widgetDataGet (wenv ^. L.model) field
  isOverlay = isNodeInOverlay wenv node
  widgetId = node ^. L.info . L.widgetId
  useNewState node = newNode where
    newNode = node
      & L.widget .~ makePopup field config state

  result
    | shouldDisplay && not isOverlay = showPopup field config state wenv node
    | not shouldDisplay && isOverlay = hidePopup node
    | otherwise = resultNode (useNewState node)

showPopup
  :: WidgetModel s
  => WidgetData s Bool
  -> PopupCfg s e
  -> PopupState
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
showPopup field config state wenv node = result where
  widgetId = node ^. L.info . L.widgetId
  path = node ^. L.info . L.path
  mousePos = wenv ^. L.inputStatus . L.mousePos
  newState = state {
    _ppsClickPos = mousePos
  }
  newNode = node
    & L.widget .~ makePopup field config newState
  reqs = [
      ResizeWidgets widgetId,
      SetOverlay widgetId path,
      MoveFocus (Just widgetId) FocusFwd
    ]
  result = resultReqs newNode reqs

hidePopup :: WidgetNode s e -> WidgetResult s e
hidePopup node = result where
  widgetId = node ^. L.info . L.widgetId
  reqs = [
      ResetOverlay widgetId,
      MoveFocus (Just widgetId) FocusBwd
    ]
  result = resultReqs node reqs

closePopup :: WidgetData s Bool -> WidgetNode s e -> WidgetResult s e
closePopup field node = result where
  widgetId = node ^. L.info . L.widgetId
  toggleShow = widgetDataSet field False
  reqs = [
      IgnoreChildrenEvents,
      ResetOverlay widgetId,
      MoveFocus (Just widgetId) FocusBwd
    ] ++ toggleShow
  result = resultReqs node reqs
