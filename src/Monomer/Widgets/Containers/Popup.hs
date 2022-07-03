{-|
Module      : Monomer.Widgets.Containers.Popup
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Popup widget, used to display content overlaid on top of the active widget tree.
When the popup is open, events will not reach the widgets below it.

In addition to the content that is displayed when open, a popup requires a
boolean lens or value to indicate if the content should be visible. This flag
can be used to programatically open/close the popup. The popup can also be
closed by clicking outside its content.

In general, it is a good idea to set a background color to the top level content
widget, since by default most widgets have a transparent background; this is
true in particular for containers.

@
popup visiblePopup $  -- visiblePopup is a lens to a Bool field in the model
  label "This will appear on top of the widget tree"
    `styleBasic` [bgColor gray, padding 10]
@

By default the popup will be open at the top-left location the widget would be
if it was directly embedded in the widget tree. One common pattern is having a
popup open when clicking a button, and the expectation is it will open below the
button. This can be achieved with:

@
vstack [
  button "Open" OpenPopup,
  popup visiblePopup (label "Content")
]
@

The popup's content can be aligned relative to the location of the popup widget
in the widget tree:

@
popup_ visiblePopup [alignTop, alignCenter] $
  label "This will appear on top of the widget tree"
    `styleBasic` [bgColor gray, padding 10]
@

Alternatively, aligning relative to the application's window is possible. It can
be useful for displaying notifications:

@
popup_ visiblePopup [popupAlignWindow, alignTop, alignCenter] $
  label "This will appear centered at the top of the main window"
    `styleBasic` [bgColor gray, padding 10]
@

It's also possible to add an offset to the location of the popup, and it can be
combined with alignment options:

@
cfgs = [popupAlignWindow, alignTop, alignCenter, popupOffset (Point 0 5)]

popup_ visiblePopup cfgs $
  label "This will appear centered almost at the top of the main window"
    `styleBasic` [bgColor gray, padding 10]
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
  popupDisableClose,
  popupDisableClose_,
  popupAlignToWindow,
  popupAlignToWindow_,
  popupOffset,
  popupOpenAtCursor,
  popupOpenAtCursor_,

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

- 'popupDisableClose': do not close the popup when clicking outside the content.
- 'popupAlignToWindow': align the popup to the application's window.
- 'popupOffset': offset to add to the default location of the popup.
- 'popupOpenAtCursor': whether to open the content at the cursor position.
- 'alignLeft': left align relative to the widget location or main window.
- 'alignRight': right align relative to the widget location or main window.
- 'alignCenter': center align relative to the widget location or main window.
- 'alignTop': top align relative to the widget location or main window.
- 'alignMiddle': middle align relative to the widget location or main window.
- 'alignBottom': bottom align relative to the widget location or main window.
- 'onChange': event to raise when the popup is opened/closed.
- 'onChangeReq': 'WidgetRequest' to generate when the popup is opened/closed.
-}
data PopupCfg s e = PopupCfg {
  _ppcOpenAtCursor :: Maybe Bool,
  _ppcDisableClose :: Maybe Bool,
  _ppcAlignToWindow :: Maybe Bool,
  _ppcAlignH :: Maybe AlignH,
  _ppcAlignV :: Maybe AlignV,
  _ppcOffset :: Maybe Point,
  _ppcOnChangeReq :: [Bool -> WidgetRequest s e]
}

instance Default (PopupCfg s e) where
  def = PopupCfg {
    _ppcOpenAtCursor = Nothing,
    _ppcDisableClose = Nothing,
    _ppcAlignToWindow = Nothing,
    _ppcAlignH = Nothing,
    _ppcAlignV = Nothing,
    _ppcOffset = Nothing,
    _ppcOnChangeReq = []
  }

instance Semigroup (PopupCfg s e) where
  (<>) t1 t2 = PopupCfg {
    _ppcOpenAtCursor = _ppcOpenAtCursor t2 <|> _ppcOpenAtCursor t1,
    _ppcDisableClose = _ppcDisableClose t2 <|> _ppcDisableClose t1,
    _ppcAlignToWindow = _ppcAlignToWindow t2 <|> _ppcAlignToWindow t1,
    _ppcAlignH = _ppcAlignH t2 <|> _ppcAlignH t1,
    _ppcAlignV = _ppcAlignV t2 <|> _ppcAlignV t1,
    _ppcOffset = _ppcOffset t2 <|> _ppcOffset t1,
    _ppcOnChangeReq = _ppcOnChangeReq t1 <> _ppcOnChangeReq t2
  }

instance Monoid (PopupCfg s e) where
  mempty = def

instance CmbAlignLeft (PopupCfg s e) where
  alignLeft_ False = def
  alignLeft_ True = def {
    _ppcAlignH = Just ALeft
  }

instance CmbAlignCenter (PopupCfg s e) where
  alignCenter_ False = def
  alignCenter_ True = def {
    _ppcAlignH = Just ACenter
  }

instance CmbAlignRight (PopupCfg s e) where
  alignRight_ False = def
  alignRight_ True = def {
    _ppcAlignH = Just ARight
  }

instance CmbAlignTop (PopupCfg s e) where
  alignTop_ False = def
  alignTop_ True = def {
    _ppcAlignV = Just ATop
  }

instance CmbAlignMiddle (PopupCfg s e) where
  alignMiddle_ False = def
  alignMiddle_ True = def {
    _ppcAlignV = Just AMiddle
  }

instance CmbAlignBottom (PopupCfg s e) where
  alignBottom_ False = def
  alignBottom_ True = def {
    _ppcAlignV = Just ABottom
  }

instance WidgetEvent e => CmbOnChange (PopupCfg s e) Bool e where
  onChange fn = def {
    _ppcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (PopupCfg s e) s e Bool where
  onChangeReq req = def {
    _ppcOnChangeReq = [req]
  }

newtype PopupState = PopupState {
  _ppsClickPos :: Point
} deriving (Eq, Show)

-- | Clicking outside the popup's content will not close it.
popupDisableClose :: PopupCfg s e
popupDisableClose = popupDisableClose_ True

-- | Sets whether clicking outside the popup's content will not close it.
popupDisableClose_ :: Bool -> PopupCfg s e
popupDisableClose_ close = def {
  _ppcDisableClose = Just close
}

-- | Alignment will be relative to the application's main window.
popupAlignToWindow :: PopupCfg s e
popupAlignToWindow = popupAlignToWindow_ True

-- | Sets whether alignment will be relative to the application's main window.
popupAlignToWindow_ :: Bool -> PopupCfg s e
popupAlignToWindow_ align = def {
  _ppcAlignToWindow = Just align
}

{-|
Offset to be applied to the location of the popup. It is applied after alignment
options but before adjusting for screen boundaries.
-}
popupOffset :: Point -> PopupCfg s e
popupOffset point = def {
  _ppcOffset = Just point
}

-- | The popup will open at the current cursor position.
popupOpenAtCursor :: PopupCfg s e
popupOpenAtCursor = popupOpenAtCursor_ True

-- | Sets whether the popup will open at the current cursor position.
popupOpenAtCursor_ :: Bool -> PopupCfg s e
popupOpenAtCursor_ open = def {
  _ppcOpenAtCursor = Just open
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
  state = PopupState def
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
  baseWidget = createContainer state def {
    containerAddStyleReq = False,
    containerInit = init,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetRender = render
  }

  init wenv node = result where
    result = checkPopup field config state wenv node

  merge wenv node oldNode oldState = result where
    result = checkPopup field config oldState wenv node

  handleEvent wenv node target evt = case evt of
    KeyAction mod code KeyPressed
      | isCloseable && isKeyEscape code -> Just (closePopup field node)
    ButtonAction point button BtnPressed clicks
      | isCloseable && not (insidePopup point) -> Just (closePopup field node)
    Click point button clicks
      | isCloseable && not (insidePopup point) -> Just (closePopup field node)
    _ -> Nothing
    where
      isVisible = widgetDataGet (wenv ^. L.model) field
      disableClose = _ppcDisableClose config == Just True
      isCloseable = isVisible && not disableClose

      child = Seq.index (node ^. L.children) 0
      cviewport = child ^. L.info . L.viewport
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

    openAtCursor = _ppcOpenAtCursor config == Just True
    alignWin = _ppcAlignToWindow config == Just True
    alignH = _ppcAlignH config
    alignV = _ppcAlignV config
    child = Seq.index children 0

    Rect ax ay aw ah
      | alignWin = wenv ^. L.viewport
      | otherwise = viewport
    cx
      | openAtCursor = sx
      | alignH == Just ALeft = ax
      | alignH == Just ACenter = ax + (aw - cw) / 2
      | alignH == Just ARight = ax + aw - cw
      | otherwise = px

    cy
      | openAtCursor = sy
      | alignV == Just ATop = ay
      | alignV == Just AMiddle = ay + (ah - ch) / 2
      | alignV == Just ABottom = ay + ah - ch
      | otherwise = py

    cw = sizeReqMaxBounded (child ^. L.info . L.sizeReqW)
    ch = sizeReqMaxBounded (child ^. L.info . L.sizeReqH)
    tmpArea = Rect (cx + ox) (cy + oy) cw ch

    winOffset = calcPopupOffset wenv tmpArea
    carea = moveRect winOffset tmpArea

    assignedAreas = Seq.fromList [carea]
    resized = (resultNode node, assignedAreas)

  render wenv node renderer =
    when isVisible $
      createOverlay renderer $
        widgetRender (child ^. L.widget) wenv child renderer
    where
      child = Seq.index (node ^. L.children) 0
      cviewport = child ^. L.info . L.viewport
      isVisible = widgetDataGet (wenv ^. L.model) field

calcPopupOffset :: WidgetEnv s e -> Rect -> Point
calcPopupOffset wenv viewport = Point offsetX offsetY where
  Size winW winH = wenv ^. L.windowSize
  Rect cx cy cw ch = moveRect (wenv ^. L.offset) viewport

  offsetX
    | cx < 0 = -cx
    | cx + cw > winW = winW - cx - cw
    | otherwise = 0
  offsetY
    | cy < 0 = -cy
    | cy + ch > winH = winH - cy - ch
    | otherwise = 0

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
