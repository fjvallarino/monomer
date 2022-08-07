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
  label "This will appear on top of the widget tree, aligned to the top-center"
    `styleBasic` [bgColor gray, padding 10]
@

Alternatively, aligning relative to the application's window is possible. It can
be useful for displaying notifications:

@
popup_ visiblePopup [popupAlignToWindow, alignTop, alignCenter] $
  label "This will appear centered at the top of the main window"
    `styleBasic` [bgColor gray, padding 10]
@

It's also possible to add an offset to the location of the popup, and it can be
combined with alignment options:

@
cfgs = [popupAlignToWindow, alignTop, alignCenter, popupOffset (Point 0 5)]

popup_ visiblePopup cfgs $
  label "This will appear centered almost at the top of the main window"
    `styleBasic` [bgColor gray, padding 10]
@

Alternatively, a widget can be provided as an anchor. This is not too different
than the previous examples, but opens up more alignment options since the
popup's content can now be aligned to the outer edges of the anchor widget.

@
anchor = toggleButton "Show popup" visiblePopup
cfgs = [popupAnchor anchor, popupAlignToOuterV, alignTop, alignCenter]

popup_ visiblePopup cfgs $
  label "The bottom of the content will be aligned to the top of the anchor"
    `styleBasic` [bgColor gray, padding 10]
@

For an example of popup's use, check 'Monomer.Widgets.Singles.ColorPopup'.

Note: style settings will be ignored by this widget. The content and anchor need
to be styled independently.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Containers.Popup (
  -- * Configuration
  PopupCfg,
  popupAnchor,
  popupAlignToOuterH,
  popupAlignToOuterH_,
  popupAlignToOuterV,
  popupAlignToOuterV_,
  popupAlignToWindow,
  popupAlignToWindow_,
  popupOffset,
  popupOpenAtCursor,
  popupOpenAtCursor_,
  popupDisableClose,
  popupDisableClose_,

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
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

{-|
Configuration options for popup:

- 'popupAnchor': a widget to be used as a reference for positioning the popup.
- 'popupAlignToOuter': align the popup to the anchor's outer borders.
- 'popupAlignToWindow': align the popup to the application's window.
- 'popupOffset': offset to add to the default location of the popup.
- 'popupOpenAtCursor': whether to open the content at the cursor position.
- 'popupDisableClose': do not close the popup when clicking outside the content.
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
  _ppcAnchor :: Maybe (WidgetNode s e),
  _ppcAlignToOuterH :: Maybe Bool,
  _ppcAlignToOuterV :: Maybe Bool,
  _ppcAlignToWindow :: Maybe Bool,
  _ppcAlignH :: Maybe AlignH,
  _ppcAlignV :: Maybe AlignV,
  _ppcOffset :: Maybe Point,
  _ppcOpenAtCursor :: Maybe Bool,
  _ppcDisableClose :: Maybe Bool,
  _ppcOnChangeReq :: [Bool -> WidgetRequest s e]
}

instance Default (PopupCfg s e) where
  def = PopupCfg {
    _ppcAnchor = Nothing,
    _ppcAlignToOuterH = Nothing,
    _ppcAlignToOuterV = Nothing,
    _ppcAlignToWindow = Nothing,
    _ppcAlignH = Nothing,
    _ppcAlignV = Nothing,
    _ppcOffset = Nothing,
    _ppcOpenAtCursor = Nothing,
    _ppcDisableClose = Nothing,
    _ppcOnChangeReq = []
  }

instance Semigroup (PopupCfg s e) where
  (<>) t1 t2 = PopupCfg {
    _ppcAnchor = _ppcAnchor t2 <|> _ppcAnchor t1,
    _ppcAlignToOuterH = _ppcAlignToOuterH t2 <|> _ppcAlignToOuterH t1,
    _ppcAlignToOuterV = _ppcAlignToOuterV t2 <|> _ppcAlignToOuterV t1,
    _ppcAlignToWindow = _ppcAlignToWindow t2 <|> _ppcAlignToWindow t1,
    _ppcAlignH = _ppcAlignH t2 <|> _ppcAlignH t1,
    _ppcAlignV = _ppcAlignV t2 <|> _ppcAlignV t1,
    _ppcOffset = _ppcOffset t2 <|> _ppcOffset t1,
    _ppcOpenAtCursor = _ppcOpenAtCursor t2 <|> _ppcOpenAtCursor t1,
    _ppcDisableClose = _ppcDisableClose t2 <|> _ppcDisableClose t1,
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

{-|
Sets the widget that will be used as the anchor for the popup. In general, this
anchor will also act as the trigger to open the popup (e.g. a button). When the
popup is open, the anchor will be used to position the content, taking scroll
and window size into consideration.
-}
popupAnchor :: WidgetNode s e -> PopupCfg s e
popupAnchor node = def {
  _ppcAnchor = Just node
}

{-
Align the popup to the horizontal outer edges of the anchor. It only works with
'alignLeft' and 'alignRight', which need to be specified separately.

This option only works when 'popupAnchor' is set.
-}
popupAlignToOuterH :: PopupCfg s e
popupAlignToOuterH = popupAlignToOuterH_ True

{-|
Sets whether to align the popup to the horizontal outer edges of the anchor. It
only works with 'alignLeft' and 'alignRight', which need to be specified
separately.

This option only works when 'popupAnchor' is set.
-}
popupAlignToOuterH_ :: Bool -> PopupCfg s e
popupAlignToOuterH_ align = def {
  _ppcAlignToOuterH = Just align
}

{-
Align the popup vertically to the outer edges of the anchor. It only works with
'alignTop' and 'alignBottom', which need to be specified separately.

This option only works when 'popupAnchor' is set.
-}
popupAlignToOuterV :: PopupCfg s e
popupAlignToOuterV = popupAlignToOuterV_ True

{-|
Sets whether to align the popup vertically to the outer edges of the anchor. It
only works with 'alignTop' and 'alignBottom', which need to be specified
separately.

This option only works when 'popupAnchor' is set.
-}
popupAlignToOuterV_ :: Bool -> PopupCfg s e
popupAlignToOuterV_ align = def {
  _ppcAlignToOuterV = Just align
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

-- | Clicking outside the popup's content will not close it.
popupDisableClose :: PopupCfg s e
popupDisableClose = popupDisableClose_ True

-- | Sets whether clicking outside the popup's content will not close it.
popupDisableClose_ :: Bool -> PopupCfg s e
popupDisableClose_ close = def {
  _ppcDisableClose = Just close
}

data PopupState = PopupState {
  _ppsClickPos :: Point,
  _ppsReleaseMs :: Millisecond
} deriving (Eq, Show)

-- | Creates a popup with the given lens to determine its visibility.
popup
  :: WidgetModel s
  => ALens' s Bool
  -> WidgetNode s e
  -> WidgetNode s e
popup field content = popup_ field def content

{-|
Creates a popup with the given lens to determine its visibility. Accepts config.
-}
popup_
  :: WidgetModel s
  => ALens' s Bool
  -> [PopupCfg s e]
  -> WidgetNode s e
  -> WidgetNode s e
popup_ field configs content = newNode where
  newNode = popupD_ (WidgetLens field) configs content

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
popupV value handler content = popupV_ value handler def content

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
popupV_ value handler configs content = newNode where
  newConfigs = onChange handler : configs
  newNode = popupD_ (WidgetValue value) newConfigs content

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
popupD_ wdata configs content = makeNode widget anchor content where
  config = mconcat configs
  state = PopupState def (-1)
  widget = makePopup wdata config state

  anchor = case _ppcAnchor config of
    Just node -> node
    Nothing -> spacer
      `styleBasic` [maxWidth 0.01, maxHeight 0.01]

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e -> WidgetNode s e
makeNode widget anchor content = defaultWidgetNode "popup" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.fromList [anchor, content]

anchorIdx :: Int
anchorIdx = 0

contentIdx :: Int
contentIdx = 1

makePopup
  :: forall s e . WidgetModel s
  => WidgetData s Bool
  -> PopupCfg s e
  -> PopupState
  -> Widget s e
makePopup field config state = widget where
  container = def {
    containerAddStyleReq = False,
    containerInitPost = initPost,
    containerMergePost = mergePost,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  baseWidget = createContainer state container
  widget = baseWidget {
    widgetRender = render
  }

  initPost wenv node newState result = newResult where
    newResult = checkPopup field config newState wenv result

  mergePost wenv node oldNode oldState newState result = newResult where
    newResult = checkPopup field config oldState wenv result

  handleEvent wenv node target evt = case evt of
    KeyAction mod code KeyPressed
      | isCloseable && isKeyEscape code -> Just closeResult

    ButtonAction point button BtnReleased clicks
      | isCloseable && not (insidePopup point) -> Just closeResult

    Click point button clicks
      | isCloseable && not (insidePopup point) -> Just closeResult

    {-
    This check is needed because the anchor is inside the overlay, and otherwise
    it would receive events when the popup is open.
    -}
    _
      | (isVisible && not isContentTarget) || matchMs -> Just ignoreResult
      | otherwise -> Nothing

    where
      path = node ^. L.info . L.path

      disableClose = _ppcDisableClose config == Just True
      matchMs = _ppsReleaseMs state == wenv ^. L.timestamp

      isVisible = widgetDataGet (wenv ^. L.model) field
      isContentTarget = isPathParent (path |> contentIdx) target
      isCloseable = isVisible && not disableClose

      content = Seq.index (node ^. L.children) contentIdx
      cviewport = content ^. L.info . L.viewport
      insidePopup point = pointInRect point cviewport

      closeResult = closePopup field config state wenv node
      ignoreResult = resultReqs node [IgnoreChildrenEvents]

  getSizeReq :: ContainerGetSizeReqHandler s e
  getSizeReq wenv node children = (newReqW, newReqH) where
    anchor = Seq.index children anchorIdx
    newReqW = anchor ^. L.info . L.sizeReqW
    newReqH = anchor ^. L.info . L.sizeReqH

  resize :: ContainerResizeHandler s e
  resize wenv node viewport children = resized where
    Size ww wh = wenv ^. L.windowSize
    Rect px py pw ph = viewport
    Point sx sy = subPoint (_ppsClickPos state) (wenv ^. L.offset)
    Point ox oy = fromMaybe def (_ppcOffset config)

    alignOuterH = _ppcAlignToOuterH config == Just True
    alignOuterV = _ppcAlignToOuterV config == Just True
    alignWin = _ppcAlignToWindow config == Just True
    alignH = _ppcAlignH config
    alignV = _ppcAlignV config
    openAtCursor = _ppcOpenAtCursor config == Just True

    content = Seq.index children contentIdx
    cw = sizeReqMaxBounded (content ^. L.info . L.sizeReqW)
    ch = sizeReqMaxBounded (content ^. L.info . L.sizeReqH)

    (alignL, alignR) = (alignH == Just ALeft, alignH == Just ARight)
    (alignT, alignB) = (alignV == Just ATop, alignV == Just ABottom)
    (alignC, alignM) = (alignH == Just ACenter, alignV == Just AMiddle)

    (atx, arx) = (ax - cw + ox, ax + aw + ox)
    (aty, aby) = (ay - ch + oy, ay + ah + oy)

    Point olx oty = calcWindowOffset wenv config (Rect atx aty cw ch)
    Point orx oby = calcWindowOffset wenv config (Rect arx aby cw ch)

    (fitL, fitR) = (abs olx < 0.01, abs orx < 0.01)
    (fitT, fitB) = (abs oty < 0.01, abs oby < 0.01)

    Rect ax ay aw ah
      | alignWin = Rect 0 0 ww wh
      | otherwise = viewport
    cx
      | openAtCursor = sx
      | alignOuterH && (alignL && (fitL || not fitR) || alignR && fitL && not fitR) = atx
      | alignOuterH && (alignR && (fitR || not fitL) || alignL && fitR && not fitL) = arx
      | alignL = ax
      | alignC = ax + (aw - cw) / 2
      | alignR = ax + aw - cw
      | otherwise = px

    cy
      | openAtCursor = sy
      | alignOuterV && (alignT && (fitT || not fitB) || alignB && fitT && not fitB) = aty
      | alignOuterV && (alignB && (fitB || not fitT) || alignT && fitB && not fitT) = aby
      | alignT = ay
      | alignM = ay + (ah - ch) / 2
      | alignB = ay + ah - ch
      | otherwise = py

    tmpArea = Rect (cx + ox) (cy + oy) cw ch
    winOffset = calcWindowOffset wenv config tmpArea
    carea = moveRect winOffset tmpArea

    assignedAreas = Seq.fromList [viewport, carea]
    resized = (resultNode node, assignedAreas)

  render wenv node renderer = do
    widgetRender (anchor ^. L.widget) awenv anchor renderer

    when isVisible $
      createOverlay renderer $
        drawInTranslation renderer scrollOffset $ do
          widgetRender (content ^. L.widget) cwenv content renderer
    where
      isVisible = widgetDataGet (wenv ^. L.model) field

      alignWin = _ppcAlignToWindow config == Just True
      scrollOffset
        | alignWin = def
        | otherwise = wenv ^. L.offset

      anchor = Seq.index (node ^. L.children) anchorIdx
      anchorVp = anchor ^. L.info . L.viewport
      content = Seq.index (node ^. L.children) contentIdx
      contentVp = content ^. L.info . L.viewport

      -- Hacky solution to avoid the anchor acting as if it were top-level.
      updateOverlay overlay
        | isVisible = Just (content ^. L.info . L.path)
        | otherwise = overlay
      -- Update viewports to avoid clipping/scissoring issues.
      awenv = updateWenvOffset container wenv node anchorVp
        & L.viewport .~ anchorVp
        & L.overlayPath %~ updateOverlay
      cwenv = updateWenvOffset container wenv node contentVp
        & L.viewport .~ contentVp

calcWindowOffset :: WidgetEnv s e -> PopupCfg s e -> Rect -> Point
calcWindowOffset wenv config viewport = Point offsetX offsetY where
  alignWin = _ppcAlignToWindow config == Just True

  Size winW winH = wenv ^. L.windowSize
  Rect cx cy cw ch
    | alignWin = viewport
    | otherwise = moveRect (wenv ^. L.offset) viewport

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
  -> WidgetResult s e
  -> WidgetResult s e
checkPopup field config state wenv result = newResult where
  node = result ^. L.node
  shouldDisplay = widgetDataGet (wenv ^. L.model) field
  isOverlay = isNodeInOverlay wenv node

  (newNode, newReqs)
    | shouldDisplay && not isOverlay = showPopup field config state wenv node
    | not shouldDisplay && isOverlay = hidePopup config node
    | otherwise = (node & L.widget .~ makePopup field config state, [])

  newResult = result
    & L.node . L.widget .~ newNode ^. L.widget
    & L.requests <>~ Seq.fromList newReqs

showPopup
  :: WidgetModel s
  => WidgetData s Bool
  -> PopupCfg s e
  -> PopupState
  -> WidgetEnv s e
  -> WidgetNode s e
  -> (WidgetNode s e, [WidgetRequest s e])
showPopup field config state wenv node = (newNode, newReqs) where
  widgetId = node ^. L.info . L.widgetId
  path = node ^. L.info . L.path
  mousePos = wenv ^. L.inputStatus . L.mousePos

  anchor = Seq.index (node ^. L.children) anchorIdx
  awidgetId = anchor ^. L.info . L.widgetId

  onChangeReqs = fmap ($ True) (_ppcOnChangeReq config)
  showReqs = [
      ResizeWidgets widgetId,
      SetOverlay widgetId path,
      MoveFocus (Just awidgetId) FocusFwd
    ]

  newState = state {
    _ppsClickPos = mousePos
  }
  newNode = node
    & L.widget .~ makePopup field config newState
  newReqs = mconcat [showReqs, onChangeReqs]

hidePopup
  :: PopupCfg s e -> WidgetNode s e -> (WidgetNode s e, [WidgetRequest s e])
hidePopup config node = (node, onChangeReqs <> hideReqs) where
  widgetId = node ^. L.info . L.widgetId

  content = Seq.index (node ^. L.children) contentIdx
  cwidgetId = content ^. L.info . L.widgetId

  onChangeReqs = fmap ($ False) (_ppcOnChangeReq config)
  hideReqs = [
      ResetOverlay widgetId,
      MoveFocus (Just cwidgetId) FocusBwd
    ]

closePopup
  :: WidgetModel s
  => WidgetData s Bool
  -> PopupCfg s e
  -> PopupState
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
closePopup field config state wenv node = result where
  widgetId = node ^. L.info . L.widgetId
  toggleShow = widgetDataSet field False
  isOverlay = isNodeInOverlay wenv node

  content = Seq.index (node ^. L.children) contentIdx
  cwidgetId = content ^. L.info . L.widgetId

  onChangeReqs
    | isOverlay = fmap ($ False) (_ppcOnChangeReq config)
    | otherwise = []
  closeReqs = [
      IgnoreChildrenEvents,
      ResetOverlay widgetId,
      MoveFocus (Just cwidgetId) FocusBwd
    ]

  newState = state {
    _ppsReleaseMs = wenv ^. L.timestamp
  }
  newNode = node
    & L.widget .~ makePopup field config newState

  reqs = mconcat [closeReqs, toggleShow, onChangeReqs]
  result = resultReqs newNode reqs
