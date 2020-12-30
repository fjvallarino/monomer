{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Util.Style (
  StyleChangeCfg(..),
  GetBaseStyle(..),
  activeTheme,
  activeTheme_,
  activeStyle,
  activeStyle_,
  focusedStyle,
  initNodeStyle,
  handleStyleChange,
  handleStyleChange_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (<>~))
import Data.Bits (xor)
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Util.Types
import Monomer.Widgets.Util.Widget

import qualified Monomer.Lens as L

-- Do not use in findByPoint
activeStyle :: WidgetEnv s e -> WidgetNode s e -> StyleState
activeStyle wenv node = activeStyle_ isHovered wenv node

activeStyle_ :: IsHovered s e -> WidgetEnv s e -> WidgetNode s e -> StyleState
activeStyle_ isHoveredFn wenv node = fromMaybe def styleState where
  Style{..} = node ^. L.info . L.style
  mousePos = wenv ^. L.inputStatus . L.mousePos
  isEnabled = node ^. L.info . L.enabled
  isHover = isHoveredFn wenv node
  isFocus = isFocused wenv node
  styleState
    | not isEnabled = _styleDisabled
    | isHover && isFocus = _styleHover <> _styleFocus
    | isHover = _styleHover
    | isFocus = _styleFocus
    | otherwise = _styleBasic

focusedStyle :: WidgetEnv s e -> WidgetNode s e -> StyleState
focusedStyle wenv node = focusedStyle_ isHovered wenv node

focusedStyle_ :: IsHovered s e -> WidgetEnv s e -> WidgetNode s e -> StyleState
focusedStyle_ isHoveredFn wenv node = fromMaybe def styleState where
  Style{..} = node ^. L.info . L.style
  isHover = isHoveredFn wenv node
  styleState
    | isHover = _styleHover <> _styleFocus
    | otherwise = _styleFocus

activeTheme :: WidgetEnv s e -> WidgetNode s e -> ThemeState
activeTheme wenv node = activeTheme_ isHovered wenv node

activeTheme_ :: IsHovered s e -> WidgetEnv s e -> WidgetNode s e -> ThemeState
activeTheme_ isHoveredFn wenv node = themeState where
  theme = _weTheme wenv
  mousePos = wenv ^. L.inputStatus . L.mousePos
  isEnabled = node ^. L.info . L.enabled
  isHover = isHoveredFn wenv node
  isFocus = isFocused wenv node
  themeState
    | not isEnabled = _themeDisabled theme
    | isHover = _themeHover theme
    | isFocus = _themeFocus theme
    | otherwise = _themeBasic theme

initNodeStyle
  :: GetBaseStyle s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
initNodeStyle getBaseStyle wenv node = newNode where
  nodeStyle = mergeBasicStyle $ node ^. L.info . L.style
  baseStyle = mergeBasicStyle $ fromMaybe def (getBaseStyle wenv node)
  themeStyle = baseStyleFromTheme (_weTheme wenv)
  newNode = node
    & L.info . L.style .~ (themeStyle <> baseStyle <> nodeStyle)

handleStyleChange
  :: WidgetEnv s e
  -> Path
  -> SystemEvent
  -> StyleState
  -> Maybe (WidgetResult s e)
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleStyleChange wenv target evt style result node = newResult where
  newResult = handleStyleChange_ wenv target evt style result def node

handleStyleChange_
  :: WidgetEnv s e
  -> Path
  -> SystemEvent
  -> StyleState
  -> Maybe (WidgetResult s e)
  -> StyleChangeCfg
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleStyleChange_ wenv target evt style result cfg node = newResult where
  baseResult = fromMaybe (resultWidget node) result
  sizeReqs = handleSizeChange wenv target evt cfg node
  cursorReqs
    | cfg ^. L.cursorIgnore = []
    | otherwise = handleCursorChange wenv target evt style cfg node
  reqs = sizeReqs ++ cursorReqs
  newResult
    | not (null reqs) = Just (baseResult & L.requests <>~ Seq.fromList reqs)
    | otherwise = result

handleSizeChange
  :: WidgetEnv s e
  -> Path
  -> SystemEvent
  -> StyleChangeCfg
  -> WidgetNode s e
  -> [WidgetRequest s]
handleSizeChange wenv target evt cfg node = reqs where
  widget = node ^. L.widget
  info = node ^. L.info
  -- Hover
  mousePos = wenv ^. L.inputStatus . L.mousePos
  mousePosPrev = wenv ^. L.inputStatus . L.mousePosPrev
  vp = info ^. L.viewport
  vpChanged = pointInRect mousePos vp `xor` pointInRect mousePosPrev vp
  hoverChanged = vpChanged && (isOnEnter evt || isOnLeave evt)
  -- Focus
  focusChanged = isOnFocus evt || isOnBlur evt
  -- Size
  checkSize = hoverChanged || focusChanged
  newReqs = widgetUpdateSizeReq widget wenv node
  oldSizeReqW = info ^. L.sizeReqW
  oldSizeReqH = info ^. L.sizeReqH
  newSizeReqW = newReqs ^. L.info . L.sizeReqW
  newSizeReqH = newReqs ^. L.info . L.sizeReqH
  sizeReqChanged = oldSizeReqW /= newSizeReqW || oldSizeReqH /= newSizeReqH
  -- Result
  resizeReq = [ ResizeWidgets | checkSize && sizeReqChanged ]
  enterReq = [ RenderOnce | isOnEnter evt || isOnLeave evt ]
  reqs = resizeReq ++ enterReq

handleCursorChange
  :: WidgetEnv s e
  -> Path
  -> SystemEvent
  -> StyleState
  -> StyleChangeCfg
  -> WidgetNode s e
  -> [WidgetRequest s]
handleCursorChange wenv target evt style cfg node = reqs where
  -- Cursor
  cfgIcon = cfg ^. L.cursorIcon
  isCursorEvt = cfg ^. L.cursorEvt
  isTarget = node ^. L.info . L.path == target
  curIcon = wenv ^. L.currentCursor
  notInOverlay = isJust (wenv ^. L.overlayPath) && not (isInOverlay wenv node)
  newIcon
    | notInOverlay = CursorArrow
    | otherwise = fromMaybe CursorArrow (style ^. L.cursorIcon <|> cfgIcon)
  setCursor = isTarget && newIcon /= curIcon && (isCursorEvt evt || notInOverlay)
  -- Result
  reqs
   | setCursor = [SetCursorIcon newIcon, RenderOnce]
   | otherwise = []

baseStyleFromTheme :: Theme -> Style
baseStyleFromTheme theme = style where
  style = Style {
    _styleBasic = fromThemeState (_themeBasic theme),
    _styleHover = fromThemeState (_themeHover theme),
    _styleFocus = fromThemeState (_themeFocus theme),
    _styleDisabled = fromThemeState (_themeDisabled theme)
  }
  fromThemeState tstate = Just $ def {
    _sstFgColor = Just $ _thsFgColor tstate,
    _sstHlColor = Just $ _thsHlColor tstate,
    _sstText = Just $ _thsText tstate
  }

mergeBasicStyle :: Style -> Style
mergeBasicStyle st = newStyle where
  newStyle = Style {
    _styleBasic = _styleBasic st,
    _styleHover = _styleBasic st <> _styleHover st,
    _styleFocus = _styleBasic st <> _styleFocus st,
    _styleDisabled = _styleBasic st <> _styleDisabled st
  }

isInOverlay :: WidgetEnv s e -> WidgetNode s e -> Bool
isInOverlay wenv node = maybe False isPrefix (wenv ^. L.overlayPath) where
  path = node ^. L.info . L.path
  isPrefix overlayPath = Seq.take (Seq.length overlayPath) path == overlayPath
