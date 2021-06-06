{-|
Module      : Monomer.Widgets.Util.Style
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for style related operations.
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Util.Style (
  collectStyleField,
  collectStyleField_,
  activeTheme,
  activeTheme_,
  activeStyle,
  activeStyle_,
  focusedStyle,
  styleStateChanged,
  initNodeStyle,
  handleStyleChange
) where

import Control.Applicative ((<|>))
import Control.Lens (Lens', (&), (^.), (^?), (.~), (?~), (<>~), _Just, _1, non)

import Data.Bits (xor)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>))

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.Graphics
import Monomer.Helper
import Monomer.Widgets.Util.Focus
import Monomer.Widgets.Util.Hover
import Monomer.Widgets.Util.Types
import Monomer.Widgets.Util.Widget

import qualified Monomer.Core.Lens as L
import qualified Monomer.Event.Lens as L

instance Default (ActiveStyleCfg s e) where
  def = ActiveStyleCfg {
    _ascIsHovered = isNodeHovered,
    _ascIsFocused = isNodeFocused,
    _ascIsActive = isNodeActive
  }

-- | Extracts/copies the field of a style into an empty style.
collectStyleField
  :: Lens' StyleState (Maybe t) -- ^ The field into the state.
  -> Style                      -- ^ The source style.
  -> Style                      -- ^ The new style.
collectStyleField fieldS source = collectStyleField_ fieldS source def

-- | Extracts/copies the field of a style into a provided style.
collectStyleField_
  :: Lens' StyleState (Maybe t) -- ^ The field into the state.
  -> Style                      -- ^ The source style.
  -> Style                      -- ^ The target style.
  -> Style                      -- ^ The updated style.
collectStyleField_ fieldS source target = style where
  basic = Just $ target ^. L.basic . non def
    & fieldS .~ source ^? L.basic . _Just . fieldS . _Just
  hover = Just $ target ^. L.hover . non def
    & fieldS .~ source ^? L.hover . _Just . fieldS . _Just
  focus = Just $ target ^. L.focus . non def
    & fieldS .~ source ^? L.focus . _Just . fieldS . _Just
  focusHover = Just $ target ^. L.focusHover . non def
    & fieldS .~ source ^? L.focusHover . _Just . fieldS . _Just
  active = Just $ target ^. L.active . non def
    & fieldS .~ source ^? L.active . _Just . fieldS . _Just
  disabled = Just $ target ^. L.disabled . non def
    & fieldS .~ source ^? L.disabled . _Just . fieldS . _Just
  style = Style basic hover focus focusHover active disabled

-- | Returns the active style for the given node.
activeStyle :: WidgetEnv s e -> WidgetNode s e -> StyleState
activeStyle wenv node = activeStyle_ def wenv node

{-|
Returns the active style for the given node, using the provided functions to
determine hover, focus and active status.
-}
activeStyle_
  :: ActiveStyleCfg s e -> WidgetEnv s e -> WidgetNode s e -> StyleState
activeStyle_ config wenv node = fromMaybe def styleState where
  Style{..} = node ^. L.info . L.style
  mousePos = wenv ^. L.inputStatus . L.mousePos
  isEnabled = node ^. L.info . L.enabled
  isHover = _ascIsHovered config wenv node
  isFocus = _ascIsFocused config wenv node
  isActive = _ascIsActive config wenv node
  styleState
    | not isEnabled = _styleDisabled
    | isActive = _styleActive
    | isHover && isFocus = _styleFocusHover
    | isHover = _styleHover
    | isFocus = _styleFocus
    | otherwise = _styleBasic

-- | Returns the correct focused style, depending if it's hovered or not.
focusedStyle :: WidgetEnv s e -> WidgetNode s e -> StyleState
focusedStyle wenv node = focusedStyle_ isNodeHovered wenv node

{-|
Returns the correct focused style, depending if it's hovered or not, using the
provided function.
-}
focusedStyle_ :: IsHovered s e -> WidgetEnv s e -> WidgetNode s e -> StyleState
focusedStyle_ isHoveredFn wenv node = fromMaybe def styleState where
  Style{..} = node ^. L.info . L.style
  isHover = isHoveredFn wenv node
  styleState
    | isHover = _styleFocusHover
    | otherwise = _styleFocus

-- | Returns the active theme for the node.
activeTheme :: WidgetEnv s e -> WidgetNode s e -> ThemeState
activeTheme wenv node = activeTheme_ isNodeHovered wenv node

-- | Returns the active theme for the node.
activeTheme_ :: IsHovered s e -> WidgetEnv s e -> WidgetNode s e -> ThemeState
activeTheme_ isHoveredFn wenv node = themeState where
  theme = _weTheme wenv
  mousePos = wenv ^. L.inputStatus . L.mousePos
  isEnabled = node ^. L.info . L.enabled
  isHover = isHoveredFn wenv node
  isFocus = isNodeFocused wenv node
  isActive = isNodeActive wenv node
  themeState
    | not isEnabled = _themeDisabled theme
    | isActive = _themeActive theme
    | isHover && isFocus = _themeFocusHover theme
    | isHover = _themeHover theme
    | isFocus = _themeFocus theme
    | otherwise = _themeBasic theme

-- | Checks if hover or focus states changed between versions of the node.
styleStateChanged :: WidgetEnv s e -> WidgetNode s e -> SystemEvent -> Bool
styleStateChanged wenv node evt = hoverChanged || focusChanged where
  -- Hover
  hoverChanged = isOnEnter evt || isOnLeave evt
  -- Focus
  focusChanged = isOnFocus evt || isOnBlur evt

{-|
Initializes the node style states. Mainly, it uses basic as the base of all the
other styles.
-}
initNodeStyle
  :: GetBaseStyle s e  -- ^ The function to get the base style.
  -> WidgetEnv s e     -- ^ The widget environment.
  -> WidgetNode s e    -- ^ The widget node.
  -> WidgetNode s e    -- ^ The updated widget node.
initNodeStyle getBaseStyle wenv node = newNode where
  nodeStyle = mergeBasicStyle $ node ^. L.info . L.style
  baseStyle = mergeBasicStyle $ fromMaybe def (getBaseStyle wenv node)
  themeStyle = baseStyleFromTheme (_weTheme wenv)
  newNode = node
    & L.info . L.style .~ (themeStyle <> baseStyle <> nodeStyle)

{-|
Checks for style changes between the old node and the provided result, in the
context of an event. Generates requests for resize, render and cursor change as
necessary.
-}
handleStyleChange
  :: WidgetEnv s e             -- ^ The widget environment.
  -> Path                      -- ^ The target of the event.
  -> StyleState                -- ^ The active style.
  -> Bool                      -- ^ Whether to check/update the cursor.
  -> WidgetNode s e            -- ^ The old node.
  -> SystemEvent               -- ^ The event.
  -> Maybe (WidgetResult s e)  -- ^ The result containing the new node.
  -> Maybe (WidgetResult s e)  -- ^ The updated result.
handleStyleChange wenv target style doCursor node evt result = newResult where
  newResult = handleSizeChange wenv target evt node result
    & handleCursorChange wenv target evt style node

-- Helpers
handleSizeChange
  :: WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
handleSizeChange wenv target evt oldNode result = newResult where
  baseResult = fromMaybe (resultNode oldNode) result
  newNode = baseResult ^. L.node
  -- Size
  oldSizeReqW = oldNode ^. L.info . L.sizeReqW
  oldSizeReqH = oldNode ^. L.info . L.sizeReqH
  newSizeReqW = newNode ^. L.info . L.sizeReqW
  newSizeReqH = newNode ^. L.info . L.sizeReqH
  sizeReqChanged = oldSizeReqW /= newSizeReqW || oldSizeReqH /= newSizeReqH
  -- Hover drag changed (if dragging, Enter/Leave is not sent)
  prevInVp = isPointInNodeVp (wenv ^. L.inputStatus . L.mousePosPrev) newNode
  currInVp = isPointInNodeVp (wenv ^. L.inputStatus . L.mousePos) newNode
  path = newNode ^. L.info . L.path
  pressedPath = wenv ^. L.mainBtnPress ^? _Just . _1
  hoverDragChg = Just path == pressedPath && prevInVp /= currInVp
  -- Result
  renderReq = isOnEnter evt || isOnLeave evt || hoverDragChg
  resizeReq = [ ResizeWidgets | sizeReqChanged ]
  enterReq = [ RenderOnce | renderReq ]
  reqs = resizeReq ++ enterReq
  newResult
    | not (null reqs) = Just $ baseResult
      & L.requests <>~ Seq.fromList reqs
    | otherwise = result

handleCursorChange
  :: WidgetEnv s e
  -> Path
  -> SystemEvent
  -> StyleState
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
handleCursorChange wenv target evt style oldNode result = newResult where
  baseResult = fromMaybe (resultNode oldNode) result
  baseReqs = baseResult ^. L.requests
  node = baseResult ^. L.node
  -- Cursor
  widgetId = node ^. L.info . L.widgetId
  path = node ^. L.info . L.path
  isTarget = path == target
  hasCursor = isJust (style ^. L.cursorIcon)
  isPressed = isNodePressed wenv node
  (curPath, curIcon) = fromMaybe def (wenv ^. L.cursor)
  isParent = seqStartsWith path curPath && path /= curPath
  newIcon = fromMaybe CursorArrow (style ^. L.cursorIcon)
  setCursor = hasCursor
    && isCursorEvt evt
    && not isParent
    && curIcon /= newIcon
  resetCursor = isTarget
    && not hasCursor
    && isCursorEvt evt
    && not isPressed
    && curPath == path
  -- Result
  newResult
    | setCursor = Just $ baseResult
      & L.requests .~ SetCursorIcon widgetId newIcon <| baseReqs
    | resetCursor = Just $ baseResult
      & L.requests .~ baseReqs |> ResetCursorIcon widgetId
    | otherwise = result

baseStyleFromTheme :: Theme -> Style
baseStyleFromTheme theme = style where
  style = Style {
    _styleBasic = fromThemeState (_themeBasic theme),
    _styleHover = fromThemeState (_themeHover theme),
    _styleFocus = fromThemeState (_themeFocus theme),
    _styleFocusHover = fromThemeState (_themeFocusHover theme),
    _styleActive = fromThemeState (_themeActive theme),
    _styleDisabled = fromThemeState (_themeDisabled theme)
  }
  fromThemeState tstate = Just $ def {
    _sstFgColor = Just $ _thsFgColor tstate,
    _sstHlColor = Just $ _thsHlColor tstate,
    _sstText = Just $ _thsTextStyle tstate
  }

mergeBasicStyle :: Style -> Style
mergeBasicStyle st = newStyle where
  focusHover = _styleHover st <> _styleFocus st <> _styleFocusHover st
  active = focusHover <> _styleActive st
  newStyle = Style {
    _styleBasic = _styleBasic st,
    _styleHover = _styleBasic st <> _styleHover st,
    _styleFocus = _styleBasic st <> _styleFocus st,
    _styleFocusHover = _styleBasic st <> focusHover,
    _styleActive = _styleBasic st <> active,
    _styleDisabled = _styleBasic st <> _styleDisabled st
  }

isCursorEvt :: SystemEvent -> Bool
isCursorEvt Enter{} = True
isCursorEvt Click{} = True
isCursorEvt DblClick{} = True
isCursorEvt ButtonAction{} = True
isCursorEvt Move{} = True
isCursorEvt _ = False
