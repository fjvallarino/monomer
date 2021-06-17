{-|
Module      : Monomer.Widgets.Singles.Button
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Button widget, with support for multiline text. At the most basic level, a
button consists of a caption and an event to raised when clicked.

Configs:

- trimSpaces: whether to remove leading/trailing spaces in the caption.
- ellipsis: if ellipsis should be used for overflown text.
- multiLine: if text may be split in multiple lines.
- maxLines: maximum number of text lines to show.
- onFocus: event to raise when focus is received.
- onFocusReq: WidgetRequest to generate when focus is received.
- onBlur: event to raise when focus is lost.
- onBlurReq: WidgetRequest to generate when focus is lost.
- onClick: event to raise when button is clicked.
- onClickReq: WidgetRequest to generate when button is clicked.
- resizeFactor: flexibility to have more or less spaced assigned.
- resizeFactorW: flexibility to have more or less horizontal spaced assigned.
- resizeFactorH: flexibility to have more or less vertical spaced assigned.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Singles.Button (
  button,
  button_,
  mainButton,
  mainButton_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Maybe
import Data.Text (Text)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

data ButtonType
  = ButtonNormal
  | ButtonMain
  deriving (Eq, Show)

data ButtonCfg s e = ButtonCfg {
  _btnButtonType :: Maybe ButtonType,
  _btnTextTrim :: Maybe Bool,
  _btnTextEllipsis :: Maybe Bool,
  _btnTextMultiLine :: Maybe Bool,
  _btnTextMaxLines :: Maybe Int,
  _btnFactorW :: Maybe Double,
  _btnFactorH :: Maybe Double,
  _btnOnFocusReq :: [Path -> WidgetRequest s e],
  _btnOnBlurReq :: [Path -> WidgetRequest s e],
  _btnOnClickReq :: [WidgetRequest s e]
}

instance Default (ButtonCfg s e) where
  def = ButtonCfg {
    _btnButtonType = Nothing,
    _btnTextTrim = Nothing,
    _btnTextEllipsis = Nothing,
    _btnTextMultiLine = Nothing,
    _btnTextMaxLines = Nothing,
    _btnFactorW = Nothing,
    _btnFactorH = Nothing,
    _btnOnFocusReq = [],
    _btnOnBlurReq = [],
    _btnOnClickReq = []
  }

instance Semigroup (ButtonCfg s e) where
  (<>) t1 t2 = ButtonCfg {
    _btnButtonType = _btnButtonType t2 <|> _btnButtonType t1,
    _btnTextTrim = _btnTextTrim t2 <|> _btnTextTrim t1,
    _btnTextEllipsis = _btnTextEllipsis t2 <|> _btnTextEllipsis t1,
    _btnTextMultiLine = _btnTextMultiLine t2 <|> _btnTextMultiLine t1,
    _btnTextMaxLines = _btnTextMaxLines t2 <|> _btnTextMaxLines t1,
    _btnFactorW = _btnFactorW t2 <|> _btnFactorW t1,
    _btnFactorH = _btnFactorH t2 <|> _btnFactorH t1,
    _btnOnFocusReq = _btnOnFocusReq t1 <> _btnOnFocusReq t2,
    _btnOnBlurReq = _btnOnBlurReq t1 <> _btnOnBlurReq t2,
    _btnOnClickReq = _btnOnClickReq t1 <> _btnOnClickReq t2
  }

instance Monoid (ButtonCfg s e) where
  mempty = def

instance CmbTrimSpaces (ButtonCfg s e) where
  trimSpaces_ trim = def {
    _btnTextTrim = Just trim
  }

instance CmbEllipsis (ButtonCfg s e) where
  ellipsis_ ellipsis = def {
    _btnTextEllipsis = Just ellipsis
  }

instance CmbMultiLine (ButtonCfg s e) where
  multiLine_ multi = def {
    _btnTextMultiLine = Just multi
  }

instance CmbMaxLines (ButtonCfg s e) where
  maxLines count = def {
    _btnTextMaxLines = Just count
  }

instance WidgetEvent e => CmbOnFocus (ButtonCfg s e) e Path where
  onFocus fn = def {
    _btnOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (ButtonCfg s e) s e Path where
  onFocusReq req = def {
    _btnOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (ButtonCfg s e) e Path where
  onBlur fn = def {
    _btnOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (ButtonCfg s e) s e Path where
  onBlurReq req = def {
    _btnOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnClick (ButtonCfg s e) e where
  onClick handler = def {
    _btnOnClickReq = [RaiseEvent handler]
  }

instance CmbOnClickReq (ButtonCfg s e) s e where
  onClickReq req = def {
    _btnOnClickReq = [req]
  }

instance CmbResizeFactor (ButtonCfg s e) where
  resizeFactor s = def {
    _btnFactorW = Just s,
    _btnFactorH = Just s
  }

instance CmbResizeFactorDim (ButtonCfg s e) where
  resizeFactorW w = def {
    _btnFactorW = Just w
  }
  resizeFactorH h = def {
    _btnFactorH = Just h
  }

mainConfig :: ButtonCfg s e
mainConfig = def {
  _btnButtonType = Just ButtonMain
}

-- | Creates a button with main styling. Useful for dialogs.
mainButton :: WidgetEvent e => Text -> e -> WidgetNode s e
mainButton caption handler = button_ caption handler [mainConfig]

-- | Creates a button with main styling. Useful for dialogs. Accepts config.
mainButton_ :: WidgetEvent e => Text -> e -> [ButtonCfg s e] -> WidgetNode s e
mainButton_ caption handler configs = button_ caption handler newConfigs where
  newConfigs = mainConfig : configs

-- | Creates a button with normal styling.
button :: WidgetEvent e => Text -> e -> WidgetNode s e
button caption handler = button_ caption handler def

-- | Creates a button with normal styling. Accepts config.
button_ :: WidgetEvent e => Text -> e -> [ButtonCfg s e] -> WidgetNode s e
button_ caption handler configs = buttonNode where
  config = onClick handler <> mconcat configs
  widget = makeButton caption config
  buttonNode = defaultWidgetNode "button" widget
    & L.info . L.focusable .~ True

makeButton :: WidgetEvent e => Text -> ButtonCfg s e -> Widget s e
makeButton caption config = widget where
  widget = createContainer () def {
    containerUseScissor = True,
    containerGetBaseStyle = getBaseStyle,
    containerInit = init,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  buttonType = fromMaybe ButtonNormal (_btnButtonType config)
  trim = _btnTextTrim config == Just True
  ellipsis = _btnTextEllipsis config == Just True
  multiLine = _btnTextMultiLine config == Just True
  maxLinesV = _btnTextMaxLines config
  factorW = _btnFactorW config
  factorH = _btnFactorH config

  getBaseStyle wenv node = case buttonType of
    ButtonNormal -> Just (collectTheme wenv L.btnStyle)
    ButtonMain -> Just (collectTheme wenv L.btnMainStyle)

  createChildNode wenv node = newNode where
    nodeStyle = node ^. L.info . L.style
    labelStyle = collectStyleField_ L.text nodeStyle def
      & collectStyleField_ L.sizeReqW nodeStyle
      & collectStyleField_ L.sizeReqH nodeStyle
    cfgs = [
      ignoreTheme,
      trimSpaces_ trim,
      ellipsis_ ellipsis,
      multiLine_ multiLine]
      ++ [maxLines (fromJust maxLinesV) | isJust maxLinesV]
      ++ [resizeFactorW (fromJust factorW) | isJust factorW]
      ++ [resizeFactorH (fromJust factorH) | isJust factorH]
    labelNode = label_ caption cfgs
      & L.info . L.style .~ labelStyle
    childNode = labelNode
    newNode = node
      & L.children .~ Seq.singleton childNode

  init wenv node = result where
    result = resultNode (createChildNode wenv node)

  merge wenv node oldNode oldState = result where
    result = resultNode (createChildNode wenv node)

  handleEvent wenv node target evt = case evt of
    Focus prev -> handleFocusChange (_btnOnFocusReq config) prev node
    Blur next -> handleFocusChange (_btnOnBlurReq config) next node
    KeyAction mode code status
      | isSelectKey code && status == KeyPressed -> Just result
      where
        isSelectKey code = isKeyReturn code || isKeySpace code
    Click p _
      | isPointInNodeVp p node -> Just result
    -- Set focus on click
    ButtonAction p btn BtnPressed 1
      | mainBtn btn && pointInVp p && not focused -> Just resultFocus
    ButtonAction p btn BtnReleased clicks
      | mainBtn btn && focused && pointInVp p && clicks > 1 -> Just result
    _ -> Nothing
    where
      mainBtn btn = btn == wenv ^. L.mainButton
      focused = isNodeFocused wenv node
      pointInVp p = isPointInNodeVp p node
      reqs = _btnOnClickReq config
      result = resultReqs node reqs
      resultFocus = resultReqs node [SetFocus (node ^. L.info . L.widgetId)]

  getSizeReq :: ContainerGetSizeReqHandler s e
  getSizeReq wenv node children = (newReqW, newReqH) where
    -- Main section reqs
    child = Seq.index children 0
    newReqW = child ^. L.info . L.sizeReqW
    newReqH = child ^. L.info . L.sizeReqH

  resize wenv node viewport children = resized where
    assignedAreas = Seq.fromList [viewport]
    resized = (resultNode node, assignedAreas)
