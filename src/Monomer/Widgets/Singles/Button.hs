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
  _btnOnFocus :: [e],
  _btnOnFocusReq :: [WidgetRequest s e],
  _btnOnBlur :: [e],
  _btnOnBlurReq :: [WidgetRequest s e],
  _btnOnClick :: [e],
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
    _btnOnFocus = [],
    _btnOnFocusReq = [],
    _btnOnBlur = [],
    _btnOnBlurReq = [],
    _btnOnClick = [],
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
    _btnOnFocus = _btnOnFocus t1 <> _btnOnFocus t2,
    _btnOnFocusReq = _btnOnFocusReq t1 <> _btnOnFocusReq t2,
    _btnOnBlur = _btnOnBlur t1 <> _btnOnBlur t2,
    _btnOnBlurReq = _btnOnBlurReq t1 <> _btnOnBlurReq t2,
    _btnOnClick = _btnOnClick t1 <> _btnOnClick t2,
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

instance CmbOnFocus (ButtonCfg s e) e where
  onFocus fn = def {
    _btnOnFocus = [fn]
  }

instance CmbOnFocusReq (ButtonCfg s e) s e where
  onFocusReq req = def {
    _btnOnFocusReq = [req]
  }

instance CmbOnBlur (ButtonCfg s e) e where
  onBlur fn = def {
    _btnOnBlur = [fn]
  }

instance CmbOnBlurReq (ButtonCfg s e) s e where
  onBlurReq req = def {
    _btnOnBlurReq = [req]
  }

instance CmbOnClick (ButtonCfg s e) e where
  onClick handler = def {
    _btnOnClick = [handler]
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

mainButton :: WidgetEvent e => Text -> e -> WidgetNode s e
mainButton caption handler = button_ caption handler [mainConfig]

mainButton_ :: WidgetEvent e => Text -> e -> [ButtonCfg s e] -> WidgetNode s e
mainButton_ caption handler configs = button_ caption handler newConfigs where
  newConfigs = mainConfig : configs

button :: WidgetEvent e => Text -> e -> WidgetNode s e
button caption handler = button_ caption handler def

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
    result = resultWidget (createChildNode wenv node)

  merge wenv oldState oldNode node = result where
    result = resultWidget (createChildNode wenv node)

  handleEvent wenv target evt node = case evt of
    Focus -> handleFocusChange _btnOnFocus _btnOnFocusReq config node
    Blur -> handleFocusChange _btnOnBlur _btnOnBlurReq config node
    KeyAction mode code status
      | isSelectKey code && status == KeyPressed -> Just result
      where
        isSelectKey code = isKeyReturn code || isKeySpace code
    Click p _
      | isPointInNodeVp p node -> Just result
    -- Set focus on click
    ButtonAction p btn PressedBtn 1
      | mainBtn btn && pointInVp p && not focused -> Just resultFocus
    ButtonAction p btn ReleasedBtn clicks
      | mainBtn btn && focused && pointInVp p && clicks > 1 -> Just result
    _ -> Nothing
    where
      mainBtn btn = btn == wenv ^. L.mainButton
      focused = isNodeFocused wenv node
      pointInVp p = isPointInNodeVp p node
      requests = _btnOnClickReq config
      events = _btnOnClick config
      result = resultReqsEvts node requests events
      resultFocus = resultReqs node [SetFocus (node ^. L.info . L.widgetId)]

  getSizeReq :: ContainerGetSizeReqHandler s e a
  getSizeReq wenv currState node children = (newReqW, newReqH) where
    -- Main section reqs
    child = Seq.index children 0
    newReqW = child ^. L.info . L.sizeReqW
    newReqH = child ^. L.info . L.sizeReqH

  resize wenv viewport children node = resized where
    assignedAreas = Seq.fromList [viewport]
    resized = (resultWidget node, assignedAreas)
