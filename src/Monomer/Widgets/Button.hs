{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Button (
  button,
  button_,
  mainButton,
  mainButton_
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
import Control.Monad (forM_, when)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..))
import Data.Text (Text)
import GHC.Generics

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data ButtonType
  = ButtonNormal
  | ButtonMain
  deriving (Eq, Show)

data ButtonCfg s e = ButtonCfg {
  _btnButtonType :: Maybe ButtonType,
  _btnTextOverflow :: Maybe TextOverflow,
  _btnTextMode :: Maybe TextMode,
  _btnTrim :: Maybe TextTrim,
  _btnFactorW :: Maybe Double,
  _btnFactorH :: Maybe Double,
  _btnOnFocus :: [e],
  _btnOnFocusReq :: [WidgetRequest s],
  _btnOnBlur :: [e],
  _btnOnBlurReq :: [WidgetRequest s],
  _btnOnClick :: [e],
  _btnOnClickReq :: [WidgetRequest s]
}

instance Default (ButtonCfg s e) where
  def = ButtonCfg {
    _btnButtonType = Nothing,
    _btnTextOverflow = Nothing,
    _btnTextMode = Nothing,
    _btnTrim = Nothing,
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
    _btnTextOverflow = _btnTextOverflow t2 <|> _btnTextOverflow t1,
    _btnTextMode = _btnTextMode t2 <|> _btnTextMode t1,
    _btnTrim = _btnTrim t2 <|> _btnTrim t1,
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

instance CmbTextOverflow (ButtonCfg s e) where
  textEllipsis = def {
    _btnTextOverflow = Just Ellipsis
  }
  textClip = def {
    _btnTextOverflow = Just ClipText
  }

instance CmbTextMode (ButtonCfg s e) where
  textSingleLine = def {
    _btnTextMode = Just SingleLine
  }
  textMultiLine = def {
    _btnTextMode = Just MultiLine
  }

instance CmbTextTrim (ButtonCfg s e) where
  textTrim = def {
    _btnTrim = Just TrimSpaces
  }
  textKeepSpaces = def {
    _btnTrim = Just KeepSpaces
  }

instance CmbOnFocus (ButtonCfg s e) e where
  onFocus fn = def {
    _btnOnFocus = [fn]
  }

instance CmbOnFocusReq (ButtonCfg s e) s where
  onFocusReq req = def {
    _btnOnFocusReq = [req]
  }

instance CmbOnBlur (ButtonCfg s e) e where
  onBlur fn = def {
    _btnOnBlur = [fn]
  }

instance CmbOnBlurReq (ButtonCfg s e) s where
  onBlurReq req = def {
    _btnOnBlurReq = [req]
  }

instance CmbOnClick (ButtonCfg s e) e where
  onClick handler = def {
    _btnOnClick = [handler]
  }

instance CmbOnClickReq (ButtonCfg s e) s where
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

data BtnState = BtnState {
  _btsCaption :: Text,
  _btsTextStyle :: Maybe TextStyle,
  _btsTextRect :: Rect,
  _btsTextLines :: Seq TextLine
} deriving (Eq, Show, Generic, Serialise)

instance WidgetModel BtnState where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

mainConfig :: ButtonCfg s e
mainConfig = def {
  _btnButtonType = Just ButtonMain
}

mainButton :: Text -> e -> WidgetNode s e
mainButton caption handler = button_ caption handler [mainConfig]

mainButton_ :: Text -> e -> [ButtonCfg s e] -> WidgetNode s e
mainButton_ caption handler configs = button_ caption handler newConfigs where
  newConfigs = mainConfig : configs

button :: Text -> e -> WidgetNode s e
button caption handler = button_ caption handler def

button_ :: Text -> e -> [ButtonCfg s e] -> WidgetNode s e
button_ caption handler configs = buttonNode where
  config = onClick handler <> mconcat configs
  state = BtnState caption Nothing def Empty
  widget = makeButton config state
  buttonNode = defaultWidgetNode "button" widget
    & L.info . L.focusable .~ True

makeButton :: ButtonCfg s e -> BtnState -> Widget s e
makeButton config state = widget where
  widget = createSingle state def {
    singleUseScissor = True,
    singleGetBaseStyle = getBaseStyle,
    singleInit = init,
    singleRestore = restore,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  buttonType = fromMaybe ButtonNormal (_btnButtonType config)
  overflow = fromMaybe Ellipsis (_btnTextOverflow config)
  mode = fromMaybe SingleLine (_btnTextMode config)
  trim = fromMaybe TrimSpaces (_btnTrim config)
  BtnState caption textStyle textRect textLines = state

  getBaseStyle wenv node = case buttonType of
    ButtonNormal -> Just (collectTheme wenv L.btnStyle)
    ButtonMain -> Just (collectTheme wenv L.btnMainStyle)

  init wenv node = resultWidget newNode where
    style = activeStyle wenv node
    newState = state {
      _btsTextStyle = style ^. L.text
    }
    newNode = node
      & L.widget .~ makeButton config newState

  restore wenv oldState oldNode newNode = result where
    style = activeStyle wenv newNode
    newTextStyle = style ^. L.text
    captionChanged = _btsCaption oldState /= caption
    styleChanged = _btsTextStyle oldState /= newTextStyle
    changeReq = captionChanged || styleChanged
    -- This is used in resize to have glyphs recalculated
    newRect
      | captionChanged = def
      | otherwise = _btsTextRect oldState
    newState = oldState {
      _btsCaption = caption,
      _btsTextRect = newRect,
      _btsTextStyle = newTextStyle
    }
    reqs = [ ResizeWidgets | changeReq ]
    resNode = newNode
      & L.widget .~ makeButton config newState
    result = resultReqs resNode reqs

  handleEvent wenv target evt node = case evt of
    Focus -> handleFocusChange _btnOnFocus _btnOnFocusReq config node
    Blur -> handleFocusChange _btnOnBlur _btnOnBlurReq config node
    KeyAction mode code status
      | isSelectKey code && status == KeyPressed -> Just result
      where
        isSelectKey code = isKeyReturn code || isKeySpace code
    Click p _
      | isPointInNodeVp p node -> Just result
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

  getSizeReq wenv currState node = (sizeW, sizeH) where
    caption = _btsCaption currState
    style = activeStyle wenv node
    targetW = fmap sizeReqMaxBounded (style ^. L.sizeReqW)
    Size w h = getTextSize_ wenv style mode trim targetW Nothing caption
    factorW = fromMaybe 0.01 (_btnFactorW config)
    factorH = fromMaybe 0 (_btnFactorH config)
    sizeW
      | abs factorW < 0.01 = fixedSize w
      | otherwise = expandSize w factorW
    sizeH
      | abs factorH < 0.01 = fixedSize h
      | otherwise = expandSize h factorH

  resize wenv viewport node = resultWidget newNode where
    style = activeStyle wenv node
    rect = fromMaybe def (removeOuterBounds style viewport)
    newTextStyle = style ^. L.text
    Rect px py pw ph = textRect
    Rect nx ny nw nh = rect
    renderer = wenv ^. L.renderer
    fittedLines = fitTextToRect renderer style overflow mode trim Nothing rect caption
    newTextLines = alignTextLines style rect fittedLines
    newGlyphsReq = pw /= nw || ph /= nh || textStyle /= newTextStyle
    newLines
      | not newGlyphsReq = moveTextLines (nx - px) (ny - py) textLines
      | otherwise = newTextLines
    newWidget = makeButton config (BtnState caption newTextStyle rect newLines)
    newNode = node
      & L.widget .~ newWidget

  render renderer wenv node = do
    forM_ textLines (drawTextLine renderer style)

    where
      style = activeStyle wenv node
