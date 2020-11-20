{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Button (
  ButtonCfg,
  button,
  button_,
  mainButton,
  mainButton_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.))
import Control.Monad (forM_, when)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..))
import Data.Text (Text)

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
  _btnTrim :: Maybe Bool,
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

instance TextOverflow_ (ButtonCfg s e) where
  textEllipsis = def {
    _btnTextOverflow = Just Ellipsis
  }
  textClip = def {
    _btnTextOverflow = Just ClipText
  }

instance TextMode_ (ButtonCfg s e) where
  textSingleLine = def {
    _btnTextMode = Just SingleLine
  }
  textMultiLine = def {
    _btnTextMode = Just MultiLine
  }

instance TextTrim (ButtonCfg s e) where
  textTrim = def {
    _btnTrim = Just True
  }
  textKeepSpaces = def {
    _btnTrim = Just False
  }

instance OnFocus (ButtonCfg s e) e where
  onFocus fn = def {
    _btnOnFocus = [fn]
  }

instance OnFocusReq (ButtonCfg s e) s where
  onFocusReq req = def {
    _btnOnFocusReq = [req]
  }

instance OnBlur (ButtonCfg s e) e where
  onBlur fn = def {
    _btnOnBlur = [fn]
  }

instance OnBlurReq (ButtonCfg s e) s where
  onBlurReq req = def {
    _btnOnBlurReq = [req]
  }

instance OnClick (ButtonCfg s e) e where
  onClick handler = def {
    _btnOnClick = [handler]
  }

instance OnClickReq (ButtonCfg s e) s where
  onClickReq req = def {
    _btnOnClickReq = [req]
  }

instance ResizeFactorDim (ButtonCfg s e) where
  resizeFactorW w = def {
    _btnFactorW = Just w
  }
  resizeFactorH h = def {
    _btnFactorH = Just h
  }

data BtnState = BtnState {
  _btnCaption :: Text,
  _btnTextLines :: Seq TextLine
} deriving (Eq, Show)

mainConfig :: ButtonCfg s e
mainConfig = def {
  _btnButtonType = Just ButtonMain
}

mainButton :: Text -> e -> WidgetInstance s e
mainButton caption handler = button_ caption handler [mainConfig]

mainButton_ :: Text -> e -> [ButtonCfg s e] -> WidgetInstance s e
mainButton_ caption handler configs = button_ caption handler newConfigs where
  newConfigs = mainConfig : configs

button :: Text -> e -> WidgetInstance s e
button caption handler = button_ caption handler def

button_ :: Text -> e -> [ButtonCfg s e] -> WidgetInstance s e
button_ caption handler configs = buttonInstance where
  config = onClick handler <> mconcat configs
  state = BtnState caption Empty
  widget = makeButton config state
  buttonInstance = (defaultWidgetInstance "button" widget) {
    _wiFocusable = True
  }

makeButton :: ButtonCfg s e -> BtnState -> Widget s e
makeButton config state = widget where
  widget = createSingle def {
    singleGetBaseStyle = getBaseStyle,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  buttonType = fromMaybe ButtonNormal (_btnButtonType config)
  overflow = fromMaybe Ellipsis (_btnTextOverflow config)
  mode = fromMaybe SingleLine (_btnTextMode config)
  trimSpaces = fromMaybe True (_btnTrim config)
  BtnState caption textLines = state

  getBaseStyle wenv inst = case buttonType of
    ButtonNormal -> Just (collectTheme wenv L.btnStyle)
    ButtonMain -> Just (collectTheme wenv L.btnMainStyle)

  handleEvent wenv ctx evt inst = case evt of
    Focus -> handleFocusChange _btnOnFocus _btnOnFocusReq config inst
    Blur -> handleFocusChange _btnOnBlur _btnOnBlurReq config inst
    KeyAction mode code status
      | isSelectKey code && status == KeyPressed -> Just result
      where
        isSelectKey code = isKeyReturn code || isKeySpace code
    Click p _
      | pointInViewport p inst -> Just result
    _ -> Nothing
    where
      requests = _btnOnClickReq config
      events = _btnOnClick config
      result = resultReqsEvents requests events inst

  getSizeReq wenv inst = (sizeW, sizeH) where
    style = activeStyle wenv inst
    targetW = fmap sizeReqMax (style ^. L.sizeReqW)
    Size w h = getTextSize_ wenv style mode trimSpaces targetW caption
    factorW = fromMaybe 0.01 (_btnFactorW config)
    factorH = fromMaybe 0 (_btnFactorH config)
    sizeW
      | abs factorW < 0.01 = FixedSize w
      | otherwise = FlexSize w factorW
    sizeH
      | abs factorH < 0.01 = FixedSize h
      | otherwise = FlexSize h factorH

  resize wenv viewport renderArea inst = newInst where
    style = activeStyle wenv inst
    rect = fromMaybe def (removeOuterBounds style renderArea)
    newLines = fitTextToRect wenv style overflow mode trimSpaces rect caption
    newWidget = makeButton config (BtnState caption newLines)
    newInst = inst {
      _wiWidget = newWidget
    }

  render renderer wenv inst = do
    when isPressed $
      drawRect renderer renderArea bgColor (_sstRadius style)
    forM_ textLines (drawTextLine renderer style)

    where
      style = activeStyle wenv inst
      inputStatus = wenv ^. L.inputStatus
      renderArea = _wiRenderArea inst
      isPressed = isButtonPressedInRect inputStatus LeftBtn renderArea
      bgColor = Just $ Color 0 0 0 0.2
