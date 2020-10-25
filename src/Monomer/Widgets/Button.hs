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
import Data.Default
import Data.Maybe
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
  _btnOnClick :: [e],
  _btnOnClickReq :: [WidgetRequest s]
}

instance Default (ButtonCfg s e) where
  def = ButtonCfg {
    _btnButtonType = Nothing,
    _btnTextOverflow = Nothing,
    _btnOnClick = [],
    _btnOnClickReq = []
  }

instance Semigroup (ButtonCfg s e) where
  (<>) t1 t2 = ButtonCfg {
    _btnButtonType = _btnButtonType t2 <|> _btnButtonType t1,
    _btnTextOverflow = _btnTextOverflow t2 <|> _btnTextOverflow t1,
    _btnOnClick = _btnOnClick t1 <> _btnOnClick t2,
    _btnOnClickReq = _btnOnClickReq t1 <> _btnOnClickReq t2
  }

instance Monoid (ButtonCfg s e) where
  mempty = def

instance OnTextOverflow (ButtonCfg s e) where
  textEllipsis = def {
    _btnTextOverflow = Just Ellipsis
  }
  textClip = def {
    _btnTextOverflow = Just ClipText
  }

instance OnClick (ButtonCfg s e) e where
  onClick handler = def {
    _btnOnClick = [handler]
  }

instance OnClickReq (ButtonCfg s e) s where
  onClickReq req = def {
    _btnOnClickReq = [req]
  }

data BtnState = BtnState {
  _btnCaption :: Text,
  _btnCaptionFit :: Text
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
button_ caption handler configs = defaultWidgetInstance "button" widget where
  config = onClick handler <> mconcat configs
  state = BtnState caption caption
  widget = makeButton config state

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
  textOverflow = fromMaybe Ellipsis (_btnTextOverflow config)
  BtnState caption captionFit = state

  getBaseStyle wenv inst = case buttonType of
    ButtonNormal -> Just (collectTheme wenv L.btnStyle)
    ButtonMain -> Just (collectTheme wenv L.btnMainStyle)

  handleEvent wenv ctx evt inst = case evt of
    Click p _
      | pointInViewport p inst -> Just result
      where
        requests = _btnOnClickReq config
        events = _btnOnClick config
        result = resultReqsEvents requests events inst
    _ -> Nothing

  getSizeReq wenv inst = sizeReq where
    style = activeStyle wenv inst
    Size w h = getTextSize wenv style caption
    factor = 1
    sizeReq = (FlexSize w factor, FixedSize h)

  resize wenv viewport renderArea inst = newInst where
    style = activeStyle wenv inst
    size = getTextSize wenv style caption
    (newCaptionFit, _) = case textOverflow of
      Ellipsis -> fitText wenv style renderArea caption
      _ -> (caption, def)
    newWidget
      | captionFit == newCaptionFit = _wiWidget inst
      | otherwise = makeButton config (BtnState caption newCaptionFit)
    newInst = inst {
      _wiWidget = newWidget
    }

  render renderer wenv inst = do
    setScissor renderer contentRect
    drawStyledText_ renderer contentRect style captionFit
    resetScissor renderer
    where
      style = activeStyle wenv inst
      contentRect = getContentRect style inst
