module Monomer.Widgets.Label (
  label,
  label_
) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Core.BaseSingle
import Monomer.Core.BasicTypes
import Monomer.Core.Style
import Monomer.Core.Types
import Monomer.Core.Util
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widgets.WidgetCombinators

newtype LabelCfg = LabelCfg {
  _lscTextOverflow :: Maybe TextOverflow
}

instance Default LabelCfg where
  def = LabelCfg {
    _lscTextOverflow = Nothing
  }

instance Semigroup LabelCfg where
  (<>) l1 l2 = LabelCfg {
    _lscTextOverflow = _lscTextOverflow l2 <|> _lscTextOverflow l1
  }

instance Monoid LabelCfg where
  mempty = def

instance OnTextOverflow LabelCfg where
  textEllipsis = def {
    _lscTextOverflow = Just Ellipsis
  }
  textClip = def {
    _lscTextOverflow = Just ClipText
  }

data LabelState = LabelState {
  _lstCaption :: Text,
  _lstCaptionFit :: Text
} deriving (Eq, Show)

label :: Text -> WidgetInstance s e
label caption = label_ caption def

label_ :: Text -> [LabelCfg] -> WidgetInstance s e
label_ caption configs = defaultWidgetInstance "label" widget where
  config = mconcat configs
  state = LabelState caption caption
  widget = makeLabel config state

makeLabel :: LabelCfg -> LabelState -> Widget s e
makeLabel config state = widget where
  widget = createSingle def {
    singleMerge = merge,
    singleGetState = makeState state,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  textOverflow = fromMaybe Ellipsis (_lscTextOverflow config)
  LabelState caption captionFit = state

  merge wenv oldState widgetInst = resultWidget newInstance where
    newState = fromMaybe state (useState oldState)
    newInstance = widgetInst {
      _wiWidget = makeLabel config newState
    }

  getSizeReq wenv widgetInst = sizeReq where
    theme = activeTheme wenv widgetInst
    style = activeStyle wenv widgetInst
    Size w h = getTextSize wenv theme style caption
    factor = 1
    sizeReq = (FlexSize w factor, FlexSize h factor)

  resize wenv viewport renderArea widgetInst = newInst where
    theme = activeTheme wenv widgetInst
    style = activeStyle wenv widgetInst
    size = getTextSize wenv theme style caption
    (newCaptionFit, _) = case textOverflow of
      Ellipsis -> fitText wenv theme style renderArea caption
      _ -> (caption, def)
    newWidget
      | captionFit == newCaptionFit = _wiWidget widgetInst
      | otherwise = makeLabel config (LabelState caption newCaptionFit)
    newInst = widgetInst {
      _wiWidget = newWidget,
      _wiViewport = viewport,
      _wiRenderArea = renderArea
    }

  render renderer wenv inst =
    drawInScissor renderer True contentRect $
      drawStyledText_ renderer contentRect style captionFit
    where
      style = activeStyle wenv inst
      contentRect = getContentRect style inst
