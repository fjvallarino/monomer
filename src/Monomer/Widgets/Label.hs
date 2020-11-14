module Monomer.Widgets.Label (
  label,
  label_
) where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..))
import Data.Text (Text)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data LabelCfg = LabelCfg {
  _lscTextOverflow :: Maybe TextOverflow,
  _lscTextMode :: Maybe TextMode,
  _lscTrim :: Maybe Bool
}

instance Default LabelCfg where
  def = LabelCfg {
    _lscTextOverflow = Nothing,
    _lscTextMode = Nothing,
    _lscTrim = Nothing
  }

instance Semigroup LabelCfg where
  (<>) l1 l2 = LabelCfg {
    _lscTextOverflow = _lscTextOverflow l2 <|> _lscTextOverflow l1,
    _lscTextMode = _lscTextMode l2 <|> _lscTextMode l1,
    _lscTrim = _lscTrim l2 <|> _lscTrim l1
  }

instance Monoid LabelCfg where
  mempty = def

instance TextOverflow_ LabelCfg where
  textEllipsis = def {
    _lscTextOverflow = Just Ellipsis
  }
  textClip = def {
    _lscTextOverflow = Just ClipText
  }

instance TextMode_ LabelCfg where
  textSingleLine = def {
    _lscTextMode = Just SingleLine
  }
  textMultiLine = def {
    _lscTextMode = Just MultiLine
  }

instance TextTrim LabelCfg where
  textTrim = def {
    _lscTrim = Just True
  }
  textKeepSpaces = def {
    _lscTrim = Just False
  }

data LabelState = LabelState {
  _lstCaption :: Text,
  _lstTextLines :: Seq TextLine
} deriving (Eq, Show)

label :: Text -> WidgetInstance s e
label caption = label_ caption def

label_ :: Text -> [LabelCfg] -> WidgetInstance s e
label_ caption configs = defaultWidgetInstance "label" widget where
  config = mconcat configs
  state = LabelState caption Seq.Empty
  widget = makeLabel config state

makeLabel :: LabelCfg -> LabelState -> Widget s e
makeLabel config state = widget where
  widget = createSingle def {
    singleGetBaseStyle = getBaseStyle,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  overflow = fromMaybe Ellipsis (_lscTextOverflow config)
  mode = fromMaybe SingleLine (_lscTextMode config)
  trimSpaces = fromMaybe True (_lscTrim config)
  LabelState caption textLines = state

  getBaseStyle wenv inst = Just style where
    style = collectTheme wenv L.labelStyle

  getSizeReq wenv inst = sizeReq where
    style = activeStyle wenv inst
    targetW = fmap sizeReqMax (style ^. L.sizeReqW)
    Size w h = getTextSize_ wenv style mode trimSpaces targetW caption
    factor = 1
    sizeReq = (FlexSize w factor, FixedSize h)

  resize wenv viewport renderArea inst = newInst where
    style = activeStyle wenv inst
    rect = fromMaybe def (removeOuterBounds style renderArea)
    newLines = fitTextToRect wenv style overflow mode trimSpaces rect caption
    newWidget = makeLabel config (LabelState caption newLines)
    newInst = inst {
      _wiWidget = newWidget
    }

  render renderer wenv inst = action where
    style = activeStyle wenv inst
    action = forM_ textLines (drawTextLine renderer style)
