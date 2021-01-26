{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Monomer.Widgets.Label (
  label,
  label_
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
import Control.Monad (forM_)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..))
import Data.Text (Text)
import GHC.Generics

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data LabelCfg = LabelCfg {
  _lscTextOverflow :: Maybe TextOverflow,
  _lscTextMode :: Maybe TextMode,
  _lscTrim :: Maybe TextTrim,
  _lscFactorW :: Maybe Double,
  _lscFactorH :: Maybe Double
}

instance Default LabelCfg where
  def = LabelCfg {
    _lscTextOverflow = Nothing,
    _lscTextMode = Nothing,
    _lscTrim = Nothing,
    _lscFactorW = Nothing,
    _lscFactorH = Nothing
  }

instance Semigroup LabelCfg where
  (<>) l1 l2 = LabelCfg {
    _lscTextOverflow = _lscTextOverflow l2 <|> _lscTextOverflow l1,
    _lscTextMode = _lscTextMode l2 <|> _lscTextMode l1,
    _lscTrim = _lscTrim l2 <|> _lscTrim l1,
    _lscFactorW = _lscFactorW l2 <|> _lscFactorW l1,
    _lscFactorH = _lscFactorH l2 <|> _lscFactorH l1
  }

instance Monoid LabelCfg where
  mempty = def

instance CmbTextOverflow LabelCfg where
  textEllipsis = def {
    _lscTextOverflow = Just Ellipsis
  }
  textClip = def {
    _lscTextOverflow = Just ClipText
  }

instance CmbTextMode LabelCfg where
  textSingleLine = def {
    _lscTextMode = Just SingleLine
  }
  textMultiLine = def {
    _lscTextMode = Just MultiLine
  }

instance CmbTextTrim LabelCfg where
  textTrim = def {
    _lscTrim = Just TrimSpaces
  }
  textKeepSpaces = def {
    _lscTrim = Just KeepSpaces
  }

instance CmbResizeFactorDim LabelCfg where
  resizeFactorW w = def {
    _lscFactorW = Just w
  }
  resizeFactorH h = def {
    _lscFactorH = Just h
  }

data LabelState = LabelState {
  _lstCaption :: Text,
  _lstTextStyle :: Maybe TextStyle,
  _lstTextRect :: Rect,
  _lstTextLines :: Seq TextLine
} deriving (Eq, Show, Generic, Serialise)

label :: Text -> WidgetNode s e
label caption = label_ caption def

label_ :: Text -> [LabelCfg] -> WidgetNode s e
label_ caption configs = defaultWidgetNode "label" widget where
  config = mconcat configs
  state = LabelState caption Nothing def Seq.Empty
  widget = makeLabel config state

makeLabel :: LabelCfg -> LabelState -> Widget s e
makeLabel config state = widget where
  widget = createSingle state def {
    singleGetBaseStyle = getBaseStyle,
    singleRestore = restore,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  overflow = fromMaybe Ellipsis (_lscTextOverflow config)
  mode = fromMaybe SingleLine (_lscTextMode config)
  trimSpaces = fromMaybe TrimSpaces (_lscTrim config)
  LabelState caption textStyle textRect textLines = state

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.labelStyle

  restore wenv oldState oldNode newNode = result where
    captionChanged = _lstCaption oldState /= caption
    -- This is used in resize to have glyphs recalculated
    newRect
      | captionChanged = def
      | otherwise = _lstTextRect oldState
    newState = oldState {
      _lstCaption = caption,
      _lstTextRect = newRect
    }
    reqs = [ ResizeWidgets | captionChanged ]
    resNode = newNode
      & L.widget .~ makeLabel config newState
    result = resultReqs resNode reqs

  getSizeReq wenv currState node = (sizeW, sizeH) where
    caption = _lstCaption currState
    style = activeStyle wenv node
    targetW = fmap sizeReqMaxBounded (style ^. L.sizeReqW)
    Size w h = getTextSize_ wenv style mode trimSpaces targetW caption
    factorW = fromMaybe 0.01 (_lscFactorW config)
    factorH = fromMaybe 0 (_lscFactorH config)
    sizeW
      | abs factorW < 0.01 = FixedSize w
      | otherwise = FlexSize w factorW
    sizeH
      | abs factorH < 0.01 = FixedSize h
      | otherwise = FlexSize h factorH

  resize wenv viewport renderArea node = resultWidget newNode where
    style = activeStyle wenv node
    rect = fromMaybe def (removeOuterBounds style renderArea)
    newTextStyle = style ^. L.text
    Rect px py pw ph = textRect
    Rect nx ny nw nh = rect
    fittedLines = fitTextToRect wenv style overflow mode trimSpaces rect caption
    newTextLines = alignTextLines style rect fittedLines
    newGlyphsReq = pw /= nw || ph /= nh || textStyle /= newTextStyle
    newLines
      | not newGlyphsReq = moveTextLines (nx - px) (ny - py) textLines
      | otherwise = newTextLines
    newWidget = makeLabel config (LabelState caption newTextStyle rect newLines)
    newNode = node
      & L.widget .~ newWidget

  render renderer wenv node = action where
    style = activeStyle wenv node
    action = forM_ textLines (drawTextLine renderer style)
