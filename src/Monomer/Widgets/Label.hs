{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Monomer.Widgets.Label (
  label,
  label_,
  labelS,
  labelS_
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
  _lscMaxLines :: Maybe Int,
  _lscTextOverflow :: Maybe TextOverflow,
  _lscTextMode :: Maybe TextMode,
  _lscTrim :: Maybe TextTrim,
  _lscFactorW :: Maybe Double,
  _lscFactorH :: Maybe Double
}

instance Default LabelCfg where
  def = LabelCfg {
    _lscMaxLines = Nothing,
    _lscTextOverflow = Nothing,
    _lscTextMode = Nothing,
    _lscTrim = Nothing,
    _lscFactorW = Nothing,
    _lscFactorH = Nothing
  }

instance Semigroup LabelCfg where
  (<>) l1 l2 = LabelCfg {
    _lscMaxLines = _lscMaxLines l2 <|> _lscMaxLines l1,
    _lscTextOverflow = _lscTextOverflow l2 <|> _lscTextOverflow l1,
    _lscTextMode = _lscTextMode l2 <|> _lscTextMode l1,
    _lscTrim = _lscTrim l2 <|> _lscTrim l1,
    _lscFactorW = _lscFactorW l2 <|> _lscFactorW l1,
    _lscFactorH = _lscFactorH l2 <|> _lscFactorH l1
  }

instance Monoid LabelCfg where
  mempty = def

instance CmbMaxLines LabelCfg where
  maxLines count = def {
    _lscMaxLines = Just count
  }

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

instance CmbResizeFactor LabelCfg where
  resizeFactor s = def {
    _lscFactorW = Just s,
    _lscFactorH = Just s
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
  _lstTextLines :: Seq TextLine,
  _lstPrevResize :: (Int, Bool)
} deriving (Eq, Show, Generic, Serialise)

instance WidgetModel LabelState where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

label :: Text -> WidgetNode s e
label caption = label_ caption def

label_ :: Text -> [LabelCfg] -> WidgetNode s e
label_ caption configs = defaultWidgetNode "label" widget where
  config = mconcat configs
  state = LabelState caption Nothing def Seq.Empty (0, False)
  widget = makeLabel config state

labelS :: Show a => a -> WidgetNode s e
labelS caption = labelS_ caption def

labelS_ :: Show a => a -> [LabelCfg] -> WidgetNode s e
labelS_ caption configs = label_ (T.pack . show $ caption) configs

makeLabel :: LabelCfg -> LabelState -> Widget s e
makeLabel config state = widget where
  widget = createSingle state def {
    singleUseScissor = True,
    singleGetBaseStyle = getBaseStyle,
    singleInit = init,
    singleRestore = restore,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  overflow = fromMaybe Ellipsis (_lscTextOverflow config)
  mode = fromMaybe SingleLine (_lscTextMode config)
  trim = fromMaybe TrimSpaces (_lscTrim config)
  maxLines = _lscMaxLines config
  LabelState caption textStyle textRect textLines prevResize = state

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.labelStyle

  init wenv node = resultWidget newNode where
    style = activeStyle wenv node
    newState = state {
      _lstTextStyle = style ^. L.text
    }
    newNode = node
      & L.widget .~ makeLabel config newState

  restore wenv oldState oldNode newNode = result where
    style = activeStyle wenv newNode
    newTextStyle = style ^. L.text
    captionChanged = _lstCaption oldState /= caption
    styleChanged = _lstTextStyle oldState /= newTextStyle
    changeReq = captionChanged || styleChanged
    -- This is used in resize to have glyphs recalculated
    newRect
      | changeReq = def
      | otherwise = _lstTextRect oldState
    newState = oldState {
      _lstCaption = caption,
      _lstTextRect = newRect,
      _lstTextStyle = newTextStyle
    }
    reqs = [ ResizeWidgets | changeReq ]
    resNode = newNode
      & L.widget .~ makeLabel config newState
    result = resultReqs resNode reqs

  getSizeReq wenv currState node = (sizeW, sizeH) where
    caption = _lstCaption currState
    prevResize = _lstPrevResize currState
    ts = wenv ^. L.timestamp
    style = activeStyle wenv node
    cw = getContentArea style node ^. L.w
    targetW
      | mode == MultiLine && prevResize == (ts, True) = Just cw
      | otherwise = fmap sizeReqMaxBounded (style ^. L.sizeReqW)
    Size w h = getTextSize_ wenv style mode trim targetW maxLines caption
    defaultFactor
      | mode == MultiLine = 0.01
      | otherwise = 0
    factorW = fromMaybe defaultFactor (_lscFactorW config)
    factorH = fromMaybe defaultFactor (_lscFactorH config)
    sizeW
      | abs factorW < 0.01 = fixedSize w
      | otherwise = expandSize w factorW
    sizeH
      | abs factorH < 0.01 = fixedSize h
      | otherwise = expandSize h factorH

  resize wenv viewport node = result where
    ts = wenv ^. L.timestamp
    style = activeStyle wenv node
    crect = fromMaybe def (removeOuterBounds style viewport)
    newTextStyle = style ^. L.text
    Rect px py pw ph = textRect
    Rect cx cy cw ch = crect
    renderer = wenv ^. L.renderer
    fittedLines = fitTextToRect renderer style overflow mode trim maxLines crect caption
    newTextLines = alignTextLines style crect fittedLines
    newGlyphsReq = pw /= cw || ph /= ch || textStyle /= newTextStyle
    newLines
      | not newGlyphsReq = moveTextLines (cx - px) (cy - py) textLines
      | otherwise = newTextLines
    (prevTs, prevStep) = prevResize
    needsSndResize = mode == MultiLine && (prevTs /= ts || not prevStep)
    newState = state {
      _lstTextStyle = newTextStyle,
      _lstTextRect = crect,
      _lstTextLines = newLines,
      _lstPrevResize = (ts, needsSndResize && prevTs == ts)
    }
    newNode = node
      & L.widget .~ makeLabel config newState
    result = resultReqs newNode [ResizeWidgets | needsSndResize]

  render renderer wenv node = action where
    style = activeStyle wenv node
    action = forM_ textLines (drawTextLine renderer style)
