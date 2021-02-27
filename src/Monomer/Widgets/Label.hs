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
  _lscIgnoreTheme :: Maybe Bool,
  _lscTextTrim :: Maybe Bool,
  _lscTextEllipsis :: Maybe Bool,
  _lscTextMultiLine :: Maybe Bool,
  _lscTextMaxLines :: Maybe Int,
  _lscFactorW :: Maybe Double,
  _lscFactorH :: Maybe Double
}

instance Default LabelCfg where
  def = LabelCfg {
    _lscIgnoreTheme = Nothing,
    _lscTextTrim = Nothing,
    _lscTextEllipsis = Nothing,
    _lscTextMultiLine = Nothing,
    _lscTextMaxLines = Nothing,
    _lscFactorW = Nothing,
    _lscFactorH = Nothing
  }

instance Semigroup LabelCfg where
  (<>) l1 l2 = LabelCfg {
    _lscIgnoreTheme = _lscIgnoreTheme l2 <|> _lscIgnoreTheme l1,
    _lscTextTrim = _lscTextTrim l2 <|> _lscTextTrim l1,
    _lscTextEllipsis = _lscTextEllipsis l2 <|> _lscTextEllipsis l1,
    _lscTextMultiLine = _lscTextMultiLine l2 <|> _lscTextMultiLine l1,
    _lscTextMaxLines = _lscTextMaxLines l2 <|> _lscTextMaxLines l1,
    _lscFactorW = _lscFactorW l2 <|> _lscFactorW l1,
    _lscFactorH = _lscFactorH l2 <|> _lscFactorH l1
  }

instance Monoid LabelCfg where
  mempty = def

instance CmbIgnoreTheme LabelCfg where
  ignoreTheme_ ignore = def {
    _lscIgnoreTheme = Just ignore
  }

instance CmbTextTrim LabelCfg where
  textTrim_ trim = def {
    _lscTextTrim = Just trim
  }

instance CmbTextEllipsis LabelCfg where
  textEllipsis_ ellipsis = def {
    _lscTextEllipsis = Just ellipsis
  }

instance CmbTextMultiLine LabelCfg where
  textMultiLine_ multi = def {
    _lscTextMultiLine = Just multi
  }

instance CmbTextMaxLines LabelCfg where
  textMaxLines count = def {
    _lscTextMaxLines = Just count
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

  ignoreTheme = _lscIgnoreTheme config == Just True
  trim
    | _lscTextTrim config == Just False = KeepSpaces
    | otherwise = TrimSpaces
  overflow
    | _lscTextEllipsis config == Just False = ClipText
    | otherwise = Ellipsis
  mode
    | _lscTextMultiLine config == Just True = MultiLine
    | otherwise = SingleLine
  maxLines = _lscTextMaxLines config
  LabelState caption textStyle textRect textLines prevResize = state

  getBaseStyle wenv node
    | ignoreTheme = Nothing
    | otherwise = Just $ collectTheme wenv L.labelStyle

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

  render renderer wenv node = do
    forM_ textLines (drawTextLine renderer style)
    where
      style = activeStyle wenv node
