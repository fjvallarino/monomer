module Monomer.Widgets.Label (
  label,
  label_
) where

import Debug.Trace

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Default
import Data.Maybe
import Data.List (foldl')
import Data.Sequence (Seq(..), (<|))
import Data.Text (Text)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data LabelCfg = LabelCfg {
  _lscTextOverflow :: Maybe TextOverflow,
  _lscMultiLine :: Maybe Bool
}

instance Default LabelCfg where
  def = LabelCfg {
    _lscTextOverflow = Nothing,
    _lscMultiLine = Nothing
  }

instance Semigroup LabelCfg where
  (<>) l1 l2 = LabelCfg {
    _lscTextOverflow = _lscTextOverflow l2 <|> _lscTextOverflow l1,
    _lscMultiLine = _lscMultiLine l2 <|> _lscMultiLine l1
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
    singleMerge = merge,
    singleGetState = makeState state,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  textOverflow = fromMaybe Ellipsis (_lscTextOverflow config)
  LabelState caption textLines = state

  getBaseStyle wenv inst = Just style where
    style = collectTheme wenv L.labelStyle

  merge wenv oldState inst = resultWidget newInstance where
    newState = fromMaybe state (useState oldState)
    newInstance = inst {
      _wiWidget = makeLabel config newState
    }

  getSizeReq wenv inst = sizeReq where
    style = activeStyle wenv inst
    Size w h = getTextSize wenv style caption
    factor = 1
    sizeReq = (FlexSize w factor, FixedSize h)

--  resize wenv viewport renderArea inst = newInst where
--    style = activeStyle wenv inst
--    contentArea = fromMaybe def (removeOuterBounds style renderArea)
--    (newCaptionFit, _) = case textOverflow of
--      Ellipsis -> fitText wenv style contentArea caption
--      _ -> (caption, def)
--    newWidget
--      | captionFit == newCaptionFit = _wiWidget inst
--      | otherwise = makeLabel config (LabelState caption newCaptionFit)
--    newInst = inst {
--      _wiWidget = newWidget
--    }

  resize wenv viewport renderArea inst = newInst where
    style = activeStyle wenv inst
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    Rect cx cy cw ch = contentArea
    (newTextLines, size) = fitTextLines wenv style cx cy cw caption
    newWidget = makeLabel config (LabelState caption newTextLines)
    newInst = inst {
      _wiWidget = newWidget
    }

  render renderer wenv inst =
    forM_ textLines (drawTextLine renderer style)
    where
      style = activeStyle wenv inst

drawTextLine :: Renderer -> StyleState -> TextLine -> IO ()
drawTextLine renderer style textLine = do
  setFillColor renderer fontColor
  renderText renderer point font fontSize text
  where
    TextLine text size rect glyphs = textLine
    Rect tx ty tw th = rect
    font = styleFont style
    fontSize = styleFontSize style
    fontColor = styleFontColor style
    point = Point tx (ty + th)

type GlyphGroup = Seq GlyphPos

data TextLine = TextLine {
  _tlText :: Text,
  _tlSize :: Size,
  _tlRect :: Rect,
  _tlGlyphs :: Seq GlyphPos
} deriving (Eq, Show)

fitTextLines
  :: WidgetEnv s e
  -> StyleState
  -> Double
  -> Double
  -> Double
  -> Text
  -> (Seq TextLine, Size)
fitTextLines wenv style left top width text = (resultLines, resultSize) where
  font = styleFont style
  fontSize = styleFontSize style
  metrics = computeTextMetrics (_weRenderer wenv) font fontSize
  lineH = _txhLineH metrics
  helper acc currLine = (currLines <> newLines, newTop) where
    (currLines, currTop) = acc
    newLines = fitTextLine wenv font fontSize left currTop width lineH currLine
    newTop = currTop + fromIntegral (Seq.length newLines) * lineH
  (resultLines, _) = foldl' helper (Empty, top) (T.lines text)
  resultWidth = maximum (fmap (_sW . _tlSize) resultLines)
  resultHeight = lineH * fromIntegral (Seq.length resultLines)
  resultSize
    | Seq.null resultLines = def
    | otherwise = Size resultWidth resultHeight

fitTextLine
  :: WidgetEnv s e
  -> Font
  -> FontSize
  -> Double
  -> Double
  -> Double
  -> Double
  -> Text
  -> Seq TextLine
fitTextLine wenv font fontSize left top width lineH text = result where
  spaces = "    "
  newText = T.replace "\t" spaces text
  glyphs = computeGlyphsPos (_weRenderer wenv) font fontSize newText
  groups = fitGroups (splitGroups glyphs) width
  resetGroups = fmap (`resetGlyphsPos` left) groups
  result = Seq.mapWithIndex (buildTextLine left top lineH) resetGroups

buildTextLine :: Double -> Double -> Double -> Int -> Seq GlyphPos -> TextLine
buildTextLine left top lineH idx glyphs = textLine where
  x = left
  y = top + fromIntegral idx * lineH
  width = glyphSeqLen glyphs
  height = lineH
  text = T.pack . reverse $ foldl' (\ac g -> _glpGlyph g : ac) [] glyphs
  textLine = TextLine {
    _tlText = text,
    _tlSize = Size width height,
    _tlRect = Rect x y width height,
    _tlGlyphs = glyphs
  }

fitGroups :: Seq GlyphGroup -> Double -> Seq GlyphGroup
fitGroups Empty _ = Empty
fitGroups (g :<| gs) width = currentLine <| extraLines where
  (lineGroups, remainingGroups) = fitExtraGroups gs (width - glyphSeqLen g)
  currentLine = g <> lineGroups
  extraLines = fitGroups remainingGroups width

fitExtraGroups :: Seq GlyphGroup -> Double -> (Seq GlyphPos, Seq GlyphGroup)
fitExtraGroups Empty _ = (Empty, Empty)
fitExtraGroups (g :<| gs) width
  | gw <= width = (g <> newFit, newRest)
  | otherwise = (Empty, g :<| gs)
  where
    gw = glyphSeqLen g
    (newFit, newRest) = fitExtraGroups gs (width - gw)

splitGroups :: Seq GlyphPos -> Seq GlyphGroup
splitGroups Empty = Empty
splitGroups glyphs = group <| splitGroups rest where
  g :<| gs = glyphs
  groupWordFn = not . isWordDelimiter . _glpGlyph
  (group, rest)
    | isWordDelimiter (_glpGlyph g) = (Seq.singleton g, gs)
    | otherwise = Seq.spanl groupWordFn glyphs

glyphSeqLen :: Seq GlyphPos -> Double
glyphSeqLen glyphs = foldl' (\ac gl -> ac + _glpW gl) 0 glyphs

resetGlyphsPos :: Seq GlyphPos -> Double -> Seq GlyphPos
resetGlyphsPos Empty _ = Empty
resetGlyphsPos (g :<| gs) start = newG <| resetGlyphsPos gs newStart where
  newG = g {
    _glpXMin = start,
    _glpXMax = start + _glpW g
  }
  newStart = start + _glpW g

isWordDelimiter :: Char -> Bool
isWordDelimiter = (== ' ')
