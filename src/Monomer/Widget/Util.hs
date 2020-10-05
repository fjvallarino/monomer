{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Util where

import Control.Lens (ALens', (&), (^#), (#~), (^.), (^?), (.~), (?~), non, _Just)
import Data.Default
import Data.Maybe
import Data.List (foldl')
import Data.Sequence (Seq(..), (><), (|>))
import Data.Text (Text)
import Data.Typeable (cast, Typeable)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Common.BasicTypes
import Monomer.Common.Style
import Monomer.Common.StyleUtil
import Monomer.Event.Core (checkKeyboard)
import Monomer.Event.Keyboard (isKeyC, isKeyV)
import Monomer.Event.Types
import Monomer.Graphics.Types
import Monomer.Widget.Types

import qualified Monomer.Common.LensStyle as S
import qualified Monomer.Widget.LensCore as C

defaultWidgetInstance :: WidgetType -> Widget s e -> WidgetInstance s e
defaultWidgetInstance widgetType widget = WidgetInstance {
  _wiWidgetType = widgetType,
  _wiKey = Nothing,
  _wiPath = Seq.empty,
  _wiWidget = widget,
  _wiChildren = Seq.empty,
  _wiSizeReqW = def,
  _wiSizeReqH = def,
  _wiEnabled = True,
  _wiVisible = True,
  _wiFocusable = False,
  _wiViewport = def,
  _wiRenderArea = def,
  _wiStyle = def
}

widgetDataGet :: s -> WidgetData s a -> a
widgetDataGet _ (WidgetValue value) = value
widgetDataGet model (WidgetLens lens) = model ^# lens

widgetDataSet :: WidgetData s a -> a -> [WidgetRequest s]
widgetDataSet WidgetValue{} _ = []
widgetDataSet (WidgetLens lens) value = [UpdateModel updateFn] where
  updateFn model = model & lens #~ value

infixl 5 `key`
infixl 5 `style`
infixl 5 `hover`
infixl 5 `focus`

key :: WidgetInstance s e -> Text -> WidgetInstance s e
key widgetInst key = widgetInst {
  _wiKey = Just (WidgetKey key)
}

style :: WidgetInstance s e -> [StyleState] -> WidgetInstance s e
style inst states = inst & C.style .~ newStyle where
  state = mconcat states
  oldStyle = _wiStyle inst
  newStyle = oldStyle & S.basic ?~ state

hover :: WidgetInstance s e -> [StyleState] -> WidgetInstance s e
hover inst states = inst & C.style .~ newStyle where
  state = mconcat states
  oldStyle = _wiStyle inst
  newStyle = oldStyle & S.hover ?~ state

focus :: WidgetInstance s e -> [StyleState] -> WidgetInstance s e
focus inst states = inst & C.style .~ newStyle where
  state = mconcat states
  oldStyle = _wiStyle inst
  newStyle = oldStyle & S.focus ?~ state

visible :: WidgetInstance s e -> Bool -> WidgetInstance s e
visible widgetInst visibility = widgetInst {
  _wiVisible = visibility
}

resultWidget :: WidgetInstance s e -> WidgetResult s e
resultWidget widgetInst = WidgetResult Seq.empty Seq.empty widgetInst

resultEvents :: [e] -> WidgetInstance s e -> WidgetResult s e
resultEvents events widgetInst = result where
  result = WidgetResult Seq.empty (Seq.fromList events) widgetInst

resultReqs :: [WidgetRequest s] -> WidgetInstance s e -> WidgetResult s e
resultReqs requests widgetInst = result where
  result = WidgetResult (Seq.fromList requests) Seq.empty widgetInst

resultReqsEvents
  :: [WidgetRequest s] -> [e] -> WidgetInstance s e -> WidgetResult s e
resultReqsEvents requests events widgetInst = result where
  result = WidgetResult (Seq.fromList requests) (Seq.fromList events) widgetInst

makeState :: Typeable i => i -> s -> Maybe WidgetState
makeState state model = Just (WidgetState state)

useState ::  Typeable i => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

instanceMatches :: WidgetInstance s e -> WidgetInstance s e -> Bool
instanceMatches newInstance oldInstance = typeMatches && keyMatches where
  typeMatches = _wiWidgetType oldInstance == _wiWidgetType newInstance
  keyMatches = _wiKey oldInstance == _wiKey newInstance

isSendMessageHandler :: WidgetRequest s -> Bool
isSendMessageHandler SendMessage{} = True
isSendMessageHandler _ = False

isTaskHandler :: WidgetRequest s -> Bool
isTaskHandler RunTask{} = True
isTaskHandler _ = False

isProducerHandler :: WidgetRequest s -> Bool
isProducerHandler RunProducer{} = True
isProducerHandler _ = False

isIgnoreParentEvents :: WidgetRequest s -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False

isIgnoreChildrenEvents :: WidgetRequest s -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isSetFocus :: WidgetRequest s -> Bool
isSetFocus SetFocus{} = True
isSetFocus _ = False

isResize :: WidgetRequest s -> Bool
isResize Resize = True
isResize _ = False

isGetClipboard :: WidgetRequest s -> Bool
isGetClipboard GetClipboard{} = True
isGetClipboard _ = False

isSetClipboard :: WidgetRequest s -> Bool
isSetClipboard SetClipboard{} = True
isSetClipboard _ = False

isSetOverlay :: WidgetRequest s -> Bool
isSetOverlay SetOverlay{} = True
isSetOverlay _ = False

isResetOverlay :: WidgetRequest s -> Bool
isResetOverlay ResetOverlay = True
isResetOverlay _ = False

isUpdateModel :: WidgetRequest s -> Bool
isUpdateModel UpdateModel{} = True
isUpdateModel _ = False

getUpdateModelReqs :: (Traversable t) => t (WidgetRequest s) -> Seq (s -> s)
getUpdateModelReqs reqs = foldl' foldHelper Seq.empty reqs where
  foldHelper acc (UpdateModel fn) = acc |> fn
  foldHelper acc _ = acc

getTextSize :: WidgetEnv s e -> ThemeState -> StyleState -> Text -> Size
getTextSize wenv theme style text = handler font fontSize text where
  handler = computeTextSize (_weRenderer wenv)
  (font, fontSize) = getFontAndSize theme style

getFullTextSize :: WidgetEnv s e -> ThemeState -> StyleState -> Text -> Size
getFullTextSize wenv theme style text = totalBounds where
  textBounds = getTextSize wenv theme style text
  totalBounds = addOuterSize style textBounds

getTextRect
  :: WidgetEnv s e -> ThemeState -> StyleState -> Rect -> Align -> Text -> Rect
getTextRect wenv theme style rect align text = textRect where
  textRect = computeTextRect (_weRenderer wenv) rect font fontSize align text
  (font, fontSize) = getFontAndSize theme style

fitText
  :: WidgetEnv s e -> ThemeState -> StyleState -> Rect -> Text -> (Text, Size)
fitText wenv theme style viewport text = (newText, newSize) where
  (font, fontSize) = getFontAndSize theme style
  sizeHandler = computeTextSize (_weRenderer wenv)
  size = sizeHandler font fontSize text
  (newText, newSize)
    | _sW size <= _rW viewport = (text, size)
    | otherwise = fitEllipsis wenv theme style viewport size text

fitEllipsis
  :: WidgetEnv s e
  -> ThemeState
  -> StyleState
  -> Rect
  -> Size
  -> Text
  -> (Text, Size)
fitEllipsis wenv theme style viewport textSize text = (newText, newSize) where
  Size tw th = textSize
  vpW = _rW viewport
  glyphs = getTextGlyphs wenv theme style (text <> ".")
  dotW = _glpW $ Seq.index glyphs (Seq.length glyphs - 1)
  dotsW = 3 * dotW
  dotsFit = vpW >= tw + dotsW
  targetW
    | dotsFit = vpW
    | otherwise = vpW - dotsW
  (gCount, gWidth) = fitGlyphsCount targetW 0 glyphs
  remW = vpW - gWidth
  dotCount = min 3 . max 0 $ round (remW / dotW)
  newText
    | dotsFit = text <> "..."
    | otherwise = T.take gCount text <> T.replicate dotCount "."
  newWidth
    | dotsFit = tw + dotsW
    | otherwise = gWidth + fromIntegral dotCount * dotW
  newSize = Size newWidth th

getTextGlyphs
  :: WidgetEnv s e -> ThemeState -> StyleState -> Text -> Seq GlyphPos
getTextGlyphs wenv theme style text = glyphs where
  (font, fontSize) = getFontAndSize theme style
  glyphs = computeGlyphsPos (_weRenderer wenv) font fontSize text

glyphsLength :: Seq GlyphPos -> Double
glyphsLength Empty = 0
glyphsLength (gs :|> g) = _glpXMax g

fitGlyphsCount :: Double -> Double -> Seq GlyphPos -> (Int, Double)
fitGlyphsCount _ _ Empty = (0, 0)
fitGlyphsCount totalW currW (g :<| gs)
  | newCurrW <= totalW = (gCount + 1, gWidth + gsW)
  | otherwise = (0, 0)
  where
    gsW = _glpW g
    newCurrW = currW + gsW
    (gCount, gWidth) = fitGlyphsCount totalW newCurrW gs

getFontAndSize :: ThemeState -> StyleState -> (Font, FontSize)
getFontAndSize theme style = (font, fontSize) where
  styleFont = style ^? S.text . _Just  . S.font . _Just
  styleFontSize = style ^? S.text . _Just . S.fontSize . _Just
  themeFont = theme ^. S.font
  themeFontSize = theme ^. S.fontSize
  font = fromMaybe themeFont styleFont
  fontSize = fromMaybe themeFontSize styleFontSize

isShortCutControl :: WidgetEnv s e -> KeyMod -> Bool
isShortCutControl wenv mod = isControl || isCommand where
  isControl = not (isMacOS wenv) && _kmLeftCtrl mod
  isCommand = isMacOS wenv && _kmLeftGUI mod

isClipboardCopy :: WidgetEnv s e -> SystemEvent -> Bool
isClipboardCopy wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod && isKeyC code

isClipboardPaste :: WidgetEnv s e -> SystemEvent -> Bool
isClipboardPaste wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod && isKeyV code

isMacOS :: WidgetEnv s e -> Bool
isMacOS wenv = _weOS wenv == "Mac OS X"

parentPath :: WidgetInstance s e -> Path
parentPath widgetInst = Seq.take (Seq.length path - 1) path where
  path = _wiPath widgetInst

firstChildPath :: WidgetInstance s e -> Path
firstChildPath widgetInst = _wiPath widgetInst |> 0

nextTargetStep :: Path -> WidgetInstance s e -> Maybe PathStep
nextTargetStep target widgetInst = nextStep where
  currentPath = _wiPath widgetInst
  nextStep = Seq.lookup (Seq.length currentPath) target

pointInViewport :: Point -> WidgetInstance s e -> Bool
pointInViewport p inst = pointInRect p (_wiViewport inst)

getContentRect :: StyleState -> WidgetInstance s e -> Rect
getContentRect style inst = removeOuterBounds style (_wiRenderArea inst)

isFocused :: WidgetEnv s e -> WidgetInstance s e -> Bool
isFocused wenv widgetInst = _weFocusedPath wenv == _wiPath widgetInst

activeStyle :: WidgetEnv s e -> WidgetInstance s e -> StyleState
activeStyle wenv inst = fromMaybe def styleState where
  Style{..} = _wiStyle inst
  mousePos = _ipsMousePos $ _weInputStatus wenv
  isHover = pointInViewport mousePos inst
  isFocus = isFocused wenv inst
  styleState
    | isHover && isFocus = _styleBasic <> _styleFocus <> _styleHover
    | isHover = _styleBasic <> _styleHover
    | isFocus = _styleBasic <> _styleFocus
    | otherwise = _styleBasic

activeTheme :: WidgetEnv s e -> WidgetInstance s e -> ThemeState
activeTheme wenv inst = themeState where
  theme = _weTheme wenv
  mousePos = _ipsMousePos $ _weInputStatus wenv
  isHover = pointInViewport mousePos inst
  isFocus = isFocused wenv inst
  themeState
    | isHover = _themeHover theme
    | isFocus = _themeFocus theme
    | otherwise = _themeBasic theme

instanceStyle :: WidgetEnv s e -> WidgetInstance s e -> StyleState
instanceStyle wenv inst = mergeThemeStyle theme style where
  style = activeStyle wenv inst
  theme = activeTheme wenv  inst

instanceFgColor :: WidgetEnv s e -> WidgetInstance s e -> Color
instanceFgColor wenv inst = fromMaybe themeColor styleColor where
  style = activeStyle wenv inst
  theme = activeTheme wenv inst
  styleColor = _sstFgColor style
  themeColor = _thsFgColor theme

instanceHlColor :: WidgetEnv s e -> WidgetInstance s e -> Color
instanceHlColor wenv inst = fromMaybe themeColor styleColor where
  style = activeStyle wenv inst
  theme = activeTheme wenv inst
  styleColor = _sstHlColor style
  themeColor = _thsHlColor theme

instanceTextStyle :: WidgetEnv s e -> WidgetInstance s e -> TextStyle
instanceTextStyle wenv inst = textStyle <> textTheme where
  style = activeStyle wenv inst
  theme = activeTheme wenv inst
  textStyle = fromMaybe def (_sstText style)
  textTheme = TextStyle {
    _txsFont = Just (_thsFont theme),
    _txsFontSize = Just (_thsFontSize theme),
    _txsFontColor = Just (_thsFontColor theme),
    _txsAlignH = Nothing,
    _txsAlignV = Nothing
  }

instanceFont :: WidgetEnv s e -> WidgetInstance s e -> Font
instanceFont wenv inst = fromJust (_txsFont textStyle) where
  textStyle = instanceTextStyle wenv inst

instanceFontSize :: WidgetEnv s e -> WidgetInstance s e -> FontSize
instanceFontSize wenv inst = fromJust (_txsFontSize textStyle) where
  textStyle = instanceTextStyle wenv inst

instanceFontColor :: WidgetEnv s e -> WidgetInstance s e -> Color
instanceFontColor wenv inst = fromJust (_txsFontColor textStyle) where
  textStyle = instanceTextStyle wenv inst

resizeInstance :: WidgetEnv s e -> WidgetInstance s e -> WidgetInstance s e
resizeInstance wenv inst = newInst where
  viewport = _wiViewport inst
  renderArea = _wiRenderArea inst
  instReqs = widgetUpdateSizeReq (_wiWidget inst) wenv inst
  newInst = widgetResize (_wiWidget instReqs) wenv viewport renderArea instReqs

isFocusCandidate :: FocusDirection -> Path -> WidgetInstance s e -> Bool
isFocusCandidate FocusFwd = isFocusFwdCandidate
isFocusCandidate FocusBwd = isFocusBwdCandidate

isFocusFwdCandidate :: Path -> WidgetInstance s e -> Bool
isFocusFwdCandidate startFrom widgetInst = isValid where
  isBefore = isTargetBeforeCurrent startFrom widgetInst
  isFocusable = _wiFocusable widgetInst
  isEnabled = _wiVisible widgetInst && _wiEnabled widgetInst
  isValid = isBefore && isFocusable && isEnabled

isFocusBwdCandidate :: Path -> WidgetInstance s e -> Bool
isFocusBwdCandidate startFrom widgetInst = isValid where
  isAfter = isTargetAfterCurrent startFrom widgetInst
  isFocusable = _wiFocusable widgetInst
  isEnabled = _wiVisible widgetInst && _wiEnabled widgetInst
  isValid = isAfter && isFocusable && isEnabled

isTargetReached :: Path -> WidgetInstance s e -> Bool
isTargetReached target widgetInst = target == _wiPath widgetInst

isTargetValid :: Path -> WidgetInstance s e -> Bool
isTargetValid target widgetInst = valid where
  children = _wiChildren widgetInst
  valid = case nextTargetStep target widgetInst of
    Just step -> step < Seq.length children
    Nothing -> False

isTargetBeforeCurrent :: Path -> WidgetInstance s e -> Bool
isTargetBeforeCurrent target widgetInst = result where
  currentPath = _wiPath widgetInst
  lenTarget = Seq.length target
  lenCurrent = Seq.length currentPath
  targetPrefix = Seq.take lenCurrent target
  result
    | lenTarget > lenCurrent = targetPrefix <= currentPath
    | otherwise = target < currentPath

isTargetAfterCurrent :: Path -> WidgetInstance s e -> Bool
isTargetAfterCurrent target widgetInst
  | target == rootPath = True
  | otherwise = target > currentPath
  where
    currentPath = _wiPath widgetInst

numberInBounds :: (Ord a, Num a) => Maybe a -> Maybe a -> a -> Bool
numberInBounds Nothing Nothing _ = True
numberInBounds (Just minVal) Nothing val = val >= minVal
numberInBounds Nothing (Just maxVal) val = val <= maxVal
numberInBounds (Just minVal) (Just maxVal) val = val >= minVal && val <= maxVal
