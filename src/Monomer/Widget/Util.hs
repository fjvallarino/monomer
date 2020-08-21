{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Reduce duplication" -}

module Monomer.Widget.Util where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^#), (#~), (^.), (.~), (?~), non, _Just)
import Data.Default
import Data.Maybe
import Data.List (foldl')
import Data.Sequence (Seq, (><), (|>))
import Data.Text (Text)
import Data.Typeable (cast, Typeable)

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Core (checkKeyboard)
import Monomer.Event.Keyboard (isKeyC, isKeyV)
import Monomer.Event.Types
import Monomer.Graphics.Drawing (calcTextBounds, drawStyledBackground)
import Monomer.Graphics.Renderer
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
  _wiSizeReq = def,
  _wiEnabled = True,
  _wiVisible = True,
  _wiFocusable = False,
  _wiViewport = def,
  _wiRenderArea = def,
  _wiStyle = def
}

widgetValueGet :: s -> WidgetValue s a -> a
widgetValueGet _ (WidgetValue value) = value
widgetValueGet model (WidgetLens lens) = model ^# lens

widgetValueSet :: WidgetValue s a -> a -> [WidgetRequest s]
widgetValueSet WidgetValue{} _ = []
widgetValueSet (WidgetLens lens) value = [UpdateModel updateFn] where
  updateFn model = model & lens #~ value

infixl 5 `key`
infixl 5 `style`
infixl 5 `hover`
infixl 5 `focus`

key :: WidgetInstance s e -> Text -> WidgetInstance s e
key widgetInst key = widgetInst {
  _wiKey = Just (WidgetKey key)
}

style :: WidgetInstance s e -> StyleState -> WidgetInstance s e
style inst state = inst & C.style .~ newStyle where
  oldStyle = _wiStyle inst
  newStyle = oldStyle & S.basic ?~ state

hover :: WidgetInstance s e -> StyleState -> WidgetInstance s e
hover inst state = inst & C.style .~ newStyle where
  oldStyle = _wiStyle inst
  newStyle = oldStyle & S.hover ?~ state

focus :: WidgetInstance s e -> StyleState -> WidgetInstance s e
focus inst state = inst & C.style .~ newStyle where
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

getTextBounds :: WidgetEnv s e -> StyleState -> Text -> Size
getTextBounds wenv style text = calcTextBounds handler textStyle text where
  textStyle = _sstText style
  handler = _wpGetTextSize (_wePlatform wenv)

getFullTextBounds :: WidgetEnv s e -> StyleState -> Text -> Size
getFullTextBounds wenv style text = getTextBounds wenv style text

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
isMacOS wenv = _wpOS (_wePlatform wenv) == "Mac OS X"

firstChildPath :: WidgetInstance s e -> Path
firstChildPath widgetInst = _wiPath widgetInst |> 0

nextTargetStep :: Path -> WidgetInstance s e -> Maybe PathStep
nextTargetStep target widgetInst = nextStep where
  currentPath = _wiPath widgetInst
  nextStep = Seq.lookup (Seq.length currentPath) target

pointInViewport :: Point -> WidgetInstance s e -> Bool
pointInViewport p inst = pointInRect p (_wiViewport inst)

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

activeFgColor :: WidgetEnv s e -> WidgetInstance s e -> Color
activeFgColor wenv inst = fromMaybe themeColor styleColor where
  style = activeStyle wenv inst
  theme = activeTheme wenv inst
  styleColor = style ^. S.fgColor
  themeColor = theme ^. S.fgColor

textStyle :: StyleState -> TextStyle
textStyle sst = fromMaybe def (_sstText sst)

textFont :: StyleState -> Font
textFont style = fromMaybe def (_txsFont $ textStyle style)

textSize :: StyleState -> FontSize
textSize style = fromMaybe def (_txsFontSize $ textStyle style)

textColor :: StyleState -> Color
textColor style = fromMaybe def (_txsColor $ textStyle style)

resizeInstance :: WidgetEnv s e -> WidgetInstance s e -> WidgetInstance s e
resizeInstance wenv inst = newInst where
  viewport = _wiViewport inst
  renderArea = _wiRenderArea inst
  instReqs = widgetUpdateSizeReq (_wiWidget inst) wenv inst
  newInst = widgetResize (_wiWidget instReqs) wenv viewport renderArea instReqs

isFocusCandidate :: Path -> WidgetInstance s e -> Bool
isFocusCandidate startFrom widgetInst = isValid where
  isBefore = isTargetBeforeCurrent startFrom widgetInst
  isFocusable = _wiFocusable widgetInst
  isEnabled = _wiVisible widgetInst && _wiEnabled widgetInst
  isValid = isBefore && isFocusable && isEnabled

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
