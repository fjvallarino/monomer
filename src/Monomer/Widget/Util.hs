module Monomer.Widget.Util where

import Control.Lens (ALens', (&), (^#), (#~))
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
import Monomer.Graphics.Drawing (calcTextBounds)
import Monomer.Widget.Types

rootPath :: Path
rootPath = Seq.empty

defaultWidgetInstance :: WidgetType -> Widget s e -> WidgetInstance s e
defaultWidgetInstance widgetType widget = WidgetInstance {
  _wiType = widgetType,
  _wiKey = Nothing,
  _wiPath = Seq.empty,
  _wiWidget = widget,
  _wiChildren = Seq.empty,
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

key :: WidgetInstance s e -> Text -> WidgetInstance s e
key widgetInst key = widgetInst {
  _wiKey = Just (WidgetKey key)
}

style :: WidgetInstance s e -> Style -> WidgetInstance s e
style widgetInst newStyle = widgetInst {
  _wiStyle = newStyle
}

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
  typeMatches = _wiType oldInstance == _wiType newInstance
  keyMatches = _wiKey oldInstance == _wiKey newInstance

updateSizeReq :: SizeReq -> WidgetInstance s e -> SizeReq
updateSizeReq sizeReq widgetInst = newSizeReq where
  width = _styleWidth . _wiStyle $ widgetInst
  height = _styleHeight . _wiStyle $ widgetInst
  tempSizeReq
    | isNothing width = sizeReq
    | otherwise = sizeReq {
      _srSize = Size (fromJust width) (_w . _srSize $ sizeReq),
      _srPolicyWidth = StrictSize
    }
  newSizeReq
    | isNothing height = tempSizeReq
    | otherwise = tempSizeReq {
      _srSize = Size (_h . _srSize $ sizeReq) (fromJust height),
      _srPolicyHeight = StrictSize
    }

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

getTextBounds :: WidgetEnv s e -> Maybe TextStyle -> Text -> Size
getTextBounds wenv style text = calcTextBounds handler style text where
  handler = _wpGetTextSize (_wePlatform wenv)

isShortCutControl :: WidgetEnv s e -> KeyMod -> Bool
isShortCutControl wenv mod = isControl || isCommand where
  isControl = not (isMacOS wenv) && kmLeftCtrl mod
  isCommand = isMacOS wenv && kmLeftGUI mod

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
isFocused ctx widgetInst = _weFocusedPath ctx == _wiPath widgetInst

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
isTargetBeforeCurrent target widgetInst = targetPrefix < currentPath where
  currentPath = _wiPath widgetInst
  lenTarget = Seq.length target
  lenCurrent = Seq.length currentPath
  targetPrefix
    | lenTarget > lenCurrent = Seq.take lenCurrent target
    | otherwise = target
