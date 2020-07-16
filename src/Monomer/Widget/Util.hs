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

defaultWidgetInstance :: WidgetType -> Widget s e -> WidgetInstance s e
defaultWidgetInstance widgetType widget = WidgetInstance {
  _instanceType = widgetType,
  _instanceKey = Nothing,
  _instanceWidget = widget,
  _instanceChildren = Seq.empty,
  _instanceEnabled = True,
  _instanceVisible = True,
  _instanceFocusable = False,
  _instanceViewport = def,
  _instanceRenderArea = def,
  _instanceStyle = def
}

widgetValueGet :: s -> WidgetValue s a -> a
widgetValueGet _ (WidgetValue value) = value
widgetValueGet model (WidgetLens lens) = model ^# lens

widgetValueSet :: WidgetValue s a -> a -> [WidgetRequest s]
widgetValueSet WidgetValue{} _ = []
widgetValueSet (WidgetLens lens) value = [UpdateUserState $ \model -> model & lens #~ value]

key :: WidgetInstance s e -> Text -> WidgetInstance s e
key wn key = wn { _instanceKey = Just (WidgetKey key) }

style :: WidgetInstance s e -> Style -> WidgetInstance s e
style widgetInstance newStyle = widgetInstance { _instanceStyle = newStyle }

visible :: WidgetInstance s e -> Bool -> WidgetInstance s e
visible widgetInstance visibility = widgetInstance { _instanceVisible = visibility }

resultWidget :: WidgetInstance s e -> WidgetResult s e
resultWidget widgetInstance = WidgetResult Seq.empty Seq.empty widgetInstance

resultEvents :: [e] -> WidgetInstance s e -> WidgetResult s e
resultEvents userEvents widgetInstance = WidgetResult Seq.empty (Seq.fromList userEvents) widgetInstance

resultReqs :: [WidgetRequest s] -> WidgetInstance s e -> WidgetResult s e
resultReqs requests widgetInstance = WidgetResult (Seq.fromList requests) Seq.empty widgetInstance

resultReqsEvents :: [WidgetRequest s] -> [e] -> WidgetInstance s e -> WidgetResult s e
resultReqsEvents requests userEvents widgetInstance = WidgetResult (Seq.fromList requests) (Seq.fromList userEvents) widgetInstance

makeState :: Typeable i => i -> s -> Maybe WidgetState
makeState state model = Just (WidgetState state)

useState ::  Typeable i => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

instanceMatches :: WidgetInstance s e -> WidgetInstance s e -> Bool
instanceMatches newInstance oldInstance = typeMatches && keyMatches where
  typeMatches = _instanceType oldInstance == _instanceType newInstance
  keyMatches = _instanceKey oldInstance == _instanceKey newInstance

updateSizeReq :: SizeReq -> WidgetInstance s e -> SizeReq
updateSizeReq sizeReq widgetInstance = newSizeReq where
  width = _styleWidth . _instanceStyle $ widgetInstance
  height = _styleHeight . _instanceStyle $ widgetInstance
  tempSizeReq = if isNothing width then sizeReq else sizeReq {
    _sizeRequested = Size (fromJust width) (_w . _sizeRequested $ sizeReq),
    _sizePolicyWidth = StrictSize
  }
  newSizeReq = if isNothing height then tempSizeReq else tempSizeReq {
    _sizeRequested = Size (_h . _sizeRequested $ sizeReq) (fromJust height),
    _sizePolicyHeight = StrictSize
  }

isSendMessageHandler :: WidgetRequest s -> Bool
isSendMessageHandler (SendMessage _ _) = True
isSendMessageHandler _ = False

isTaskHandler :: WidgetRequest s -> Bool
isTaskHandler (RunTask _ _) = True
isTaskHandler _ = False

isProducerHandler :: WidgetRequest s -> Bool
isProducerHandler (RunProducer _ _) = True
isProducerHandler _ = False

isIgnoreParentEvents :: WidgetRequest s -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False

isIgnoreChildrenEvents :: WidgetRequest s -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isSetFocus :: WidgetRequest s -> Bool
isSetFocus (SetFocus _) = True
isSetFocus _ = False

isGetClipboard :: WidgetRequest s -> Bool
isGetClipboard (GetClipboard _) = True
isGetClipboard _ = False

isSetClipboard :: WidgetRequest s -> Bool
isSetClipboard (SetClipboard _) = True
isSetClipboard _ = False

isSetOverlay :: WidgetRequest s -> Bool
isSetOverlay (SetOverlay _) = True
isSetOverlay _ = False

isResetOverlay :: WidgetRequest s -> Bool
isResetOverlay ResetOverlay = True
isResetOverlay _ = False

isUpdateUserState :: WidgetRequest s -> Bool
isUpdateUserState (UpdateUserState _) = True
isUpdateUserState _ = False

getUpdateUserStates :: (Traversable t) => t (WidgetRequest s) -> Seq (s -> s)
getUpdateUserStates reqs = foldl' foldHelper Seq.empty reqs where
  foldHelper acc (UpdateUserState fn) = acc |> fn
  foldHelper acc _ = acc

getTextBounds :: WidgetContext s e -> Maybe TextStyle -> Text -> Size
getTextBounds wctx style text = calcTextBounds handler style text where
  handler = _wpTextBounds (_wcPlatform wctx)

isShortCutControl :: WidgetContext s e -> KeyMod -> Bool
isShortCutControl wctx mod = isControl || isCommand where
  isControl = not (isMacOS wctx) && keyModLeftCtrl mod
  isCommand = isMacOS wctx && keyModLeftGUI mod

isClipboardCopy :: WidgetContext s e -> SystemEvent -> Bool
isClipboardCopy wctx event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wctx mod && isKeyC code

isClipboardPaste :: WidgetContext s e -> SystemEvent -> Bool
isClipboardPaste wctx event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wctx mod && isKeyV code

isMacOS :: WidgetContext s e -> Bool
isMacOS wctx = _wpOS (_wcPlatform wctx) == "Mac OS X"
