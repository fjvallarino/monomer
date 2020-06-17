{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Util where

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
import Monomer.Event.Types
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

key :: WidgetInstance s e -> Text -> WidgetInstance s e
key wn key = wn { _instanceKey = Just (WidgetKey key) }

style :: WidgetInstance s e -> Style -> WidgetInstance s e
style widgetInstance newStyle = widgetInstance { _instanceStyle = newStyle }

visible :: WidgetInstance s e -> Bool -> WidgetInstance s e
visible widgetInstance visibility = widgetInstance { _instanceVisible = visibility }

children :: WidgetInstance s e -> [WidgetInstance s e] -> WidgetInstance s e
children widgetInstance newChildren = widgetInstance { _instanceChildren = Seq.fromList newChildren }

isFocusable :: WidgetInstance s e -> Bool
isFocusable (WidgetInstance { _instanceWidget = Widget{..}, ..}) = _instanceVisible && _instanceEnabled && _instanceFocusable

rWidget :: WidgetInstance s e -> WidgetResult s e
rWidget widgetInstance = WidgetResult Seq.empty Seq.empty widgetInstance

resultWidget :: WidgetInstance s e -> Maybe (WidgetResult s e)
resultWidget widgetInstance = Just $ WidgetResult Seq.empty Seq.empty widgetInstance

resultEvents :: [e] -> WidgetInstance s e -> Maybe (WidgetResult s e)
resultEvents userEvents widgetInstance = Just $ WidgetResult Seq.empty (Seq.fromList userEvents) widgetInstance

resultReqs :: [WidgetRequest s] -> WidgetInstance s e -> Maybe (WidgetResult s e)
resultReqs requests widgetInstance = Just $ WidgetResult (Seq.fromList requests) Seq.empty widgetInstance

resultReqsEvents :: [WidgetRequest s] -> [e] -> WidgetInstance s e -> Maybe (WidgetResult s e)
resultReqsEvents requests userEvents widgetInstance = Just $ WidgetResult (Seq.fromList requests) (Seq.fromList userEvents) widgetInstance

makeState :: Typeable i => i -> s -> Maybe WidgetState
makeState state app = Just (WidgetState state)

useState ::  Typeable i => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

instanceMatches :: WidgetInstance s e -> WidgetInstance s e -> Bool
instanceMatches newInstance oldInstance = typeMatches && keyMatches where
  typeMatches = _instanceType oldInstance == _instanceType newInstance
  keyMatches = _instanceKey oldInstance == _instanceKey newInstance

updateSizeReq :: SizeReq -> WidgetInstance s e -> SizeReq
updateSizeReq sizeReq widgetInstance = newSizeReq where
  width = _fixedWidth . _instanceStyle $ widgetInstance
  height = _fixedHeight . _instanceStyle $ widgetInstance
  tempSizeReq = if isNothing width then sizeReq else sizeReq {
    _sizeRequested = Size (fromJust width) (_w . _sizeRequested $ sizeReq),
    _sizePolicyWidth = StrictSize
  }
  newSizeReq = if isNothing height then tempSizeReq else tempSizeReq {
    _sizeRequested = Size (_h . _sizeRequested $ sizeReq) (fromJust height),
    _sizePolicyHeight = StrictSize
  }

concatSeq :: Seq (Seq a) -> Seq a
concatSeq seqs = foldl' (><) Seq.empty seqs

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

isUpdateUserState :: WidgetRequest s -> Bool
isUpdateUserState (UpdateUserState _) = True
isUpdateUserState _ = False

getUpdateUserStates :: (Traversable t) => t (WidgetRequest s) -> Seq (s -> s)
getUpdateUserStates reqs = foldl' foldHelper Seq.empty reqs where
  foldHelper acc (UpdateUserState fn) = acc |> fn
  foldHelper acc _ = acc

convertRequest :: WidgetRequest s -> Maybe (WidgetRequest s2)
convertRequest IgnoreParentEvents = Just IgnoreParentEvents
convertRequest IgnoreChildrenEvents = Just IgnoreChildrenEvents
convertRequest (SetFocus path) = Just (SetFocus path)
convertRequest (GetClipboard path) = Just (GetClipboard path)
convertRequest (SetClipboard clipboard) = Just (SetClipboard clipboard)
convertRequest (SendMessage path message) = Just (SendMessage path message)
convertRequest (RunTask path action) = Just (RunTask path action)
convertRequest (RunProducer path action) = Just (RunProducer path action)
convertRequest (UpdateUserState fn) = Nothing

convertRequests :: Seq (WidgetRequest s) -> Seq (WidgetRequest sp)
convertRequests reqs = fmap fromJust $ Seq.filter isJust $ fmap convertRequest reqs
