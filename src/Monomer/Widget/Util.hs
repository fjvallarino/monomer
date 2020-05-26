{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Util where

import Data.Default
import Data.Typeable (cast, Typeable)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Main.Types
import Monomer.Widget.Types

defaultWidgetInstance :: (Monad m) => WidgetType -> Widget s e m -> WidgetInstance s e m
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

key :: (Monad m) => WidgetKey -> WidgetInstance s e m -> WidgetInstance s e m
key key wn = wn { _instanceKey = Just key }

style :: (Monad m) => WidgetInstance s e m -> Style -> WidgetInstance s e m
style widgetInstance newStyle = widgetInstance { _instanceStyle = newStyle }

visible :: (Monad m) => WidgetInstance s e m -> Bool -> WidgetInstance s e m
visible widgetInstance visibility = widgetInstance { _instanceVisible = visibility }

children :: (Monad m) => WidgetInstance s e m -> [WidgetInstance s e m] -> WidgetInstance s e m
children widgetInstance newChildren = widgetInstance { _instanceChildren = Seq.fromList newChildren }

isFocusable :: (Monad m) => WidgetInstance s e m -> Bool
isFocusable (WidgetInstance { _instanceWidget = Widget{..}, ..}) = _instanceVisible && _instanceEnabled && _instanceFocusable

resultEvents :: [e] -> WidgetInstance s e m -> Maybe (EventResult s e m)
resultEvents userEvents widgetInstance = Just $ EventResult Seq.empty (Seq.fromList userEvents) widgetInstance

resultReqs :: [EventRequest s] -> WidgetInstance s e m -> Maybe (EventResult s e m)
resultReqs requests widgetInstance = Just $ EventResult (Seq.fromList requests) Seq.empty widgetInstance

resultReqsEvents :: [EventRequest s] -> [e] -> WidgetInstance s e m -> Maybe (EventResult s e m)
resultReqsEvents requests userEvents widgetInstance = Just $ EventResult (Seq.fromList requests) (Seq.fromList userEvents) widgetInstance

makeState :: (Typeable i, Generic i) => i -> s -> Maybe WidgetState
makeState state app = Just (WidgetState state)

useState ::  (Typeable i, Generic i) => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

defaultRestoreState :: (Monad m, Typeable i, Generic i) => (i -> Widget s e m) -> s -> Maybe WidgetState -> Maybe (Widget s e m)
defaultRestoreState makeState _ oldState = fmap makeState $ useState oldState
