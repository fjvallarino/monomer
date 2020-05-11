{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Util where

import Data.Typeable (cast, Typeable)

import GHC.Generics

import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Common.Tree
import Monomer.Main.Types
import Monomer.Widget.Internal
import Monomer.Widget.Types
import Monomer.Widget.Widgets.Base

key :: (Monad m) => WidgetKey -> WidgetInstance s e m -> WidgetInstance s e m
key key wn = wn { _widgetInstanceKey = Just key }

style :: (Monad m) => WidgetNode s e m -> Style -> WidgetNode s e m
style (Node value children) newStyle = Node (value { _widgetInstanceStyle = newStyle }) children

visible :: (Monad m) => WidgetNode s e m -> Bool -> WidgetNode s e m
visible (Node value children) visibility = Node (value { _widgetInstanceVisible = visibility }) children

children :: (Monad m) => WidgetNode s e m -> [WidgetNode s e m] -> WidgetNode s e m
children (Node value _) newChildren = fromList value newChildren

isFocusable :: (Monad m) => WidgetInstance s e m -> Bool
isFocusable (WidgetInstance { _widgetInstanceWidget = Widget{..}, ..}) = _widgetInstanceVisible && _widgetInstanceEnabled && _widgetFocusable

empty :: (Monad m) => WidgetNode s e m
empty = singleWidget baseWidget

singleWidget :: (Monad m) => Widget s e m -> WidgetNode s e m
singleWidget widget = singleton (defaultWidgetInstance widget)

parentWidget :: (Monad m) => Widget s e m -> [WidgetNode s e m] -> WidgetNode s e m
parentWidget widget = fromList (defaultWidgetInstance widget)

sizeReq :: Size -> SizePolicy -> SizePolicy -> SizeReq
sizeReq size policyWidth policyHeight = SizeReq size policyWidth policyHeight True

resultEvents :: [e] -> Maybe (WidgetEventResult s e m)
resultEvents userEvents = Just $ WidgetEventResult [] userEvents Nothing id

resultEventsWidget :: [e] -> (Widget s e m) -> Maybe (WidgetEventResult s e m)
resultEventsWidget userEvents newWidget = Just $ WidgetEventResult [] userEvents (Just newWidget) id

makeState :: (Typeable i, Generic i) => i -> s -> Maybe WidgetState
makeState state app = Just (WidgetState state)

useState ::  (Typeable i, Generic i) => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

defaultRestoreState :: (Monad m, Typeable i, Generic i) => (i -> Widget s e m) -> s -> Maybe WidgetState -> Maybe (Widget s e m)
defaultRestoreState makeState _ oldState = fmap makeState $ useState oldState
