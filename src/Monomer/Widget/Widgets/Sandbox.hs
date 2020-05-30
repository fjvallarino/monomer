{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Sandbox (sandbox) where

import Control.Monad
import Data.Maybe
import Data.Typeable
import Debug.Trace

import qualified Data.Text as T

import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Widget.BaseWidget
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

data SandboxData = SandboxData | SandboxData2 deriving (Eq, Show, Typeable)
data SandboxState = SandboxState {
  _clickCount :: Int
} deriving (Eq, Show, Typeable)

sandbox :: (Monad m) => e -> WidgetInstance s e m
sandbox onClick = makeInstance $ makeSandbox onClick (SandboxState 0)

makeInstance :: (Monad m) => Widget s e m -> WidgetInstance s e m
makeInstance widget = defaultWidgetInstance "sandbox" widget

makeSandbox :: (Monad m) => e -> SandboxState -> Widget s e m
makeSandbox onClick state = createWidget {
    _widgetGetState = getState,
    _widgetMerge = defaultMerge merge,
    _widgetHandleEvent = handleEvent,
    _widgetHandleCustom = handleCustom,
    _widgetPreferredSize = preferredSize,
    _widgetRender = render
  }
  where
    getState = makeState state
    merge app oldState = makeSandbox onClick newState where
      newState = fromMaybe state (useState oldState)

    handleEvent ctx evt app widgetInstance = case evt of
      Click (Point x y) _ status -> resultReqsEvents requests events newInstance where
        isPressed = status == PressedBtn -- && inRect view (Point x y)
        events = if isPressed then [onClick] else []
        requests = if isPressed then [RunCustom (currentPath ctx) runCustom] else []
        newState = if isPressed then SandboxState (_clickCount state + 1) else state
        newInstance = makeInstance $ makeSandbox onClick newState
      Enter p -> Nothing --trace ("Enter: " ++ show p) Nothing
      Move p -> Nothing --trace ("Move: " ++ show p) Nothing
      Leave _ p -> Nothing --trace ("Leave: " ++ show p) Nothing
      _ -> Nothing

    runCustom = do
      return SandboxData2

    handleCustom ctx bd app widgetInstance = case cast bd of
      Just val -> if val == SandboxData2 then Nothing else Nothing
      Nothing -> Nothing

    preferredSize renderer app widgetInstance = singleNode sizeReq where
      Style{..} = _instanceStyle widgetInstance
      size = calcTextBounds renderer _textStyle (T.pack (show (_clickCount state)))
      sizeReq = SizeReq size FlexibleSize FlexibleSize

    render renderer ts ctx app WidgetInstance{..} =
      do
        drawBgRect renderer _instanceRenderArea _instanceStyle
        drawText_ renderer _instanceRenderArea (_textStyle _instanceStyle) (T.pack (show (_clickCount state)))
