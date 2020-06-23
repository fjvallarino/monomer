{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Sandbox (sandbox) where

import Control.Monad
import Data.Maybe
import Data.Typeable
import Debug.Trace

import qualified Data.Text as T

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
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

sandbox :: e -> WidgetInstance s e
sandbox onClick = makeInstance $ makeSandbox onClick (SandboxState 0)

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = defaultWidgetInstance "sandbox" widget

makeSandbox :: e -> SandboxState -> Widget s e
makeSandbox onClick state = createWidget {
    _widgetGetState = getState,
    _widgetMerge = widgetMerge merge,
    _widgetHandleEvent = handleEvent,
    _widgetHandleMessage = handleMessage,
    _widgetPreferredSize = preferredSize,
    _widgetRender = render
  }
  where
    label = "Sandbox: " ++ show (_clickCount state)

    getState = makeState state
    merge wctx oldState = makeSandbox onClick newState where
      newState = fromMaybe state (useState oldState)

    handleEvent wctx ctx evt widgetInstance = case evt of
      Click (Point x y) _ status -> Just $ resultReqsEvents requests events newInstance where
        isPressed = status == PressedBtn -- && inRect view (Point x y)
        events = if isPressed then [onClick] else []
        requests = if isPressed then [RunTask (currentPath ctx) runTask] else []
        newState = if isPressed then SandboxState (_clickCount state + 1) else state
        newInstance = makeInstance $ makeSandbox onClick newState
      Enter p -> Nothing --trace ("Enter: " ++ show p) Nothing
      Move p -> Nothing --trace ("Move: " ++ show p) Nothing
      Leave _ p -> Nothing --trace ("Leave: " ++ show p) Nothing
      _ -> Nothing

    runTask = do
      return SandboxData2

    handleMessage ctx bd app widgetInstance = case cast bd of
      Just val -> if val == SandboxData2 then trace "Sandbox handleMessage called" Nothing else Nothing
      Nothing -> Nothing

    preferredSize renderer app widgetInstance = singleNode sizeReq where
      Style{..} = _instanceStyle widgetInstance
      size = calcTextBounds renderer _textStyle (T.pack label)
      sizeReq = SizeReq size FlexibleSize FlexibleSize

    render renderer wctx ctx WidgetInstance{..} =
      do
        drawBgRect renderer _instanceRenderArea _instanceStyle
        drawText_ renderer _instanceRenderArea (_textStyle _instanceStyle) (T.pack label)
