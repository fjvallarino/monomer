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
newtype SandboxState = SandboxState {
  _clickCount :: Int
} deriving (Eq, Show, Typeable)

sandbox :: e -> WidgetInstance s e
sandbox onClick = makeInstance $ makeSandbox onClick (SandboxState 0)

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = defaultWidgetInstance "sandbox" widget

makeSandbox :: e -> SandboxState -> Widget s e
makeSandbox onClick state = createWidget {
    _widgetGetState = makeState state,
    _widgetMerge = widgetMerge merge,
    _widgetHandleEvent = handleEvent,
    _widgetHandleMessage = handleMessage,
    _widgetPreferredSize = preferredSize,
    _widgetRender = render
  }
  where
    label = "Sandbox: " ++ show (_clickCount state)

    merge wctx ctx oldState widgetInstance = resultWidget newInstance where
      newState = fromMaybe state (useState oldState)
      newInstance = widgetInstance {
        _instanceWidget = makeSandbox onClick newState
      }

    handleEvent wctx ctx evt widgetInstance = case evt of
      Click (Point x y) _ -> Just $ resultReqsEvents requests events newInstance where
        events = [onClick]
        requests = [RunTask (currentPath ctx) runTask]
        newState = SandboxState (_clickCount state + 1)
        newInstance = makeInstance $ makeSandbox onClick newState
      Enter p -> Nothing --trace ("Enter: " ++ show p) Nothing
      Move p -> Nothing --trace ("Move: " ++ show p) Nothing
      Leave _ p -> Nothing --trace ("Leave: " ++ show p) Nothing
      _ -> Nothing

    runTask = return SandboxData2

    handleMessage wctx ctx bd widgetInstance = case cast bd of
      Just val -> if val == SandboxData2 then trace "Sandbox handleMessage called" Nothing else Nothing
      Nothing -> Nothing

    preferredSize wctx widgetInstance = singleNode sizeReq where
      Style{..} = _instanceStyle widgetInstance
      size = getTextBounds wctx _styleText (T.pack label)
      sizeReq = SizeReq size FlexibleSize FlexibleSize

    render renderer wctx ctx WidgetInstance{..} =
      do
        drawStyledBackground renderer _instanceRenderArea _instanceStyle
        drawStyledText_ renderer _instanceRenderArea _instanceStyle (T.pack label)
