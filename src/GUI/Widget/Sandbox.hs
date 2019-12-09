{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Sandbox (sandbox) where

import Control.Monad
import Control.Monad.State

import Data.Typeable
import Debug.Trace

import GUI.Common.Core
import GUI.Common.Drawing
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

import qualified Data.Text as T

data SandboxData = SandboxData | SandboxData2 deriving (Eq, Show, Typeable)

sandbox :: (MonadState s m, MonadIO m) => e -> WidgetNode s e m
sandbox onClick = singleWidget (makeSandbox 0 onClick)

makeSandbox :: (MonadState s m, MonadIO m) => Int -> e -> Widget s e m
makeSandbox state onClick = Widget {
    _widgetType = "button",
    _widgetFocusable = False,
    _widgetHandleEvent = handleEvent,
    _widgetHandleCustom = handleCustom,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    handleEvent view evt = case evt of
      Click (Point x y) _ status -> resultReqsEventsWidget requests events (makeSandbox newState onClick) where
        isPressed = status == PressedBtn && inRect view (Point x y)
        newState = if isPressed then state + 1 else state
        events = if isPressed then [onClick] else []
        requests = if isPressed then [RunCustom runCustom] else []
      _ -> Nothing
    runCustom = do
      return SandboxData2
    handleCustom bd = case cast bd of
      Just val -> if val == SandboxData2 then Nothing else Nothing
      Nothing -> Nothing
    preferredSize renderer (style@Style{..}) _ = calcTextBounds renderer _textStyle (T.pack (show state))
    resizeChildren _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts =
      do
        drawBgRect renderer _widgetInstanceRenderArea _widgetInstanceStyle
        drawText renderer _widgetInstanceRenderArea (_textStyle _widgetInstanceStyle) (T.pack (show state))
