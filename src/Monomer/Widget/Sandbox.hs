{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Sandbox (sandbox) where

import Control.Monad
import Control.Monad.State

import Data.Typeable

import Debug.Trace

import Monomer.Common.Core
import Monomer.Common.Event
import Monomer.Common.Drawing
import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Data.Tree

import GHC.Generics

import qualified Data.Text as T

data SandboxData = SandboxData | SandboxData2 deriving (Eq, Show, Typeable)
data SandboxState = SandboxState {
  _clickCount :: Int
} deriving (Eq, Show, Typeable, Generic)

sandbox :: (MonadState s m, MonadIO m) => e -> WidgetNode s e m
sandbox onClick = singleWidget (makeSandbox onClick (SandboxState 0))

makeSandbox :: (MonadState s m, MonadIO m) => e -> SandboxState -> Widget s e m
makeSandbox onClick state = baseWidget {
    _widgetType = "button",
    _widgetRestoreState = defaultRestoreState (makeSandbox onClick),
    _widgetSaveState = makeState state,
    _widgetHandleEvent = handleEvent,
    _widgetHandleCustom = handleCustom,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    handleEvent view evt = case evt of
      Click (Point x y) _ status -> resultReqsEventsWidget requests events (makeSandbox onClick newState) where
        isPressed = status == PressedBtn && inRect view (Point x y)
        newState = if isPressed then SandboxState (_clickCount state + 1) else state
        events = if isPressed then [onClick] else []
        requests = if isPressed then [RunCustom runCustom] else []
      Enter p -> Nothing --trace ("Enter: " ++ show p) Nothing
      Move p -> Nothing --trace ("Move: " ++ show p) Nothing
      Leave _ p -> Nothing --trace ("Leave: " ++ show p) Nothing
      _ -> Nothing
    runCustom = do
      return SandboxData2
    handleCustom bd = case cast bd of
      Just val -> if val == SandboxData2 then Nothing else Nothing
      Nothing -> Nothing
    preferredSize renderer (style@Style{..}) _ = do
      size <- calcTextBounds renderer _textStyle (T.pack (show (_clickCount state)))
      return $ sizeReq size FlexibleSize FlexibleSize
    resizeChildren _ _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts =
      do
        drawBgRect renderer _widgetInstanceRenderArea _widgetInstanceStyle
        drawText_ renderer _widgetInstanceRenderArea (_textStyle _widgetInstanceStyle) (T.pack (show (_clickCount state)))
