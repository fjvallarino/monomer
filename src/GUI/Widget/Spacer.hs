{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Spacer (spacer) where

import Control.Monad
import Control.Monad.State

import GUI.Common.Core
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

spacer :: (MonadState s m) => WidgetNode s e m
spacer = singleWidget makeSpacer

defaultSpace :: Int
defaultSpace = 10

makeSpacer :: (MonadState s m) => Widget s e m
makeSpacer = Widget {
    _widgetType = "spacer",
    _widgetFocusable = False,
    _widgetRestoreState = defaultRestoreState,
    _widgetSaveState = defaultSaveState,
    _widgetHandleEvent = handleEvent,
    _widgetHandleCustom = defaultCustomHandler,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    handleEvent view evt = Nothing
    preferredSize renderer (style@Style{..}) _ = return $ SizeReq (Size 10 10) RemainderSize RemainderSize
    resizeChildren _ _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts = return ()
