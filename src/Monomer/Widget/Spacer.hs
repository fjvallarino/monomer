{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Spacer (spacer) where

import Control.Monad
import Control.Monad.State

import Monomer.Common.Core
import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Data.Tree

spacer :: (MonadState s m) => WidgetNode s e m
spacer = singleWidget makeSpacer

defaultSpace :: Double
defaultSpace = 10

makeSpacer :: (MonadState s m) => Widget s e m
makeSpacer = baseWidget {
    _widgetType = "spacer",
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    handleEvent view evt = Nothing
    preferredSize renderer (style@Style{..}) _ = return $ sizeReq (Size defaultSpace defaultSpace) RemainderSize RemainderSize
    resizeChildren _ _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts = return ()
