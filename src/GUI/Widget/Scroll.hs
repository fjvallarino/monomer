{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Scroll (scroll) where

import Control.Monad
import Control.Monad.State

import qualified Data.Text as T

import GUI.Common.Core
import GUI.Common.Drawing
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

data ScrollState = ScrollState {
  _scPosition :: Int
} deriving (Eq, Show)

scroll :: (MonadState s m) => WidgetNode s e m -> WidgetNode s e m
scroll managedWidget = parentWidget (makeScroll (ScrollState 0)) [managedWidget]

makeScroll :: (MonadState s m) => ScrollState -> Widget s e m
makeScroll state = Widget widgetType focusable handleEvent preferredSize resizeChildren render
  where
    widgetType = "scroll"
    focusable = False
    handleEvent view evt = Nothing
    preferredSize _ _ children = return (head children)
    resizeChildren (Rect l t _ _) _ children = Just $ WidgetResizeResult viewport renderArea newWidget where
      Size w h = (head children)
      newWidget = Just $ makeScroll state
      viewport = [Rect l t w h]
      renderArea = [Rect l t w h]
    render renderer WidgetInstance{..} children ts =
      do
        scissor renderer _widgetInstanceViewport
        handleRenderChildren renderer children ts
        resetScissor renderer

        drawText renderer _widgetInstanceRenderArea (_textStyle _widgetInstanceStyle) (T.pack (show state))
