{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Label (label) where

import Control.Monad
import Control.Monad.State

import GUI.Common.Core
import GUI.Common.Drawing
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

import qualified Data.Text as T

label :: (MonadState s m) => T.Text -> WidgetNode s e m
label caption = singleWidget (makeLabel caption)

makeLabel :: (MonadState s m) => T.Text -> Widget s e m
makeLabel caption = Widget widgetType focusable handleEvent preferredSize resizeChildren render
  where
    widgetType = "label"
    focusable = False
    handleEvent view evt = Nothing
    preferredSize renderer (style@Style{..}) _ = calcTextBounds renderer _textStyle caption
    resizeChildren _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts =
      do
        drawBgRect renderer _widgetInstanceRenderArea _widgetInstanceStyle
        drawText renderer _widgetInstanceRenderArea (_textStyle _widgetInstanceStyle) caption
