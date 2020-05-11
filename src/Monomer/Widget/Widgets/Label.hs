{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Label (label) where

import Control.Monad

import qualified Data.Text as T

import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Graphics.Drawing
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.Base

label :: (Monad m) => T.Text -> WidgetNode s e m
label caption = singleWidget (makeLabel caption)

makeLabel :: (Monad m) => T.Text -> Widget s e m
makeLabel caption = baseWidget {
    _widgetType = "label",
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    handleEvent app view evt = Nothing
    preferredSize renderer app (style@Style{..}) _ = do
      size <- calcTextBounds renderer _textStyle (if caption == "" then " " else caption)
      return $ sizeReq size FlexibleSize FlexibleSize
    resizeChildren _ _ _ _ = Nothing
    render renderer app WidgetInstance{..} ts =
      do
        drawBgRect renderer _widgetInstanceRenderArea _widgetInstanceStyle
        drawText_ renderer _widgetInstanceRenderArea (_textStyle _widgetInstanceStyle) caption
