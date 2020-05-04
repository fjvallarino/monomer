{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Button (button) where

import Control.Monad
import Control.Monad.State

import Data.Typeable

import Monomer.Common.Core
import Monomer.Common.Event
import Monomer.Common.Drawing
import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Data.Tree

import qualified Data.Text as T

button :: (MonadState s m, MonadIO m) => T.Text -> e -> WidgetNode s e m
button label onClick = singleWidget (makeButton label onClick)

makeButton :: (MonadState s m, MonadIO m) => T.Text -> e -> Widget s e m
makeButton label onClick = baseWidget {
    _widgetType = "button",
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    handleEvent view evt = case evt of
      Click (Point x y) _ status -> resultEvents events where
        isPressed = status == PressedBtn && inRect view (Point x y)
        events = if isPressed then [onClick] else []
      _ -> Nothing
    preferredSize renderer (style@Style{..}) _ = do
      size <- calcTextBounds renderer _textStyle label
      return $ sizeReq size FlexibleSize FlexibleSize
    resizeChildren _ _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts =
      do
        drawBgRect renderer _widgetInstanceRenderArea _widgetInstanceStyle
        drawText_ renderer _widgetInstanceRenderArea (_textStyle _widgetInstanceStyle) label
