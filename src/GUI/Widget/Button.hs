{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Button (button, label) where

import Control.Monad
import Control.Monad.State

import GUI.Common.Core
import GUI.Common.Drawing
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

import qualified Data.Text as T

button :: (MonadState s m) => e -> Tree (WidgetNode s e m)
button onClick = singleWidget (makeButton 0 onClick)

makeButton :: (MonadState s m) => Int -> e -> Widget s e m
makeButton state onClick = Widget widgetType modifiesContext focusable handleEvent preferredSize resizeChildren render
  where
    widgetType = "button"
    modifiesContext = False
    focusable = False
    handleEvent view evt = case evt of
      Click (Point x y) _ status -> widgetEventResult False events (makeButton newState onClick) where
        isPressed = status == PressedBtn && inRect view (Point x y)
        newState = if isPressed then state + 1 else state
        events = if isPressed then [onClick] else []
      _ -> Nothing
    preferredSize renderer (style@Style{..}) _ = calcTextBounds renderer _textStyle (T.pack (show state))
    resizeChildren _ _ _ = []
    render renderer viewport (style@Style{..}) status ts =
      do
        drawBgRect renderer viewport style
        drawText renderer viewport _textStyle (T.pack (show state))

label :: (MonadState s m) => T.Text -> Tree (WidgetNode s e m)
label caption = singleWidget (makeLabel caption)

makeLabel :: (MonadState s m) => T.Text -> Widget s e m
makeLabel caption = Widget widgetType modifiesContext focusable handleEvent preferredSize resizeChildren render
  where
    widgetType = "label"
    modifiesContext = False
    focusable = False
    handleEvent view evt = Nothing
    preferredSize renderer (style@Style{..}) _ = calcTextBounds renderer _textStyle caption
    resizeChildren _ _ _ = []
    render renderer viewport (style@Style{..}) status ts =
      do
        drawBgRect renderer viewport style
        drawText renderer viewport _textStyle caption
