{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Button where

import Control.Monad
import Control.Monad.State

import GUI.Core
import GUI.Data.Tree
import GUI.Widget.Core
import GUI.Widget.Drawing
import GUI.Widget.Style

import qualified Data.Text as T

button :: (Monad m) => e -> Tree (WidgetNode e m)
button onClick = singleWidget (makeButton 0 onClick)

makeButton :: (Monad m) => Int -> e -> Widget e m
makeButton state onClick = Widget widgetType handleEvent preferredSize resizeChildren render
  where
    widgetType = "button"
    handleEvent view evt = case evt of
      Click (Point x y) _ status -> EventsState events (makeButton newState onClick) where
        isPressed = status == PressedBtn && inRect view (Point x y)
        newState = if isPressed then state + 1 else state
        events = if isPressed then [onClick] else []
      _ -> NoEvents
    preferredSize renderer (style@Style{..}) _ = calcTextBounds renderer _textStyle (T.pack (show state))
    resizeChildren _ _ _ = []
    render renderer ts viewport (style@Style{..}) =
      do
        drawBgRect renderer viewport style
        drawText renderer viewport _textStyle (T.pack (show state))
