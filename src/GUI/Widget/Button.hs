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

button :: (MonadState s m) => e -> Tree (WidgetNode s e m)
button onClick = singleWidget (makeButton 0 onClick)

makeButton :: (MonadState s m) => Int -> e -> Widget s e m
makeButton state onClick = Widget widgetType widgetFocusable handleEvent preferredSize resizeChildren render
  where
    widgetType = "button"
    widgetFocusable = False
    handleEvent view evt = case evt of
      Click (Point x y) _ status -> mkWidgetEventResult False events (makeButton newState onClick) where
        isPressed = status == PressedBtn && inRect view (Point x y)
        newState = if isPressed then state + 1 else state
        events = if isPressed then [onClick] else []
      _ -> Nothing
    preferredSize renderer (style@Style{..}) _ = calcTextBounds renderer _textStyle (T.pack (show state))
    resizeChildren _ _ _ = []
    render renderer viewport (style@Style{..}) enabled focused ts =
      do
        drawBgRect renderer viewport style
        drawText renderer viewport _textStyle (T.pack (show state))



{--



labelField :: (MonadState s m) => Rect -> String -> Widget s m
labelField (Rect l t w h) label = widget
  where
    widget = Widget widgetData (pure True) handleEvent render resize undefined
    widgetData = WidgetData l t w h
    handleEvent _ _ = pure widget
    render r rt@(Rect x y w h) = do
      fillColor r (RGB 255 0 0)
      text r rt "sans" 32 (Align Center Middle) (T.pack label)
    resize w@Widget{..} = labelField (widgetDataToRect _widgetData) label



--}
