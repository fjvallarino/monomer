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

scroll :: (MonadState s m) => Tree (WidgetInstance s e m) -> Tree (WidgetInstance s e m)
scroll managedWidget = parentWidget (makeScroll (ScrollState 0)) [managedWidget]

makeScroll :: (MonadState s m) => ScrollState -> Widget s e m
makeScroll state = Widget widgetType modifiesContext focusable handleEvent preferredSize resizeChildren render
  where
    widgetType = "scroll"
    modifiesContext = True
    focusable = False
    handleEvent view evt = Nothing
    preferredSize _ _ children = return (head children)
    resizeChildren (Rect l t _ _) _ children = [Rect l t w h] where
      Size w h = (head children)
    render renderer viewport (style@Style{..}) status ts =
      do
        drawText renderer viewport _textStyle (T.pack (show state))
