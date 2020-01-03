{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Label (label) where

import Control.Monad
import Control.Monad.State

import GUI.Common.Core
import GUI.Common.Drawing
import GUI.Common.Style
import GUI.Common.Types
import GUI.Data.Tree

import qualified Data.Text as T

{--

***********************************

Implement auto scalable label! Selects correct size to fit the given text

***********************************

--}
label :: (MonadState s m) => T.Text -> WidgetNode s e m
label caption = singleWidget (makeLabel caption)

makeLabel :: (MonadState s m) => T.Text -> Widget s e m
makeLabel caption = Widget {
    _widgetType = "label",
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
    preferredSize renderer (style@Style{..}) _ = do
      size <- calcTextBounds renderer _textStyle (if caption == "" then " " else caption)
      return $ sizeReq size FlexibleSize FlexibleSize
    resizeChildren _ _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts =
      do
        drawBgRect renderer _widgetInstanceRenderArea _widgetInstanceStyle
        drawText renderer _widgetInstanceRenderArea (_textStyle _widgetInstanceStyle) caption
