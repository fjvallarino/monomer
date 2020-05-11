module Monomer.Widget.Widgets.Base where

import Control.Monad
import Data.Typeable (Typeable)

import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Widget.Types

baseWidget :: (Monad m) => Widget s e m
baseWidget = Widget {
  _widgetType = "base",
  _widgetFocusable = False,
  _widgetRestoreState = ignoreRestoreState,
  _widgetSaveState = ignoreSaveState,
  _widgetHandleEvent = ignoreHandleEvent,
  _widgetHandleCustom = defaultCustomHandler,
  _widgetPreferredSize = ignorePreferredSize,
  _widgetResizeChildren = ignoreResizeChildren,
  _widgetRender = ignoreRender,
  _widgetRenderPost = ignoreRenderPost
}

ignoreRestoreState :: s -> Maybe WidgetState -> Maybe (Widget s e m)
ignoreRestoreState _ _ = Nothing

ignoreSaveState :: s -> Maybe WidgetState
ignoreSaveState _ = Nothing

ignoreHandleEvent :: (Monad m) => s -> Rect -> SystemEvent -> Maybe (WidgetEventResult s e m)
ignoreHandleEvent _ _ _ = Nothing

defaultCustomHandler :: Typeable i => s -> i -> Maybe (WidgetEventResult s e m)
defaultCustomHandler _ _ = Nothing

ignorePreferredSize :: (Monad m) => Renderer m -> s -> Style -> [SizeReq] -> m SizeReq
ignorePreferredSize _ _ _ _ = return $ SizeReq (Size 0 0) FlexibleSize FlexibleSize False

ignoreResizeChildren :: (Monad m) => Rect -> Rect -> Style -> [SizeReq] -> Maybe (WidgetResizeResult s e m)
ignoreResizeChildren _ _ _ _ = Nothing

ignoreRender :: (Monad m) => Renderer m -> s -> WidgetInstance s e m -> Timestamp -> m ()
ignoreRender _ _ _ _ = return ()

ignoreRenderPost :: (Monad m) => Renderer m -> s -> WidgetInstance s e m -> Timestamp -> m ()
ignoreRenderPost _ _ _ _ = return ()
