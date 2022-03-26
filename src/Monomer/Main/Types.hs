{-|
Module      : Monomer.Main.Types
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Basic types for Main module.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}

module Monomer.Main.Types where

import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad.Catch
import Control.Monad.State
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Sequence (Seq)
import GHC.Generics

import qualified Data.Map as M
import qualified SDL
import qualified SDL.Raw.Types as SDLR

import Monomer.Common
import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Core.WidgetTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

-- | Main Monomer monad.
type MonomerM s e m = (Eq s, MonadState (MonomerCtx s e) m, MonadCatch m, MonadIO m)

-- | Messages received by the rendering thread.
data RenderMsg s e
  = MsgRender (WidgetEnv s e) (WidgetNode s e)
  | MsgResize Size
  | MsgRemoveImage Text
  | forall i . MsgRunInRender (TChan i) (IO i)

{-|
Requirements for periodic rendering by a widget. Start time is stored to
calculate next frame based on the step ms. A maximum number of repetitions may
be provided.
-}
data RenderSchedule = RenderSchedule {
  _rsWidgetId :: WidgetId,
  _rsStart :: Timestamp,
  _rsMs :: Timestamp,
  _rsRepeat :: Maybe Int
} deriving (Eq, Show, Generic)

-- | Drag action started by "WidgetId", with an associated message.
data DragAction = DragAction {
  _dgaWidgetId :: WidgetId,
  _dgaDragData :: WidgetDragMsg
} deriving (Eq, Show)

{-|
Asychronous widget task. Results must be provided as user defined, Typeable,
types. Error handling should be done inside the task and reporting handled as
part of the user type.
-}
data WidgetTask
  -- | Task generating a single result (for example, an HTTP request).
  = forall i . Typeable i => WidgetTask WidgetId (Async i)
  -- | Task generating a multiple result (for example, a Socket).
  | forall i . Typeable i => WidgetProducer WidgetId (TChan i) (Async ())

-- | Current state of the Monomer runtime.
data MonomerCtx s e = MonomerCtx {
  -- | Main application model.
  _mcMainModel :: s,
  -- | Active window.
  _mcWindow :: ~SDL.Window,
  -- | Main window size.
  _mcWindowSize :: Size,
  -- | Device pixel rate.
  _mcDpr :: Double,
  -- | Event pixel rate.
  _mcEpr :: Double,
  -- | Event pixel rate.
  _mcRenderChannel :: TChan (RenderMsg s e),
  -- | Input status (mouse and keyboard).
  _mcInputStatus :: InputStatus,
  -- | Cursor icons (a stack is used because of parent -> child relationship).
  _mcCursorStack :: [(WidgetId, CursorIcon)],
  -- | WidgetId of focused widget.
  _mcFocusedWidgetId :: WidgetId,
  -- | WidgetId of hovered widget, if any.
  _mcHoveredWidgetId :: Maybe WidgetId,
  -- | WidgetId of overlay widget, if any.
  _mcOverlayWidgetId :: Maybe WidgetId,
  -- | Active drag action, if any.
  _mcDragAction :: Maybe DragAction,
  -- | Start point and target of latest main button press, if any.
  _mcMainBtnPress :: Maybe (Path, Point),
  -- | Active widget tasks.
  _mcWidgetTasks :: Seq WidgetTask,
  {-|
  Associations of WidgetId to updated paths. Only WidgetIds whose initial path
  changed are included.
  -}
  _mcWidgetPaths :: Map WidgetId Path,
  -- | Association of Monomer CursorIcon to SDL Cursors.
  _mcCursorIcons :: Map CursorIcon SDLR.Cursor,
  {-|
  Hacky flag to avoid resizing when transitioning hover. Needed because sizes
  may change and new target of hover should not change.
  -}
  _mcLeaveEnterPair :: Bool,
  -- | Widgets with pending resize requests.
  _mcResizeRequests :: Seq WidgetId,
  -- | Flag indicating render was requested in this cycle.
  _mcRenderRequested :: Bool,
  -- | Active periodic rendering requests.
  _mcRenderSchedule :: Map WidgetId RenderSchedule,
  -- | Whether there was a request to exit the application.
  _mcExitApplication :: Bool
}

-- | Requests for main window size.
data MainWindowState
  -- | Normal window with a given size.
  = MainWindowNormal (Int, Int)
  -- | Maximized window.
  | MainWindowMaximized
  -- | Full screen window.
  | MainWindowFullScreen
  deriving (Eq, Show)

-- | Main application config.
data AppConfig e = AppConfig {
  -- | Initial size of the main window.
  _apcWindowState :: Maybe MainWindowState,
  -- | Title of the main window.
  _apcWindowTitle :: Maybe Text,
  -- | Whether the main window is resizable.
  _apcWindowResizable :: Maybe Bool,
  -- | Whether the main window has a border.
  _apcWindowBorder :: Maybe Bool,
  -- | Path to an icon file in BMP format.
  _apcWindowIcon :: Maybe Text,
  -- | Whether a separate render thread should be used. Defaults to True.
  _apcUseRenderThread :: Maybe Bool,
  {-|
  Max number of FPS the application will run at. It does not necessarily mean
  rendering will happen every frame, but events and schedules will be checked at
  this rate.
  -}
  _apcMaxFps :: Maybe Int,
  {-|
  Scale factor to apply. This factor only affects the content, not the size of
  the window. It is applied in addition to the OS zoom in plaforms where it is
  reliably detected (i.e., system scaling may not be detected reliably on Linux)
  -}
  _apcScaleFactor :: Maybe Double,
  {-|
  Available fonts to the application. An empty list will make it impossible to
  render text.
  -}
  _apcFonts :: [FontDef],
  -- | Initial theme.
  _apcTheme :: Maybe Theme,
  -- | Initial event, useful for loading resources.
  _apcInitEvent :: [e],
  -- | Dispose event, useful for closing resources.
  _apcDisposeEvent :: [e],
  -- | Exit event, useful for cancelling an application close event.
  _apcExitEvent :: [e],
  -- | Resize event handler.
  _apcResizeEvent :: [Rect -> e],
  -- | Defines which mouse button is considered main.
  _apcMainButton :: Maybe Button,
  -- | Defines which mouse button is considered secondary or context button.
  _apcContextButton :: Maybe Button,
  -- | Whether wheel/trackpad horizontal movement should be inverted.
  _apcInvertWheelX :: Maybe Bool,
  -- | Whether wheel/trackpad vertical movement should be inverted.
  _apcInvertWheelY :: Maybe Bool,
  -- | Whether compositing should be disabled. Defaults to False.
  _apcDisableCompositing :: Maybe Bool
}

instance Default (AppConfig e) where
  def = AppConfig {
    _apcWindowState = Nothing,
    _apcWindowTitle = Nothing,
    _apcWindowResizable = Nothing,
    _apcWindowBorder = Nothing,
    _apcWindowIcon = Nothing,
    _apcUseRenderThread = Nothing,
    _apcMaxFps = Nothing,
    _apcScaleFactor = Nothing,
    _apcFonts = [],
    _apcTheme = Nothing,
    _apcInitEvent = [],
    _apcDisposeEvent = [],
    _apcExitEvent = [],
    _apcResizeEvent = [],
    _apcMainButton = Nothing,
    _apcContextButton = Nothing,
    _apcInvertWheelX = Nothing,
    _apcInvertWheelY = Nothing,
    _apcDisableCompositing = Nothing
  }

instance Semigroup (AppConfig e) where
  (<>) a1 a2 = AppConfig {
    _apcWindowState = _apcWindowState a2 <|> _apcWindowState a1,
    _apcWindowTitle = _apcWindowTitle a2 <|> _apcWindowTitle a1,
    _apcWindowResizable = _apcWindowResizable a2 <|> _apcWindowResizable a1,
    _apcWindowBorder = _apcWindowBorder a2 <|> _apcWindowBorder a1,
    _apcWindowIcon = _apcWindowIcon a2 <|> _apcWindowIcon a1,
    _apcUseRenderThread = _apcUseRenderThread a2 <|> _apcUseRenderThread a1,
    _apcMaxFps = _apcMaxFps a2 <|> _apcMaxFps a1,
    _apcScaleFactor = _apcScaleFactor a2 <|> _apcScaleFactor a1,
    _apcFonts = _apcFonts a1 ++ _apcFonts a2,
    _apcTheme = _apcTheme a2 <|> _apcTheme a1,
    _apcInitEvent = _apcInitEvent a1 ++ _apcInitEvent a2,
    _apcDisposeEvent = _apcDisposeEvent a1 ++ _apcDisposeEvent a2,
    _apcExitEvent = _apcExitEvent a1 ++ _apcExitEvent a2,
    _apcResizeEvent = _apcResizeEvent a1 ++ _apcResizeEvent a2,
    _apcMainButton = _apcMainButton a2 <|> _apcMainButton a1,
    _apcContextButton = _apcContextButton a2 <|> _apcContextButton a1,
    _apcInvertWheelX = _apcInvertWheelX a2 <|> _apcInvertWheelX a1,
    _apcInvertWheelY = _apcInvertWheelY a2 <|> _apcInvertWheelY a1,
    _apcDisableCompositing = _apcDisableCompositing a2 <|> _apcDisableCompositing a1
  }

instance Monoid (AppConfig e) where
  mempty = def

-- | Initial size of the main window.
appWindowState :: MainWindowState -> AppConfig e
appWindowState title = def {
  _apcWindowState = Just title
}

-- | Title of the main window.
appWindowTitle :: Text -> AppConfig e
appWindowTitle title = def {
  _apcWindowTitle = Just title
}

-- | Whether the main window is resizable.
appWindowResizable :: Bool -> AppConfig e
appWindowResizable resizable = def {
  _apcWindowResizable = Just resizable
}

-- | Whether the main window has a border.
appWindowBorder :: Bool -> AppConfig e
appWindowBorder border = def {
  _apcWindowBorder = Just border
}

-- | Path to an icon file in BMP format.
appWindowIcon :: Text -> AppConfig e
appWindowIcon path = def {
  _apcWindowIcon = Just path
}

{-|
Performs rendering on the main thread. On macOS and Windows this also disables
continuous rendering on window resize, but in some Linux configurations it still
works.

This option is useful when OpenGL driver issues prevent normal startup showing
the "Unable to make GL context current" error.

It can also be used for single threaded applications (without -threaded).
-}
appRenderOnMainThread :: AppConfig e
appRenderOnMainThread = def {
  _apcUseRenderThread = Just False
}

{-|
Max number of FPS the application will run on. It does not necessarily mean
rendering will happen every frame, but events and schedules will be checked at
this rate and may cause it.
-}
appMaxFps :: Int -> AppConfig e
appMaxFps fps = def {
  _apcMaxFps = Just fps
}

{-|
Scale factor to apply. This factor only affects the content, not the size of the
window. It is applied in addition to the OS zoom in plaforms where it is
reliably detected (i.e., system scaling may not be detected reliably on Linux).
-}
appScaleFactor :: Double -> AppConfig e
appScaleFactor factor = def {
  _apcScaleFactor = Just factor
}

{-|
Available fonts to the application. An empty list will make it impossible to
render text.
-}
appFontDef :: Text -> Text -> AppConfig e
appFontDef name path = def {
  _apcFonts = [ FontDef name path ]
}

-- | Initial theme.
appTheme :: Theme -> AppConfig e
appTheme t = def {
  _apcTheme = Just t
}

-- | Initial event, useful for loading resources.
appInitEvent :: e -> AppConfig e
appInitEvent evt = def {
  _apcInitEvent = [evt]
}

-- | Dispose event, useful for closing resources.
appDisposeEvent :: e -> AppConfig e
appDisposeEvent evt = def {
  _apcDisposeEvent = [evt]
}

-- | Exit event, useful for cancelling an application close event.
appExitEvent :: e -> AppConfig e
appExitEvent evt = def {
  _apcExitEvent = [evt]
}

-- | Resize event handler.
appResizeEvent :: (Rect -> e) -> AppConfig e
appResizeEvent evt = def {
  _apcResizeEvent = [evt]
}

-- | Defines which mouse button is considered main.
appMainButton :: Button -> AppConfig e
appMainButton btn = def {
  _apcMainButton = Just btn
}

-- | Defines which mouse button is considered secondary or context button.
appContextButton :: Button -> AppConfig e
appContextButton btn = def {
  _apcContextButton = Just btn
}

{-|
Whether the horizontal wheel/trackpad movement should be inverted. In general
platform detection should do the right thing.
-}
appInvertWheelX :: Bool -> AppConfig e
appInvertWheelX invert = def {
  _apcInvertWheelX = Just invert
}

{-|
Whether the vertical wheel/trackpad movement should be inverted. In general
platform detection should do the right thing.
-}
appInvertWheelY :: Bool -> AppConfig e
appInvertWheelY invert = def {
  _apcInvertWheelY = Just invert
}

{-|
Whether compositing should be disabled. Linux only, ignored in other platforms.
Defaults to False.

Desktop applications should leave compositing as is, since disabling it may
cause visual glitches in other programs. When creating games or fullscreen
applications, disabling compositing may improve performance.
-}
appDisableCompositing :: Bool -> AppConfig e
appDisableCompositing invert = def {
  _apcDisableCompositing = Just invert
}
