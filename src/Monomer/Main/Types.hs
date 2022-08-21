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
import Data.ByteString (ByteString)

-- | Main Monomer monad.
type MonomerM s e m = (Eq s, MonadState (MonomerCtx s e) m, MonadCatch m, MonadIO m)

-- | Messages received by the rendering thread.
data RenderMsg s e
  = MsgInit (WidgetEnv s e) (WidgetNode s e)
  | MsgRender (WidgetEnv s e) (WidgetNode s e)
  | MsgResize Size
  | MsgRemoveImage Text
  | forall i . MsgRunInRender (TChan i) (IO i)

-- | Result from attempting to set up the secondary rendering thread.
data RenderSetupResult
  = RenderSetupSingle
  | RenderSetupMulti
  | RenderSetupMakeCurrentFailed String
  deriving (Eq, Show)

{-|
Requirements for periodic rendering by a widget. Start time is stored to
calculate next frame based on the step ms. A maximum number of repetitions may
be provided.
-}
data RenderSchedule = RenderSchedule {
  _rsWidgetId :: WidgetId,
  _rsStart :: Millisecond,
  _rsMs :: Millisecond,
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
  -- | Renderer instance or communication channel with the render thread.
  _mcRenderMethod :: Either Renderer (TChan (RenderMsg s e)),
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
  the window. It is applied in addition to the detected display scaling.
  -}
  _apcScaleFactor :: Maybe Double,
  {-|
  Whether display scaling detection should not be attempted. If set to True, the
  display scale will be set to 1. This works together with 'appScaleFactor'.
  -}
  _apcDisableAutoScale :: Maybe Bool,
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
  _apcDisableCompositing :: Maybe Bool,
  -- | Whether the screensaver should be disabled. Defaults to False.
  _apcDisableScreensaver :: Maybe Bool
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
    _apcDisableAutoScale = Nothing,
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
    _apcDisableCompositing = Nothing,
    _apcDisableScreensaver = Nothing
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
    _apcDisableAutoScale = _apcDisableAutoScale a2 <|> _apcDisableAutoScale a1,
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
    _apcDisableCompositing = _apcDisableCompositing a2 <|> _apcDisableCompositing a1,
    _apcDisableScreensaver = _apcDisableScreensaver a2 <|> _apcDisableScreensaver a1
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

This configuration option was originally available to handle:

  - OpenGL driver issues which prevented normal startup showing the "Unable to
    make GL context current" error.
  - Single threaded applications (without -threaded) which cannot use forkOS.

This flag is no longer necessary for those cases, since the library will:

  - Attempt to fall back to rendering on the main thread if setting up a
    secondary rendering thread fails.
  - Will not attempt to set up a secondary rendering thread if the runtime does
    not support bound threads (i.e. compiled without the -threaded flag).
-}
{-# DEPRECATED appRenderOnMainThread
  "Should no longer be needed. Check appRenderOnMainThread's Haddock page." #-}
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
Scale factor to apply to the viewport. This factor only affects the content, not
the size of the window. It is applied in addition to the detected display scale
factor, and can be useful if the detected value is not the desired.
-}
appScaleFactor :: Double -> AppConfig e
appScaleFactor factor = def {
  _apcScaleFactor = Just factor
}

{-|
Whether display scaling detection should not be attempted. If set to True, the
display scale will be set to 1. This flag does not cause an effect on macOS.

Disabling auto scaling also affects window size on Linux and Windows in the
cases where the library would have applied scaling. This happens because window
and viewport size are the same in those operating systems. Window size can be
adjusted with 'appWindowState'.

The logic for detecting display scaling varies depending on the platform:

__macOS__

Scaling can be detected based on the window size and viewport size; the ratio
between these two give the scaling factor.

Using window and viewport size for detecting DPI only works on macOS; both
Windows and Linux return the same value for window and viewport size.

__Windows__

SDL_GetDisplayDPI returns the DPI of the screen, and dividing by 96 gives the
scaling factor. This factor is used to scale the window size and the content.

__Linux__

The situation is more complex, since SDL_GetDisplayDPI does not always return
valid information. There is not a practical DPI/scale detection solution that
works for all combinations of Linux display servers and window managers. Even
when using the most popular window managers, the scaling factor may be handled
differently by the distribution (GNOME in Ubuntu). For a reference of some of
the existing options for DPI scaling detection, check here:
https://wiki.archlinux.org/title/HiDPI.

Considering the above, when SDL_GetDisplayDPI fails, the library assumes that a
screen width larger than 1920 belongs to an HiDPI display and uses a scale
factor of 2. This factor is used to scale the window size and the content.
-}
appDisableAutoScale :: Bool -> AppConfig e
appDisableAutoScale disable = def {
  _apcDisableAutoScale = Just disable
}

{-|
Available fonts to the application, loaded from the specified path. 
Specifying no fonts will make it impossible to render text.
-}
appFontDefFile :: Text -> Text -> AppConfig e
appFontDefFile name path = def {
  _apcFonts = [ FontDefFile name path ]
}

{- |
Alias for 'appFontDefFile' for backwards compatibility.
-}
appFontDef = appFontDefFile
{-# DEPRECATED appFontDef "Use appFontDefFile directly" #-}

{-|
Available fonts to the application, loaded from the bytes in memory. 
Specifying no fonts will make it impossible to render text.
-}
appFontDefMem :: Text -> ByteString -> AppConfig e
appFontDefMem name bytes = def {
  _apcFonts = [ FontDefMem name bytes ]
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

Desktop applications should leave compositing as is since disabling it may
cause visual glitches in other programs. When creating games or fullscreen
applications, disabling compositing may improve performance.
-}
appDisableCompositing :: Bool -> AppConfig e
appDisableCompositing disable = def {
  _apcDisableCompositing = Just disable
}

{-|
Whether the screensaver should be disabled. Defaults to False.

Desktop applications should leave the screensaver as is since disabling it also
affects power saving features, including turning off the screen. When creating
games or fullscreen applications, disabling the screensaver may make sense.
-}
appDisableScreensaver :: Bool -> AppConfig e
appDisableScreensaver disable = def {
  _apcDisableScreensaver = Just disable
}
