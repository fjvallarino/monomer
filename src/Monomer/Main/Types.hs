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
import Monomer.Widgets.Composite (EventResponse, UIBuilder)

-- | Type of response an App event handler can return, with __s__ being the
-- | model and __e__ the user's event type.
type AppEventResponse s e = EventResponse s e s ()
-- | Type of an App event handler.
type AppEventHandler s e
  = WidgetEnv s e            -- ^ The widget environment.
  -> WidgetNode s e          -- ^ The root node of the application.
  -> s                       -- ^ The application's model.
  -> e                       -- ^ The event to handle.
  -> [AppEventResponse s e]  -- ^ The list of requested actions.
-- | Type of the function responsible of creating the App UI.
type AppUIBuilder s e = UIBuilder s e

-- | Main Monomer monad.
type MonomerM s m = (Eq s, MonadState (MonomerCtx s) m, MonadCatch m, MonadIO m)

{-|
Requirements for periodic rendering by a widget. Start time is stored to
calculate next frame based on the step ms. A maximum number of repetitions may
be provided.
-}
data RenderSchedule = RenderSchedule {
  _rsWidgetId :: WidgetId,
  _rsStart :: Int,
  _rsMs :: Int,
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
data MonomerCtx s = MonomerCtx {
  -- | Main application model.
  _mcMainModel :: s,
  -- | Active window.
  _mcWindow :: SDL.Window,
  -- | Main window size.
  _mcWindowSize :: Size,
  -- | Whether HDPI is enabled.
  _mcHdpi :: Bool,
  -- | Device pixel rate.
  _mcDpr :: Double,
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
  -- | Associations of WidgetId to updated paths. Only WidgetIds whose initial
  -- | path changed are included.
  _mcWidgetPaths :: Map WidgetId Path,
  -- | Association of Monomer CursorIcon to SDL Cursors.
  _mcCursorIcons :: Map CursorIcon SDLR.Cursor,
  -- | Hacky flag to avoid resizing when transitioning hover. Needed because
  -- | sizes may change and new target of hover should not change.
  _mcLeaveEnterPair :: Bool,
  -- | Flag indicating resize was requested in this cycle and is still pending.
  _mcResizePending :: Bool,
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
  -- | Whether to enable HDPI.
  _apcHdpi :: Maybe Bool,
  -- | Max number of FPS the application will run. It does not necessarily mean
  -- | rendering will happen every frame, but events and schedules will be
  -- | checked at this rate and may cause it.
  _apcMaxFps :: Maybe Int,
  -- | Available fonts to the application. An empty list will make it impossible
  -- | to render text.
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
  _apcMainButton :: Maybe Button
}

instance Default (AppConfig e) where
  def = AppConfig {
    _apcWindowState = Nothing,
    _apcWindowTitle = Nothing,
    _apcWindowResizable = Nothing,
    _apcWindowBorder = Nothing,
    _apcHdpi = Nothing,
    _apcMaxFps = Nothing,
    _apcFonts = [],
    _apcTheme = Nothing,
    _apcInitEvent = [],
    _apcDisposeEvent = [],
    _apcExitEvent = [],
    _apcResizeEvent = [],
    _apcMainButton = Nothing
  }

instance Semigroup (AppConfig e) where
  (<>) a1 a2 = AppConfig {
    _apcWindowState = _apcWindowState a2 <|> _apcWindowState a1,
    _apcWindowTitle = _apcWindowTitle a2 <|> _apcWindowTitle a1,
    _apcWindowResizable = _apcWindowResizable a2 <|> _apcWindowResizable a1,
    _apcWindowBorder = _apcWindowBorder a2 <|> _apcWindowBorder a1,
    _apcHdpi = _apcHdpi a2 <|> _apcHdpi a1,
    _apcMaxFps = _apcMaxFps a2 <|> _apcMaxFps a1,
    _apcFonts = _apcFonts a1 ++ _apcFonts a2,
    _apcTheme = _apcTheme a2 <|> _apcTheme a1,
    _apcInitEvent = _apcInitEvent a1 ++ _apcInitEvent a2,
    _apcDisposeEvent = _apcDisposeEvent a1 ++ _apcDisposeEvent a2,
    _apcExitEvent = _apcExitEvent a1 ++ _apcExitEvent a2,
    _apcResizeEvent = _apcResizeEvent a1 ++ _apcResizeEvent a2,
    _apcMainButton = _apcMainButton a2 <|> _apcMainButton a1
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

-- | Whether to enable HDPI.
appUseHdpi :: Bool -> AppConfig e
appUseHdpi use = def {
  _apcHdpi = Just use
}

{-|
Max number of FPS the application will run. It does not necessarily mean
rendering will happen every frame, but events and schedules will be checked at
this rate and may cause it.
-}
appMaxFps :: Int -> AppConfig e
appMaxFps fps = def {
  _apcMaxFps = Just fps
}

-- | Available fonts to the application. An empty list will make it impossible
-- | to render text.
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
