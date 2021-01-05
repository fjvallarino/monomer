{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Types where

import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad.State
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Sequence (Seq)

import qualified SDL
import qualified SDL.Raw.Types as SDLR

import Monomer.Core.BasicTypes
import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Core.WidgetTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

type MonomerM s m = (Eq s, MonadState (MonomerContext s) m, MonadIO m)

data RenderSchedule = RenderSchedule {
  _rsPath :: Path,
  _rsStart :: Int,
  _rsMs :: Int
} deriving (Eq, Show)

data WidgetTask
  = forall i . Typeable i => WidgetTask WidgetId (Async i)
  | forall i . Typeable i => WidgetProducer WidgetId (TChan i) (Async ())

data MonomerContext s = MonomerContext {
  _mcMainModel :: s,
  _mcWindow :: SDL.Window,
  _mcWindowSize :: Size,
  _mcHdpi :: Bool,
  _mcDpr :: Double,
  _mcInputStatus :: InputStatus,
  _mcCurrentCursor :: CursorIcon,
  _mcFocusedPath :: Path,
  _mcHoveredPath :: Maybe Path,
  _mcOverlayPath :: Maybe Path,
  _mcMainBtnPress :: Maybe (Path, Point),
  _mcWidgetTasks :: Seq WidgetTask,
  _mcWidgetPaths :: Map WidgetId (Path, Int),
  _mcCursorIcons :: Map CursorIcon SDLR.Cursor,
  -- Hacky flag to avoid resizing when transitioning hover
  -- Needed because sizes may change and new target of hover should not change
  _mcLeaveEnterPair :: Bool,
  _mcRenderRequested :: Bool,
  _mcRenderSchedule :: Map Path RenderSchedule,
  _mcExitApplication :: Bool
}

data MainWindowState
  = MainWindowNormal (Int, Int)
  | MainWindowMaximized
  | MainWindowFullScreen
  deriving (Eq, Show)

data AppConfig e = AppConfig {
  _apcWindowState :: Maybe MainWindowState,
  _apcWindowTitle :: Maybe Text,
  _apcWindowResizable :: Maybe Bool,
  _apcWindowBorder :: Maybe Bool,
  _apcHdpi :: Maybe Bool,
  _apcMaxFps :: Maybe Int,
  _apcFonts :: [FontDef],
  _apcTheme :: Maybe Theme,
  _apcInitEvent :: Maybe e,
  _apcExitEvent :: Maybe e,
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
    _apcInitEvent = Nothing,
    _apcExitEvent = Nothing,
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
    _apcInitEvent = _apcInitEvent a2 <|> _apcInitEvent a1,
    _apcExitEvent = _apcExitEvent a2 <|> _apcExitEvent a1,
    _apcMainButton = _apcMainButton a2 <|> _apcMainButton a1
  }

instance Monoid (AppConfig e) where
  mempty = def

appWindowState :: MainWindowState -> AppConfig e
appWindowState title = def {
  _apcWindowState = Just title
}

appWindowTitle :: Text -> AppConfig e
appWindowTitle title = def {
  _apcWindowTitle = Just title
}

appWindowResizable :: Bool -> AppConfig e
appWindowResizable resizable = def {
  _apcWindowResizable = Just resizable
}

appWindowBorder :: Bool -> AppConfig e
appWindowBorder border = def {
  _apcWindowBorder = Just border
}

appUseHdpi :: Bool -> AppConfig e
appUseHdpi use = def {
  _apcHdpi = Just use
}

appMaxFps :: Int -> AppConfig e
appMaxFps fps = def {
  _apcMaxFps = Just fps
}

appFontDef :: Text -> Text -> AppConfig e
appFontDef name path = def {
  _apcFonts = [ FontDef name path ]
}

appTheme :: Theme -> AppConfig e
appTheme t = def {
  _apcTheme = Just t
}

appInitEvent :: e -> AppConfig e
appInitEvent e = def {
  _apcInitEvent = Just e
}

appExitEvent :: e -> AppConfig e
appExitEvent e = def {
  _apcExitEvent = Just e
}

appMainButton :: Button -> AppConfig e
appMainButton btn = def {
  _apcMainButton = Just btn
}
