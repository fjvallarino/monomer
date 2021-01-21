{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Main.Types where

import Codec.Serialise
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

import Monomer.Core.BasicTypes
import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Core.WidgetTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

type MonomerM s m = (Eq s, MonadState (MonomerCtx s) m, MonadCatch m, MonadIO m)

data RenderSchedule = RenderSchedule {
  _rsPath :: Path,
  _rsStart :: Int,
  _rsMs :: Int
} deriving (Eq, Show, Generic, Serialise)

data WidgetTask
  = forall i . Typeable i => WidgetTask WidgetId (Async i)
  | forall i . Typeable i => WidgetProducer WidgetId (TChan i) (Async ())

data MonomerCtx s = MonomerCtx {
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
  _mcResizePending :: Bool,
  _mcRenderRequested :: Bool,
  _mcRenderSchedule :: Map Path RenderSchedule,
  _mcExitApplication :: Bool
}

data MonomerCtxPersist = MonomerCtxPersist {
  _mcpCurrentCursor :: CursorIcon,
  _mcpFocusedPath :: Path,
  _mcpHoveredPath :: Maybe Path,
  _mcpOverlayPath :: Maybe Path,
  _mcpResizePending :: Bool,
  _mcpRenderRequested :: Bool,
  _mcpRenderSchedule :: Map Path RenderSchedule
} deriving (Eq, Show, Generic, Serialise)

instance Default MonomerCtxPersist where
  def = MonomerCtxPersist {
    _mcpCurrentCursor = CursorArrow,
    _mcpFocusedPath = rootPath,
    _mcpHoveredPath = Nothing,
    _mcpOverlayPath = Nothing,
    _mcpResizePending = False,
    _mcpRenderRequested = False,
    _mcpRenderSchedule = M.empty
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
  _apcInitEvent :: [e],
  _apcExitEvent :: [e],
  _apcMainButton :: Maybe Button,
  _apcStateFileMain :: Maybe String
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
    _apcExitEvent = [],
    _apcMainButton = Nothing,
    _apcStateFileMain = Nothing
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
    _apcExitEvent = _apcExitEvent a1 ++ _apcExitEvent a2,
    _apcMainButton = _apcMainButton a2 <|> _apcMainButton a1,
    _apcStateFileMain = _apcStateFileMain a2 <|> _apcStateFileMain a1
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
appInitEvent evt = def {
  _apcInitEvent = [evt]
}

appExitEvent :: e -> AppConfig e
appExitEvent evt = def {
  _apcExitEvent = [evt]
}

appMainButton :: Button -> AppConfig e
appMainButton btn = def {
  _apcMainButton = Just btn
}

appStateFileMain :: String -> AppConfig e
appStateFileMain file = def {
  _apcStateFileMain = Just file
}
