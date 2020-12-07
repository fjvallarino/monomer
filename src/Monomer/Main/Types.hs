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
import Monomer.Event.Types
import Monomer.Graphics.Types

type MonomerM s m = (Eq s, MonadState (MonomerContext s) m, MonadIO m)

data RenderSchedule = RenderSchedule {
  _rsPath :: Path,
  _rsStart :: Int,
  _rsMs :: Int
} deriving (Eq, Show)

data WidgetTask
  = forall i . Typeable i => WidgetTask Path (Async i)
  | forall i . Typeable i => WidgetProducer Path (TChan i) (Async ())

data MonomerContext s = MonomerContext {
  _mcMainModel :: s,
  _mcWindow :: SDL.Window,
  _mcWindowSize :: Size,
  _mcHdpi :: Bool,
  _mcDpr :: Double,
  _mcInputStatus :: InputStatus,
  _mcCurrentCursor :: CursorIcon,
  _mcPathFocus :: Path,
  _mcPathHover :: Maybe Path,
  _mcPathPressed :: Maybe Path,
  _mcPathOverlay :: Maybe Path,
  _mcWidgetTasks :: Seq WidgetTask,
  _mcCursorIcons :: Map CursorIcon SDLR.Cursor,
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
  _apcExitEvent :: Maybe e
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
    _apcExitEvent = Nothing
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
    _apcExitEvent = _apcExitEvent a2 <|> _apcExitEvent a1
  }

instance Monoid (AppConfig e) where
  mempty = def

mainWindowState :: MainWindowState -> AppConfig e
mainWindowState title = def {
  _apcWindowState = Just title
}

mainWindowTitle :: Text -> AppConfig e
mainWindowTitle title = def {
  _apcWindowTitle = Just title
}

mainWindowResizable :: Bool -> AppConfig e
mainWindowResizable resizable = def {
  _apcWindowResizable = Just resizable
}

mainWindowBorder :: Bool -> AppConfig e
mainWindowBorder border = def {
  _apcWindowBorder = Just border
}

useHdpi :: Bool -> AppConfig e
useHdpi use = def {
  _apcHdpi = Just use
}

maxFps :: Int -> AppConfig e
maxFps fps = def {
  _apcMaxFps = Just fps
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

fontDef :: Text -> Text -> AppConfig e
fontDef name path = def {
  _apcFonts = [ FontDef name path ]
}
