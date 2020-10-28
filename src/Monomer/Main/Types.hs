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

import qualified SDL.Raw.Types as SDL

import Monomer.Core.BasicTypes
import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

type MonomerM s m = (Eq s, MonadState (MonomerContext s) m, MonadIO m)

data WidgetTask
  = forall i . Typeable i => WidgetTask Path (Async i)
  | forall i . Typeable i => WidgetProducer Path (TChan i) (Async ())

data MonomerContext s = MonomerContext {
  _mcMainModel :: s,
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
  _mcCursorIcons :: Map CursorIcon SDL.Cursor
}

data AppConfig e = AppConfig {
  _apcWindowSize :: Maybe (Int, Int),
  _apcHdpi :: Maybe Bool,
  _apcFonts :: [FontDef],
  _apcTheme :: Maybe Theme,
  _apcInitEvent :: Maybe e
}

instance Default (AppConfig e) where
  def = AppConfig {
    _apcWindowSize = Nothing,
    _apcHdpi = Nothing,
    _apcFonts = [],
    _apcTheme = Nothing,
    _apcInitEvent = Nothing
  }

instance Semigroup (AppConfig e) where
  (<>) a1 a2 = AppConfig {
    _apcWindowSize = _apcWindowSize a2 <|> _apcWindowSize a1,
    _apcHdpi = _apcHdpi a2 <|> _apcHdpi a1,
    _apcFonts = _apcFonts a1 ++ _apcFonts a2,
    _apcTheme = _apcTheme a2 <|> _apcTheme a1,
    _apcInitEvent = _apcInitEvent a2 <|> _apcInitEvent a1
  }

instance Monoid (AppConfig e) where
  mempty = def

instance WindowSize (AppConfig e) (Int, Int) where
  windowSize size = def {
    _apcWindowSize = Just size
  }

useHdpi :: Bool -> AppConfig e
useHdpi use = def {
  _apcHdpi = Just use
}

appTheme :: Theme -> AppConfig e
appTheme t = def {
  _apcTheme = Just t
}

appInitEvent :: e -> AppConfig e
appInitEvent e = def {
  _apcInitEvent = Just e
}

fontDef :: Text -> Text -> AppConfig e
fontDef name path = def {
  _apcFonts = [ FontDef name path ]
}
