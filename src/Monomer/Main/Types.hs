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
import Data.Typeable (Typeable)
import Data.Sequence (Seq)

import Monomer.Core.BasicTypes
import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Event.Types

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
  _mcPathFocus :: Path,
  _mcPathHover :: Maybe Path,
  _mcPathPressed :: Maybe Path,
  _mcPathOverlay :: Maybe Path,
  _mcWidgetTasks :: Seq WidgetTask
}

data AppConfig e = AppConfig {
  _apcWindowSize :: Maybe (Int, Int),
  _apcHdpi :: Maybe Bool
}

instance Default (AppConfig e) where
  def = AppConfig {
    _apcWindowSize = Nothing,
    _apcHdpi = Nothing
  }

instance Semigroup (AppConfig e) where
  (<>) a1 a2 = AppConfig {
    _apcWindowSize = _apcWindowSize a2 <|> _apcWindowSize a1,
    _apcHdpi = _apcHdpi a2 <|> _apcHdpi a1
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
