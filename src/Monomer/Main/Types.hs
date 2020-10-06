{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Types where

import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad.State
import Data.Typeable (Typeable)
import Data.Sequence (Seq)

import Monomer.Core.BasicTypes
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
