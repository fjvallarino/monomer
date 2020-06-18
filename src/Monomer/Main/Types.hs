{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Main.Types where

import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad.State
import Data.Typeable (Typeable)
import Data.Sequence (Seq)
import Lens.Micro.TH (makeLenses)

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Widget.CompositeWidget
import Monomer.Widget.Types

type MonomerM s m = (Eq s, MonadState (MonomerContext s) m, MonadIO m)

data MonomerApp s e = MonomerApp {
  _uiBuilder :: UIBuilder s e,
  _appEventHandler :: EventHandler s e ()
}

data MonomerContext s = MonomerContext {
  _appContext :: s,
  _windowSize :: Rect,
  _useHiDPI :: Bool,
  _devicePixelRate :: Double,
  _inputStatus :: InputStatus,
  _focused :: Path,
  _latestHover :: Maybe Path,
  _widgetTasks :: Seq WidgetTask
}

data WidgetTask
  = forall a . Typeable a => WidgetTask Path (Async a)
  | forall a . Typeable a => WidgetProducer Path (TChan a) (Async ())

makeLenses ''MonomerContext
