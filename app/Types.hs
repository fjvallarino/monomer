{-# LANGUAGE TemplateHaskell #-}

module Types where

import Lens.Micro
import Lens.Micro.TH (makeLenses)

import Data.Default
import qualified Data.Text as T

import Control.Monad
import Control.Monad.State

import qualified GUI.Data.Tree as TR
import GUI.Common.Core (AsyncHandler, GUIContext, UserTask, WidgetTask, _appContext, _focusRing, _userTasks, _widgetTasks)

data App = App {
  _clickCount :: !Int,
  _textField1 :: T.Text,
  _textField2 :: T.Text,
  _textField3 :: T.Text
} deriving (Show, Eq)

instance Default App where
  def = App 0 "" "" ""

makeLenses ''App
makeLenses ''GUIContext

{--
appContext :: (MonadState s m) => Lens' (GUIContext s e) s
appContext = lens _appContext (\app val -> app { _appContext = val })

eventHandler :: (MonadState s m) => Lens' (GUIContext s e m) (s -> e -> m [AsyncHandler e])
eventHandler = lens _eventHandler (\app val -> app { _eventHandler = val })

focusRing :: (MonadState s m) => Lens' (GUIContext s e) [TR.Path]
focusRing = lens _focusRing (\app val -> app { _focusRing = val })

userTasks :: (MonadState s m) => Lens' (GUIContext s e) [UserTask e]
userTasks = lens _userTasks (\app val -> app { _userTasks = val })

widgetTasks :: (MonadState s m) => Lens' (GUIContext s e) [WidgetTask]
widgetTasks = lens _widgetTasks (\app val -> app { _widgetTasks = val })
--}

{--
  _appContext = app,
  _windowSize = winSize,
  _useHiDPI = useHiDPI,
  _devicePixelRate = devicePixelRate,
  _inputStatus = defInputStatus,
  _focusRing = [],
  _latestHover = Nothing,
  _userTasks = [],
  _widgetTasks = [],
  _eventHandler = \_ _ -> return []
--}