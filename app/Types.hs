{-# LANGUAGE TemplateHaskell #-}

module Types where

import Lens.Micro
import Lens.Micro.TH (makeLenses)

import Data.Default
import qualified Data.Text as T

import Control.Monad
import Control.Monad.State

import qualified GUI.Data.Tree as TR
import GUI.Common.Core (GUIContext, WidgetTask, _appContext, _focusRing, _widgetTasks)

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
appContext :: (MonadState s m) => Lens' (GUIContext s m) s
appContext = lens _appContext (\app val -> app { _appContext = val })

focusRing :: (MonadState s m) => Lens' (GUIContext s m) [TR.Path]
focusRing = lens _focusRing (\app val -> app { _focusRing = val })

widgetTasks :: (MonadState s m) => Lens' (GUIContext s m) [WidgetTask s m]
widgetTasks = lens _widgetTasks (\app val -> app { _widgetTasks = val })
--}