{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Animate.Fade (
  AnimateMessage(..),
  fadeIn,
  fadeIn_,
  fadeOut,
  fadeOut_
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~), at)
import Control.Monad (when)
import Data.Default
import Data.Maybe
import Data.Typeable (cast)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

data AnimateMessage
  = AnimateStart
  | AnimateStop
  deriving (Eq, Show)

data FadeCfg = FadeCfg {
  _fdcAutoStart :: Maybe Bool,
  _fdcDuration :: Maybe Int
} deriving (Eq, Show)

instance Default FadeCfg where
  def = FadeCfg {
    _fdcAutoStart = Nothing,
    _fdcDuration = Nothing
  }

instance Semigroup FadeCfg where
  (<>) fc1 fc2 = FadeCfg {
    _fdcAutoStart = _fdcAutoStart fc2 <|> _fdcAutoStart fc1,
    _fdcDuration = _fdcDuration fc2 <|> _fdcDuration fc1
  }

instance Monoid FadeCfg where
  mempty = def

instance CmbAutoStart FadeCfg where
  autoStart_ start = def {
    _fdcAutoStart = Just start
  }

instance CmbDuration FadeCfg Int where
  duration dur = def {
    _fdcDuration = Just dur
  }

data FadeState = FadeState {
  _fdsRunning :: Bool,
  _fdsStartTs :: Int
} deriving (Eq, Show, Generic, Serialise)

instance Default FadeState where
  def = FadeState {
    _fdsRunning = False,
    _fdsStartTs = 0
  }

instance WidgetModel FadeState where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

fadeIn :: WidgetNode s e -> WidgetNode s e
fadeIn managed = fadeIn_ def managed

fadeIn_ :: [FadeCfg] -> WidgetNode s e -> WidgetNode s e
fadeIn_ configs managed = makeNode widget managed where
  config = mconcat configs
  widget = makeFade True config def

fadeOut :: WidgetNode s e -> WidgetNode s e
fadeOut managed = fadeOut_ def managed

fadeOut_ :: [FadeCfg] -> WidgetNode s e -> WidgetNode s e
fadeOut_ configs managed = makeNode widget managed where
  config = mconcat configs
  widget = makeFade False config def

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "fadeIn" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeFade :: Bool -> FadeCfg -> FadeState -> Widget s e
makeFade isFadeIn config state = widget where
  widget = createContainer state def {
    containerInit = init,
    containerRestore = restore,
    containerHandleMessage = handleMessage,
    containerRender = render,
    containerRenderAfter = renderPost
  }

  FadeState running start = state
  autoStart = fromMaybe False (_fdcAutoStart config)
  duration = fromMaybe 2000 (_fdcDuration config)
  period = 20
  steps = duration `div` period

  renderReq wenv node = req where
    widgetId = node ^. L.info . L.widgetId
    req = RenderEvery widgetId period (Just steps)

  init wenv node = result where
    newState = state {
      _fdsStartTs = wenv ^. L.timestamp
    }
    newNode = node
      & L.widget .~ makeFade isFadeIn config newState
    result
      | autoStart = resultReqs newNode [renderReq wenv node]
      | otherwise = resultWidget newNode

  restore wenv oldState oldInfo node = resultWidget newNode where
    newNode = node
      & L.widget .~ makeFade isFadeIn config oldState

  handleMessage wenv target message node = result where
    ts = wenv ^. L.timestamp
    widgetId = node ^. L.info . L.widgetId
    newResult start newState = resultReqs newNode newReqs where
      newNode = node
        & L.widget .~ makeFade isFadeIn config newState
      newReqs
        | start = [renderReq wenv node]
        | otherwise = [RenderStop widgetId]
    handleAnimateMsg AnimateStart = newResult True (FadeState True ts)
    handleAnimateMsg AnimateStop = newResult False def
    result = cast message >>= Just . handleAnimateMsg

  render renderer wenv node = do
    saveContext renderer
    when running $
      setGlobalAlpha renderer alpha
    where
      ts = wenv ^. L.timestamp
      currStep = clampAlpha $ fromIntegral (ts - start) / fromIntegral duration
      alpha
        | isFadeIn = currStep
        | otherwise = 1 - currStep

  renderPost renderer wenv node = do
    restoreContext renderer
