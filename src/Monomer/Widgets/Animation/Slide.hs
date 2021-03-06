{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Animation.Slide (
  slideIn,
  slideIn_,
  slideOut,
  slideOut_
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~), at)
import Control.Monad (when)
import Data.Default
import Data.Maybe
import Data.Typeable (Typeable, cast)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Animation.Types

import qualified Monomer.Lens as L

data SlideCfg e = SlideCfg {
  _slcAutoStart :: Maybe Bool,
  _slcDuration :: Maybe Int,
  _slcOnFinished :: [e]
} deriving (Eq, Show)

instance Default (SlideCfg e) where
  def = SlideCfg {
    _slcAutoStart = Nothing,
    _slcDuration = Nothing,
    _slcOnFinished = []
  }

instance Semigroup (SlideCfg e) where
  (<>) fc1 fc2 = SlideCfg {
    _slcAutoStart = _slcAutoStart fc2 <|> _slcAutoStart fc1,
    _slcDuration = _slcDuration fc2 <|> _slcDuration fc1,
    _slcOnFinished = _slcOnFinished fc1 <> _slcOnFinished fc2
  }

instance Monoid (SlideCfg e) where
  mempty = def

instance CmbAutoStart (SlideCfg e) where
  autoStart_ start = def {
    _slcAutoStart = Just start
  }

instance CmbDuration (SlideCfg e) Int where
  duration dur = def {
    _slcDuration = Just dur
  }

instance CmbOnFinished (SlideCfg e) e where
  onFinished fn = def {
    _slcOnFinished = [fn]
  }

data SlideState = SlideState {
  _slsRunning :: Bool,
  _slsStartTs :: Int
} deriving (Eq, Show, Generic, Serialise)

instance Default SlideState where
  def = SlideState {
    _slsRunning = False,
    _slsStartTs = 0
  }

instance WidgetModel SlideState where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

slideIn :: WidgetNode s e -> WidgetNode s e
slideIn managed = slideIn_ def managed

slideIn_ :: [SlideCfg e] -> WidgetNode s e -> WidgetNode s e
slideIn_ configs managed = makeNode "slideIn" widget managed where
  config = mconcat configs
  widget = makeSlide True config def

slideOut :: WidgetNode s e -> WidgetNode s e
slideOut managed = slideOut_ def managed

slideOut_ :: [SlideCfg e] -> WidgetNode s e -> WidgetNode s e
slideOut_ configs managed = makeNode "slideOut" widget managed where
  config = mconcat configs
  widget = makeSlide False config def

makeNode :: WidgetType -> Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode wType widget managedWidget = defaultWidgetNode wType widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeSlide :: Bool -> SlideCfg e -> SlideState -> Widget s e
makeSlide isSlideIn config state = widget where
  widget = createContainer state def {
    containerInit = init,
    containerRestore = restore,
    containerHandleMessage = handleMessage,
    containerRender = render,
    containerRenderAfter = renderPost
  }

  SlideState running start = state
  autoStart = fromMaybe False (_slcAutoStart config)
  duration = fromMaybe 500 (_slcDuration config)
  period = 20
  steps = duration `div` period

  finishedReq node = delayedMessage node AnimationFinished duration
  renderReq wenv node = req where
    widgetId = node ^. L.info . L.widgetId
    req = RenderEvery widgetId period (Just steps)

  init wenv node = result where
    ts = wenv ^. L.timestamp
    newNode = node
      & L.widget .~ makeSlide isSlideIn config (SlideState True ts)
    result
      | autoStart = resultReqs newNode [finishedReq node, renderReq wenv node]
      | otherwise = resultWidget node

  restore wenv oldState oldInfo node = resultWidget newNode where
    newNode = node
      & L.widget .~ makeSlide isSlideIn config oldState

  handleMessage wenv target message node = result where
    result = cast message >>= Just . handleAnimateMsg wenv node

  handleAnimateMsg wenv node msg = result where
    widgetId = node ^. L.info . L.widgetId
    ts = wenv ^. L.timestamp
    startState = SlideState True ts
    startReqs = [renderReq wenv node, finishedReq node]
    newNode newState = node
      & L.widget .~ makeSlide isSlideIn config newState
    result = case msg of
      AnimationStart -> resultReqs (newNode startState) startReqs
      AnimationStop -> resultReqs (newNode def) [RenderStop widgetId]
      AnimationFinished
        | _slsRunning state -> resultEvts node (_slcOnFinished config)
        | otherwise -> resultWidget node

  render renderer wenv node = do
    saveContext renderer
    when running $
      setTranslation renderer (Point (-offsetX) 0)
    where
      viewport = node ^. L.info . L.viewport
      ts = wenv ^. L.timestamp
      currStep = clamp 0 1 $ fromIntegral (ts - start) / fromIntegral duration
      offsetX
        | isSlideIn = (1 - currStep) * viewport ^. L.w
        | otherwise = currStep * viewport ^. L.w

  renderPost renderer wenv node = do
    restoreContext renderer
