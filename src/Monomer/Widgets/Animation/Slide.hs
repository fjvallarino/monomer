{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Animation.Slide (
  slideIn,
  slideIn_,
  slideOut,
  slideOut_,
  leftSide,
  rightSide,
  topSide,
  bottomSide
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~), at)
import Control.Monad (when)
import Data.Default
import Data.Maybe
import Data.Typeable (cast)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Animation.Types

import qualified Monomer.Lens as L

data SlideDirection
  = SlideLeft
  | SlideRight
  | SlideUp
  | SlideDown
  deriving (Eq, Show)

data SlideCfg e = SlideCfg {
  _slcDirection :: Maybe SlideDirection,
  _slcAutoStart :: Maybe Bool,
  _slcDuration :: Maybe Int,
  _slcOnFinished :: [e]
} deriving (Eq, Show)

instance Default (SlideCfg e) where
  def = SlideCfg {
    _slcDirection = Nothing,
    _slcAutoStart = Nothing,
    _slcDuration = Nothing,
    _slcOnFinished = []
  }

instance Semigroup (SlideCfg e) where
  (<>) fc1 fc2 = SlideCfg {
    _slcDirection = _slcDirection fc2 <|> _slcDirection fc1,
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

leftSide :: SlideCfg e
leftSide = def { _slcDirection = Just SlideLeft }

rightSide :: SlideCfg e
rightSide = def { _slcDirection = Just SlideRight }

topSide :: SlideCfg e
topSide = def { _slcDirection = Just SlideUp }

bottomSide :: SlideCfg e
bottomSide = def { _slcDirection = Just SlideDown }

data SlideState = SlideState {
  _slsRunning :: Bool,
  _slsStartTs :: Int
} deriving (Eq, Show, Generic)

instance Default SlideState where
  def = SlideState {
    _slsRunning = False,
    _slsStartTs = 0
  }

slideIn :: WidgetEvent e => WidgetNode s e -> WidgetNode s e
slideIn managed = slideIn_ def managed

slideIn_ :: WidgetEvent e => [SlideCfg e] -> WidgetNode s e -> WidgetNode s e
slideIn_ configs managed = makeNode "slideIn" widget managed where
  config = mconcat configs
  widget = makeSlide True config def

slideOut :: WidgetEvent e => WidgetNode s e -> WidgetNode s e
slideOut managed = slideOut_ def managed

slideOut_ :: WidgetEvent e => [SlideCfg e] -> WidgetNode s e -> WidgetNode s e
slideOut_ configs managed = makeNode "slideOut" widget managed where
  config = mconcat configs
  widget = makeSlide False config def

makeNode
  :: WidgetEvent e => WidgetType -> Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode wType widget managedWidget = defaultWidgetNode wType widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeSlide :: WidgetEvent e => Bool -> SlideCfg e -> SlideState -> Widget s e
makeSlide isSlideIn config state = widget where
  widget = createContainer state def {
    containerUseScissor = True,
    containerInit = init,
    containerMerge = merge,
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

  merge wenv node oldNode oldState = resultWidget newNode where
    newNode = node
      & L.widget .~ makeSlide isSlideIn config oldState

  handleMessage wenv node target message = result where
    result = cast message >>= Just . handleAnimateMsg wenv node

  handleAnimateMsg wenv node msg = result where
    widgetId = node ^. L.info . L.widgetId
    ts = wenv ^. L.timestamp
    startState = SlideState True ts
    startReqs = [finishedReq node, renderReq wenv node]
    newNode newState = node
      & L.widget .~ makeSlide isSlideIn config newState
    result = case msg of
      AnimationStart -> resultReqs (newNode startState) startReqs
      AnimationStop -> resultReqs (newNode def) [RenderStop widgetId]
      AnimationFinished
        | _slsRunning state -> resultEvts node (_slcOnFinished config)
        | otherwise -> resultWidget node

  render wenv node renderer = do
    saveContext renderer
    when running $
      setTranslation renderer (Point offsetX offsetY)
    where
      viewport = node ^. L.info . L.viewport
      ts = wenv ^. L.timestamp
      dir = fromMaybe SlideLeft (_slcDirection config)
      bwdStep = clamp 0 1 $ fromIntegral (ts - start) / fromIntegral duration
      fwdStep = 1 - bwdStep
      offsetX
        | dir == SlideLeft && isSlideIn = -1 * fwdStep * viewport ^. L.w
        | dir == SlideLeft = -1 * bwdStep * viewport ^. L.w
        | dir == SlideRight && isSlideIn = fwdStep * viewport ^. L.w
        | dir == SlideRight = bwdStep * viewport ^. L.w
        | otherwise = 0
      offsetY
        | dir == SlideUp && isSlideIn = -1 * fwdStep * viewport ^. L.h
        | dir == SlideUp = -1 * bwdStep * viewport ^. L.h
        | dir == SlideDown && isSlideIn = fwdStep * viewport ^. L.h
        | dir == SlideDown = bwdStep * viewport ^. L.h
        | otherwise = 0

  renderPost wenv node renderer = do
    restoreContext renderer
