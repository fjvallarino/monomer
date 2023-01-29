{-|
Module      : Monomer.Widgets.Animation.Slide
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Slide animation widget. Wraps a child widget whose content will be animated.

Messages:

- Accepts a 'AnimationMsg', used to control the state of the animation.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Animation.Slide (
  -- * Configuration
  SlideCfg,
  slideLeft,
  slideRight,
  slideTop,
  slideBottom,
  -- * Constructors
  animSlideIn,
  animSlideIn_,
  animSlideOut,
  animSlideOut_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~), at)
import Control.Monad (when)
import Data.Default
import Data.Maybe
import Data.Typeable (cast)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Helper
import Monomer.Widgets.Container
import Monomer.Widgets.Animation.Types

import qualified Monomer.Lens as L

data SlideDirection
  = SlideLeft
  | SlideRight
  | SlideUp
  | SlideDown
  deriving (Eq, Show)

{-|
Configuration options for slide:

- 'autoStart': whether the first time the widget is added, animation should run.
- 'duration': how long the animation lasts in ms.
- 'onFinished': event to raise when animation is complete.
- Individual combinators for direction.
-}
data SlideCfg e = SlideCfg {
  _slcDirection :: Maybe SlideDirection,
  _slcAutoStart :: Maybe Bool,
  _slcDuration :: Maybe Millisecond,
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

instance CmbDuration (SlideCfg e) Millisecond where
  duration dur = def {
    _slcDuration = Just dur
  }

instance CmbOnFinished (SlideCfg e) e where
  onFinished fn = def {
    _slcOnFinished = [fn]
  }

-- | Slide from/to left.
slideLeft :: SlideCfg e
slideLeft = def { _slcDirection = Just SlideLeft }

-- | Slide from/to right.
slideRight :: SlideCfg e
slideRight = def { _slcDirection = Just SlideRight }

-- | Slide from/to top.
slideTop :: SlideCfg e
slideTop = def { _slcDirection = Just SlideUp }

-- | Slide from/to bottom.
slideBottom :: SlideCfg e
slideBottom = def { _slcDirection = Just SlideDown }

data SlideState = SlideState {
  _slsRunning :: Bool,
  _slsStartTs :: Millisecond
} deriving (Eq, Show, Generic)

instance Default SlideState where
  def = SlideState {
    _slsRunning = False,
    _slsStartTs = 0
  }

-- | Animates a widget from the left to fully visible.
animSlideIn
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animSlideIn managed = animSlideIn_ def managed

-- | Animates a widget from the provided direction to fully visible (defaults
--   to left). Accepts config.
animSlideIn_
  :: WidgetEvent e
  => [SlideCfg e]    -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animSlideIn_ configs managed = makeNode "animSlideIn" widget managed where
  config = mconcat configs
  widget = makeSlide True config def

-- | Animates a widget to the left from visible to not visible.
animSlideOut
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animSlideOut managed = animSlideOut_ def managed

-- | Animates a widget to the the provided direction from visible to not
--   visible (defaults to left). Accepts config.
animSlideOut_
  :: WidgetEvent e
  => [SlideCfg e]    -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animSlideOut_ configs managed = makeNode "animSlideOut" widget managed where
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
  steps = fromIntegral $ duration `div` period

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
      | otherwise = resultNode node

  merge wenv node oldNode oldState = resultNode newNode where
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
        | otherwise -> resultNode node

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
