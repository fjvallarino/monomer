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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

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
import Control.Lens ((&), (.~))
import Data.Default
import Data.Maybe

import Monomer.Helper
import Monomer.Widgets.Container
import Monomer.Widgets.Animation.Transform

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
  _slcTransformCfg :: TransformCfg e
} deriving (Eq, Show)

instance Default (SlideCfg e) where
  def = SlideCfg {
    _slcDirection = Nothing,
    _slcTransformCfg = def
  }

instance Semigroup (SlideCfg e) where
  (<>) fc1 fc2 = SlideCfg {
    _slcDirection = _slcDirection fc2 <|> _slcDirection fc1,
    _slcTransformCfg = _slcTransformCfg fc1 <> _slcTransformCfg fc2
  }

instance Monoid (SlideCfg e) where
  mempty = def

instance CmbAutoStart (SlideCfg e) where
  autoStart_ start = def {
    _slcTransformCfg = autoStart_ start
  }

instance CmbDuration (SlideCfg e) Millisecond where
  duration dur = def {
    _slcTransformCfg = duration dur
  }

instance CmbOnFinished (SlideCfg e) e where
  onFinished fn = def {
    _slcTransformCfg = onFinished fn
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
animSlideIn_ configs managed = makeNode configs managed True
  & L.info . L.widgetType .~ "animSlideIn"

-- | Animates a widget to the left from visible to not visible.
animSlideOut
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animSlideOut managed = animSlideOut_ def managed

-- | Animates a widget to the provided direction from visible to not
--   visible (defaults to left). Accepts config.
animSlideOut_
  :: WidgetEvent e
  => [SlideCfg e]    -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animSlideOut_ configs managed = makeNode configs managed False
  & L.info . L.widgetType .~ "animSlideOut"

makeNode
  :: WidgetEvent e
  => [SlideCfg e]
  -> WidgetNode s e
  -> Bool
  -> WidgetNode s e
makeNode configs managed isSlideIn = node where
  node = animTransform_ [_slcTransformCfg] f managed
  f t vp = [animTranslation $ Point (fx t vp) (fy t vp)]
  fx t (Rect _ _ w _) = case dir of
    SlideLeft -> -1*(step t)*w
    SlideRight -> (step t)*w
    _ -> 0
  fy t (Rect _ _ _ h) = case dir of
    SlideUp -> -1*(step t)*h
    SlideDown -> (step t)*h
    _ -> 0
  step t = if isSlideIn
    then 1-(fwdStep t)
    else fwdStep t
  fwdStep t = clamp 0 1 $ t/(fromIntegral dur)
  dir = fromMaybe SlideLeft _slcDirection
  dur = fromMaybe 500 _tfcDuration
  TransformCfg{..} = _slcTransformCfg
  SlideCfg{..} = mconcat configs
