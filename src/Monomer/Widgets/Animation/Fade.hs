{-|
Module      : Monomer.Widgets.Animation.Fade
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Fade animation widget. Wraps a child widget whose content will be animated.

Messages:

- Accepts an 'AnimationMsg', used to control the state of the animation.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Animation.Fade (
  -- * Configuration
  FadeCfg,
  -- * Constructors
  animFadeIn,
  animFadeIn_,
  animFadeOut,
  animFadeOut_
) where

import Control.Lens ((&), (.~))
import Data.Default
import Data.Maybe

import Monomer.Widgets.Container
import Monomer.Widgets.Animation.Transform

import qualified Monomer.Lens as L

{-|
Configuration options for fade:

- 'autoStart': whether the first time the widget is added, animation should run.
- 'duration': how long the animation lasts in ms.
- 'onFinished': event to raise when animation is complete.
-}
data FadeCfg e = FadeCfg {
  _fdcTransformCfg :: TransformCfg e
} deriving (Eq, Show)

instance Default (FadeCfg e) where
  def = FadeCfg {
    _fdcTransformCfg = def
  }

instance Semigroup (FadeCfg e) where
  (<>) fc1 fc2 = FadeCfg {
    _fdcTransformCfg = _fdcTransformCfg fc1 <> _fdcTransformCfg fc2
  }

instance Monoid (FadeCfg e) where
  mempty = def

instance CmbAutoStart (FadeCfg e) where
  autoStart_ start = def {
    _fdcTransformCfg = autoStart_ start
  }

instance CmbDuration (FadeCfg e) Millisecond where
  duration dur = def {
    _fdcTransformCfg = duration dur
  }

instance CmbOnFinished (FadeCfg e) e where
  onFinished fn = def {
    _fdcTransformCfg = onFinished fn
  }

-- | Animates a widget from not visible state to fully visible.
animFadeIn
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animFadeIn managed = animFadeIn_ def managed

-- | Animates a widget from not visible state to fully visible. Accepts config.
animFadeIn_
  :: WidgetEvent e
  => [FadeCfg e]     -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animFadeIn_ configs managed = makeNode configs managed True
  & L.info . L.widgetType .~ "animFadeIn"

-- | Animates a widget from visible state to not visible.
animFadeOut
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animFadeOut managed = animFadeOut_ def managed

-- | Animates a widget from visible state to not visible. Accepts config.
animFadeOut_
  :: WidgetEvent e
  => [FadeCfg e]     -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animFadeOut_ configs managed = makeNode configs managed False
  & L.info . L.widgetType .~ "animFadeOut"

makeNode
  :: WidgetEvent e
  => [FadeCfg e]
  -> WidgetNode s e
  -> Bool
  -> WidgetNode s e
makeNode configs managed isFadeIn = node where
  node = animTransform_ [_fdcTransformCfg] f managed
  f t _ = [animGlobalAlpha $ alpha t]
  alpha t = if isFadeIn
    then (currStep t)
    else 1-(currStep t)
  currStep t = clampAlpha $ t/(fromIntegral dur)
  dur = fromMaybe 500 _tfcDuration
  TransformCfg{..} = _fdcTransformCfg
  FadeCfg{..} = mconcat configs
