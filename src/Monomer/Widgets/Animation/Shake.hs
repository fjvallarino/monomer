{-|
Module      : Monomer.Widgets.Animation.Shake
Copyright   : (c) 2023 Ruslan Gadeev, Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Shake animation widget. Wraps a child widget whose content will be animated.

Messages:

- Accepts a 'AnimationMsg', used to control the state of the animation.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Animation.Shake (
  -- * Configuration
  ShakeCfg,
  shakeH,
  shakeV,
  shakeR,
  shakeS,
  shakeAmplitude,
  shakeFrequency,
  -- * Constructors
  animShake,
  animShake_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~))
import Data.Default
import Data.Maybe

import Monomer.Helper
import Monomer.Widgets.Container
import Monomer.Widgets.Animation.Transform

import qualified Monomer.Lens as L

data ShakeDirection
  = ShakeH
  | ShakeV
  | ShakeR
  | ShakeS
  deriving (Eq, Show)

{-|
Configuration options for shake:

- 'autoStart': whether the first time the widget is added, animation should run.
- 'duration': how long the animation lasts in ms.
- 'onFinished': event to raise when animation is complete.
- 'shakeAmplitude': amplitude of the animation. Defaults to 0.1.
- 'shakeFrequency': frequency of the animation. Defaults to 2.
- Individual combinators for direction.
-}
data ShakeCfg e = ShakeCfg {
  _shcDirection :: Maybe ShakeDirection,
  _shcAmplitude :: Maybe Double,
  _shcFrequency :: Maybe Int,
  _shcTransformCfg :: TransformCfg e
} deriving (Eq, Show)

instance Default (ShakeCfg e) where
  def = ShakeCfg {
    _shcDirection = Nothing,
    _shcAmplitude = Nothing,
    _shcFrequency = Nothing,
    _shcTransformCfg = def
  }

instance Semigroup (ShakeCfg e) where
  (<>) sc1 sc2 = ShakeCfg {
    _shcDirection = _shcDirection sc2 <|> _shcDirection sc1,
    _shcAmplitude = _shcAmplitude sc2 <|> _shcAmplitude sc1,
    _shcFrequency = _shcFrequency sc2 <|> _shcFrequency sc1,
    _shcTransformCfg = _shcTransformCfg sc1 <> _shcTransformCfg sc2
  }

instance Monoid (ShakeCfg e) where
  mempty = def

instance CmbAutoStart (ShakeCfg e) where
  autoStart_ start = def {
    _shcTransformCfg = autoStart_ start
  }

instance CmbDuration (ShakeCfg e) Millisecond where
  duration dur = def {
    _shcTransformCfg = duration dur
  }

instance CmbOnFinished (ShakeCfg e) e where
  onFinished fn = def {
    _shcTransformCfg = onFinished fn
  }

-- | Shake horizontally.
shakeH :: ShakeCfg e
shakeH = def { _shcDirection = Just ShakeH }

-- | Shake vertically.
shakeV :: ShakeCfg e
shakeV = def { _shcDirection = Just ShakeV }

-- | Shake by rotating.
shakeR :: ShakeCfg e
shakeR = def { _shcDirection = Just ShakeR }

-- | Shake by scaling.
shakeS :: ShakeCfg e
shakeS = def { _shcDirection = Just ShakeS }

-- | Amplitude of the animation. Defaults to 1.
shakeAmplitude :: Double -> ShakeCfg e
shakeAmplitude amp = def { _shcAmplitude = Just amp }

-- | Frequency of the animation. Defaults to 2.
shakeFrequency :: Int -> ShakeCfg e
shakeFrequency freq = def { _shcFrequency = Just freq }

-- | Shakes a widget.
animShake
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animShake managed = animShake_ def managed

-- | Shakes a widget. Accepts config.
animShake_
  :: WidgetEvent e
  => [ShakeCfg e]    -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animShake_ configs managed = node where
  node = animTransform_ [_shcTransformCfg] f managed
    & L.info . L.widgetType .~ "animShake"
  f t vp@(Rect _ _ w h) = (noScissor vp) <> case dir of
    ShakeH -> [animTranslation $ Point ((step t)*w) 0]
    ShakeV -> [animTranslation $ Point 0 ((step t)*h)]
    ShakeR -> [animRotation $ (step t)*180]
    ShakeS ->
      [ animTranslation $ Point ((1-(ss t))*w/2) ((1-(ss t))*h/2)
      , animScale $ Point (ss t) (ss t)
      ]
  noScissor (Rect x y w h) =
    [animScissor $ Rect (x-w*10) (y-h*10) (w*20) (h*20)]
  step t = (sin $ (fs t)*freq*2*pi)*amp
  ss t = 1-(amp/2)+(cos $ (fs t)*freq*2*pi)*amp/2
  fs t = clamp 0 1 $ t/(fromIntegral dur)
  dir = fromMaybe ShakeH _shcDirection
  amp = fromMaybe 0.1 _shcAmplitude
  freq = fromIntegral $ fromMaybe 2 _shcFrequency
  dur = fromMaybe 500 _tfcDuration
  TransformCfg{..} = _shcTransformCfg
  ShakeCfg{..} = mconcat configs
