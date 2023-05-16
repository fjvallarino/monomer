{-|
Module      : Monomer.Widgets.Animation.Zoom
Copyright   : (c) 2023 Ruslan Gadeev, Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Zoom animation widget. Wraps a child widget whose content will be animated.

Messages:

- Accepts a 'AnimationMsg', used to control the state of the animation.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Animation.Zoom (
  -- * Configuration
  ZoomCfg,
  -- * Constructors
  animZoomIn,
  animZoomIn_,
  animZoomOut,
  animZoomOut_
) where

import Control.Lens ((&), (.~))
import Data.Default
import Data.Maybe

import Monomer.Helper
import Monomer.Widgets.Container
import Monomer.Widgets.Animation.Transform

import qualified Monomer.Lens as L

{-|
Configuration options for zoom:

- 'autoStart': whether the first time the widget is added, animation should run.
- 'duration': how long the animation lasts in ms.
- 'onFinished': event to raise when animation is complete.
- 'onFinishedReq': 'WidgetRequest' to generate when animation is complete.
-}
data ZoomCfg s e = ZoomCfg {
  _zmcTransformCfg :: TransformCfg s e
} deriving (Eq, Show)

instance Default (ZoomCfg s e) where
  def = ZoomCfg {
    _zmcTransformCfg = def
  }

instance Semigroup (ZoomCfg s e) where
  (<>) zc1 zc2 = ZoomCfg {
    _zmcTransformCfg = _zmcTransformCfg zc1 <> _zmcTransformCfg zc2
  }

instance Monoid (ZoomCfg s e) where
  mempty = def

instance CmbAutoStart (ZoomCfg s e) where
  autoStart_ start = def {
    _zmcTransformCfg = autoStart_ start
  }

instance CmbDuration (ZoomCfg s e) Millisecond where
  duration dur = def {
    _zmcTransformCfg = duration dur
  }

instance WidgetEvent e => CmbOnFinished (ZoomCfg s e) e where
  onFinished handler = def {
    _zmcTransformCfg = onFinished handler
  }

instance CmbOnFinishedReq (ZoomCfg s e) s e where
  onFinishedReq req = def {
    _zmcTransformCfg = onFinishedReq req
  }

-- | Animates a widget to fully visible by increasing scale.
animZoomIn
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animZoomIn managed = animZoomIn_ def managed

-- | Animates a widget to fully visible by increasing scale. Accepts config.
animZoomIn_
  :: WidgetEvent e
  => [ZoomCfg s e]     -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animZoomIn_ configs managed = makeNode configs managed True
  & L.info . L.widgetType .~ "animZoomIn"

-- | Animates a widget to not visible by decreasing scale.
animZoomOut
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animZoomOut managed = animZoomOut_ def managed

-- | Animates a widget to not visible by decreasing scale. Accepts config.
animZoomOut_
  :: WidgetEvent e
  => [ZoomCfg s e]     -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animZoomOut_ configs managed = makeNode configs managed False
  & L.info . L.widgetType .~ "animZoomOut"

makeNode
  :: WidgetEvent e
  => [ZoomCfg s e]
  -> WidgetNode s e
  -> Bool
  -> WidgetNode s e
makeNode configs managed isZoomIn = node where
  node = animTransform_ [_zmcTransformCfg] f managed
  f t (Rect _ _ w h) =
    [ animTranslation $ Point (ft t w) (ft t h)
    , animScale $ Point (fs t) (fs t)
    ]
  ft t s = (1-(fs t))*s/2
  fs t = if isZoomIn
    then fwdStep t
    else 1-(fwdStep t)
  fwdStep t = clamp 0 1 $ t/(fromIntegral dur)
  dur = fromMaybe 500 _tfcDuration
  TransformCfg{..} = _zmcTransformCfg
  ZoomCfg{..} = mconcat configs
