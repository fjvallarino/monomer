{-|
Module      : Monomer.Widgets.Animation.Transform
Copyright   : (c) 2023 Ruslan Gadeev, Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Transform animation widget. Wraps a child widget whose content will be animated.
Acts as a base for most animation widgets.

Messages:

- Accepts an 'AnimationMsg', used to control the state of the animation.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Animation.Transform (
  -- * Configuration
  TransformCfg(..),
  -- * Render transformations
  RenderTransform,
  animTranslation,
  animScale,
  animRotation,
  animGlobalAlpha,
  animScissor,
  -- * Constructors
  animTransform,
  animTransform_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
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

{-|
Configuration options for transform:

- 'autoStart': whether the first time the widget is added, animation should run.
- 'duration': how long the animation lasts in ms.
- 'onFinished': event to raise when animation is complete.
-}
data TransformCfg e = TransformCfg {
  _tfcAutoStart :: Maybe Bool,
  _tfcDuration :: Maybe Millisecond,
  _tfcOnFinished :: [e]
} deriving (Eq, Show)

instance Default (TransformCfg e) where
  def = TransformCfg {
    _tfcAutoStart = Nothing,
    _tfcDuration = Nothing,
    _tfcOnFinished = []
  }

instance Semigroup (TransformCfg e) where
  (<>) tc1 tc2 = TransformCfg {
    _tfcAutoStart = _tfcAutoStart tc2 <|> _tfcAutoStart tc1,
    _tfcDuration = _tfcDuration tc2 <|> _tfcDuration tc1,
    _tfcOnFinished = _tfcOnFinished tc1 <> _tfcOnFinished tc2
  }

instance Monoid (TransformCfg e) where
  mempty = def

instance CmbAutoStart (TransformCfg e) where
  autoStart_ start = def {
    _tfcAutoStart = Just start
  }

instance CmbDuration (TransformCfg e) Millisecond where
  duration dur = def {
    _tfcDuration = Just dur
  }

instance CmbOnFinished (TransformCfg e) e where
  onFinished fn = def {
    _tfcOnFinished = [fn]
  }

data TransformState = TransformState {
  _tfsRunning :: Bool,
  _tfsStartTs :: Millisecond
} deriving (Eq, Show, Generic)

instance Default TransformState where
  def = TransformState {
    _tfsRunning = False,
    _tfsStartTs = 0
  }

{-|
Possible render transformations:

- 'animTranslation': translates by the given offset.
- 'animScale': scales by the given size.
- 'animRotation': rotates by the given angle.
- 'animGlobalAlpha': applies the given alpha.
- 'animScissor': scissors to the given viewport.
-}
data RenderTransform = RenderTransform {
  _rtTranslation :: Maybe Point,
  _rtScale :: Maybe Point,
  _rtRotation :: Maybe Double,
  _rtGlobalAlpha :: Maybe Double,
  _rtScissor :: Maybe Rect
}

instance Default RenderTransform where
  def = RenderTransform {
    _rtTranslation = Nothing,
    _rtScale = Nothing,
    _rtRotation = Nothing,
    _rtGlobalAlpha = Nothing,
    _rtScissor = Nothing
  }

instance Semigroup RenderTransform where
  (<>) rt1 rt2 = RenderTransform {
    _rtTranslation = _rtTranslation rt2 <|> _rtTranslation rt1,
    _rtScale = _rtScale rt2 <|> _rtScale rt1,
    _rtRotation = _rtRotation rt2 <|> _rtRotation rt1,
    _rtGlobalAlpha = _rtGlobalAlpha rt2 <|> _rtGlobalAlpha rt1,
    _rtScissor = _rtScissor rt2 <|> _rtScissor rt1
  }

instance Monoid RenderTransform where
  mempty = def

-- | Translate by the given offset.
animTranslation :: Point -> RenderTransform
animTranslation p = def { _rtTranslation = Just p }

-- | Scale by the given size.
animScale :: Point -> RenderTransform
animScale p = def { _rtScale = Just p }

-- | Rotate by the given angle.
animRotation :: Double -> RenderTransform
animRotation r = def { _rtRotation = Just r }

-- | Apply the given alpha.
animGlobalAlpha :: Double -> RenderTransform
animGlobalAlpha a = def { _rtGlobalAlpha = Just a }

-- | Scissor to the given viewport.
animScissor :: Rect -> RenderTransform
animScissor vp = def { _rtScissor = Just vp }

type Transformer = (Double -> Rect -> [RenderTransform])

-- | Animates a widget through translation, scaling, rotation,
--   transparency and scissor.
animTransform
  :: WidgetEvent e
  => Transformer     -- ^ Transformations from time (in ms) and viewport.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animTransform f managed = animTransform_ def f managed

-- | Animates a widget through translation, scaling, rotation,
--   transparency and scissor. Accepts config.
animTransform_
  :: WidgetEvent e
  => [TransformCfg e]  -- ^ The config options.
  -> Transformer       -- ^ Transformations from time (in ms) and viewport.
  -> WidgetNode s e    -- ^ The child node.
  -> WidgetNode s e    -- ^ The created animation container.
animTransform_ configs f managed = node where
  node = defaultWidgetNode widgetType widget
    & L.info . L.focusable .~ False
    & L.children .~ Seq.singleton managed
  widgetType = WidgetType "animTransform"
  widget = makeTransform f config def
  config = mconcat configs

makeTransform
  :: WidgetEvent e
  => Transformer
  -> TransformCfg e
  -> TransformState
  -> Widget s e
makeTransform f config state = widget where
  baseWidget = createContainer state def {
    containerInit = init,
    containerMerge = merge,
    containerHandleMessage = handleMessage
  }
  widget = baseWidget {
    widgetRender = render
  }

  TransformCfg{..} = config
  TransformState{..} = state
  autoStart = fromMaybe False _tfcAutoStart
  duration = fromMaybe 500 _tfcDuration
  period = 20
  steps = fromIntegral $ duration `div` period

  finishedReq node ts = delayedMessage node (AnimationFinished ts) duration
  renderReq wenv node = req where
    widgetId = node ^. L.info . L.widgetId
    req = RenderEvery widgetId period (Just steps)

  init wenv node = result where
    ts = wenv ^. L.timestamp
    newNode = node
      & L.widget .~ makeTransform f config (TransformState True ts)
    result
      | autoStart = resultReqs newNode [finishedReq node ts, renderReq wenv node]
      | otherwise = resultNode node

  merge wenv node oldNode oldState = resultNode newNode where
    newNode = node
      & L.widget .~ makeTransform f config oldState

  handleMessage wenv node target message = result where
    result = cast message >>= Just . handleAnimateMsg wenv node

  handleAnimateMsg wenv node msg = result where
    widgetId = node ^. L.info . L.widgetId
    ts = wenv ^. L.timestamp
    startState = TransformState True ts
    startReqs = [finishedReq node ts, renderReq wenv node]

    newNode newState = node
      & L.widget .~ makeTransform f config newState
    result = case msg of
      AnimationStart -> resultReqs (newNode startState) startReqs
      AnimationStop -> resultReqs (newNode def) [RenderStop widgetId]
      AnimationFinished ts'
        | isRelevant -> resultEvts node _tfcOnFinished
        | otherwise -> resultNode node
        where isRelevant = _tfsRunning && ts' == _tfsStartTs

  render wenv node renderer = do
    if _tfsRunning
      then createOverlay renderer $ do
        saveContext renderer
        setTranslation renderer $ wenv ^. L.offset
        intersectScissor renderer scissorViewport
        setTranslation renderer $ Point (x+w/2) (y+h/2)
        setRotation renderer rotation
        setTranslation renderer $ Point (-x-w/2) (-y-h/2)
        setTranslation renderer $ Point (tx+x*(1-sx)) (ty+y*(1-sy))
        setScale renderer scale
        setGlobalAlpha renderer alpha
        widgetRender (cnode ^. L.widget) wenv cnode renderer
        restoreContext renderer
      else widgetRender (cnode ^. L.widget) wenv cnode renderer
    where
      vp@(Rect x y w h) = node ^. L.info . L.viewport
      t = clamp 0 duration $ (wenv ^. L.timestamp) - _tfsStartTs
      RenderTransform{..} = mconcat $ f (fromIntegral t) vp
      Point tx ty = fromMaybe (Point 0 0) _rtTranslation
      scale@(Point sx sy) = fromMaybe (Point 1 1) _rtScale
      rotation = fromMaybe 0 _rtRotation
      alpha = fromMaybe 1 _rtGlobalAlpha
      scissorViewport = fromMaybe vp _rtScissor
      cnode = Seq.index (node ^. L.children) 0
