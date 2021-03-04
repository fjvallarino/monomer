{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Animate.Fade (
  fadeIn,
  fadeIn_,
  fadeOut,
  fadeOut_
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~), at)
import Data.Default
import Data.Maybe
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

newtype FadeCfg = FadeCfg {
  _fdcDuration :: Maybe Int
} deriving (Eq, Show)

instance Default FadeCfg where
  def = FadeCfg {
    _fdcDuration = Nothing
  }

instance Semigroup FadeCfg where
  (<>) fc1 fc2 = FadeCfg {
    _fdcDuration = _fdcDuration fc2 <|> _fdcDuration fc1
  }

instance Monoid FadeCfg where
  mempty = def

newtype FadeState = FadeState {
  _fdsStart :: Int
} deriving (Eq, Show, Generic, Serialise)

instance Default FadeState where
  def = FadeState {
    _fdsStart = 0
  }

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
  widget = createContainer () def {
    containerInit = init,
    containerRestore = restore,
    containerRender = render,
    containerRenderAfter = renderPost
  }

  FadeState start = state
  duration = fromMaybe 2000 (_fdcDuration config)
  period = 50
  steps = duration `div` period

  renderReq wenv node = req where
    widgetId = node ^. L.info . L.widgetId
    req = RenderEvery widgetId period (Just steps)

  init wenv node = result where
    newState = state {
      _fdsStart = wenv ^. L.timestamp
    }
    newNode = node
      & L.widget .~ makeFade isFadeIn config newState
    result = resultReqs newNode [renderReq wenv node]

  restore wenv oldState oldInfo node = result where
    result = resultWidget node

  render renderer wenv node = do
    saveContext renderer
    setGlobalAlpha renderer alpha
    where
      ts = wenv ^. L.timestamp
      currStep = clampAlpha $ fromIntegral (ts - start) / fromIntegral duration
      alpha
        | isFadeIn = currStep
        | otherwise = 1 - currStep

  renderPost renderer wenv node = do
    restoreContext renderer
