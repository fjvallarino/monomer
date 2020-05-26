{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Scroll (scroll) where

import Control.Monad
import Data.Default
import Data.Typeable
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Event.Types
import Monomer.Graphics.Color
import Monomer.Graphics.Drawing
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types
import Monomer.Widget.BaseContainer
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

import qualified Monomer.Common.Tree as Tr

data ScrollState = ScrollState {
  _scDeltaX :: !Double,
  _scDeltaY :: !Double,
  _scChildSize :: Size
} deriving (Eq, Show, Typeable, Generic)

defaultState = ScrollState 0 0 def

scroll :: (Monad m) => WidgetInstance s e m -> WidgetInstance s e m
scroll managedWidget = makeInstance (makeScroll defaultState) managedWidget

makeInstance :: (Monad m) => Widget s e m -> WidgetInstance s e m -> WidgetInstance s e m
makeInstance widget managedWidget = (defaultWidgetInstance "scroll" widget) {
  _instanceChildren = Seq.singleton managedWidget,
  _instanceFocusable = True
}

makeScroll :: (Monad m) => ScrollState -> Widget s e m
makeScroll state@(ScrollState dx dy cs@(Size cw ch)) = createContainer {
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize,
    _widgetRender = render
  }
  where
    stepSize = 50
    wheelRate = 10
    handleEvent ctx evt app widgetInstance = case evt of
      Click (Point px py) btn status -> result where
        view = _instanceViewport widgetInstance
        result = if isPressed then resultReqs [ResizeChildren (_pathCurrent ctx)] newInstance else Nothing
        isPressed = status == PressedBtn && inRect view (Point px py)
        isLeftClick = isPressed && btn == LeftBtn
        isRigthClick = isPressed && btn == RightBtn
        step = if | isLeftClick -> stepSize
                  | isRigthClick -> -stepSize
                  | otherwise -> 0
        newState = ScrollState (scrollAxis step dx cw (_rw view)) dy cs
        newInstance = widgetInstance { _instanceWidget = makeScroll newState }
      WheelScroll _ (Point wx wy) wheelDirection -> result where
        Rect rx ry rw rh = _instanceViewport widgetInstance
        needsUpdate = (wx /= 0 && cw > rw) || (wy /= 0 && ch > rh)
        result = if needsUpdate then resultReqs [ResizeChildren (_pathCurrent ctx)] newInstance else Nothing
        stepX = wx * if wheelDirection == WheelNormal then -wheelRate else wheelRate
        stepY = wy * if wheelDirection == WheelNormal then wheelRate else -wheelRate
        newState = ScrollState (scrollAxis stepX dx cw rw) (scrollAxis stepY dy ch rh) cs
        newInstance = widgetInstance { _instanceWidget = makeScroll newState }
      _ -> Nothing
    scrollAxis reqDelta currScroll childPos viewportLimit
      | reqDelta >= 0 = if currScroll + reqDelta < 0
                          then currScroll + reqDelta
                          else 0
      | otherwise = if childPos - viewportLimit + currScroll + reqDelta > 0
                      then currScroll + reqDelta
                      else viewportLimit - childPos

    --preferredSize renderer app childrenPairs = return (Tr.Node reqSize children) where
    preferredSize renderer app childrenPairs = return (Tr.Node sizeReq childrenReqs) where
      --reqsTree = fmap snd childrenPairs
      --childrenReqs = fmap (Tr.nodeValue . snd) childrenPairs
      childrenReqs = fmap snd childrenPairs
      sizeReq = SizeReq (_sizeRequested . Tr.nodeValue $ Seq.index childrenReqs 0) FlexibleSize FlexibleSize

    --resize app viewport renderArea childrenPairs = assignedAreas where
    resize app viewport renderArea childrenPairs = Seq.singleton (childViewport, childRenderArea) where
      Rect l t w h = viewport
      Size cw2 ch2 = _sizeRequested (Tr.nodeValue . snd $ Seq.index childrenPairs 0)
      areaW = max w cw2
      areaH = max h ch2
      childViewport = Rect l t w h
      childRenderArea = Rect (l + dx) (t + dy) areaW areaH
    render renderer ts app widgetInstance =
      do
        scissor renderer viewport
        containerRender renderer ts app widgetInstance
        resetScissor renderer

        when (barRatioH < 1) $ do
          drawRect renderer scrollRectH (Just $ darkGray { _alpha = 0.6 }) Nothing

        when (barRatioV < 1) $ do
          drawRect renderer scrollRectV (Just $ darkGray { _alpha = 0.6 }) Nothing
      where
        barThickness = 10
        viewport = _instanceViewport widgetInstance
        vpLeft = _rx viewport
        vpTop = _ry viewport
        vpWidth = _rw viewport
        vpHeight = _rh viewport
        barTop = vpHeight - barThickness
        barLeft = vpWidth - barThickness
        barRatioH = vpWidth / cw
        barRatioV = vpHeight / ch
        scrollRectH = Rect (vpLeft - barRatioH * dx) (vpTop + barTop) (barRatioH * vpWidth) barThickness
        scrollRectV = Rect (vpLeft + barLeft) (vpTop - barRatioV * dy) barThickness (barRatioV * vpHeight)
