{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Scroll (scroll) where

import Control.Monad
import Data.Default
import Data.Maybe
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
  _scChildSize :: Size,
  _scReqSize :: Tr.Tree SizeReq
} deriving (Typeable, Generic)

defaultState = ScrollState 0 0 def (Tr.singleton def)

scroll :: (Monad m) => WidgetInstance s e m -> WidgetInstance s e m
scroll managedWidget = makeInstance (makeScroll defaultState) managedWidget

makeInstance :: (Monad m) => Widget s e m -> WidgetInstance s e m -> WidgetInstance s e m
makeInstance widget managedWidget = (defaultWidgetInstance "scroll" widget) {
  _instanceChildren = Seq.singleton managedWidget,
  _instanceFocusable = False
}

makeScroll :: (Monad m) => ScrollState -> Widget s e m
makeScroll state@(ScrollState dx dy cs@(Size cw ch) prevReqs) = createContainer {
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = scrollResize Nothing, -- containerResize resize,
    _widgetRender = render
  }
  where
    stepSize = 50
    wheelRate = 10
    handleEvent ctx evt app widgetInstance = case evt of
      Click (Point px py) btn status -> result where
        viewport = _instanceViewport widgetInstance
        result = if | isPressed -> resultWidget (rebuildWidget app newState widgetInstance prevReqs)
                    | otherwise -> Nothing
        isPressed = status == PressedBtn && inRect viewport (Point px py)
        isLeftClick = isPressed && btn == LeftBtn
        isRigthClick = isPressed && btn == RightBtn
        step = if | isLeftClick -> stepSize
                  | isRigthClick -> -stepSize
                  | otherwise -> 0
        newState = ScrollState (scrollAxis step dx cw (_rw viewport)) dy cs prevReqs
      WheelScroll _ (Point wx wy) wheelDirection -> result where
        Rect rx ry rw rh = _instanceViewport widgetInstance
        needsUpdate = (wx /= 0 && cw > rw) || (wy /= 0 && ch > rh)
        result = if | needsUpdate -> resultWidget (rebuildWidget app newState widgetInstance prevReqs)
                    | otherwise   -> Nothing
        stepX = wx * if wheelDirection == WheelNormal then -wheelRate else wheelRate
        stepY = wy * if wheelDirection == WheelNormal then wheelRate else -wheelRate
        newState = ScrollState (scrollAxis stepX dx cw rw) (scrollAxis stepY dy ch rh) cs prevReqs
      _ -> Nothing
    scrollAxis reqDelta currScroll childPos viewportLimit
      | reqDelta >= 0 = if currScroll + reqDelta < 0
                          then currScroll + reqDelta
                          else 0
      | otherwise = if childPos - viewportLimit + currScroll + reqDelta > 0
                      then currScroll + reqDelta
                      else viewportLimit - childPos

    rebuildWidget app newState widgetInstance reqs = newInstance where
      newWidget = makeScroll newState
      tempInstance = widgetInstance { _instanceWidget = newWidget }
      newInstance = scrollResize (Just newWidget) app (_instanceViewport tempInstance) (_instanceRenderArea tempInstance) tempInstance reqs

    preferredSize renderer app childrenPairs = Tr.Node sizeReq childrenReqs where
      childrenReqs = fmap snd childrenPairs
      sizeReq = SizeReq (_sizeRequested . Tr.nodeValue $ Seq.index childrenReqs 0) FlexibleSize FlexibleSize

    scrollResize updatedWidget app viewport renderArea widgetInstance reqs = newInstance where
      Rect l t w h = renderArea
      child = Seq.index (_instanceChildren widgetInstance) 0
      childReq = Seq.index (Tr.nodeChildren reqs) 0

      Size cw2 ch2 = _sizeRequested $ Tr.nodeValue childReq
      areaW = max w cw2
      areaH = max h ch2
      childRenderArea = Rect (l + dx) (t + dy) areaW areaH

      newWidget = fromMaybe (makeScroll $ ScrollState dx dy (Size areaW areaH) reqs) updatedWidget
      newChildWidget = _widgetResize (_instanceWidget child) app viewport childRenderArea child childReq

      newInstance = widgetInstance {
        _instanceViewport = viewport,
        _instanceRenderArea = renderArea,
        _instanceWidget = newWidget,
        _instanceChildren = Seq.singleton newChildWidget
      }

    render renderer ts ctx app widgetInstance =
      do
        scissor renderer viewport
        containerRender renderer ts ctx app widgetInstance
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
