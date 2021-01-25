{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Tooltip (
  tooltip,
  tooltip_,
  tooltipDelay
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~), at)
import Control.Monad (forM_, when)
import Data.Default
import Data.Maybe
import Data.Text (Text)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

data TooltipCfg = TooltipCfg {
  _ttcDelay :: Maybe Int,
  _ttcFollowCursor :: Maybe Bool
}

instance Default TooltipCfg where
  def = TooltipCfg {
    _ttcDelay = Nothing,
    _ttcFollowCursor = Nothing
  }

instance Semigroup TooltipCfg where
  (<>) s1 s2 = TooltipCfg {
    _ttcDelay = _ttcDelay s2 <|> _ttcDelay s1,
    _ttcFollowCursor = _ttcFollowCursor s2 <|> _ttcFollowCursor s1
  }

instance Monoid TooltipCfg where
  mempty = def

tooltipDelay :: Int -> TooltipCfg
tooltipDelay ms = def {
  _ttcDelay = Just ms
}

data TooltipState = TooltipState {
  _ttsLastPos :: Point,
  _ttsLastPosTs :: Int
} deriving (Eq, Show, Generic, Serialise)

tooltip :: Text -> WidgetNode s e -> WidgetNode s e
tooltip caption managed = tooltip_ caption managed def

tooltip_ :: Text -> WidgetNode s e -> [TooltipCfg] -> WidgetNode s e
tooltip_ caption managed configs = makeNode widget managed where
  config = mconcat configs
  state = TooltipState def maxBound
  widget = makeTooltip caption config state

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "tooltip" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeTooltip :: Text -> TooltipCfg -> TooltipState-> Widget s e
makeTooltip caption config state = widget where
  baseWidget = createContainer state def {
    containerGetBaseStyle = getBaseStyle,
    containerRestore = restore,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetRender = render
  }

  delay = fromMaybe 1000 (_ttcDelay config)
  followCursor = fromMaybe False (_ttcFollowCursor config)

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.tooltipStyle

  restore wenv oldState oldInfo node = result where
    newNode = node
      & L.widget .~ makeTooltip caption config oldState
    result = resultWidget newNode

  handleEvent wenv target evt node = case evt of
    Leave point -> Just $ resultReqs newNode [RenderOnce] where
      newState = state {
        _ttsLastPos = Point (-1) (-1),
        _ttsLastPosTs = maxBound
      }
      newNode = node
        & L.widget .~ makeTooltip caption config newState
    Move point
      | isPointInNodeVp point node -> Just result where
        path = node ^. L.info . L.path
        prevDisplayed = tooltipDisplayed wenv node
        newState = state {
          _ttsLastPos = point,
          _ttsLastPosTs = wenv ^. L.timestamp
        }
        newNode = node
          & L.widget .~ makeTooltip caption config newState
        delayedRender = RenderEvery path delay (Just 1)
        result
          | not prevDisplayed = resultReqs newNode [delayedRender]
          | prevDisplayed && followCursor = resultReqs node [RenderOnce]
          | otherwise = resultWidget node
    _ -> Nothing

  getSizeReq :: ContainerGetSizeReqHandler s e a
  getSizeReq wenv currState node children = (newReqW, newReqH) where
    child = Seq.index children 0
    newReqW = child ^. L.info . L.sizeReqW
    newReqH = child ^. L.info . L.sizeReqH

  resize :: ContainerResizeHandler s e
  resize wenv viewport renderArea children node = resized where
    resized = (resultWidget node, Seq.singleton (renderArea, renderArea))
  
  render renderer wenv node = do
    forM_ children $ \child -> when (isWidgetVisible child viewport) $
      widgetRender (child ^. L.widget) renderer wenv child

    when tooltipVisible $
      createOverlay renderer $ do
        drawStyledAction renderer rect style $ \textRect ->
          drawStyledText_ renderer textRect style caption
    where
      style = activeStyle wenv node
      children = node ^. L.children
      viewport = node ^. L.info . L.viewport
      mousePos = wenv ^. L.inputStatus . L.mousePos
      textSize = getTextSize wenv style caption
      Size tw th = fromMaybe def (addOuterSize style textSize)
      TooltipState lastPos _ = state
      Point mx my
        | followCursor = mousePos
        | otherwise = lastPos
      dy = 24
      rect = Rect mx (my + dy) tw th
      tooltipVisible = tooltipDisplayed wenv node

  tooltipDisplayed wenv node = displayed where
    TooltipState lastPos lastPosTs = state
    ts = wenv ^. L.timestamp
    viewport = node ^. L.info . L.viewport
    inViewport = pointInRect lastPos viewport
    delayEllapsed = ts - lastPosTs >= delay
    displayed = inViewport && delayEllapsed
