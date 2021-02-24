{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Tooltip (
  tooltip,
  tooltip_,
  tooltipDelay,
  tooltipFollow
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
  _ttcFollowCursor :: Maybe Bool,
  _ttcWidth :: Maybe Double,
  _ttcHeight :: Maybe Double
}

instance Default TooltipCfg where
  def = TooltipCfg {
    _ttcDelay = Nothing,
    _ttcFollowCursor = Nothing,
    _ttcWidth = Nothing,
    _ttcHeight = Nothing
  }

instance Semigroup TooltipCfg where
  (<>) s1 s2 = TooltipCfg {
    _ttcDelay = _ttcDelay s2 <|> _ttcDelay s1,
    _ttcFollowCursor = _ttcFollowCursor s2 <|> _ttcFollowCursor s1,
    _ttcWidth = _ttcWidth s2 <|> _ttcWidth s1,
    _ttcHeight = _ttcHeight s2 <|> _ttcHeight s1
  }

instance Monoid TooltipCfg where
  mempty = def

instance CmbWidth TooltipCfg where
  width w = def {
    _ttcWidth = Just w
  }

instance CmbHeight TooltipCfg where
  height h = def {
    _ttcHeight = Just h
  }

tooltipDelay :: Int -> TooltipCfg
tooltipDelay ms = def {
  _ttcDelay = Just ms
}

tooltipFollow :: TooltipCfg
tooltipFollow = def {
  _ttcFollowCursor = Just True
}

data TooltipState = TooltipState {
  _ttsLastPos :: Point,
  _ttsLastPosTs :: Int
} deriving (Eq, Show, Generic, Serialise)

instance WidgetModel TooltipState where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

tooltip :: Text -> WidgetNode s e -> WidgetNode s e
tooltip caption managed = tooltip_ caption def managed

tooltip_ :: Text -> [TooltipCfg] -> WidgetNode s e -> WidgetNode s e
tooltip_ caption configs managed = makeNode widget managed where
  config = mconcat configs
  state = TooltipState def maxBound
  widget = makeTooltip caption config state

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "tooltip" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeTooltip :: Text -> TooltipCfg -> TooltipState -> Widget s e
makeTooltip caption config state = widget where
  baseWidget = createContainer state def {
    containerAddStyleReq = False,
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
        widgetId = node ^. L.info . L.widgetId
        prevDisplayed = tooltipDisplayed wenv node
        newState = state {
          _ttsLastPos = point,
          _ttsLastPosTs = wenv ^. L.timestamp
        }
        newNode = node
          & L.widget .~ makeTooltip caption config newState
        delayedRender = RenderEvery widgetId delay (Just 1)
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
  resize wenv viewport children node = resized where
    resized = (resultWidget node, Seq.singleton viewport)

  render renderer wenv node = do
    forM_ children $ \child ->
      widgetRender (child ^. L.widget) renderer wenv child

    when tooltipVisible $
      createOverlay renderer $ do
        drawStyledAction renderer rect style $ \textRect ->
          forM_ textLines (drawTextLine renderer style)
    where
      style = activeStyle wenv node
      children = node ^. L.children
      mousePos = wenv ^. L.inputStatus . L.mousePos
      scOffset = wenv ^. L.offset
      isDragging = isJust (wenv ^. L.dragStatus)
      maxW = wenv^. L.windowSize . L.w
      maxH = wenv^. L.windowSize . L.h
      targetW = fromMaybe maxW (_ttcWidth config)
      targetH = fromMaybe maxH (_ttcHeight config)
      targetR = Rect 0 0 targetW targetH
      fittedLines = fitTextToRect renderer style Ellipsis MultiLine TrimSpaces
        Nothing targetR caption
      textSize = getTextLinesSize fittedLines
      Size tw th = fromMaybe def (addOuterSize style textSize)
      TooltipState lastPos _ = state
      Point mx my
        | followCursor = addPoint scOffset mousePos
        | otherwise = addPoint scOffset lastPos
      rx
        | wenv ^. L.windowSize . L.w - mx > tw = mx
        | otherwise = wenv ^. L.windowSize . L.w - tw
      -- Add offset to have space between the tooltip and the cursor
      ry
        | wenv ^. L.windowSize . L.h - my > th = my + 20
        | otherwise = my - th - 5
      rect = Rect rx ry tw th
      textLines = alignTextLines style rect fittedLines
      tooltipVisible = tooltipDisplayed wenv node && not isDragging

  tooltipDisplayed wenv node = displayed where
    TooltipState lastPos lastPosTs = state
    ts = wenv ^. L.timestamp
    viewport = node ^. L.info . L.viewport
    inViewport = pointInRect lastPos viewport
    delayEllapsed = ts - lastPosTs >= delay
    displayed = inViewport && delayEllapsed
