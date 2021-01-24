module Monomer.Widgets.Tooltip (
  tooltip,
  tooltip_,
  tooltipDuration
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~), at)
import Control.Monad (forM_, when)
import Data.Default
import Data.Maybe
import Data.Text (Text)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

newtype TooltipCfg = TooltipCfg {
  _ttcDuration :: Maybe Int
}

instance Default TooltipCfg where
  def = TooltipCfg {
    _ttcDuration = Nothing
  }

instance Semigroup TooltipCfg where
  (<>) s1 s2 = TooltipCfg {
    _ttcDuration = _ttcDuration s2 <|> _ttcDuration s1
  }

instance Monoid TooltipCfg where
  mempty = def

tooltipDuration :: Int -> TooltipCfg
tooltipDuration ms = def {
  _ttcDuration = Just ms
}

-- Max width
-- Max height
tooltip :: Text -> WidgetNode s e -> WidgetNode s e
tooltip caption managed = tooltip_ caption managed def

tooltip_ :: Text -> WidgetNode s e -> [TooltipCfg] -> WidgetNode s e
tooltip_ caption managed configs = makeNode widget managed where
  config = mconcat configs
  widget = makeTooltip caption config

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "tooltip" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeTooltip :: Text -> TooltipCfg -> Widget s e
makeTooltip caption config = widget where
  baseWidget = createContainer () def {
    containerGetBaseStyle = getBaseStyle,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetRender = render
  }

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.tooltipStyle

  handleEvent wenv target evt node = case evt of
    Move point
      | isPointInNodeVp point node -> Just $ resultReqs node [RenderOnce]
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
--        drawRect renderer rect ttBgColor ttRadius
--
--        drawStyledText_ renderer rect style caption
--
--        when (isJust ttBorder) $
--          drawRectBorder renderer rect (fromJust ttBorder) ttRadius
    where
      style = activeStyle wenv node
      children = node ^. L.children
      viewport = node ^. L.info . L.viewport
      mousePos = wenv ^. L.inputStatus . L.mousePos
      textSize = getTextSize wenv style caption
      Point mx my = mousePos
      Size tw th = fromMaybe def (addOuterSize style textSize)
      dy = 24
      rect = Rect mx (my + dy) tw th
      tooltipVisible = pointInRect mousePos viewport
      ttBgColor = style ^. L.bgColor
      ttBorder = style ^. L.border
      ttRadius = style ^. L.radius
