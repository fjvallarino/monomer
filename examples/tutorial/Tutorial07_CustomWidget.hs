{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial07_CustomWidget where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad (forM_)
import Data.Default
import Data.Text (Text)
import Data.Typeable (cast)
import Monomer
import Monomer.Widgets.Single

import qualified Data.Text as T
import qualified Monomer.Lens as L

data CanvasMessage
  = ResetCanvas
  deriving (Eq, Show)

newtype CanvasState = CanvasState {
  _clickedPoints :: [Point]
} deriving (Eq, Show)

makeLenses 'CanvasState

canvas :: WidgetNode s e
canvas = defaultWidgetNode "canvas" newWidget where
  state = CanvasState []
  newWidget = makeCanvas state

makeCanvas :: CanvasState -> Widget s e
makeCanvas state = widget where
  widget = createSingle state def {
    singleMerge = merge,
    singleHandleEvent = handleEvent,
    singleHandleMessage = handleMessage,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  colors = [orange, green, steelBlue, deepPink]
  nextColor idx = colors !! (idx `mod` length colors)

  merge wenv node oldNode oldState = result where
    newNode = node
      & L.widget .~ makeCanvas oldState
    result = resultNode newNode

  handleEvent wenv node target evt = case evt of
    Click point button -> Just result where
      newPoint = subPoint point origin
      newPoints = newPoint : state ^. clickedPoints
      newState = CanvasState newPoints
      newNode = node
        & L.widget .~ makeCanvas newState
      result = resultNode newNode
    Move _ -> Just (resultReqs node [RenderOnce])
    _ -> Nothing
    where
      vp = node ^. L.info . L.viewport
      origin = Point (vp ^. L.x) (vp ^. L.y)

  handleMessage wenv node target msg = case cast msg of
    Just ResetCanvas -> Just result where
      newState = CanvasState []
      newNode = node
        & L.widget .~ makeCanvas newState
      result = resultNode newNode
    _ -> Nothing

  getSizeReq wenv node = (sizeReqW, sizeReqH) where
    sizeReqW = minWidth 100
    sizeReqH = minHeight 100

  render wenv node renderer = do
    drawInTranslation renderer origin $
      forM_ tuples $ \(idx, pointA, pointB) -> do
        setStrokeColor renderer (nextColor idx)
        setStrokeWidth renderer 2
        beginPath renderer
        renderLine renderer pointA pointB
        stroke renderer
    where
      vp = node ^. L.info . L.viewport
      mousePos = wenv ^. L.inputStatus . L.mousePos
      newPoint = subPoint mousePos origin
      origin = Point (vp ^. L.x) (vp ^. L.y)
      clicked = state ^. clickedPoints
      points
        | isPointInNodeVp mousePos node = reverse $ newPoint : clicked
        | otherwise = reverse clicked
      tuples = zip3 [0..] points (drop 1 points)

data AppModel
  = AppModel
  deriving (Eq, Show)

data AppEvent
  = AppResetCanvas
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      button "Reset canvas" AppResetCanvas,
      spacer,
      canvas `key` "mainCanvas" `style` [border 1 gray]
    ] `style` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppResetCanvas -> [Message "mainCanvas" ResetCanvas]

main07 :: IO ()
main07 = do
  simpleApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Tutorial 07 - Custom Widget",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
      ]
    model = AppModel
